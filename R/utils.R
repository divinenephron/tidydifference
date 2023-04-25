library(rlang)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(broom)
library(ggplot2)
library(glue)
library(snakecase)

## read_pathmanager
# Reads citm csv files, and cleans them by:
# - Creating numeric results.
# - Creating a column indicating censored results.
# - (optional) Censoring results using the "lower_limit" and "upper_limit"
#   columns of test_info.
# - (optional) Rounding results to the number of decimal places given in the
#   digits column of test_info.
# - (optional) Adding on any additional colums in test_info by matching them
#   using the winpath_code column (useful for mapping codes used by other
#   systems).
# 
# Columns required in the CSV file:
# - Test Code
# - Result Text
# - Specimen Number
read_pathmanager <- function(f, test_info = NULL) {
  f %>%
    read_csv(
      col_types = cols(
        `Specimen Number` = col_character(),
        `Test Code` = col_character(),
        `Result Text` = col_character(),
        .default = col_character()
      ),
      na = c(""),
      id = "file",
      lazy = TRUE) %>%
    rename_with(to_snake_case) %>%
    rename_with(~ str_replace(.x, "&", "and")) %>%
    rename(sample = specimen_number,
           winpath_code = test_code,
           result = result_text) %>%
    mutate(sample = str_sub(sample, 2, 11)) %>%
    clean_results(test_info, join_by = "winpath_code")
}

## read_citm
# See read_pathmanager for features
#
# Columns required in the CSV file:
# - Test
# - Result
# - Sample ID
read_citm <- function(f, test_info = NULL) {
  data <- f %>%
    read_delim(delim = ";",
               col_types = cols(
                 `Test` = col_character(),
                 `Result` = col_character(),
                 `Sample ID` = col_character(),
                 .default = col_character(),
               ),
               na = c(""),
               id = "file")
  
  
  if (" " %in% colnames(data)) {
    data <- data %>%
      rename(`status` = ` `)
  }
  
  data <- data %>%
    rename_with(to_snake_case) %>%
    rename(
      sample = sample_id,
      citm_code = test) %>%
    clean_results(test_info, join_by = "citm_code")
  
  data
}


## read_datamanager
# See read_pathmanager for features.
#
# Columns required in the CSV file:
# - Test
# - Result
# - Sample ID
read_datamanager <- function(f, test_info = NULL) {
  f %>%
    map_dfr(read_xls, .id = "file", na = c("")) %>%
    rename_with(to_snake_case) %>%
    rename(sample = sample_id,
           datamanager_code = test) %>%
    mutate(datamanager_code =
             str_extract(
               datamanager_code,
               "^[^(]+(?= \\([^)]+\\)$)")) %>%
    clean_results(test_info, join_by = "datamanager_code")
}

## clean_result()
clean_results <- function (data, test_info = NULL, join_by = NULL) {
  data <- data %>%
    mutate(
      result_original = result,
      result_censored = str_extract(result, "^[<>]"),
      result = as.numeric(str_extract(result, "(?<=^[<>]?)[\\d.]+")))
  
  if (!is.null(test_info) & !is.null(join_by)) {
    data <- data %>%
      left_join(test_info,
                by = join_by)
    if("lower_limit" %in% colnames(data)) {
      data <- data %>%
        mutate(
          result_censored := if_else(result <= lower_limit, "<", result_censored),
          result = if_else(result <= lower_limit, lower_limit, result))
    }
    if("upper_limit" %in% colnames(data)) {
      data <- data %>%
        mutate(
          result_censored := if_else(result >= upper_limit, ">", result_censored),
          result = if_else(result >= upper_limit, upper_limit, result))
    }
    if("digits" %in% colnames(data)) {
      data <- data %>%
        mutate(
          result = round(result, digits))
    }
    # Remove some of the columns from test_info
    data <- data %>%
      select(-any_of(c("lower_limit", "upper_limit", "digits")))
  }
  data
}

## bland_altman
bland_altman <- function(data, candidate_result, comparison_result,
                         type = c("absolute", "relative"),
                         title = "", candidate_name = NULL, comparison_name = NULL,
                         units = NULL) {
  type <- match.arg(type)

  if(type == "absolute") {
    ba_data <- data %>%
      ungroup() %>%
      transmute(
        .result_mean = ({{candidate_result}} + {{comparison_result}}) / 2,
        .result_diff = {{candidate_result}} - {{comparison_result}})
  } else { # type == "relative"
    ba_data <- data %>%
      transmute(
        .result_mean = ({{candidate_result}} + {{comparison_result}}) / 2,
        .result_diff = (({{candidate_result}} - {{comparison_result}}) / .result_mean) * 100)
  }

  ba_summary <- ba_data %>%
    summarise(tidy(t.test(.result_diff)),
              stdev = sd(.result_diff),
              loa.low = estimate - 2 * stdev,
              loa.high = estimate + 2 * stdev)
  nudge_dist <- (max(ba_data$.result_diff) - min(ba_data$.result_diff)) / 50
  label_x_pos <- max(ba_data$.result_mean)

  ba_lines <- 
    ba_summary %>%
    select(estimate, loa.low, loa.high) %>%
    pivot_longer(cols = everything()) %>%
    mutate(type = c("Mean", "Limits of agreement", "Limits of agreement"),
           label = formatC(signif(value, 2), digits = 2, format = "fg", flag = "+#"))
  
  ba_plot <- ggplot(data = ba_data) +
    geom_hline(yintercept = 0, colour = "black", alpha= 1.0) +
    geom_point(
      mapping = aes(x = .result_mean,
                    y = .result_diff),
      shape = 15,
      alpha = 0.6) +
    geom_rect(
      data = ba_summary,
      mapping = aes(ymin = conf.low, ymax = conf.high,
                    xmin = -Inf, xmax = Inf,
                    fill = "Confidence interval")) +
    scale_fill_manual(
      name = NULL,
      values = alpha("blue", 0.1),
      guide = guide_legend(override.aes = list(colour = NA))) +
    geom_hline(
      data = ba_lines,
      mapping = aes(yintercept = value,
                    linetype = type),
      show.legend = TRUE,
      colour = "blue") +
    scale_linetype_manual(
      name = NULL,
      values = setNames(c(1, 2), c("Mean", "Limits of agreement"))) +
    labs(caption = glue("n = {nrow(ba_data)}")) +
    geom_text(
      data = ba_lines,
      mapping = aes(
        label = label,
        y = value,
        x = label_x_pos),
      nudge_y = nudge_dist,
      vjust = "bottom",
      hjust = "right",
      colour = "blue"
    )  +
  theme_bw()
  
  # Create title
  if (!missing(title)) {
    ba_plot <- ba_plot + labs(title = title)
  }
  
  # Create axis labels
  if (!missing(candidate_name) & !missing(comparison_name)) {
    x_label <- glue("Average of {candidate_name} and {comparison_name}")
    y_label <- glue("Difference: {candidate_name} - {comparison_name}")
  } else {
    x_label <- "Mean"
    y_label <- "Difference"
  }
  if(type == "absolute") {
    if (!missing(units)) {
      y_label <- glue("{y_label} ({units})")
    }
  } else { # type == "relative"
    ba_plot <- ba_plot +
      scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1))
    y_label <- glue("{y_label} (%)")
  }
  if (!missing(units)) {
    x_label <- glue("{x_label} ({units})")
  }
  
  ba_plot <- ba_plot +
    xlab(x_label) +
    ylab(y_label)
  
  ba_plot
}

## TODO: Fix x-axis labels when comparison is not mean

## bland_altman
# Arguments:
# - data - A data frame, where each row is a measurement of a sample by an
#          analyser.
# - sample_col - The name of a column in `data` that identifies each sample.
# - result_col - The name of a column in `data` that identifies the result.
# - analyser_col - The name of a column in `data` that identifies the
#                  analyser.
# - difference - A string, either "absolute" (default) or "relative". Whether
#                the difference on the y-axis is plotted in the reported units,
#                or as a % of the comparison.
# - comparison - A string, either "mean" (default) or a value from
#                the `analyser_col` column in `data`. If "mean", then x-axis
#                will be the mean of all results for each sample. If it is an
#                analyser, then the x-axis will be the result for that analyser,
#                and no points will be plotted for that analyser (because the
#                difference will always be zero).
# - title - A string.
# - units - A string.
# - subtitle - A string.
bland_altman_2 <- function(data, sample_col, result_col, analyser_col,
                         difference = c("absolute", "relative"),
                         comparison = "mean",
                         title = "",
                         subtitle = "",
                         units = NULL) {
  # Check arguments
  difference <- arg_match(difference)
  
  # Calculate target
  if (comparison == "mean") {
    ba_data <- data %>%
      group_by({{sample_col}}) %>%
      mutate(.target = mean({{result_col}}))
  } else if (comparison %in% pull(data, {{analyser_col}})) {
    targets <- data %>%
      filter({{analyser_col}} == comparison) %>%
      select({{sample_col}}, .target = {{result_col}})
    ba_data <- data %>%
      filter({{analyser_col}} != comparison) %>%
      left_join(targets,
                by = englue("{{sample_col}}"))
  } else {
    abort("`comparison` must be a character vector with the value 'mean', or a value in the `analyser_col` column.")
  }
  
  # Remove results without a target
  if (any(is.na(ba_data$.target))) {
    warn(glue("{sum(is.na(ba_data$.target))} observation with no target have been removed."))
    ba_data <- ba_data %>%
      filter(!is.na(.target))
  }
  
  # Remove result with a target of 0 if doing a relative difference
  # (n / 0 = inf.)
  if (difference == "relative" && any(ba_data$.target == 0)) {
    warn(glue("{sum(ba_data$.target == 0)} observations with a target of 0 have been removed."))
    ba_data <- ba_data %>%
      filter(.target != 0)
  }
  
  # Calculate difference
  if(difference == "absolute") {
    ba_data <- ba_data %>%
      group_by({{sample_col}}) %>%
      mutate(
        .result_diff = {{result_col}} - .target)
  } else { # difference == "relative"
    ba_data <- ba_data %>%
      group_by({{sample_col}}) %>%
      mutate(
        .result_diff = ({{result_col}} - .target) / .target)
  }
  
  # ba_summary <- ba_data %>%
  #   group_by({{analyser_col}}) %>%
  #   summarise(tidy(t.test(.result_diff)),
  #             stdev = sd(.result_diff),
  #             loa.low = estimate - 2 * stdev,
  #             loa.high = estimate + 2 * stdev)
  # nudge_dist <- (max(ba_data$.result_diff) - min(ba_data$.result_diff)) / 50
  # label_x_pos <- max(ba_data$.target)
  #
  # ba_lines <-
  #   ba_summary %>%
  #   select(estimate, loa.low, loa.high) %>%
  #   pivot_longer(cols = everything(),
  #                names_to = "type",
  #                values_to = "value") %>%
  #   drop_na(value) %>%
  #   mutate(type = recode(type, estimate = "Mean",
  #                        loa.low = "Limits of agreement",
  #                        loa.high = "Limits of agreement"),
  #          label = formatC(signif(value, 2),
  #                          digits = 2,
  #                          format = "fg",
  #                          flag = "+#"))  

  ba_plot <- ggplot(data = ba_data) +
    geom_hline(yintercept = 0, colour = "black", alpha= 1.0) +
    geom_point(
      mapping = aes(x = .target,
                    y = .result_diff,
                    colour = {{analyser_col}}),
      shape = 15,
      alpha = 0.6) +
  theme_bw()
  
  # Create title
  if (!missing(title)) {
    ba_plot <- ba_plot + labs(title = title)
  }
  if (!missing(subtitle)) {
    ba_plot <- ba_plot + labs(subtitle = subtitle)
  }
  
  # x axis
  if (comparison == "mean") {
    x_label <- "Mean"
  } else {
    x_label <- glue("{comparison} result")
  }
  if (!missing(units)) {
    x_label <- glue("{x_label} ({units})")
  }
  ba_plot <- ba_plot +
    xlab(x_label)
  
  # y axis
  y_label <- "Difference"
  if(difference == "absolute") {
    if (!missing(units)) {
      y_label <- glue("{y_label} ({units})")
    }
  } else { # difference == "relative"
    ba_plot <- ba_plot +
      scale_y_continuous(labels = scales::label_percent(scale = 100, accuracy = 1))
    y_label <- glue("{y_label} (%)")
  }
  ba_plot <- ba_plot +
    ylab(y_label)
  
  ba_plot
}

tidy.MCResultAnalytical <- function(x) {
  getCoefficients(x) %>%
    as_tibble(rownames = "term") %>%
    rename(estimate = EST,
           std.error = SE,
           conf.low = LCI,
           conf.high = UCI)
}

## passing_bablok
plot_pb <- function(code, test_name, ...) {
  d <- comparison_data %>% 
    filter(winpath_code == code,
           is.na(result_l12_censored),
           is.na(result_l3_censored),
           !is.na(result_l3),
           !is.na(result_l12),
           ...)
  units <- d$units[1]
  
  m <- mcreg(x = d$result_l12, y = d$result_l3, method.reg = "PaBa",
             method.ci = "analytical", slope.measure = "radian") %>%
    tidy()
  
  ggplot(data = d,
         aes(x = result_l12,
             y = result_l3)) +
    geom_abline(
      intercept = 0,
      slope = 1,
      colour = "black",
      alpha = 0.5
    ) +
    geom_point(
      shape = 15,
      alpha = 0.6) +
    geom_abline(
      intercept = m$estimate[1],
      slope = m$estimate[2],
      colour = "BLUE") +
    scale_linetype_manual(
      name = NULL,
      values = c(1)) +
    coord_fixed() +
    theme_bw() +
    labs(
      title = glue("Passing-Bablok regression for {test_name}"),
      caption = glue("n = {nrow(d)}")) +
    xlab(glue("Line 1&2 ({units})")) +
    ylab(glue("Line 3 ({units})")) +
    annotate(
      "text", x = min(d$result_l3), y = max(d$result_l3),
      vjust = "top",
      hjust = "left",
      colour = "blue",
      label = paste0("Intercept: ",
                     formatC(signif(m$estimate[1], 2), digits = 2, format = "fg", flag = "#"),
                     "\nSlope: ",
                     formatC(round(m$estimate[2], digits = 3), digits = 3, format = "f"))
    )
}

## plot_abs_ba
plot_abs_ba <- function(code, test_name, ...) {
  d <- comparison_data %>% 
    filter(winpath_code == code,
           is.na(result_l12_censored),
           is.na(result_l3_censored),
           !is.na(result_l3),
           !is.na(result_l12),
           ...)
  units <- d$units[1]
  d %>%
    bland_altman(result_l3, result_l12, 
                 title = glue("Absolute Bland-Altman plot for {test_name}"),
                 candidate_name = "Line 3",
                 comparison_name = "Line 1&2",
                 units = units,
                 type = "absolute") %>%
    print()
}

## plot_rel_ba
plot_rel_ba <- function(code, test_name, ...) {
  d <- comparison_data %>% 
    filter(winpath_code == code,
           is.na(result_l12_censored),
           is.na(result_l3_censored),
           !is.na(result_l3),
           !is.na(result_l12),
           ...)
  units <- d$units[1]
  d %>%
    bland_altman(result_l3, result_l12, 
                 title = glue("Relative Bland-Altman plot for {test_name}"),
                 candidate_name = "Line 3",
                 comparison_name = "Line 1&2",
                 units = units,
                 type = "relative") %>%
    print()
}