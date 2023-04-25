library(rlang)
library(dplyr)
library(ggplot2)
library(glue)

## TODO: Fix x-axis labels when comparison is not mean

#' Draw a difference plot (aka a Bland-Altman plot)
#'
#' @param data A data frame, where each row is a measurement of a sample by an
#'   analyser.
#' @param sample_col The name of a column in `data` that identifies each sample.
#' @param result_col The name of a column in `data` that identifies the result.
#' @param analyser_col The name of a column in `data` that identifies the analyser.
#' @param difference A string, either "absolute" (default) or "relative".
#'   Whether the difference on the y-axis is plotted in the reported units, or
#'   as a % of the comparison.
#' @param comparison A string, either "mean" (default) or a value from the
#'   `analyser_col` column in `data`. If "mean", then x-axis will be the mean of
#'   all results for each sample. If it is an analyser, then the x-axis will be
#'   the result for that analyser,and no points will be plotted for that
#'   analyser (because the difference will always be zero).
#' @param title A string.
#' @param subtitle A string.
#' @param units A string.
#'
#' @return A ggplot2 object
#' @importFrom rlang .data
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
#' ## Plot the absolute difference (default) against the mean result (default)
#' difference_plot(hidx_eqa,
#'                 sample_col = sample,
#'                 result_col = h_index,
#'                 analyser_col = analyser)
#'
#' ## Plot the relative difference against the EQA method mean (which we've
#' ## recorded as if it were an analyser)
#' difference_plot(hidx_eqa,
#'                 sample_col = sample,
#'                 result_col = h_index,
#'                 analyser_col = analyser,
#'                 comparison = "EQA Method Mean",
#'                 difference = "relative")

difference_plot <- function(data, sample_col, result_col, analyser_col,
                         difference = c("absolute", "relative"),
                         comparison = "mean",
                         title = "",
                         subtitle = "",
                         units = NULL) {
  # Check arguments
  difference <- rlang::arg_match(difference)

  # Calculate target
  if (comparison == "mean") {
    ba_data <- data %>%
      dplyr::group_by({{sample_col}}) %>%
      dplyr::mutate(.target = mean({{result_col}}))
  } else if (comparison %in% dplyr::pull(data, {{analyser_col}})) {
    targets <- data %>%
      dplyr::filter({{analyser_col}} == comparison) %>%
      dplyr::select({{sample_col}}, .target = {{result_col}})
    ba_data <- data %>%
      dplyr::filter({{analyser_col}} != comparison) %>%
      dplyr::left_join(targets,
                by = rlang::englue("{{sample_col}}"))
  } else {
    rlang::abort("`comparison` must be a character vector with the value 'mean', or a value in the `analyser_col` column.")
  }

  # Remove results without a target
  if (any(is.na(ba_data$.target))) {
    rlang::warn(glue::glue("{sum(is.na(ba_data$.target))} observation with no target have been removed."))
    ba_data <- ba_data %>%
      dplyr::filter(!is.na(.data$.target))
  }

  # Remove result with a target of 0 if doing a relative difference
  # (n / 0 = inf.)
  if (difference == "relative" && any(ba_data$.target == 0)) {
    rlang::warn(glue::glue("{sum(ba_data$.target == 0)} observations with a target of 0 have been removed."))
    ba_data <- ba_data %>%
      dplyr::filter(.data$.target != 0)
  }

  # Calculate difference
  if(difference == "absolute") {
    ba_data <- ba_data %>%
      dplyr::group_by({{sample_col}}) %>%
      dplyr::mutate(
        .result_diff = {{result_col}} - .data$.target)
  } else { # difference == "relative"
    ba_data <- ba_data %>%
      dplyr::group_by({{sample_col}}) %>%
      dplyr::mutate(
        .result_diff = ({{result_col}} - .data$.target) / .data$.target)
  }

  # ba_summary <- ba_data %>%
  #   dplyr::group_by({{analyser_col}}) %>%
  #   dplyr::summarise(tidy(t.test(.data$.result_diff)),
  #             stdev = sd(.data$.result_diff),
  #             loa.low = estimate - 2 * stdev,
  #             loa.high = estimate + 2 * stdev)
  # nudge_dist <- (max(ba_data$.result_diff) - min(ba_data$.result_diff)) / 50
  # label_x_pos <- max(ba_data$.target)
  #
  # ba_lines <-
  #   ba_summary %>%
  #   dplyr::select(estimate, loa.low, loa.high) %>%
  #   tidyr::pivot_longer(cols = everything(),
  #                names_to = "type",
  #                values_to = "value") %>%
  #   tidyr::drop_na(value) %>%
  #   dplyr::mutate(type = recode(type, estimate = "Mean",
  #                        loa.low = "Limits of agreement",
  #                        loa.high = "Limits of agreement"),
  #          label = formatC(signif(value, 2),
  #                          digits = 2,
  #                          format = "fg",
  #                          flag = "+#"))

  ba_plot <- ggplot2::ggplot(data = ba_data) +
    ggplot2::geom_hline(yintercept = 0, colour = "black", alpha= 1.0) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(x = .data$.target,
                             y = .data$.result_diff,
                             colour = {{analyser_col}}),
      shape = 15,
      alpha = 0.6) +
  ggplot2::theme_bw()

  # Create title
  if (!missing(title)) {
    ba_plot <- ba_plot +
      ggplot2::labs(title = title)
  }
  if (!missing(subtitle)) {
    ba_plot <- ba_plot +
      ggplot2::labs(subtitle = subtitle)
  }

  # x axis
  if (comparison == "mean") {
    x_label <- "Mean"
  } else {
    x_label <- glue::glue("{comparison} result")
  }
  if (!missing(units)) {
    x_label <- glue::glue("{x_label} ({units})")
  }
  ba_plot <- ba_plot +
    ggplot2::xlab(x_label)

  # y axis
  y_label <- "Difference"
  if(difference == "absolute") {
    if (!missing(units)) {
      y_label <- glue::glue("{y_label} ({units})")
    }
  } else { # difference == "relative"
    ba_plot <- ba_plot +
      ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 100, accuracy = 1))
    y_label <- glue::glue("{y_label} (%)")
  }
  ba_plot <- ba_plot +
    ggplot2::ylab(y_label)

  ba_plot
}
