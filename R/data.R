#' EQA Data for Haemolysis Index Across Two Different Laboratories
#'
#' Measurements of the haemolysis index of EQA samples from six analysers across
#' two laboratories, and the mean result from all users of the same method in
#' the EQA scheme.
#'
#' @format ## `hidx_eqa`
#' A data frame with 150 rows and 4 columns:
#' \describe{
#'   \item{sample}{EQA Samples are numbered 1 to 26}
#'   \item{date}{The date the EQA distribution closed}
#'   \item{h_index}{The H index. The index approximates the concentration of haemoglobin in µmol/L.}
#'   \item{analyser}{The analyser name, or "EQA Method Mean".}
#' }
#' @source EQA data for the author's local laboratory
"hidx_eqa"
