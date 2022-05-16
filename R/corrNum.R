#' Cor numeric
#'
#' Takes all the numeric columns in a dataset and returns a correlation plot
#' @param season a year of interest
#' @return correlation plot of the numeric variables of a given data set
#' @export
#' @examples
#' corrNum()

corrNum <- function(season) {
  data <- select_if(nba, is.numeric)
  data <- data[data$year == season, ]
  cor_data <- cor(data, use="pairwise.complete.obs")
  corrplot(cor_data, type = "upper")
}