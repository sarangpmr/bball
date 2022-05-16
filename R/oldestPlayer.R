#' Oldest player
#'
#' Returns the oldest player for any given season. If there are multiple older players, then the first alphabetically listed player will be returned.
#' @param season a year of interest
#' @return oldest player for a given year
#' @export
#' @examples
#' oldestPlayer()

oldestPlayer <- function(season) {
  nba %>%
    group_by(Year) %>%
    select(Year, Player, Age) %>%
    filter(Age == max(Age) & Year == season) %>%
    arrange(Player) %>%
    slice(1)
}