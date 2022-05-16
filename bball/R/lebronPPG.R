#' Points Per Game
#'
#' This function will return the points per game for player "LeBron James" for any of the seasons that he has played (2004 - 2017).
#' @param season a year of interest.
#' @return points per game (taking the total points (PTS) scored in a season divided by the total games (G) played in that season)
#' @export
#' @examples
#' lebronPPG()

lebronPPG <- function(season) {
  nba$PPG <- (nba$PTS / nba$G)
  if(season >= 2004 && season <= 2017) {
    nba %>%
      group_by(Player) %>%
      select(Year, G, PTS, PPG) %>%
      filter(Player == "LeBron James" & Year == season)
  }
  else {
    print("Error: LeBron did not play in this season! Please try a season between 2004 and 2017.")
  }
}