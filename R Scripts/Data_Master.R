# Data_Master.R - Master data constants and helpers

trad_cols = c("+/-", "PTS", 
              "FGM-A", "FG%", "3PM-A", "3P%", "FTM-A", "FT%", 
              "OREB", "DREB", "REB", "TO")
adv_cols = c("POSS", "Poss. Length", 
             "NET", "PPP", "FT-R", "3P-R", 
             "ORB%", "DRB%", "REB%", 
             "AST%", "TO%")
misc_cols = c("PTS", 
              "Points off Turnovers", "POT %", 
              "2nd Chance Points", "2ND %", 
              "Points in the Paint", "PITP %", 
              "Fastbreak Points", "FB %",
              "Bench Points", "BENCH %")

player_cols = c("PTS", "FGM-A", "FG%", 
                "3PM-A", "3P%", "3P-R", "FTM-A", "FT%", "FT-R",
                "OREB", "DREB", "REB", "AST", "TO", "AST/TO", 
                "STL", "BLK", "PF")

# Read team data files with error handling
w_non_conf_opponents25 <- tryCatch({
  read_csv("./Team Data/w_non_conf_opponents.csv", show_col_types = FALSE)$Team
}, error = function(e) c())

m_non_conf_opponents25 <- tryCatch({
  read_csv("./Team Data/m_non_conf_opponents.csv", show_col_types = FALSE)$Team
}, error = function(e) c())

w_roster25 <- tryCatch({
  read_csv("./Team Data/w_chicago_roster.csv", show_col_types = FALSE)$Player
}, error = function(e) c())

m_roster25 <- tryCatch({
  read_csv("./Team Data/m_chicago_roster.csv", show_col_types = FALSE)$Player
}, error = function(e) c())

region_labels <- tryCatch({
  read_csv("./Team Data/region_labels.csv", show_col_types = FALSE)
}, error = function(e) data.frame())

shading_df <- tryCatch({
  read_csv("./Team Data/shading.csv", show_col_types = FALSE)
}, error = function(e) data.frame())

uaa_teams_csv <- tryCatch({
  read_csv("./Team Data/uaa_teams.csv", show_col_types = FALSE)
}, error = function(e) data.frame(Team = c()))

uaa_teams = if (nrow(uaa_teams_csv) > 0) uaa_teams_csv$Team else c()

extract_player <- function(action) {
  raw_player <- str_split(action, " by ")[[1]][2] 
  
  first_name <- str_split(raw_player, ",")[[1]][2]
  last_name <- str_split(raw_player, ",")[[1]][1]
  
  return( str_to_title(paste(first_name, last_name)) )
}

threshold = as.Date("2024-09-01", format="%Y-%m-%d")

date_filter_helper = function(dates, y=2024) {
  dates =
    dates %>%
    as.Date(format="%Y-%m-%d")
  
  if (y == 2024) {
    return( dates[(dates < threshold)] )
  } else {
    return( dates[(dates >= threshold)] )
  }
}
