# Simplified Data Master for Shot Plotter
# This version handles missing files gracefully

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

# Try to load team data, but don't fail if files don't exist
w_non_conf_opponents25 <- tryCatch({
  read.csv("./Team Data/w_non_conf_opponents.csv", stringsAsFactors = FALSE)$Team
}, error = function(e) {
  c("Opponent 1", "Opponent 2", "Opponent 3")
})

m_non_conf_opponents25 <- tryCatch({
  read.csv("./Team Data/m_non_conf_opponents.csv", stringsAsFactors = FALSE)$Team
}, error = function(e) {
  c("Opponent 1", "Opponent 2", "Opponent 3")
})

w_roster25 <- tryCatch({
  read.csv("./Team Data/w_chicago_roster.csv", stringsAsFactors = FALSE)$Player
}, error = function(e) {
  paste("Player", LETTERS[1:10])
})

m_roster25 <- tryCatch({
  read.csv("./Team Data/m_chicago_roster.csv", stringsAsFactors = FALSE)$Player
}, error = function(e) {
  paste("Player", 1:10)
})

region_labels <- tryCatch({
  read.csv("./Team Data/region_labels.csv", stringsAsFactors = FALSE)
}, error = function(e) {
  data.frame(Region = character(), Label = character())
})

shading_df <- tryCatch({
  read.csv("./Team Data/shading.csv", stringsAsFactors = FALSE)
}, error = function(e) {
  data.frame()
})

uaa_teams_csv <- tryCatch({
  read.csv("./Team Data/uaa_teams.csv", stringsAsFactors = FALSE)
}, error = function(e) {
  data.frame(Team = c("Brandeis", "Carnegie Mellon", "Case Western", "Chicago", 
                      "Emory", "NYU", "Rochester", "WashU"))
})
uaa_teams <- uaa_teams_csv$Team

extract_player <- function(action) {
  raw_player <- str_split(action, " by ")[[1]][2] 
  
  first_name <- str_split(raw_player, ",")[[1]][2]
  last_name <- str_split(raw_player, ",")[[1]][1]
  
  return( str_to_title(paste(first_name, last_name)) )
}

threshold <- as.Date("2024-09-01", format="%Y-%m-%d")

date_filter_helper <- function(dates, y=2024) {
  dates <- as.Date(dates, format="%Y-%m-%d")
  
  if (y == 2024) {
    return( dates[dates < threshold] )
  } else {
    return( dates[dates >= threshold] )
  }
}
