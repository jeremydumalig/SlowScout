# Data24.R - 2024 Season Data (Simplified for fast startup)

# Read CSV files with tryCatch for safety
raw_player_box24 <- tryCatch({
  read_csv("./player_box24.csv", show_col_types = FALSE)
}, error = function(e) data.frame())

raw_team_box24 <- tryCatch({
  read_csv("./team_box24.csv", show_col_types = FALSE)
}, error = function(e) data.frame())

# Process player_box24 only if data exists
player_box24 <- if (nrow(raw_player_box24) > 0) {
  raw_player_box24 %>%
    merge(uaa_teams_csv, by="Team", all.x=T) %>%
    separate(`FGM-A`, into = c("FGM", "FGA"), sep = "-") %>%
    separate(`3PM-A`, into = c("3PM", "3PA"), sep = "-") %>%
    separate(`FTM-A`, into = c("FTM", "FTA"), sep = "-") %>%
    mutate(FGM = as.integer(FGM),
           FGA = as.integer(FGA),
           `3PM` = as.integer(`3PM`),
           `3PA` = as.integer(`3PA`),
           `2PM` = FGM - `3PM`,
           `2PA` = FGA - `3PA`,
           FTM = as.integer(FTM),
           FTA = as.integer(FTA),
           `FG%` = 100 * FGM / FGA,
           `3P%` = 100 * `3PM` / `3PA`,
           `FT%` = 100 * FTM / FTA,
           PPS = PTS / (FGA + 0.44*FTA),
           `AST/TO` = AST / TO,
           `FT-R` = 100 * FTA / FGA,
           `3P-R` = 100 * `3PA` / `FGA`,
           across(where(is.numeric), ~ round(., 1)),
           `FGM-A` = paste(as.character(FGM), as.character(FGA), sep="-"),
           `3PM-A` = paste(as.character(`3PM`), as.character(`3PA`), sep="-"),
           `FTM-A` = paste(as.character(FTM), as.character(FTA), sep="-"))
} else {
  data.frame()
}

# Process team_box24 only if data exists
team_box24 <- if (nrow(raw_team_box24) > 0) {
  raw_team_box24 %>%
    merge(uaa_teams_csv, by="Team", all.x=T) %>%
    merge(select(raw_team_box24,
                 League, Date, Opponent, 
                 PTS, `FGM-A`, `3PM-A`, `FTM-A`, OREB, DREB, REB, TO),
          by.x = c("League", "Date", "Team"),
          by.y = c("League", "Date", "Opponent"),
          suffixes = c("", "_OPP")) %>%
    separate(`FGM-A`, into = c("FGM", "FGA"), sep = "-") %>%
    separate(`3PM-A`, into = c("3PM", "3PA"), sep = "-") %>%
    separate(`FTM-A`, into = c("FTM", "FTA"), sep = "-") %>%
    separate(`FGM-A_OPP`, into = c("FGM_OPP", "FGA_OPP"), sep = "-") %>%
    separate(`3PM-A_OPP`, into = c("3PM_OPP", "3PA_OPP"), sep = "-") %>%
    separate(`FTM-A_OPP`, into = c("FTM_OPP", "FTA_OPP"), sep = "-") %>%
    mutate(FGM = as.integer(FGM),
           FGA = as.integer(FGA),
           `3PM` = as.integer(`3PM`),
           `3PA` = as.integer(`3PA`),
           `2PM` = FGM - `3PM`,
           `2PA` = FGA - `3PA`,
           FTM = as.integer(FTM),
           FTA = as.integer(FTA),
           FGM_OPP = as.integer(FGM_OPP),
           FGA_OPP = as.integer(FGA_OPP),
           `3PM_OPP` = as.integer(`3PM_OPP`),
           `3PA_OPP` = as.integer(`3PA_OPP`),
           `2PM_OPP` = FGM_OPP - `3PM_OPP`,
           `2PA_OPP` = FGA_OPP - `3PA_OPP`,
           FTM_OPP = as.integer(FTM_OPP),
           FTA_OPP = as.integer(FTA_OPP),
           `FG%` = 100 * FGM / FGA,
           `3P%` = 100 * `3PM` / `3PA`,
           `FT%` = 100 * FTM / FTA,
           POSS = 0.96 * (FGA + 0.44*FTA + TO - OREB),
           POSS_OPP = 0.96 * (FGA_OPP + 0.44*FTA_OPP + TO_OPP - OREB_OPP),
           `+/-` = PTS - PTS_OPP,
           `AST%` = 100 * AST / TO,
           `FT-R` = 100 * FTA / FGA,
           `3P-R` = 100 * `3PA` / `FGA`,
           `ORB%` = 100 * OREB / (OREB + DREB_OPP),
           `DRB%` = 100 * DREB / (DREB + OREB_OPP),
           `REB%` = 100 * REB / (REB + REB_OPP),
           `TO%` = 100 * TO / POSS,
           `POT %` = 100 * `Points off Turnovers` / PTS,
           `2ND %` = 100 * `2nd Chance Points` / PTS,
           `PITP %` = 100 * `Points in the Paint` / PTS,
           `FB %` = 100 * `Fastbreak Points` / PTS,
           `BENCH %` = 100 * `Bench Points` / PTS,
           across(where(is.numeric), ~ round(., 1)),
           PPP = round(PTS / POSS, 2),
           PPP_OPP = round(PTS_OPP / POSS_OPP, 2),
           NET = round(PPP - PPP_OPP, 2),
           `FGM-A` = paste(as.character(FGM), as.character(FGA), sep="-"),
           `3PM-A` = paste(as.character(`3PM`), as.character(`3PA`), sep="-"),
           `FTM-A` = paste(as.character(FTM), as.character(FTA), sep="-"),
           `Poss. Length` = NA_real_)
} else {
  data.frame()
}

# Initialize date variables
w_all_dates24 <- c()
w_game_dates24 <- c()
w_practices24 <- c()
m_all_dates24 <- c()
m_game_dates24 <- c()
m_practices24 <- c()

refresh_dates24 = function(y=2024) {
  tryCatch({
    w_all_dates24 <<- 
      unique(c(get_shots("WBB", "Chicago", y=y)$Date, 
               get_events("WBB", "Chicago", y=y)$Date)) %>%
      date_filter_helper(y=2024)
    
    if (nrow(team_box24) > 0) {
      w_game_dates24 <<- unique(filter(team_box24, 
                                       League == "WBB",
                                       Team == "Chicago")$Date)
    }
    w_practices24 <<- w_all_dates24[!(w_all_dates24 %in% w_game_dates24)]
    
    m_all_dates24 <<- 
      unique(c(get_shots("MBB", "Chicago", y=y)$Date, 
               get_events("MBB", "Chicago", y=y)$Date)) %>%
      date_filter_helper(y=2024)
    
    if (nrow(team_box24) > 0) {
      m_game_dates24 <<- unique(filter(team_box24, 
                                       League == "MBB",
                                       Team == "Chicago")$Date)
    }
    m_practices24 <<- m_all_dates24[!(m_all_dates24 %in% m_game_dates24)]
  }, error = function(e) {
    message("Error in refresh_dates24: ", e$message)
  })
}

# Call refresh_dates24 at startup
refresh_dates24()
