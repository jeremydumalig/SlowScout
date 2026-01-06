# Data25.R - 2025 Season Data (optional - loads from GitHub if available)

exists_helper = function(string) {
  tryCatch({
    df <- read_csv(string, show_col_types = FALSE)
    if (nrow(df) > 0) return(df)
    return(data.frame())
  }, error = function(e) {
    return(data.frame())
  }, warning = function(w) {
    return(data.frame())
  })
}

# Try to load 2025 data from GitHub (will return empty dataframes if not available)
raw_player_box25 = tryCatch({
  rbind(
    exists_helper(
      paste0("https://raw.githubusercontent.com/jeremydumalig/SlowScout/main/",
             "w_player_box25.csv")
    ),
    exists_helper(
      paste0("https://raw.githubusercontent.com/jeremydumalig/SlowScout/main/",
             "m_player_box25.csv")
    )
  )
}, error = function(e) data.frame())

raw_team_box25 = tryCatch({
  rbind(
    exists_helper(
      paste0("https://raw.githubusercontent.com/jeremydumalig/SlowScout/main/",
             "w_team_box25.csv")
    ),
    exists_helper(
      paste0("https://raw.githubusercontent.com/jeremydumalig/SlowScout/main/",
             "m_team_box25.csv")
    )
  )
}, error = function(e) data.frame())

raw_plays25 = tryCatch({
  rbind(
    exists_helper(
      paste0("https://raw.githubusercontent.com/jeremydumalig/SlowScout/main/",
             "w_plays25.csv")
    ),
    exists_helper(
      paste0("https://raw.githubusercontent.com/jeremydumalig/SlowScout/main/",
             "m_plays25.csv")
    )
  )
}, error = function(e) data.frame())

# Initialize empty variables for 2025 data
team_box25 <- data.frame()
player_box25 <- data.frame()
w_game_dates25 <- c()
m_game_dates25 <- c()
w_practices25 <- c()
m_practices25 <- c()
w_all_dates25 <- c()
m_all_dates25 <- c()

# Only process 2025 data if we actually have it
if (nrow(raw_player_box25) > 0 && nrow(raw_team_box25) > 0) {
  tryCatch({
    player_box25 = 
      raw_player_box25 %>%
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
    
    team_box25 = 
      raw_team_box25 %>%
      merge(uaa_teams_csv, by="Team", all.x=T) %>%
      merge(select(raw_team_box25,
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
             across(where(is.numeric), ~ round(., 1)),
             PPP = round(PTS / POSS, 2),
             PPP_OPP = round(PTS_OPP / POSS_OPP, 2),
             NET = round(PPP - PPP_OPP, 2),
             `FGM-A` = paste(as.character(FGM), as.character(FGA), sep="-"),
             `3PM-A` = paste(as.character(`3PM`), as.character(`3PA`), sep="-"),
             `FTM-A` = paste(as.character(FTM), as.character(FTA), sep="-"))
  }, error = function(e) {
    # If processing fails, keep empty dataframes
  })
}

refresh_dates25 = function() {
  tryCatch({
    if (exists("team_box25") && nrow(team_box25) > 0) {
      w_game_dates25 <<- unique(filter(team_box25, 
                                       League == "WBB",
                                       Team == "Chicago")$Date)
      m_game_dates25 <<- unique(filter(team_box25, 
                                       League == "MBB",
                                       Team == "Chicago")$Date)
    } else {
      w_game_dates25 <<- c()
      m_game_dates25 <<- c()
    }
    
    w_all_dates25 <<- tryCatch({
      unique(c(get_shots("WBB", "Chicago", y=2025)$Date, 
               get_events("WBB", "Chicago", y=2025)$Date)) %>%
        date_filter_helper(y=2025)
    }, error = function(e) c())
    
    w_practices25 <<- w_all_dates25[!(w_all_dates25 %in% w_game_dates25)]
    
    m_all_dates25 <<- tryCatch({
      unique(c(get_shots("MBB", "Chicago", y=2025)$Date, 
               get_events("MBB", "Chicago", y=2025)$Date)) %>%
        date_filter_helper(y=2025)
    }, error = function(e) c())
    
    m_practices25 <<- m_all_dates25[!(m_all_dates25 %in% m_game_dates25)]
  }, error = function(e) {
    w_game_dates25 <<- c()
    m_game_dates25 <<- c()
    w_practices25 <<- c()
    m_practices25 <<- c()
    w_all_dates25 <<- c()
    m_all_dates25 <<- c()
  })
}

refresh_dates25()
