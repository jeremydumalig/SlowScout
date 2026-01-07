# Data25.R - 2025 Season Data
# Simplified version - no remote fetching, fast startup

# Initialize empty variables for 2025 data
team_box25 <- data.frame()
player_box25 <- data.frame()
w_game_dates25 <- c()
m_game_dates25 <- c()
w_practices25 <- c()
m_practices25 <- c()
w_all_dates25 <- c()
m_all_dates25 <- c()

refresh_dates25 = function() {
  tryCatch({
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
    # Silently fail
  })
}

# Don't call refresh_dates25() on startup - defer to server
