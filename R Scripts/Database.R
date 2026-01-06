## Database Functions - In-Memory Storage
## Works on shinyapps.io (read-only filesystem)

Sys.setenv(TZ='CST6CDT')

# Function to read shots from local CSV (if exists)
read_local_shots <- function() {
  tryCatch({
    if (file.exists("local_shots.csv")) {
      shots <- read.csv("local_shots.csv", stringsAsFactors = FALSE)
      names(shots) <- gsub("\\.", " ", names(shots))
      return(shots)
    }
  }, error = function(e) {})
  
  # Return empty dataframe with correct structure
  return(data.frame(
    `Shot ID` = numeric(),
    League = character(),
    Team = character(),
    Date = character(),
    Player = character(),
    x = numeric(),
    y = numeric(),
    Region = character(),
    `Shot Type` = character(),
    Outcome = character(),
    check.names = FALSE
  ))
}

# Function to read events from local CSV (if exists)
read_local_events <- function() {
  tryCatch({
    if (file.exists("local_events.csv")) {
      events <- read.csv("local_events.csv", stringsAsFactors = FALSE)
      names(events) <- gsub("\\.", " ", names(events))
      return(events)
    }
  }, error = function(e) {})
  
  return(data.frame(
    `Event ID` = numeric(),
    League = character(),
    Team = character(),
    Date = character(),
    Player = character(),
    OREB = numeric(),
    DREB = numeric(),
    AST = numeric(),
    TO = numeric(),
    check.names = FALSE
  ))
}

# Function to read turnovers from local CSV (if exists)
read_local_turnovers <- function() {
  tryCatch({
    if (file.exists("local_turnovers.csv")) {
      turnovers <- read.csv("local_turnovers.csv", stringsAsFactors = FALSE)
      names(turnovers) <- gsub("\\.", " ", names(turnovers))
      return(turnovers)
    }
  }, error = function(e) {})
  
  return(data.frame(
    `Event ID` = numeric(),
    League = character(),
    Team = character(),
    Date = character(),
    Player = character(),
    TO = character(),
    check.names = FALSE
  ))
}

# Initialize master data from local files
master_shots <- read_local_shots()
master_events <- read_local_events()
master_turnovers <- read_local_turnovers()

# Threshold for separating 2024 and 2025 seasons
threshold <- as.Date("2024-09-01")

get_all_shots <- function(y=2024) {
  if (nrow(master_shots) == 0) return(master_shots)
  
  if (y == 2024) {
    master_shots %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < threshold) %>%
      return()
  } else {
    master_shots %>% 
      filter(as.Date(Date, format="%Y-%m-%d") >= threshold) %>%
      return()
  }
}

get_shots <- function(league, team, y=2024) {
  if (nrow(master_shots) == 0) return(master_shots)
  
  if (y == 2024) {
    master_shots %>% 
      filter(League == league,
             Team == team,
             as.Date(Date, format="%Y-%m-%d") < threshold) %>%
      return()
  } else {
    master_shots %>% 
      filter(League == league,
             Team == team,
             as.Date(Date, format="%Y-%m-%d") >= threshold) %>%
      return()
  }
}

add_shot <- function(df) {
  # Update master_shots in memory only
  master_shots <<- rbind(master_shots, df)
  
  # Try to write to file (will fail silently on shinyapps.io)
  tryCatch({
    write.csv(master_shots, "local_shots.csv", row.names = FALSE)
  }, error = function(e) {})
}

remove_shot <- function(id) {
  # Update master_shots in memory
  master_shots <<- master_shots %>%
    filter(`Shot ID` != id)
  
  # Try to write to file (will fail silently on shinyapps.io)
  tryCatch({
    write.csv(master_shots, "local_shots.csv", row.names = FALSE)
  }, error = function(e) {})
}

get_all_turnovers <- function(y=2024) {
  if (nrow(master_turnovers) == 0) return(master_turnovers)
  
  if (y == 2024) {
    master_turnovers %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < threshold) %>%
      return()
  } else {
    master_turnovers %>% 
      filter(as.Date(Date, format="%Y-%m-%d") >= threshold) %>%
      return()
  }
}

get_turnovers <- function(league, team, y=2024) {
  if (nrow(master_turnovers) == 0) return(master_turnovers)
  
  if (y == 2024) {
    master_turnovers %>% 
      filter(League == league,
             Team == team,
             as.Date(Date, format="%Y-%m-%d") < threshold) %>%
      return()
  } else {
    master_turnovers %>% 
      filter(League == league,
             Team == team,
             as.Date(Date, format="%Y-%m-%d") >= threshold) %>%
      return()
  }
}

add_turnover <- function(df) {
  # Update in memory only
  master_turnovers <<- rbind(master_turnovers, df)
  
  tryCatch({
    write.csv(master_turnovers, "local_turnovers.csv", row.names = FALSE)
  }, error = function(e) {})
}

remove_turnover <- function(id) {
  if (nrow(master_turnovers) == 0) return()
  
  master_turnovers <<- master_turnovers %>%
    filter(`Event ID` != id)
  
  tryCatch({
    write.csv(master_turnovers, "local_turnovers.csv", row.names = FALSE)
  }, error = function(e) {})
}

get_all_events <- function(y=2024) {
  if (nrow(master_events) == 0) return(master_events)
  
  if (y == 2024) {
    master_events %>% 
      filter(as.Date(Date, format="%Y-%m-%d") < threshold) %>%
      return()
  } else {
    master_events %>% 
      filter(as.Date(Date, format="%Y-%m-%d") >= threshold) %>%
      return()
  }
}

get_events <- function(league, team, y=2024) {
  if (nrow(master_events) == 0) return(master_events)
  
  if (y == 2024) {
    master_events %>% 
      filter(League == league,
             Team == team,
             as.Date(Date, format="%Y-%m-%d") < threshold) %>%
      return()
  } else {
    master_events %>% 
      filter(League == league,
             Team == team,
             as.Date(Date, format="%Y-%m-%d") >= threshold) %>%
      return()
  }
}

add_oreb <- function(df) {
  add_event_helper(df)
}

add_dreb <- function(df) {
  add_event_helper(df)
}

add_ast <- function(df) {
  add_event_helper(df)
}

add_to <- function(df) {
  add_event_helper(df)
}

add_event_helper <- function(df) {
  # Update in memory only
  master_events <<- rbind(master_events, df)
  
  tryCatch({
    write.csv(master_events, "local_events.csv", row.names = FALSE)
  }, error = function(e) {})
}

remove_event <- function(id) {
  if (nrow(master_events) == 0) return()
  
  master_events <<- master_events %>%
    filter(`Event ID` != id)
  
  tryCatch({
    write.csv(master_events, "local_events.csv", row.names = FALSE)
  }, error = function(e) {})
  
  # Also remove from turnovers if exists
  remove_turnover(id)
}

new_row_id <- function(shot=FALSE, event=FALSE) {
  if (shot) {
    if (nrow(master_shots) == 0) {
      return(1)
    }
    return(max(master_shots$`Shot ID`, na.rm = TRUE) + 1)
  } else {
    if (nrow(master_events) == 0) {
      return(1)
    }
    return(max(master_events$`Event ID`, na.rm = TRUE) + 1)
  }
}

get_dates <- function(league, team, y=2024) {
  if (nrow(master_shots) == 0) {
    return(character(0))
  }
  
  dates <- master_shots %>%
    filter(League == league, Team == team) %>%
    pull(Date) %>%
    unique()
  
  if (length(dates) == 0) return(character(0))
  
  date_filter_helper(dates, y=y)
}

# Refresh data from local files (no-op on shinyapps.io)
refresh_local_data <- function() {
  tryCatch({
    master_shots <<- read_local_shots()
    master_events <<- read_local_events()
    master_turnovers <<- read_local_turnovers()
  }, error = function(e) {})
}

db_backup <- function() {
  tryCatch({
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    file.copy("local_shots.csv", paste0("backup_shots_", timestamp, ".csv"))
    file.copy("local_events.csv", paste0("backup_events_", timestamp, ".csv"))
    file.copy("local_turnovers.csv", paste0("backup_turnovers_", timestamp, ".csv"))
    cat("Backup created with timestamp:", timestamp, "\n")
  }, error = function(e) {
    cat("Backup failed (read-only filesystem)\n")
  })
}

get_points <- function(df) {
  if (nrow(df) == 0) {
    return(data.frame(PTS = 0))
  }
  
  df %>%
    filter((Outcome == "Make") | (str_detect(Outcome, "Foul"))) %>%
    mutate(Points = case_when((Outcome == "Foul (+3)") ~ 3,
                              (Outcome == "Foul (+2)") ~ 2,
                              (Outcome == "Foul (+1)") ~ 1,
                              (Outcome == "Foul (+0)") ~ 0,
                              (Region %in% regions3) ~ 3,
                              TRUE ~ 2)) %>%
    summarize(PTS = sum(Points))
}
