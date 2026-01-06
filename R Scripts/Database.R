## Local Database Functions (No MongoDB)
## Uses local CSV file operations for data storage

Sys.setenv(TZ='CST6CDT')

# Function to read shots from local CSV
read_local_shots <- function() {
  if (file.exists("local_shots.csv")) {
    shots <- read.csv("local_shots.csv", stringsAsFactors = FALSE)
    # Ensure column names match expected format
    names(shots) <- gsub("\\.", " ", names(shots))
    return(shots)
  } else {
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
}

# Function to read events from local CSV
read_local_events <- function() {
  if (file.exists("local_events.csv")) {
    events <- read.csv("local_events.csv", stringsAsFactors = FALSE)
    names(events) <- gsub("\\.", " ", names(events))
    return(events)
  } else {
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
}

# Function to read turnovers from local CSV
read_local_turnovers <- function() {
  if (file.exists("local_turnovers.csv")) {
    turnovers <- read.csv("local_turnovers.csv", stringsAsFactors = FALSE)
    names(turnovers) <- gsub("\\.", " ", names(turnovers))
    return(turnovers)
  } else {
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
}

# Initialize master data from local files
master_shots <- read_local_shots()
master_events <- read_local_events()
master_turnovers <- read_local_turnovers()

# Threshold for separating 2024 and 2025 seasons
threshold <- as.Date("2024-09-01")

get_all_shots <- function(y=2024) {
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
  # Read current data
  current_shots <- read_local_shots()
  
  # Append new shot
  updated_shots <- rbind(current_shots, df)
  
  # Write back to CSV
  write.csv(updated_shots, "local_shots.csv", row.names = FALSE)
  
  # Update master_shots in memory
  master_shots <<- updated_shots
}

remove_shot <- function(id) {
  # Read current data
  current_shots <- read_local_shots()
  
  # Remove shot with given ID
  updated_shots <- current_shots %>%
    filter(`Shot ID` != id)
  
  # Write back to CSV
  write.csv(updated_shots, "local_shots.csv", row.names = FALSE)
  
  # Update master_shots in memory
  master_shots <<- updated_shots
}

get_all_turnovers <- function(y=2024) {
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
  # Read current data
  current_turnovers <- read_local_turnovers()
  
  # Append new turnover
  updated_turnovers <- rbind(current_turnovers, df)
  
  # Write back to CSV
  write.csv(updated_turnovers, "local_turnovers.csv", row.names = FALSE)
  
  # Update master_turnovers in memory
  master_turnovers <<- updated_turnovers
}

remove_turnover <- function(id) {
  # Read current data
  current_turnovers <- read_local_turnovers()
  
  # Remove turnover with given ID
  updated_turnovers <- current_turnovers %>%
    filter(`Event ID` != id)
  
  # Write back to CSV
  write.csv(updated_turnovers, "local_turnovers.csv", row.names = FALSE)
  
  # Update master_turnovers in memory
  master_turnovers <<- updated_turnovers
}

get_all_events <- function(y=2024) {
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
  # Read current data
  current_events <- read_local_events()
  
  # Append new event
  updated_events <- rbind(current_events, df)
  
  # Write back to CSV
  write.csv(updated_events, "local_events.csv", row.names = FALSE)
  
  # Update master_events in memory
  master_events <<- updated_events
}

remove_event <- function(id) {
  # Read current data
  current_events <- read_local_events()
  
  # Remove event with given ID
  updated_events <- current_events %>%
    filter(`Event ID` != id)
  
  # Write back to CSV
  write.csv(updated_events, "local_events.csv", row.names = FALSE)
  
  # Update master_events in memory
  master_events <<- updated_events
  
  # Also remove from turnovers if exists
  remove_turnover(id)
}

new_row_id <- function(shot=FALSE, event=FALSE) {
  if (shot) {
    df <- read_local_shots()
    if (nrow(df) == 0) {
      return(1)
    }
    return(max(df$`Shot ID`, na.rm = TRUE) + 1)
  } else {
    df <- read_local_events()
    if (nrow(df) == 0) {
      return(1)
    }
    return(max(df$`Event ID`, na.rm = TRUE) + 1)
  }
}

get_dates <- function(league, team, y=2024) {
  shots <- read_local_shots()
  
  if (nrow(shots) == 0) {
    return(character(0))
  }
  
  dates <- shots %>%
    filter(League == league, Team == team) %>%
    pull(Date) %>%
    unique()
  
  date_filter_helper(dates, y=y)
}

# Refresh data from local files
refresh_local_data <- function() {
  master_shots <<- read_local_shots()
  master_events <<- read_local_events()
  master_turnovers <<- read_local_turnovers()
}

db_backup <- function() {
  # Create backup files with timestamp
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  file.copy("local_shots.csv", paste0("backup_shots_", timestamp, ".csv"))
  file.copy("local_events.csv", paste0("backup_events_", timestamp, ".csv"))
  file.copy("local_turnovers.csv", paste0("backup_turnovers_", timestamp, ".csv"))
  
  cat("Backup created with timestamp:", timestamp, "\n")
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
