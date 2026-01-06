## Database Functions - Google Sheets Storage
## Persists data to Google Sheets for shinyapps.io compatibility

library(googlesheets4)

Sys.setenv(TZ='CST6CDT')

# Google Sheets configuration
SHEET_ID <- "1URS4hxvfRXtf_kqDnFoyrRCE8ETlhBlN3J0MPdUlXTw"

# Authenticate with service account using environment variable
tryCatch({
  # Get JSON key from environment variable
  json_key <- Sys.getenv("GOOGLE_SHEETS_KEY")
  
  if (nchar(json_key) > 0) {
    # Write to temp file for authentication
    temp_key_file <- tempfile(fileext = ".json")
    writeLines(json_key, temp_key_file)
    gs4_auth(path = temp_key_file)
    unlink(temp_key_file)  # Clean up
  } else {
    # Try local file for development
    if (file.exists("google-sheets-key.json")) {
      gs4_auth(path = "google-sheets-key.json")
    } else {
      message("No Google Sheets credentials found - using anonymous access")
      gs4_deauth()
    }
  }
}, error = function(e) {
  message("Google Sheets auth failed: ", e$message)
  gs4_deauth()
})

# Function to read shots from Google Sheets
read_sheets_shots <- function() {
  tryCatch({
    shots <- read_sheet(SHEET_ID, sheet = "shots")
    if (nrow(shots) > 0) {
      return(as.data.frame(shots))
    }
  }, error = function(e) {
    message("Error reading shots: ", e$message)
  })
  
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

# Function to read events from Google Sheets
read_sheets_events <- function() {
  tryCatch({
    events <- read_sheet(SHEET_ID, sheet = "events")
    if (nrow(events) > 0) {
      return(as.data.frame(events))
    }
  }, error = function(e) {
    message("Error reading events: ", e$message)
  })
  
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

# Function to read turnovers from Google Sheets
read_sheets_turnovers <- function() {
  tryCatch({
    turnovers <- read_sheet(SHEET_ID, sheet = "turnovers")
    if (nrow(turnovers) > 0) {
      return(as.data.frame(turnovers))
    }
  }, error = function(e) {
    message("Error reading turnovers: ", e$message)
  })
  
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

# Initialize master data from Google Sheets
master_shots <- read_sheets_shots()
master_events <- read_sheets_events()
master_turnovers <- read_sheets_turnovers()

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
  # Update master_shots in memory
  master_shots <<- rbind(master_shots, df)
  
  # Write to Google Sheets
  tryCatch({
    sheet_append(SHEET_ID, df, sheet = "shots")
  }, error = function(e) {
    message("Error writing shot to Google Sheets: ", e$message)
  })
}

remove_shot <- function(id) {
  # Update master_shots in memory
  master_shots <<- master_shots %>%
    filter(`Shot ID` != id)
  
  # Rewrite entire sheet (Google Sheets doesn't support row deletion easily)
  tryCatch({
    write_sheet(master_shots, SHEET_ID, sheet = "shots")
  }, error = function(e) {
    message("Error removing shot from Google Sheets: ", e$message)
  })
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
  # Update in memory
  master_turnovers <<- rbind(master_turnovers, df)
  
  # Write to Google Sheets
  tryCatch({
    sheet_append(SHEET_ID, df, sheet = "turnovers")
  }, error = function(e) {
    message("Error writing turnover to Google Sheets: ", e$message)
  })
}

remove_turnover <- function(id) {
  if (nrow(master_turnovers) == 0) return()
  
  master_turnovers <<- master_turnovers %>%
    filter(`Event ID` != id)
  
  tryCatch({
    write_sheet(master_turnovers, SHEET_ID, sheet = "turnovers")
  }, error = function(e) {
    message("Error removing turnover from Google Sheets: ", e$message)
  })
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
  # Update in memory
  master_events <<- rbind(master_events, df)
  
  # Write to Google Sheets
  tryCatch({
    sheet_append(SHEET_ID, df, sheet = "events")
  }, error = function(e) {
    message("Error writing event to Google Sheets: ", e$message)
  })
}

remove_event <- function(id) {
  if (nrow(master_events) == 0) return()
  
  master_events <<- master_events %>%
    filter(`Event ID` != id)
  
  tryCatch({
    write_sheet(master_events, SHEET_ID, sheet = "events")
  }, error = function(e) {
    message("Error removing event from Google Sheets: ", e$message)
  })
  
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

# Refresh data from Google Sheets
refresh_local_data <- function() {
  tryCatch({
    master_shots <<- read_sheets_shots()
    master_events <<- read_sheets_events()
    master_turnovers <<- read_sheets_turnovers()
  }, error = function(e) {
    message("Error refreshing data: ", e$message)
  })
}

db_backup <- function() {
  message("Data is stored in Google Sheets - no local backup needed")
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
