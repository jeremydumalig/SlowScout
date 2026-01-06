# SlowScout Simplified - Shot Plotter and Stats Glossary

This is a simplified version of SlowScout that includes only:
- **Shot Plotter**: Click on the court to record shots and track basketball events
- **Stats Glossary**: Reference for basketball analytics terms

## Key Changes from Original
- **No MongoDB Required**: All data is stored locally in CSV files
- **Simplified Features**: Removed complex features to focus on shot tracking
- **Easy Setup**: No database configuration needed

## Installation

1. Make sure you have R installed (version 4.0 or higher)

2. Install required packages (will be done automatically when you run the app):
```R
required_packages <- c(
  "shiny", "shinydashboard", "shinyWidgets", "shinyvalidate",
  "tidyverse", "dplyr", "readr", "tidyr", "stringr",
  "ggplot2", "plotly", "cowplot",
  "gt", "gtExtras", "DT", "reactable", "reactR",
  "janitor", "paletteer", "zoo"
)
```

## Running the App

### Option 1: Using the launcher script
```bash
Rscript run_simplified.R
```

### Option 2: From R console
```R
shiny::runApp("app_simplified.R")
```

### Option 3: From RStudio
Open `app_simplified.R` and click the "Run App" button

## How to Use

### Shot Plotter
1. Select your season, league (WBB/MBB), and team
2. Choose whether tracking your team (TEAM) or opponent (OPP)
3. Select the player who is taking the shot
4. Choose the shot type and outcome
5. **Click on the court** where the shot was taken
6. The shot will be recorded and displayed in the table below
7. You can:
   - Rotate the court view using "Rotate Court" button
   - Refresh data from files using "Refresh Data"
   - Clear all shots using "Clear All Shots"
   - Remove individual shots using the dropdown

### Recording Events
Use the buttons to record:
- **OREB**: Offensive rebound
- **DREB**: Defensive rebound
- **AST**: Assist
- **Turnovers**: Various turnover types (Perimeter/Strip, Bad Pass, Drive, etc.)

### Stats Glossary
Navigate to the Stats Glossary tab to see definitions of:
- Basic stats (PPP, PPS, etc.)
- Advanced metrics (eFG%, TS%, PER)
- Shot regions and types

## Data Storage

All data is stored locally in CSV files:
- `local_shots.csv`: Shot tracking data
- `local_events.csv`: Event tracking (rebounds, assists, turnovers)
- `local_turnovers.csv`: Detailed turnover information

These files are created automatically when you first run the app.

## Troubleshooting

### App won't start
- Make sure all required R packages are installed
- Check that you're in the correct directory
- Verify that `app_simplified.R` and required R Scripts exist

### Data not saving
- Check that you have write permissions in the directory
- Ensure CSV files aren't open in another program

### Court display issues
- Try using the "Rotate Court" button
- Refresh your browser if using the web interface

## Files Structure

```
SlowScout/
├── app_simplified.R          # Main simplified app
├── run_simplified.R          # Launcher script
├── R Scripts/
│   ├── Database_Local.R     # Local database functions (replaces MongoDB)
│   ├── Court.R              # Court plotting functions
│   ├── Data_Master.R        # Data definitions
│   ├── Data24.R             # 2024 season data
│   ├── Data25.R             # 2025 season data
│   ├── Color.R              # Color schemes
│   └── setSliderColor.R     # UI helper functions
├── Team Data/               # Team roster and configuration files
├── local_shots.csv          # Shot data (created automatically)
├── local_events.csv         # Event data (created automatically)
└── local_turnovers.csv      # Turnover data (created automatically)
```

## Notes

- This simplified version is designed for local use only
- No network connection required
- All data remains on your local machine
- Perfect for individual coaches or analysts who want simple shot tracking
