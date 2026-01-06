# SlowScout

## Shot Plotter Data Storage

### Google Sheets Integration
Data is stored in Google Sheets for persistent storage on shinyapps.io.

**Sheet ID:** `1URS4hxvfRXtf_kqDnFoyrRCE8ETlhBlN3J0MPdUlXTw`

### Required Google Sheet Tabs
Make sure your Google Sheet has these three tabs (exact names):
1. `shots` - Stores all shot data
2. `events` - Stores rebounds, assists, turnovers  
3. `turnovers` - Stores turnover type details

### Setup for shinyapps.io
Set the environment variable `GOOGLE_SHEETS_KEY` with the full JSON content of your service account key.

### Local Development
For local development, place your service account JSON key file as `google-sheets-key.json` in the project root.
