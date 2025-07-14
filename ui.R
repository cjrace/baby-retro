ui <- bslib::page_navbar(
  theme = bs_theme(bootswatch = "minty"),
  title = "Baby retros",
  id = "pages",

  # Scores panel --------------------------------------------------------------
  bslib::nav_panel(
    "Scores",
    selectInput(
      "question_selection",
      "Select Question",
      choices = "Data loading"
    ),
    highcharter::highchartOutput("score_chart"),
    actionButton("refresh_scores_data_btn", "Refresh scores data")
  ),

  # Written responses panel ---------------------------------------------------
  bslib::nav_panel(
    "Written responses",
    dateRangeInput(
      "written_date_range",
      "Select date range",
      start = Sys.Date() - 6,
      end = Sys.Date()
    ),
    uiOutput("written_tables"),
    actionButton("refresh_written_data_btn", "Refresh written data")
  ),
)
