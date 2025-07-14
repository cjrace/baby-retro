server <- function(input, output, session) {
  # Initialise blank reactive values
  scores_data <- shiny::reactiveVal(NULL)
  questions <- shiny::reactiveVal(NULL)
  written_data <- shiny::reactiveVal(NULL)

  # Custom function to read data ==============================================
  # Custom functions
  read_data <- function(type) {
    if (type == "scores") {
      data <- googlesheets4::read_sheet(
        "https://docs.google.com/spreadsheets/d/1C9Vh5qULlI-6GKOwS94iATmLfi63dBHwp0glFZWWR80/edit?resourcekey=&gid=597595104",
        sheet = NULL
      ) |>
        tidyr::pivot_longer(
          cols = 3:8,
          names_to = "question",
          values_to = "score"
        ) |>
        dplyr::rename(
          "name" = `Who are you?`,
          "date" = Timestamp,
        ) |>
        dplyr::select(name, date, question, score) |>
        dplyr::mutate(
          date = as.Date(date, format = "%d/%m/%Y"),
          score = as.numeric(score),
          question = gsub("^Scores \\[|\\]$", "", question)
        )

      scores_data(data)
    } else if (type == "written") {
      data <- googlesheets4::read_sheet(
        "https://docs.google.com/spreadsheets/d/1C9Vh5qULlI-6GKOwS94iATmLfi63dBHwp0glFZWWR80/edit?resourcekey=&gid=597595104",
        sheet = NULL
      ) |>
        dplyr::select(-c(3:8)) |>
        dplyr::rename(
          "name" = `Who are you?`,
          "date" = Timestamp,
        ) |>
        dplyr::mutate(date = as.Date(date, format = "%d/%m/%Y"))

      written_data(data)
    } else {
      stop(paste(type, " is not an expected type in read_data()"))
    }
  }

  # Scores page ===============================================================
  # Read data from Google Sheets ----------------------------------------------
  shiny::observe({
    if (is.null(scores_data())) {
      return(NULL)
    }
    questions(unique(scores_data()$question))
  })

  # Expose questions for UI dropdown
  output$questions <- shiny::reactive({
    validate(need(questions(), "Data not loaded"))
    questions()
  })

  # Update the selectInput choices when data is loaded
  shiny::observe({
    if (!is.null(scores_data())) {
      shiny::updateSelectInput(
        session,
        "question_selection",
        choices = questions()
      )
    }
  })

  # Run on app load
  shiny::observe({
    read_data("scores")
  })

  ## Trigger reading data when button is pressed
  shiny::observeEvent(input$refresh_scores_data_btn, {
    read_data("scores")
  })

  # Plot scores data ----------------------------------------------------------
  filtered_scores_data <- shiny::reactive({
    validate(need(scores_data(), "Data not loaded"))
    scores_data() |>
      dplyr::filter(question == input$question_selection) |>
      dplyr::select(-question)
  })

  output$score_chart <- highcharter::renderHighchart({
    if (is.null(scores_data())) {
      highchart() |>
        hc_title(text = "Data not loaded")
    } else {
      filtered_data <- filtered_scores_data()

      names_split <- filtered_data |>
        dplyr::group_split(name)

      hc <- highchart() |>
        hc_chart(type = "line") |>
        hc_xAxis(
          categories = as.character(unique(filtered_data$date))
        )

      for (name in names_split) {
        line_color <- NULL
        if (unique(name$name) == "Cam") {
          line_color <- cam_orange
        } else if (unique(name$name) == "Laura") {
          line_color <- laura_yellow
        }
        hc <- hc |>
          hc_add_series(
            name = unique(name$name),
            data = name$score,
            type = "line",
            color = line_color
          )
      }
      hc |>
        hc_title(
          text = paste("Scores over time for", input$question_selection)
        )
    }
  })

  # Written questions page ====================================================
  # Run on when selecting written page
  shiny::observeEvent(input$pages, {
    if (input$pages == "Written responses") {
      read_data("written")
    }
  })

  ## Trigger reading data when button is pressed
  shiny::observeEvent(input$refresh_written_data_btn, {
    read_data("written")
  })

  ## Filter written data based on date range
  filtered_written_data <- shiny::reactive({
    validate(need(written_data(), "Data not loaded"))
    written_data() |>
      dplyr::filter(
        date >= input$written_date_range[1] &
          date <= input$written_date_range[2]
      )
  })

  # Render written responses --------------------------------------------------
  # For every column after date and name, create a separate table with name, date, question
  output$written_tables <- shiny::renderUI({
    validate(need(written_data(), "Data not loaded"))
    data <- filtered_written_data()
    cols <- setdiff(names(data), c("name", "date"))
    tables <- lapply(cols, function(col) {
      table_data <- data |>
        dplyr::select(name, date, !!col) |>
        dplyr::rename(response = !!col) |>
        dplyr::mutate(
          date = format(as.Date(date, format = "%d/%m/%Y"), "%d %B %Y")
        ) |>

        dplyr::filter(!is.na(response) & response != "")
      shiny::tagList(
        shiny::h4(col),
        shiny::tableOutput(paste0("table_", col))
      )
    })
    shiny::tagList(tables)
  })

  # Dynamically render each table
  observe({
    data <- filtered_written_data()
    cols <- setdiff(names(data), c("name", "date"))
    for (col in cols) {
      local({
        col_name <- col
        output[[paste0("table_", col_name)]] <- shiny::renderTable({
          table_data <- data |>
            dplyr::select(name, date, !!col_name) |>
            dplyr::rename(response = !!col_name) |>
            dplyr::filter(!is.na(response) & response != "") |>
            dplyr::mutate(
              date = format(as.Date(date, format = "%d/%m/%Y"), "%d %B %Y")
            ) |>
            dplyr::arrange(desc(date))
          table_data
        })
      })
    }
  })
}
