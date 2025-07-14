# Baby retro dashboard

Quick R Shiny app to visualise the latest results from our Google sheets to use in baby retros.

## Run

1. Install dependencies

`pak::lockfile_install()`

2. Run

`shiny::runApp()`

## TODO

- Handle Google auth for a server
- Deploy to shinyapps.io (POSIT connect cloud doesn't have private link sharing)
- Add other workflows (Air, tests)