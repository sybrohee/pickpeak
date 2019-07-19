#' launch shiny app
#'
#' Launch main shiny app in package with usual params of \code{shiny::runApp}
#'
#' @param launch_param param passed at launch time by user
#' @param ... params are passed to shiny::runApp, e.g. port, launch.browser,
#'   etc.
#'
#' @return This function normally does not return; interrupt R to stop the
#'   application (usually by pressing Ctrl+C or Esc).
#'
#' @import shiny data.table plotly shinyalert rjson DT XLConnect seqinr shinyjs pracma shinyWidgets

#' @examples \donttest{runShinyPackageApp(launch_param = "/path/to/jsonparamfile")}
#' @export
runShinyPackageApp <- function(launch_param, ...) {

    .GlobalEnv$launch_param <- launch_param
    on.exit(rm(launch_param, envir=.GlobalEnv))

    app <- shinyApp(
        ui = shinypackage_ui,
        server = shinypackage_server
    )
    runApp(app,  ...)
}

