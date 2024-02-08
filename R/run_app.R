#' By using run_app() function in 'TestAnaAPP' package without any arguments,
#'  you can perform your test analysis in a friendly interactive interface.
#'
#' @export
#' @importFrom shiny shinyApp
#' @returns No explicit return value. The function is called for its side effects,
#' which include running the interactive application.
#' @examples
#' if(interactive()){
#' TestAnaAPP::run_app()
#' }
#'
run_app <- function() {
  shinyApp(
    ui = app_ui,
    server = app_server,
  )
}


