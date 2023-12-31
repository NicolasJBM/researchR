#' @name load_study_ui
#' @title Load course data
#' @author Nicolas Mangin
#' @description Module facilitating the loading of all relevant course data.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return A list of course data.
#' @importFrom shiny NS
#' @importFrom shiny actionButton
#' @importFrom shiny icon
#' @export


load_study_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::actionButton(
    ns("loadstudy"), "Load", icon = shiny::icon("upload"),
    style = "background-color:#003366;color:#FFF;width:250px;border:0px"
  )
}

