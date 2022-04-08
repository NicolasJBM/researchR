#' @name diagram_edit_ui
#' @title Edit diagrams
#' @author Nicolas Mangin
#' @description Module facilitating the quick creation of diagrams embedded in functions or documents.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Write lines of codes creating the different parts of a diagram which can then be embedded in function or document.
#' @import shiny
#' @importFrom shinydashboardPlus box
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom DiagrammeR grVizOutput
#' @export


diagram_edit_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::actionButton(
          ns("applychanges"), "Apply changes", icon = shiny::icon("sync"),
          style = "background-color:#000066;color:#FFF;width:100%;
          margin-top:10px;margin-bottom:25px;"
        ),
        shinydashboardPlus::box(
          title = "Nodes", status = "navy",
          solidHeader = TRUE, width = 12, collapsible = TRUE,
          collapsed = FALSE, closable = FALSE,
          icon = shiny::icon("compress-arrows-alt"),
          background = NULL, gradient = FALSE,
          rhandsontable::rHandsontableOutput(ns("editnodes"))
        ),
        shinydashboardPlus::box(
          title = "Relations", status = "primary",
          solidHeader = TRUE, width = 12, collapsible = TRUE,
          collapsed = FALSE, closable = FALSE,
          icon = shiny::icon("exchange-alt"),
          background = NULL, gradient = FALSE,
          rhandsontable::rHandsontableOutput(ns("editrelations"))
        ),
        shinydashboardPlus::box(
          title = "Moderations", status = "info",
          solidHeader = TRUE, width = 12, collapsible = TRUE,
          collapsed = FALSE, closable = FALSE,
          icon = shiny::icon("level-down-alt"),
          background = NULL, gradient = FALSE,
          rhandsontable::rHandsontableOutput(ns("editmoderations"))
        )
      ),
      shiny::column(
        6,
        shiny::actionButton(
          ns("clear"), "Clear", icon = shiny::icon("trash"),
          style = "background-color:#660000;color:#FFF;width:100%;
          margin-top:10px;margin-bottom:25px;"
        ),
        shinydashboardPlus::box(
          title = "Diagram", status = "teal",
          solidHeader = TRUE, width = 12, collapsible = TRUE,
          collapsed = FALSE, closable = FALSE,
          icon = shiny::icon("project-diagram"),
          background = NULL, gradient = FALSE,
          DiagrammeR::grVizOutput(ns("displaydiagram"))
        ),
        shinydashboardPlus::box(
          title = "Code", status = "success",
          solidHeader = TRUE, width = 12, collapsible = TRUE,
          collapsed = FALSE, closable = FALSE,
          icon = shiny::icon("code"),
          background = NULL, gradient = FALSE,
          shiny::uiOutput(ns("displaycode"))
        )
      )
    )
  )
}

