#' @name processing_edit_ui
#' @title Edit notes on the literature
#' @author Nicolas Mangin
#' @description Module facilitating the quick edition of notes on the literature.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Save the new or modified note in the folder "3_documents".
#' @import shiny
#' @importFrom visNetwork visNetworkOutput
#' @export


processing_edit_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::actionButton(
          ns("updateobjects"), "Update objects",
          icon = shiny::icon("sync"),
          style = "width:100%;background-color:#660066;color:white;
          margin-bottom:10px;"
        ),
        shiny::actionButton(
          ns("savetargets"), "Save", icon = shiny::icon("save"),
          style = "width:100%;background-color:#006633;color:white;
          margin-bottom:10px;"
        ),
        shiny::uiOutput(ns("edittargets"))
      ),
      shiny::column(
        8,
        shiny::actionButton(
          ns("updatenetwork"), "Update network",
          icon = shiny::icon("project-diagram"),
          style = "width:100%;background-color:#660066;color:white;
          margin-bottom:10px;"
        ),
        shiny::sliderInput(
          ns("deflevsep"), "Level of separation",
          value = 500, min = 100, max = 1000, step = 1, width = "100%"
        ),
        visNetwork::visNetworkOutput(ns("targetsnet"), height = "800px")
      )
    )
  )
}

