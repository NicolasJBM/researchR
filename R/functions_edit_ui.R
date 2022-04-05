#' @name functions_edit_ui
#' @title Edit functions
#' @author Nicolas Mangin
#' @description Module facilitating the quick creation or modification of functions used in documents.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Save the new or modified function in the folder "1_preparation/functions".
#' @import shiny
#' @export


functions_edit_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(4, shiny::uiOutput(ns("selectfunction"))),
      shiny::column(
        2,
        shiny::actionButton(
          ns("newfunction"), "New", icon = shiny::icon("magic"),
          style = "background-color:#003366;color:#FFF;
          width:100%;margin-top:25px;"
        )
      ),
      shiny::column(
        2,
        shiny::actionButton(
          ns("savefunction"), "Save", icon = shiny::icon("save"),
          style = "background-color:#006633;color:#FFF;
          width:100%;margin-top:25px;"
        )
      ),
      shiny::column(
        2,
        shiny::actionButton(
          ns("functioninrstudio"), "RStutio",
          icon = shiny::icon("r-project"),
          style = "background-color:#222222;color:#FFF;
          width:100%;margin-top:25px;"
        )
      ),
      shiny::column(
        2,
        shiny::actionButton(
          ns("deletefunction"), "Delete",
          icon = shiny::icon("trash"),
          style = "background-color:#990000;color:#FFF;
          width:100%;margin-top:25px;"
        )
      )
    ),
    shiny::uiOutput(ns("editfunction"))
  )
}

