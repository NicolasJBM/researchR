#' @name literature_edit_ui
#' @title Edit notes on the literature
#' @author Nicolas Mangin
#' @description Module facilitating the quick edition of notes on the literature.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Save the new or modified note in the folder "3_documents".
#' @import shiny
#' @export


literature_edit_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::uiOutput(ns("select_pdf")),
    shiny::fluidRow(
      shiny::column(
        8,
        shiny::actionButton(
          ns("openpdf"), "Open", icon = shiny::icon("file-pdf"),
          style = "background-color:#003366;color:#FFF;
              width:100%;margin-bottom:10px;"
        ),
        shiny::uiOutput(ns("pdfviewer"))
      ),
      shiny::column(
        4,
        shiny::actionButton(
          ns("saveliterature"), "Save", icon = shiny::icon("save"),
          style = "background-color:#006633;color:#FFF;
          width:100%;margin-bottom:10px;"
        ),
        shiny::uiOutput(ns("editliterature"))
      )
    )
  )
}

