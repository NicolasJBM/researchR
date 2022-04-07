#' @name documents_edit_server
#' @title Edit notes on the literature
#' @author Nicolas Mangin
#' @description Module facilitating the quick edition of notes on the literature.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Save the new or modified note in the folder "3_documents".
#' @import shiny
#' @importFrom rstudioapi navigateToFile
#' @importFrom shinyAce aceEditor
#' @importFrom shinyalert shinyalert
#' @export


documents_edit_server <- function(id){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    
  
  })
}

