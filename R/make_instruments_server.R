#' @name make_instruments_server
#' @title Make survey instruments
#' @author Nicolas Mangin
#' @description Module facilitating the creation of question, questionnaires, and experiments.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param documents Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @param references Reactive. List of references.
#' @param study_paths Reactive. Paths to project databases and sub-folders.
#' @return Modify instruments and databases
#' @export


make_instruments_server <- function(id, documents, study_paths){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    
    
    
  })
}

