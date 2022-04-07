#' @name processing_edit_server
#' @title Edit script and update objects
#' @author Nicolas Mangin
#' @description Module facilitating the quick edition of script and update objects for targets
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Save the new or modified script in the root folder.
#' @import shiny
#' @importFrom shinybusy show_modal_spinner
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom shinyalert shinyalert
#' @importFrom targets tar_make
#' @importFrom shinyAce aceEditor
#' @importFrom visNetwork renderVisNetwork
#' @importFrom targets tar_visnetwork
#' @export


processing_edit_server <- function(id){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    shiny::observeEvent(input$updateobjects, {
      shinybusy::show_modal_spinner(
        spin = "orbit",
        text = "Please wait while outdated objects are updated..."
      )
      targets::tar_make()
      shinybusy::remove_modal_spinner()
      shinyalert::shinyalert(
        title = "All objects have been updated.",
        type = "success",
        closeOnEsc = FALSE,
        closeOnClickOutside = TRUE
      )
    })
    
    output$edittargets <- shiny::renderUI({
      code <- base::readLines("_targets.R")
      shinyAce::aceEditor(
        outputId = ns("editedtargets"), value = code, mode = "r",
        wordWrap = TRUE, debounce = 10, autoComplete = "live", height = "700px"
      )
    })
    
    shiny::observeEvent(input$savetargets, {
      shiny::req(!base::is.null(input$editedtargets))
      base::writeLines(
        input$editedtargets,
        base::paste0("_targets.R"),
        useBytes = TRUE
      )
      shinyalert::shinyalert(
        "Targets script saved!",
        type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
      )
    })
    
    output$targetsnet <- visNetwork::renderVisNetwork({
      input$updatenetwork
      targets::tar_visnetwork(
        targets_only = FALSE,
        level_separation = 500
      )
    })
  
  })
}

