#' @name literature_edit_server
#' @title Edit notes on the literature
#' @author Nicolas Mangin
#' @description Module facilitating the quick edition of notes on the literature.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param selected_documents Tibble. List of documents.
#' @return Save the new or modified note in the folder "3_documents".
#' @import shiny
#' @importFrom rstudioapi navigateToFile
#' @importFrom shinyAce aceEditor
#' @importFrom shinyalert shinyalert
#' @export


literature_edit_server <- function(id, selected_documents){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    tag_pdf <- NULL
    type <- NULL
    
    output$select_pdf <- shiny::renderUI({
      pdf <- selected_documents |>
        dplyr::filter(type == "Literature")
      pdflist <- stats::na.omit(pdf$tag_pdf)
      base::names(pdflist) <- base::paste0(pdf$tag_pdf, " - ", pdf$title)
      shiny::req(base::length(pdflist) > 0)
      shiny::selectInput(
        ns("slctpdf"), "Select a paper",
        choices = pdflist, selected = pdflist[1],
        width = "100%"
      )
    })
    
    output$pdfviewer <- shiny::renderUI({
      shiny::req(!base::is.null(input$slctpdf))
      shiny::tags$iframe(
        style="height:650px; width:100%; scrolling=yes",
        src = base::paste0("pdf/", input$slctpdf)
      )
    })
    
    shiny::observeEvent(input$openpdf, {
      shiny::req(!base::is.null(input$slctpdf))
      path <- base::paste0(base::getwd(), "/www/pdf/", input$slctpdf)
      command <- base::paste0("open ", path)
      base::system(command, intern = FALSE)
    })
    
    output$editliterature <- shiny::renderUI({
      shiny::req(!base::is.null(input$slctpdf))
      slctdoc <- selected_documents |>
        dplyr::filter(tag_pdf == input$slctpdf)
      code <- base::readLines(base::paste0("3_documents/", slctdoc$file))
      shinyAce::aceEditor(
        outputId = ns("editedliterature"), value = code, mode = "r",
        wordWrap = TRUE, debounce = 10, autoComplete = "live", height = "700px"
      )
    })
    
    shiny::observeEvent(input$saveliterature, {
      shiny::req(!base::is.null(input$slctpdf))
      slctdoc <- selected_documents |>
        dplyr::filter(tag_pdf == input$slctpdf)
      shiny::req(!base::is.null(input$editedliterature))
      base::writeLines(
        input$editedliterature,
        base::paste0("3_documents/", slctdoc$file),
        useBytes = TRUE
      )
      shinyalert::shinyalert(
        "Note on the literature saved!",
        type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
      )
    })
    
  
  })
}

