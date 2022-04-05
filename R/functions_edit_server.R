#' @name functions_edit_server
#' @title Edit functions
#' @author Nicolas Mangin
#' @description Module facilitating the quick creation or modification of functions used in documents.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Save the new or modified function in the folder "1_preparation/functions".
#' @import shiny
#' @importFrom rstudioapi navigateToFile
#' @importFrom shinyAce aceEditor
#' @importFrom shinyalert shinyalert
#' @export


functions_edit_server <- function(id){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    modrval <- shiny::reactiveValues()
    
    shiny::observe({
      modrval$function_list <- c(
        "", base::list.files("1_preparation/functions")
      )
    })
    
    output$selectfunction <- shiny::renderUI({
      shiny::req(!base::is.null(modrval$function_list))
      shiny::selectInput(
        ns("selectedfunction"), "Select a function", choices = modrval$function_list,
        selected = "", width = "100%"
      )
    })
    
    shiny::observeEvent(input$newfunction, {
      shiny::showModal(
        shiny::modalDialog(
          style = "background-color:#001F3F;color:#FFF;margin-top:300px;",
          shiny::textInput(
            ns("newfunctionname"), "Name of the new function:", value = "",
            width = "100%"
          ),
          shiny::selectInput(
            ns("slctfunctionbasis"), "Based on the following function:",
            choices = modrval$function_list, selected = "", width = "100%"
          ),
          footer = tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(
              ns("createfunction"), "OK", icon = shiny::icon("check"),
              style = "background-color:#007777;color:#FFF;"
            )
          )
        )
      )
    })
    
    shiny::observeEvent(input$createfunction, {
      shiny::removeModal()
      shiny::req(input$newfunctionname != "")
      newname <- base::paste0(input$newfunctionname, ".R")
      if (newname %in% modrval$function_list){
        shinyalert::shinyalert(
          "Name already used!",
          base::paste0(
            "Sorry, but a function named ", input$selectedfunction,
            " already exists. Please use a different name."
          ),
          type = "error", closeOnEsc = FALSE, closeOnClickOutside = TRUE
        )
      } else {
        if (input$slctfunctionbasis == "") lines <- "" else {
          lines = base::readLines(
            base::paste0(
              "1_preparation/functions/", input$slctfunctionbasis
            )
          )
        }
        base::writeLines(
          lines,
          base::paste0(
            "1_preparation/functions/", newname
          ), useBytes = TRUE
        )
        modrval$function_list <- c(
          "", base::list.files("1_preparation/functions")
        )
        shiny::updateSelectInput(
          session,
          "selectedfunction",
          choices = modrval$function_list,
          selected = newname
        )
        shinyalert::shinyalert(
          "function created!",
          base::paste0(
            "The function ", newname, " has been created."
          ),
          type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
        )
      }
    })
    
    
    shiny::observeEvent(input$functioninrstudio, {
      shiny::req(input$selectedfunction != "")
      rstudioapi::navigateToFile(
        base::paste0(
          "1_preparation/functions/", input$selectedfunction
        )
      )
    })
    
    output$editfunction <- shiny::renderUI({
      shiny::req(input$selectedfunction != "")
      code <- base::readLines(
        base::paste0(
          "1_preparation/functions/", input$selectedfunction
        )
      )
      shinyAce::aceEditor(
        outputId = ns("editedfunction"), value = code, mode = "r",
        wordWrap = TRUE, debounce = 10, autoComplete = "live", height = "700px"
      )
    })
    
    shiny::observeEvent(input$savefunction, {
      shiny::req(input$selectedfunction != "")
      shiny::req(!base::is.null(input$editedfunction))
      base::writeLines(
        input$editedfunction,
        base::paste0(
          "1_preparation/functions/", input$selectedfunction
        ),
        useBytes = TRUE
      )
      shinyalert::shinyalert(
        "function saved!",
        "The changes you made to references have been saved.
        Refresh the application to see them.",
        type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
      )
    })
    
    
    shiny::observeEvent(input$deletefunction, {
      shiny::req(input$selectedfunction != "")
      shinyalert::shinyalert(
        "Are you sure?",
        base::paste0(
          "Are you sure you want to delete the function ",
          input$selectedfunction, "?"
        ),
        type = "warning", closeOnEsc = FALSE, closeOnClickOutside = TRUE,
        inputId = "confirmdeletefunction", showCancelButton = TRUE
      )
    })
    
    shiny::observeEvent(input$confirmdeletefunction, {
      shiny::req(input$selectedfunction != "")
      if (input$confirmdeletefunction){
        base::unlink(
          base::paste0(
            "1_preparation/functions/", input$selectedfunction
          )
        )
        modrval$function_list <- c(
          "", base::list.files("1_preparation/functions")
        )
        shiny::updateSelectInput(
          session,
          "selectedfunction",
          choices = modrval$function_list,
          selected = ""
        )
        shinyalert::shinyalert(
          "function deleted",
          base::paste0(
            "The function ", input$selectedfunction,
            " has been deleted."
          ),
          type = "success", closeOnEsc = FALSE, closeOnClickOutside = TRUE
        )
      }
    })
  
  })
}

