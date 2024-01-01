#' @name annotate_source_server
#' @title Load course data
#' @author Nicolas Mangin
#' @description Module facilitating the loading of all relevant course data.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param documents Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @param references Reactive. List of references.
#' @param study_paths Reactive. Paths to project databases and sub-folders.
#' @return A list of course data.
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom editR make_new_name
#' @importFrom editR selection_server
#' @importFrom rhandsontable hot_cols
#' @importFrom rhandsontable hot_context_menu
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom rhandsontable rhandsontable
#' @importFrom shiny NS
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny modalButton
#' @importFrom shiny modalDialog
#' @importFrom shiny moduleServer
#' @importFrom shiny observe
#' @importFrom shiny observeEvent
#' @importFrom shiny reactive
#' @importFrom shiny reactiveValues
#' @importFrom shiny removeModal
#' @importFrom shiny renderUI
#' @importFrom shiny req
#' @importFrom shiny selectInput
#' @importFrom shiny showModal
#' @importFrom shiny sliderInput
#' @importFrom shiny tagList
#' @importFrom shiny textInput
#' @importFrom shinyWidgets airDatepickerInput
#' @importFrom shinyWidgets checkboxGroupButtons
#' @importFrom shinyalert shinyalert
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_split
#' @export


annotate_source_server <- function(id, documents, references, study_paths){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    keywords <- NULL
    title <- NULL
    type <- NULL
    nature <- NULL
    segment <- NULL
    segments <- NULL
    
    modrval <- shiny::reactiveValues()
    shiny::observe({
      
      
      
      base::load(study_paths()$databases$segments)
      modrval$segments <- segments
      
      
      
      
      
    })
    
    ############################################################################
    # Source creation and selection
    
    preselection <- shiny::reactive({
      shiny::req(base::length(documents()) > 2)
      preselection_table <- documents() |>
        dplyr::filter(
          !base::is.na(source),
          type == input$slctdoctype
        )
      intitle <- base::as.character(base::tolower(stringr::str_split(input$regexintitle, pattern = " ", simplify = TRUE)))
      if (intitle[1] != ""){
        for (p in intitle) preselection_table <- dplyr::filter(preselection_table, stringr::str_detect(base::tolower(title), p))
      } else preselection_table <- preselection_table
      
      inkeywords <- base::as.character(base::tolower(stringr::str_split(input$regexinkeywords, pattern = " ", simplify = TRUE)))
      if (inkeywords[1] != ""){
        for (p in inkeywords) preselection_table <- dplyr::filter(preselection_table, stringr::str_detect(base::tolower(keywords), p))
      }
      
      if (base::nrow(preselection_table) > 0){
        preselection <- base::as.character(preselection_table$file)
        base::names(preselection) <- base::paste0(
          stringr::str_remove_all(preselection_table$source, "...$"),
          " - ",
          preselection_table$title
        )
      } else preselection <- ""
      preselection
    })
    
    shiny::observeEvent(input$newsource, {
      shiny::showModal(shiny::modalDialog(
        title = "New source",
        "Fill in the following meta-information:",
        shiny::fluidRow(
          shiny::column(
            2, shiny::textInput(ns("defsource"), "Source:", value = NA, width = "100%")
          ),
          shiny::column(
            8, shiny::textInput(ns("deftitle"), "Title:", value = NA, width = "100%")
          ),
          shiny::column(
            2,
            shinyWidgets::airDatepickerInput(
              inputId = ns("defdate"), label = "Date:",
              value = Sys.Date(), width = "100%"
            )
          )
        ),
        shiny::textInput(ns("defkeywords"), "Keywords:", value = NA, width = "100%"),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::selectInput(
              ns("slctprdgm"), "Paradigm:",
              choices = c("Psychology","Sociology","Economics","Social-Psychology","Behavioral Economics","Other"),
              selected = "Other", width = "100%"
            )
          ),
          shiny::column(
            6,
            shiny::selectInput(
              ns("slctstrat"), "Strategy:",
              choices = c(
                "Formal Theory","Computer Simulation",
                "Field Study","Field Experiment",
                "Experimental Simulations","Laboratory Experiment",
                "Judgment Task","Sample Survey",
                "Other"
              ),
              selected = "Other", width = "100%"
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            6, shiny::textInput(ns("linkinstrument"), "Instrument:", value = NA, width = "100%")
          ),
          shiny::column(
            6, shiny::textInput(ns("defcontext"), "Context:", value = NA, width = "100%")
          )
        ),
        shiny::fluidRow(
          shiny::column(
            6, shiny::textInput(ns("deflocation"), "Informant(s):", value = NA, width = "100%")
          ),
          shiny::column(
            6, shiny::textInput(ns("definformant"), "Location:", value = NA, width = "100%")
          )
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("addsource"), "OK")
        )
      ))
    })
    
    sourcedoc <- editR::selection_server("slctsource", preselection)
    
    filepath <- shiny::reactive({
      selected <-  documents() |>
        dplyr::filter(file == sourcedoc())
      base::paste0(selected$type, "/", selected$source)
    })
    
    shiny::observeEvent(input$opensource, {
      filepath <- base::paste0(base::getwd(),"/www/", filepath())
      if (base::file.exists(filepath)){
        system2("open", filepath)
      }
    })
    
    shiny::observeEvent(input$addsource, {
      shiny::removeModal()
      newfile <- editR::make_new_name(
        stringr::str_extract(input$slctdoctype, "^."),
        study_paths
      )
      path <- base::paste0(study_paths()$subfolders$original, "/", newfile)
      content <- c(
        "",
        "",
        "",
        "",
        "",
        "Meta-information",
        "================",
        base::paste0("exextra[source]: ", input$defsource,"  "),
        base::paste0("exextra[title]: ", input$deftitle,"  "),
        base::paste0("exextra[keywords]: ", input$defkeywords,"  "),
        base::paste0("exextra[paradigm]: ", input$slctprdgm,"  "),
        base::paste0("exextra[strategy]: ", input$slctstrat,"  "),
        base::paste0("exextra[instrument]: ", input$linkinstrument,"  "),
        base::paste0("exextra[context]: ", input$defcontext,"  "),
        base::paste0("exextra[location]: ", input$deflocation,"  "),
        base::paste0("exextra[informant]: ", input$definformant,"  "),
        base::paste0("exextra[date]: ", input$defdate,"  "),
        ""
      )
      base::writeLines(content, path)
      shinyalert::shinyalert(
        title = "New source added!",
        text = "Reload the investigation to update the document database.",
        type = "success"
      )
    })
    
    
    
    ############################################################################
    # Source display and edition
    
    selected_segments <- shiny::reactive({
      modrval$segments |>
        dplyr::filter(file == sourcedoc())
    })
    
    output$editsegments <- rhandsontable::renderRHandsontable({
      selected_segments() |>
        dplyr::select(-file) |>
        dplyr::filter(
          nature %in% input$slctnature,
          segment >= input$slctsegment[1], segment <= input$slctsegment[2]
        ) |>
        dplyr::mutate(nature = base::factor(nature, levels = c("Section","Content","Question","Answer"))) |>
        rhandsontable::rhandsontable(width = "100%", rowHeaders = NULL, stretchH = "all") |>
        rhandsontable::hot_cols(
          colWidths = c("10%","10%","80%"),
          manualColumnResize = TRUE
        ) |>
        rhandsontable::hot_context_menu(
          allowRowEdit = FALSE, allowColEdit = FALSE
        )
    })
    
    output$displaysource <- shiny::renderUI({
      shiny::req(!base::is.null(sourcedoc()))
      extension <- stringr::str_extract(filepath(), "...$")
      if (extension == "pdf"){
        shiny::tags$iframe(style="height:700px; width:100%; scrolling=yes", src=filepath())
      } else if (extension == "txt"){
        if (input$slctdoctype == "Document"){
          relevantchoices <- c(
            `<i class='fa fa-list-ol'> Section </i>` = "Section",
            `<i class='fa fa-bars'> Content </i>` = "Content"
          )
        } else {
          relevantchoices <- c(
            `<i class='fa fa-question-circle'> Q </i>` = "Question",
            `<i class='fa fa-comment-dots'> A </i>` = "Answer"
          )
        }
        base::list(
          shiny::fluidRow(
            shiny::column(
              3,
              shinyWidgets::checkboxGroupButtons(
                inputId = ns("slctnature"),
                label = "Nature:", 
                choices = relevantchoices,
                justified = TRUE
              )
            ),
            shiny::column(
              7,
              shiny::sliderInput(
                ns("slctsegment"), "Segments:",
                min = 1, max = base::max(selected_segments()$segment),
                value = c(1,2), width = "100%"
              )
            ),
            shiny::column(
              2,
              shiny::actionButton(
                ns("savesegments"), "Save", icon = shiny::icon("floppy-disk"),
                style = "background-color:#006633;color:#FFF;width:100%;margin-top:25px;"
              )
            )
          ),
          rhandsontable::rHandsontableOutput(ns("editsegments"))
        )
      }
    })
    
    
    
    
    
    
    
    
    # Save...
    shiny::observeEvent(input$savesegments, {
      
    })
    
    
    
    
    ############################################################################
    # Edition of notes and conceptual bases
    
    
    
    
    
    
    
    
    
    
    
  })
}

