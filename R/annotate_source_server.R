#' @name annotate_source_server
#' @title Load course data
#' @author Nicolas Mangin
#' @description Module facilitating the loading of all relevant course data.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param documents Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @param references Reactive. List of references.
#' @param study_paths Reactive. Paths to project databases and sub-folders.
#' @return A list of course data.
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom editR make_new_name
#' @importFrom editR selection_server
#' @importFrom rhandsontable hot_col
#' @importFrom rhandsontable hot_cols
#' @importFrom rhandsontable hot_context_menu
#' @importFrom rhandsontable hot_to_r
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
#' @importFrom shinyAce aceEditor
#' @importFrom shinyWidgets airDatepickerInput
#' @importFrom shinyWidgets checkboxGroupButtons
#' @importFrom shinyalert shinyalert
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_split
#' @importFrom tibble tibble
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
    concept_id <- NULL
    concept_id1 <- NULL
    concept_id2 <- NULL
    concept_label <- NULL
    concept_label1 <- NULL
    concept_label2 <- NULL
    definition <- NULL
    definition_id <- NULL
    direction <- NULL
    explanation <- NULL
    indicators <- NULL
    moderations <- NULL
    observations <- NULL
    operationalizations <- NULL
    position <- NULL
    relation_id <- NULL
    destination <- NULL
    moderation_id <- NULL
    moderator <- NULL
    origin <- NULL
    relation_label <- NULL
    keep <- NULL
    
    modrval <- shiny::reactiveValues()
    shiny::observe({
      shiny::req(base::length(study_paths()) == 2)
      base::load(study_paths()$databases$segments)
      modrval$segments <- segments
      
      base::load(study_paths()$databases$concepts)
      modrval$concepts <- concepts
      
      base::load(study_paths()$databases$definitions)
      modrval$definitions <- definitions
      
      base::load(study_paths()$databases$relations)
      modrval$relations <- relations
      
      base::load(study_paths()$databases$moderations)
      modrval$moderations <- moderations
      
      base::load(study_paths()$databases$indicators)
      modrval$indicators <- indicators
      
      base::load(study_paths()$databases$operationalizations)
      modrval$operationalizations <- operationalizations
      
      base::load(study_paths()$databases$observations)
      modrval$observations <- observations
      
    })
    
    ############################################################################
    # Source creation and selection
    
    preselection <- shiny::reactive({
      shiny::req(base::length(documents()) > 2)
      preselection_table <- documents() |>
        dplyr::filter(type == input$slctdoctype)
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
          slct <- c("Section","Content")
        } else {
          relevantchoices <- c(
            `<i class='fa fa-question-circle'> Q </i>` = "Question",
            `<i class='fa fa-comment-dots'> A </i>` = "Answer"
          )
          slct <- c("Question","Answer")
        }
        base::list(
          shiny::fluidRow(
            shiny::column(
              3,
              shinyWidgets::checkboxGroupButtons(
                inputId = ns("slctnature"),
                label = "Nature:", 
                choices = relevantchoices, selected = slct,
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
    # Edition of notes
    
    output$editnotes <- shiny::renderUI({
      shiny::req(!base::is.null(sourcedoc()))
      shiny::req(!base::is.na(sourcedoc()))
      shiny::req(sourcedoc() != "")
      path <- base::paste0(study_paths()$subfolders$original, "/", sourcedoc())
      shiny::req(base::file.exists(path))
      notes <- base::readLines(path)
      shinyAce::aceEditor(
        outputId = ns("editednotes"), value = notes,
        mode = "markdown", wordWrap = TRUE, debounce = 10,
        autoComplete = "live", height = "700"
      )
    })
    
    
    
    
    
    
    
    shiny::observeEvent(input$savenotes, {
      
    })
    
    
    
    
    
    
    
    ############################################################################
    # Edition of concepts
    
    output$editconcepts <- rhandsontable::renderRHandsontable({
      if (input$regexconcepts != ""){
        selected <- modrval$concepts |>
          dplyr::filter(stringr::str_detect(base::tolower(concept_label), base::tolower(input$regexconcepts)))
      } else selected <- modrval$concepts
      newid <- searchR::make_new_id(modrval$concepts$concept_id, "CPT")
      add <- tibble::tibble(
        concept_id = newid,
        concept_symbol = base::as.character(NA),
        concept_label = base::as.character(NA)
      )
      dplyr::bind_rows(selected, add) |>
        dplyr::arrange(concept_label) |>
        dplyr::mutate(keep = TRUE) |>
        rhandsontable::rhandsontable(width = "100%", rowHeaders = NULL, stretchH = "all") |>
        rhandsontable::hot_col(1, readOnly = TRUE) |>
        rhandsontable::hot_cols(
          colWidths = c("15%","25%","55%","5%"),
          manualColumnResize = TRUE
        ) |>
        rhandsontable::hot_context_menu(
          allowRowEdit = TRUE, allowColEdit = FALSE
        )
    })
    
    shiny::observeEvent(input$saveconcepts, {
      table <- input$editconcepts |>
        rhandsontable::hot_to_r() |>
        stats::na.omit() |>
        dplyr::filter(concept_label != "")
      excluded <-  dplyr::filter(modrval$concepts, !(concept_id %in% table$concept_id))
      edited <- dplyr::filter(table, concept_id %in% modrval$concepts$concept_id)
      added <- dplyr::filter(table, !(concept_id %in% modrval$concepts$concept_id))
      concepts <- dplyr::bind_rows(base::list(excluded, edited, added)) |>
        dplyr::filter(keep == TRUE) |> dplyr::select(-keep) |>
        dplyr::arrange(concept_label)
      modrval$concepts <- concepts
      base::save(concepts, file = study_paths()$databases$concepts)
      shinyalert::shinyalert(title = "Concepts saved", type = "success")
    })
    
    
    
    ############################################################################
    # Edition of definitions
    
    output$editdefinitions <- rhandsontable::renderRHandsontable({
      shiny::req(!base::is.null(sourcedoc()))
      definitions <- modrval$definitions |>
        dplyr::filter(file == sourcedoc()) |>
        dplyr::left_join(dplyr::select(modrval$concepts, concept_id, concept_label), by = "concept_id") |>
        dplyr::select(definition_id, file, concept_label, definition, segment, position)
      if (input$regexdefinitions != ""){
        selected <- definitions |>
          dplyr::filter(stringr::str_detect(base::tolower(concept_label), base::tolower(input$regexdefinitions)))
      } else selected <- definitions
      newid <- searchR::make_new_id(modrval$definitions$definition_id, "DEF")
      add <- tibble::tibble(
        definition_id = newid,
        file = sourcedoc(),
        concept_label = base::as.character(NA),
        definition = base::as.character(NA),
        segment = base::as.integer(NA),
        position = base::as.character(NA)
      )
      dplyr::bind_rows(selected, add) |>
        dplyr::arrange(concept_label) |>
        dplyr::mutate(
          concept_label = base::factor(concept_label, levels = c("", modrval$concepts$concept_label)),
          keep = TRUE
        ) |>
        rhandsontable::rhandsontable(width = "100%", rowHeaders = NULL, stretchH = "all") |>
        rhandsontable::hot_col(c(1,2), readOnly = TRUE) |>
        rhandsontable::hot_cols(
          colWidths = c("10%","10%","20%","35%","10%","10%","5%"),
          manualColumnResize = TRUE
        ) |>
        rhandsontable::hot_context_menu(
          allowRowEdit = TRUE, allowColEdit = FALSE
        )
    })
    
    shiny::observeEvent(input$savedefinitions, {
      shiny::req(!base::is.null(input$editdefinitions))
      table <- input$editdefinitions |>
        rhandsontable::hot_to_r() |>
        dplyr::filter(!base::is.na(concept_label)) |>
        dplyr::filter(concept_label != "", definition != "")
      excluded <-  dplyr::filter(modrval$definitions, !(definition_id %in% table$definition_id))
      edited <- dplyr::filter(table, definition_id %in% modrval$definitions$definition_id)
      added <- dplyr::filter(table, !(definition_id %in% modrval$definitions$definition_id))
      definitions <- dplyr::bind_rows(base::list(edited, added)) |>
        dplyr::filter(keep == TRUE) |> dplyr::select(-keep) |>
        dplyr::mutate(concept_label = base::as.character(concept_label)) |>
        dplyr::left_join(dplyr::select(modrval$concepts, concept_label, concept_id), by = "concept_label") |>
        dplyr::select(definition_id, file, concept_id, definition, segment, position) |>
        dplyr::bind_rows(excluded) |>
        dplyr::arrange(concept_label)
      modrval$definitions <- definitions
      base::save(definitions, file = study_paths()$databases$definitions)
      shinyalert::shinyalert(title = "Definitions saved", type = "success")
    })
    
    
    
    ############################################################################
    # Edition of relations
    
    output$editrelations <- rhandsontable::renderRHandsontable({
      shiny::req(!base::is.null(sourcedoc()))
      relations <- modrval$relations |>
        dplyr::filter(file == sourcedoc()) |>
        dplyr::left_join(dplyr::select(modrval$concepts, concept_id1 = concept_id, concept_label1 = concept_label), by = "concept_id1") |>
        dplyr::left_join(dplyr::select(modrval$concepts, concept_id2 = concept_id, concept_label2 = concept_label), by = "concept_id2") |>
        dplyr::select(relation_id, file, concept_label1, direction, sign, concept_label2, type, explanation, segment, position)
      if (input$regexrelations != ""){
        selected1 <- relations |>
          dplyr::filter(stringr::str_detect(base::tolower(concept_label1), base::tolower(input$regexrelations)))
        selected2 <- relations |>
          dplyr::filter(stringr::str_detect(base::tolower(concept_label2), base::tolower(input$regexrelations)))
        selected <- base::unique(dplyr::bind_rows(selected1, selected2))
      } else selected <- relations
      newid <- searchR::make_new_id(modrval$relations$relation_id, "REL")
      add <- tibble::tibble(
        relation_id = newid,
        file = sourcedoc(),
        concept_label1 = base::as.character(NA),
        direction = base::as.character(NA),
        sign = base::as.character(NA),
        concept_label2 = base::as.character(NA),
        type = base::as.character(NA),
        explanation = base::as.character(NA), 
        segment = base::as.integer(NA),
        position = base::as.character(NA)
      )
      dplyr::bind_rows(selected, add) |>
        dplyr::arrange(concept_label1, concept_label2) |>
        dplyr::mutate(
          concept_label1 = base::factor(concept_label1, levels = c("", modrval$concepts$concept_label)),
          direction = base::factor(direction, levels = c("-->","<--","---","<->")),
          sign = base::factor(sign, levels = c("+","-","?","+/-")),
          concept_label2 = base::factor(concept_label2, levels = c("", modrval$concepts$concept_label)),
          type = base::factor(type, levels = c("Defitinion","Composition","Causation","Other")),
          keep = TRUE
        ) |>
        rhandsontable::rhandsontable(width = "100%", rowHeaders = NULL, stretchH = "all") |>
        rhandsontable::hot_col(c(1,2), readOnly = TRUE) |>
        rhandsontable::hot_cols(
          colWidths = c("5%","5%","15%","5%","5%","15%","10%","25%","5%","5%","5%"),
          manualColumnResize = TRUE
        ) |>
        rhandsontable::hot_context_menu(
          allowRowEdit = TRUE, allowColEdit = FALSE
        )
    })
    
    shiny::observeEvent(input$saverelations, {
      shiny::req(!base::is.null(input$editrelations))
      table <- input$editrelations |>
        rhandsontable::hot_to_r() |>
        dplyr::filter(!base::is.na(concept_label1), !base::is.na(concept_label2)) |>
        dplyr::filter(concept_label1 != "", concept_label2 != "")
      excluded <-  dplyr::filter(modrval$relations, !(relation_id %in% table$relation_id))
      edited <- dplyr::filter(table, relation_id %in% modrval$relations$relation_id)
      added <- dplyr::filter(table, !(relation_id %in% modrval$relations$relation_id))
      relations <- dplyr::bind_rows(base::list(edited, added)) |>
        dplyr::filter(keep == TRUE) |> dplyr::select(-keep) |>
        dplyr::mutate(
          concept_label1 = base::as.character(concept_label1),
          direction = base::as.character(direction),
          sign = base::as.character(sign),
          concept_label2 = base::as.character(concept_label2),
          type = base::as.character(type)
        ) |>
        dplyr::arrange(concept_label1, concept_label2) |>
        dplyr::left_join(dplyr::select(modrval$concepts, concept_label1 = concept_label, concept_id1 = concept_id), by = "concept_label1") |>
        dplyr::left_join(dplyr::select(modrval$concepts, concept_label2 = concept_label, concept_id2 = concept_id), by = "concept_label2") |>
        dplyr::select(relation_id, file, concept_id1, direction, sign, concept_id2, type, explanation, segment, position) |>
        dplyr::bind_rows(excluded)
      modrval$relations <- relations
      base::save(relations, file = study_paths()$databases$relations)
      shinyalert::shinyalert(title = "relations saved", type = "success")
    })
    
    
    
    ############################################################################
    # Edition of moderations
    
    short_relations <- shiny::reactive({
      shiny::req(!base::is.null(sourcedoc()))
      shiny::req(!base::is.null(modrval$relations))
      modrval$relations |>
        dplyr::filter(file == sourcedoc()) |>
        dplyr::select(relation_id, file, concept_id1, concept_id2, direction) |>
        dplyr::left_join(dplyr::select(modrval$concepts, concept_id1 = concept_id, origin = concept_label), by = "concept_id1") |>
        dplyr::left_join(dplyr::select(modrval$concepts, concept_id2 = concept_id, destination = concept_label), by = "concept_id2") |>
        dplyr::mutate(relation_label = base::paste0(origin, " ", direction, " ", destination)) |>
        dplyr::select(relation_id, file, relation_label)
    })
    
    output$editmoderations <- rhandsontable::renderRHandsontable({
      shiny::req(!base::is.null(sourcedoc()))
      shiny::req(!base::is.null(short_relations()))
      moderations <- modrval$moderations |>
        dplyr::filter(file == sourcedoc()) |>
        dplyr::left_join(short_relations(), by = c("relation_id", "file")) |>
        dplyr::left_join(dplyr::select(modrval$concepts, concept_id, moderator = concept_label), by = "concept_id") |>
        dplyr::select(moderation_id, file, moderator, sign, relation_label, explanation, segment, position)
      if (input$regexmoderations != ""){
        selected1 <- moderations |>
          dplyr::filter(stringr::str_detect(base::tolower(moderator), base::tolower(input$regexmoderations)))
        selected2 <- moderations |>
          dplyr::filter(stringr::str_detect(base::tolower(relation_label), base::tolower(input$regexmoderations)))
        selected <- base::unique(dplyr::bind_rows(selected1, selected2))
      } else selected <- moderations
      newid <- searchR::make_new_id(modrval$moderations$moderation_id, "MOD")
      add <- tibble::tibble(
        moderation_id = newid,
        file = sourcedoc(),
        moderator = base::as.character(NA),
        sign = base::as.character(NA),
        relation_label = base::as.character(NA),
        explanation = base::as.character(NA), 
        segment = base::as.integer(NA),
        position = base::as.character(NA)
      )
      dplyr::bind_rows(selected, add) |>
        dplyr::arrange(moderator, relation_label) |>
        dplyr::mutate(
          moderator = base::factor(moderator, levels = c("", modrval$concepts$concept_label)),
          sign = base::factor(sign, levels = c("+","-","?","+/-")),
          relation_label = base::factor(relation_label, levels = c("", short_relations()$relation_label)),
          keep = TRUE
        ) |>
        rhandsontable::rhandsontable(width = "100%", rowHeaders = NULL, stretchH = "all") |>
        rhandsontable::hot_col(c(1,2), readOnly = TRUE) |>
        rhandsontable::hot_cols(
          colWidths = c("5%","5%","20%","5%","20%","30%","5%","5%","5%"),
          manualColumnResize = TRUE
        ) |>
        rhandsontable::hot_context_menu(
          allowRowEdit = TRUE, allowColEdit = FALSE
        )
    })
    
    shiny::observeEvent(input$savemoderations, {
      shiny::req(!base::is.null(input$editmoderations))
      shiny::req(!base::is.null(short_relations()))
      table <- input$editmoderations |>
        rhandsontable::hot_to_r() |>
        dplyr::filter(!base::is.na(moderator), !base::is.na(relation_label)) |>
        dplyr::filter(moderator != "", relation_label != "")
      excluded <-  dplyr::filter(modrval$moderations, !(moderation_id %in% table$moderation_id))
      edited <- dplyr::filter(table, moderation_id %in% modrval$moderations$moderation_id)
      added <- dplyr::filter(table, !(moderation_id %in% modrval$moderations$moderation_id))
      moderations <- dplyr::bind_rows(base::list(edited, added)) |>
        dplyr::filter(keep == TRUE) |> dplyr::select(-keep) |>
        dplyr::mutate(
          moderator = base::as.character(moderator),
          relation_label = base::as.character(relation_label),
          sign = base::as.character(sign)
        ) |>
        dplyr::arrange(moderator, relation_label) |>
        dplyr::left_join(dplyr::select(modrval$concepts, moderator = concept_label, concept_id), by = "moderator") |>
        dplyr::left_join(short_relations(), by = c("relation_label","file")) |>
        dplyr::select(moderation_id, file, concept_id, sign, relation_id, explanation, segment, position) |>
        dplyr::bind_rows(excluded)
      modrval$moderations <- moderations
      base::save(moderations, file = study_paths()$databases$moderations)
      shinyalert::shinyalert(title = "moderations saved", type = "success")
    })
    
    
    
    
    
    
    
    
    
    
    
    
  })
}

