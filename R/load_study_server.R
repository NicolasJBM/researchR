#' @name load_study_server
#' @title Load course data
#' @author Nicolas Mangin
#' @description Module facilitating the loading of all relevant course data.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @param study_paths Reactive. Function containing a list of paths to the different folders and databases on local disk.
#' @return A list of course data.
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom readr read_csv
#' @importFrom readxl read_excel
#' @importFrom scholR document_data
#' @importFrom shiny NS
#' @importFrom shiny isolate
#' @importFrom shiny moduleServer
#' @importFrom shiny reactive
#' @importFrom shiny req
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy remove_modal_spinner
#' @importFrom shinybusy show_modal_progress_line
#' @importFrom shinybusy update_modal_progress
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_replace
#' @importFrom stringr str_split
#' @importFrom tibble rowid_to_column
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr separate
#' @importFrom tidyr unnest
#' @export


load_study_server <- function(id, study_paths){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    V1 <- NULL
    V2 <- NULL
    concepts <- NULL
    data <- NULL
    definitions <- NULL
    document <- NULL
    document_types <- NULL
    indicators <- NULL
    language <- NULL
    moderations <- NULL
    nature <- NULL
    observations <- NULL
    operationalizations <- NULL
    relations <- NULL
    segment <- NULL
    tags <- NULL
    text <- NULL
    translation <- NULL
    type <- NULL
    
    documents <- shiny::reactive({
      input$loadstudy
      
      shiny::isolate({
        shiny::req(base::length(study_paths()) == 2)
        if (base::length(study_paths()) != 2){
          
          shinyalert::shinyalert(
            title = "Please select an investigation folder.",
            text = "You must first select a course folder to perform this action.",
            type = "error"
          )
          
        } else {
          
          # Get appendices
          shinybusy::show_modal_progress_line(value = 0/5, text = "Retrieve appendices")
          base::load(study_paths()$databases$doctypes)
          translations <- tibble::tibble(
            file = base::list.files(study_paths()$subfolders$translated, full.names = FALSE)
          ) |>
            tidyr::separate(file, into = c("document","translation"), sep = "_", remove = FALSE) |>
            dplyr::mutate(translation = stringr::str_remove_all(translation, ".Rmd$")) |>
            dplyr::group_by(document) |>
            dplyr::summarise(translations = base::paste(translation, collapse = "-"))
          
          # List and classify documents
          shinybusy::update_modal_progress(value = 1/5, text = "List and classify all documents")
          documents <- tibble::tibble(
            path = base::list.files(study_paths()$subfolders$original, full.names = TRUE),
            file = base::list.files(study_paths()$subfolders$original, full.names = FALSE)
          ) |>
            tidyr::separate(file, into = c("document","language"), sep = "_", remove = FALSE) |>
            dplyr::mutate(language = stringr::str_remove_all(language, ".Rmd$")) |>
            dplyr::mutate(
              type = purrr::map_chr(document, base::substr, 1,1),
              type = dplyr::case_when(
                type == "A" ~ "Article",
                type == "I" ~ "Interview",
                type == "D" ~ "Document",
                type == "Q" ~ "Question",
                type == "C" ~ "Choice",
                type == "E" ~ "Experiment",
                type == "N" ~ "Note",
                type == "S" ~ "Slide",
                type == "V" ~ "Video",
                type == "P" ~ "Paper",
                TRUE ~ "Book"
              )
            ) |>
            dplyr::left_join(translations, by = "document") |>
            dplyr::left_join(document_types, by = "type")
          
          # Read meta-information
          shinybusy::update_modal_progress(value = 2/5, text = "Read and update documents' meta-information")
          documents <- documents |>
            dplyr::mutate(tags = purrr::map(path, function(x){
              x <- base::readLines(x)
              x <- x[stringr::str_detect(x, "^exextra")] |>
                stringr::str_split("]:", simplify = TRUE) |>
                base::as.data.frame() |>
                dplyr::mutate(
                  V1 = stringr::str_remove_all(V1, pattern = "exextra\\["),
                  V2 = base::trimws(V2)
                ) |>
                tidyr::pivot_wider(names_from = "V1", values_from = "V2") 
            })) |>
            dplyr::select(-path) |>
            tidyr::unnest(tags) |>
            dplyr::mutate_all(function(x) base::replace(x, x == "", NA))
          base::save(documents, file = study_paths()$databases$documents)
          
          # Segment additional interviews and documents
          shinybusy::update_modal_progress(value = 3/5, text = "Segment new interviews and documents")
          base::load(study_paths()$databases$segments)
          all_segments <- documents |>
            dplyr::filter(
              type %in% c("Interview","Document"),
              !base::is.na(source),
              stringr::str_detect(source, ".txt$")
            ) |>
            dplyr::select(file, type, source)
          existing_segments <- base::intersect(all_segments$file, segments$file)
          missing_segment <- base::setdiff(all_segments$file, segments$file)
          extra_segments <- base::setdiff(segments$file, all_segments$file)
          segments <- segments |>
            dplyr::filter(file %in% existing_segments)
          if (base::length(missing_segment) > 0){
            new_segments <- all_segments |>
              dplyr::filter(file %in% missing_segment) |>
              dplyr::mutate(path = base::paste0("www/", type, "/", source)) |>
              dplyr::mutate(
                data = purrr::map(path, function(x){
                  y <- base::readLines(x)
                  y <- tibble::tibble(text = y) |>
                    tibble::rowid_to_column("segment") |>
                    dplyr::mutate(segment = base::as.integer(segment)) 
                })
              ) |>
              tidyr::unnest(data) |>
              dplyr::mutate(
                nature = dplyr::case_when(
                  type == "Interview" & stringr::str_detect(text, "^Q[0-9]{0,3} : ") ~ "Question",
                  type == "Interview" & stringr::str_detect(text, "^A[0-9]{0,3} : ") ~ "Answer",
                  type == "Document" & stringr::str_detect(text, "^S[0-9]{0,3} : ") ~ "Section",
                  TRUE ~ "Content"
                )
              ) |>
              dplyr::mutate(text = stringr::str_remove_all(text, "^[Q,A,S][0-9]{0,3} : ")) |>
              dplyr::select(file, segment, nature, text)
            
            segments <- segments |>
              dplyr::bind_rows(new_segments)
          }
          base::save(segments, file = study_paths()$databases$segments)
          
          # Add functions and databases
          shinybusy::update_modal_progress(value = 4/5, text = "Adding functions and databases to package")
          dfRfolder <- base::paste0(study_paths()$subfolders$package, "/R")
          if (!base::dir.exists(dfRfolder)) base::dir.create(dfRfolder)
          base::unlink(base::list.files(dfRfolder, full.names = TRUE))
          # Add functions
          base::lapply(
            base::list.files(
              study_paths()$subfolders$functions,
              full.names = TRUE, recursive = TRUE
            ), function(x){
              if (stringr::str_detect(x, "\\.R$")) {
                base::source(x)
                base::file.copy(
                  from = x,
                  to = stringr::str_replace(
                    x,
                    study_paths()$subfolders$functions,
                    dfRfolder
                  ),
                  overwrite = TRUE
                )
              }
            }
          )
          # Add custom databases
          dfdatafolder <- base::paste0(study_paths()$subfolders$package, "/data")
          if (!base::dir.exists(dfdatafolder)) base::dir.create(dfdatafolder)
          base::unlink(base::list.files(dfdatafolder, full.names = TRUE))
          databases <- base::list.files(study_paths()$subfolders$collected, full.names = FALSE)
          databases <- databases[stringr::str_detect(databases, "csv$|xlsx$")]
          if (base::length(databases) > 0){
            for (d in databases){
              path <- base::paste0(study_paths()$subfolders$collected, "/", d)
              if (stringr::str_detect(path, "csv$")){
                dcontent <- readr::read_csv(path, show_col_types = FALSE)
                dname <- stringr::str_remove(d, ".csv$")
              } else {
                dcontent <- readxl::read_excel(path)
                dname <- stringr::str_remove(d, ".xlsx$")
              }
              base::assign(x = dname, value = dcontent, envir = .GlobalEnv)
              scholR::document_data(
                x = dcontent,
                datname = dname,
                path = dfRfolder
              )
              base::save(list = dname, file=base::paste0(dfdatafolder, '/', dname, '.RData'))
            }
            base::rm(databases, d, dcontent, dname)
          }
          
          # Add system databases
          base::load(study_paths()$databases$documents)
          scholR::document_data(x = documents, datname = "documents", path = dfRfolder)
          base::save(documents, file=base::paste0(dfdatafolder, '/documents.RData'))
          
          base::load(study_paths()$databases$segments)
          scholR::document_data(x = segments, datname = "segments", path = dfRfolder)
          base::save(segments, file=base::paste0(dfdatafolder, '/segments.RData'))
          
          base::load(study_paths()$databases$concepts)
          scholR::document_data(x = concepts, datname = "concepts", path = dfRfolder)
          base::save(concepts, file=base::paste0(dfdatafolder, '/concepts.RData'))
          
          base::load(study_paths()$databases$definitions)
          scholR::document_data(x = definitions, datname = "definitions", path = dfRfolder)
          base::save(definitions, file=base::paste0(dfdatafolder, '/definitions.RData'))
          
          base::load(study_paths()$databases$relations)
          scholR::document_data(x = relations, datname = "relations", path = dfRfolder)
          base::save(relations, file=base::paste0(dfdatafolder, '/relations.RData'))
          
          base::load(study_paths()$databases$moderations)
          scholR::document_data(x = moderations, datname = "moderations", path = dfRfolder)
          base::save(moderations, file=base::paste0(dfdatafolder, '/moderations.RData'))
          
          base::load(study_paths()$databases$indicators)
          scholR::document_data(x = indicators, datname = "indicators", path = dfRfolder)
          base::save(indicators, file=base::paste0(dfdatafolder, '/indicators.RData'))
          
          base::load(study_paths()$databases$operationalizations)
          scholR::document_data(x = operationalizations, datname = "operationalizations", path = dfRfolder)
          base::save(operationalizations, file=base::paste0(dfdatafolder, '/operationalizations.RData'))
          
          base::load(study_paths()$databases$observations)
          scholR::document_data(x = observations, datname = "observations", path = dfRfolder)
          base::save(observations, file=base::paste0(dfdatafolder, '/observations.RData'))
          
          
          shinybusy::update_modal_progress(value = 5/5, text = "Study loaded")
          shinybusy::remove_modal_spinner()
          shinyalert::shinyalert(
            title = "Investigation loaded and updated!",
            text = "All study documents and data are now loaded.",
            type = "success"
          )
        }
        
        documents
      })
    })
    
    return(documents)
  })
}

