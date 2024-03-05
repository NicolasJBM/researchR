#' @name annotate_source_ui
#' @title Annotate sources
#' @author Nicolas Mangin
#' @description Module facilitating literature reviews, document and questionnaire coding.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Change internal sources and databases
#' @importFrom editR selection_ui
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shiny NS
#' @importFrom shiny actionButton
#' @importFrom shiny column
#' @importFrom shiny fluidRow
#' @importFrom shiny icon
#' @importFrom shiny span
#' @importFrom shiny tabPanel
#' @importFrom shiny uiOutput
#' @importFrom shinyWidgets radioGroupButtons
#' @importFrom shinyWidgets searchInput
#' @importFrom shinydashboard tabBox
#' @export


annotate_source_ui <- function(id){
  ns <- shiny::NS(id)
  base::list(
    shiny::fluidRow(
      shiny::column(
        2,
        shinyWidgets::radioGroupButtons(
          inputId = ns("slctdoctype"),
          label = "Document type:", 
          choices = c(
            `<i class='fa fa-newspaper'></i>` = "Article",
            `<i class='fa fa-file-lines'></i>` = "Document",
            `<i class='fa fa-microphone'></i>` = "Interview"
          ),
          justified = TRUE
        ),
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::actionButton(
              ns("newsource"), "New", icon = shiny::icon("wand-magic-sparkles"),
              style = "background-color:#003366;color:#FFF;width:100%;margin-top:25px;"
            )
          ),
          shiny::column(
            6,
            shiny::actionButton(
              ns("opensource"), "Open", icon = shiny::icon("book-open-reader"),
              style = "background-color:#660033;color:#FFF;width:100%;margin-top:25px;"
            )
          )
        )
      ),
      shiny::column(
        6,
        shinyWidgets::searchInput(
          inputId = ns("regexintitle"),
          label = "Search in title:", 
          placeholder = "Regular expressions",
          btnSearch = icon("magnifying-glass"), 
          btnReset = icon("xmark"),
          width = "100%"
        ),
        shinyWidgets::searchInput(
          inputId = ns("regexinkeywords"),
          label = "Search in keywords:", 
          placeholder = "Regular expressions",
          btnSearch = icon("magnifying-glass"), 
          btnReset = icon("xmark"),
          width = "100%"
        )
      ),
      shiny::column(
        4,
        editR::selection_ui(ns("slctsource"))
      )
    ),
    shiny::fluidRow(
      shiny::column(
        8,
        shinydashboard::tabBox(
          side = "left", width = "100%",
          
          
          
          shiny::tabPanel(
            title = shiny::span(
              shiny::icon("clipboard-list"), "Notes",
              title = ""
            ),
            shiny::actionButton(
              ns("savenotes"), "Save", icon = shiny::icon("floppy-disk"),
              style = "background-color:#006633;color:#FFF;width:100%;margin-top:25px;"
            ),
            shiny::uiOutput(ns("editnotes"))
          ),
          
          
          
          shiny::tabPanel(
            title = shiny::span(
              shiny::icon("lightbulb"), "Concepts",
              title = ""
            ),
            shiny::fluidRow(
              shiny::column(
                3,
                shiny::actionButton(
                  ns("saveconcepts"), "Save", icon = shiny::icon("floppy-disk"),
                  style = "background-color:#006633;color:#FFF;width:100%;margin-top:25px;"
                )
              ),
              shiny::column(
                9,
                shinyWidgets::searchInput(
                  inputId = ns("regexconcepts"),
                  label = "Search for concepts:", 
                  placeholder = "Regular expressions",
                  btnSearch = icon("magnifying-glass"), 
                  btnReset = icon("xmark"),
                  width = "100%"
                )
              )
            ),
            rhandsontable::rHandsontableOutput(ns("editconcepts"))
          ),
          
          
          
          shiny::tabPanel(
            title = shiny::span(
              shiny::icon("user-graduate"), "Definitions",
              title = ""
            ),
            shiny::fluidRow(
              shiny::column(
                3,
                shiny::actionButton(
                  ns("savedefinitions"), "Save", icon = shiny::icon("floppy-disk"),
                  style = "background-color:#006633;color:#FFF;width:100%;margin-top:25px;"
                )
              ),
              shiny::column(
                9,
                shinyWidgets::searchInput(
                  inputId = ns("regexdefinitions"),
                  label = "Search for concepts:", 
                  placeholder = "Regular expressions",
                  btnSearch = icon("magnifying-glass"), 
                  btnReset = icon("xmark"),
                  width = "100%"
                )
              )
            ),
            rhandsontable::rHandsontableOutput(ns("editdefinitions"))
          ),
          
          
          
          shiny::tabPanel(
            title = shiny::span(
              shiny::icon("circle-nodes"), "Relations",
              title = ""
            ),
            shiny::fluidRow(
              shiny::column(
                3,
                shiny::actionButton(
                  ns("saverelations"), "Save", icon = shiny::icon("floppy-disk"),
                  style = "background-color:#006633;color:#FFF;width:100%;margin-top:25px;"
                )
              ),
              shiny::column(
                9,
                shinyWidgets::searchInput(
                  inputId = ns("regexrelations"),
                  label = "Search for concepts:", 
                  placeholder = "Regular expressions",
                  btnSearch = icon("magnifying-glass"), 
                  btnReset = icon("xmark"),
                  width = "100%"
                )
              )
            ),
            rhandsontable::rHandsontableOutput(ns("editrelations"))
          ),
          
          
          
          shiny::tabPanel(
            title = shiny::span(
              shiny::icon("arrows-down-to-line"), "Moderations",
              title = ""
            ),
            shiny::fluidRow(
              shiny::column(
                3,
                shiny::actionButton(
                  ns("savemoderations"), "Save", icon = shiny::icon("floppy-disk"),
                  style = "background-color:#006633;color:#FFF;width:100%;margin-top:25px;"
                )
              ),
              shiny::column(
                9,
                shinyWidgets::searchInput(
                  inputId = ns("regexmoderations"),
                  label = "Search for concepts:", 
                  placeholder = "Regular expressions",
                  btnSearch = icon("magnifying-glass"), 
                  btnReset = icon("xmark"),
                  width = "100%"
                )
              )
            ),
            rhandsontable::rHandsontableOutput(ns("editmoderations"))
          ),
          
          
          
          shiny::tabPanel(
            title = shiny::span(
              shiny::icon("arrow-up-right-from-square"), "Indicators",
              title = ""
            ),
            shiny::fluidRow(
              shiny::column(
                3,
                shiny::actionButton(
                  ns("saveindicators"), "Save", icon = shiny::icon("floppy-disk"),
                  style = "background-color:#006633;color:#FFF;width:100%;margin-top:25px;"
                )
              ),
              shiny::column(
                9,
                shinyWidgets::searchInput(
                  inputId = ns("regexindicators"),
                  label = "Search for indicators:", 
                  placeholder = "Regular expressions",
                  btnSearch = icon("magnifying-glass"), 
                  btnReset = icon("xmark"),
                  width = "100%"
                )
              )
            ),
            rhandsontable::rHandsontableOutput(ns("editindicators"))
          ),
          
          
          
          shiny::tabPanel(
            title = shiny::span(
              shiny::icon("ruler"), "Operationalizations",
              title = ""
            ),
            shiny::fluidRow(
              shiny::column(
                3,
                shiny::actionButton(
                  ns("saveoperationalizations"), "Save", icon = shiny::icon("floppy-disk"),
                  style = "background-color:#006633;color:#FFF;width:100%;margin-top:25px;"
                )
              ),
              shiny::column(
                9,
                shinyWidgets::searchInput(
                  inputId = ns("regexoperationalizations"),
                  label = "Search for indicators:", 
                  placeholder = "Regular expressions",
                  btnSearch = icon("magnifying-glass"), 
                  btnReset = icon("xmark"),
                  width = "100%"
                )
              )
            ),
            rhandsontable::rHandsontableOutput(ns("editoperationalizations"))
          ),
          
          
          
          shiny::tabPanel(
            title = shiny::span(
              shiny::icon("magnifying-glass-chart"), "Observations",
              title = ""
            ),
            shiny::fluidRow(
              shiny::column(
                3,
                shiny::actionButton(
                  ns("saveobservations"), "Save", icon = shiny::icon("floppy-disk"),
                  style = "background-color:#006633;color:#FFF;width:100%;margin-top:25px;"
                )
              ),
              shiny::column(
                9,
                shinyWidgets::searchInput(
                  inputId = ns("regexobservations"),
                  label = "Search for indicators:", 
                  placeholder = "Regular expressions",
                  btnSearch = icon("magnifying-glass"), 
                  btnReset = icon("xmark"),
                  width = "100%"
                )
              )
            ),
            rhandsontable::rHandsontableOutput(ns("editobservations"))
          ),
          
          
          
          shiny::tabPanel(
            title = shiny::span(
              shiny::icon("diagram-project"), "Map",
              title = ""
            ),
            shiny::plotOutput(ns("mapnet"))
          )
          
          
          
        )
      ),
      shiny::column(
        4,
        shiny::uiOutput(ns("displaysource"))
      )
    )
  )
}

