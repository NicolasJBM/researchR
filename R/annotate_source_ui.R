#' @name annotate_source_ui
#' @title Load course data
#' @author Nicolas Mangin
#' @description Module facilitating the loading of all relevant course data.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return A list of course data.
#' @importFrom editR selection_ui
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
        shiny::actionButton(
          ns("newsource"), "New", icon = shiny::icon("wand-magic-sparkles"),
          style = "background-color:#003366;color:#FFF;width:100%;margin-bottom:5px;"
        ),
        shiny::actionButton(
          ns("opensource"), "Open", icon = shiny::icon("book-open-reader"),
          style = "background-color:#660033;color:#FFF;width:100%;margin-bottom:5px;"
        )
      ),
      shiny::column(
        5,
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
        5,
        editR::selection_ui(ns("slctsource"))
      )
    ),
    shiny::fluidRow(
      shiny::column(
        7,
        shinydashboard::tabBox(
          side = "left", width = "100%",
          shiny::tabPanel(
            title = shiny::span(
              shiny::icon("clipboard-list"), "Notes",
              title = ""
            )
          ),
          shiny::tabPanel(
            title = shiny::span(
              shiny::icon("lightbulb"), "Concepts",
              title = ""
            )
          ),
          shiny::tabPanel(
            title = shiny::span(
              shiny::icon("user-graduate"), "definitions",
              title = ""
            )
          ),
          shiny::tabPanel(
            title = shiny::span(
              shiny::icon("diagram-project"), "Relations",
              title = ""
            )
          ),
          shiny::tabPanel(
            title = shiny::span(
              shiny::icon("diagram-project"), "Moderations",
              title = ""
            )
          ),
          shiny::tabPanel(
            title = shiny::span(
              shiny::icon("ruler"), "Indicators",
              title = ""
            )
          ),
          shiny::tabPanel(
            title = shiny::span(
              shiny::icon("ruler"), "Operationalizations",
              title = ""
            )
          ),
          shiny::tabPanel(
            title = shiny::span(
              shiny::icon("magnifying-glass-chart"), "Observations",
              title = ""
            )
          )
        )
      ),
      shiny::column(
        5,
        shiny::uiOutput(ns("displaysource"))
      )
    )
  )
}

