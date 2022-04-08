#' @name diagram_edit_ui
#' @title Edit diagrams
#' @author Nicolas Mangin
#' @description Module facilitating the quick creation of diagrams embedded in functions or documents.
#' @param id Character. ID of the module to connect the user interface to the appropriate server side.
#' @return Write lines of codes creating the different parts of a diagram which can then be embedded in function or document.
#' @import shiny
#' @importFrom tibble tibble
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom rhandsontable rhandsontable
#' @importFrom rhandsontable hot_context_menu
#' @importFrom DiagrammeR renderGrViz
#' @importFrom DiagrammeR render_graph
#' @importFrom shinyAce aceEditor
#' @importFrom rhandsontable hot_to_r
#' @export


diagram_edit_server <- function(id){
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    
    # Initialize ###############################################################
    
    modrval <- shiny::reactiveValues()
    
    shiny::observe({
      diagram <- searchR::diagram_initialize()
      modrval$nodes <- diagram$nodes
      modrval$relations <- diagram$relations
      modrval$moderations <- diagram$moderations
    })
    
    
    shiny::observeEvent(input$clear, {
      diagram <- searchR::diagram_initialize()
      modrval$nodes <- diagram$nodes
      modrval$relations <- diagram$relations
      modrval$moderations <- diagram$moderations
    })
    
    # Edit #####################################################################
    
    output$editnodes <- rhandsontable::renderRHandsontable({
      modrval$nodes |>
        rhandsontable::rhandsontable(
          height = 400, width = "100%", stretchH = "all"
        ) |>
        rhandsontable::hot_context_menu(
          allowRowEdit = TRUE, allowColEdit = FALSE
        )
    })
    
    output$editrelations <- rhandsontable::renderRHandsontable({
      modrval$relations |>
        rhandsontable::rhandsontable(
          height = 400, width = "100%", stretchH = "all"
        ) |>
        rhandsontable::hot_context_menu(
          allowRowEdit = TRUE, allowColEdit = FALSE
        )
    })
    
    output$editmoderations <- rhandsontable::renderRHandsontable({
      modrval$moderations |>
        rhandsontable::rhandsontable(
          height = 400, width = "100%", stretchH = "all"
        ) |>
        rhandsontable::hot_context_menu(
          allowRowEdit = TRUE, allowColEdit = FALSE
        )
    })
    
    shiny::observeEvent(input$applychanges, {
      nodes <- rhandsontable::hot_to_r(input$editnodes)
      relations <- rhandsontable::hot_to_r(input$editrelations)
      moderations <- rhandsontable::hot_to_r(input$editmoderations)
      diagram <- searchR::diagram_update(nodes, relations, moderations)
      modrval$nodes <- diagram$nodes
      modrval$relations <- diagram$relations
      modrval$moderations <- diagram$moderations
    })
    
    
    
    
    # Display ##################################################################
    
    output$displaydiagram <- DiagrammeR::renderGrViz({
      searchR::diagram_draw(
        nodes = modrval$nodes,
        relations = modrval$relations,
        moderations = modrval$moderations
      ) |>
        DiagrammeR::render_graph()
    })
    
    output$displaycode <- shiny::renderUI({
      code = c(
        "",
        searchR::diagram_write_table_code(modrval$nodes, "nodes"),
        "",
        searchR::diagram_write_table_code(modrval$relations, "relations"),
        "",
        searchR::diagram_write_table_code(modrval$moderations, "moderations"),
        "",
        "searchR::diagram_draw(nodes, relations, moderations) |>",
        "  DiagrammeR::render_graph()"
      )
      shinyAce::aceEditor(
        outputId = ns("diagramcode"), value = code, mode = "r",
        wordWrap = TRUE, debounce = 10, autoComplete = "live", height = "700px"
      )
    })
  
  })
}

