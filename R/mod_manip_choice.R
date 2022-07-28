#' manip_choice UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_manip_choice_ui <- function(id){
  cat("  start manip choice module\n")
  ns <- NS(id)
  tagList(
    selectInput(ns("select_tool"), "Sélectionner un outil de manipulation de données", c("votre sélection", "Sélectionner des lignes", "other")),
    uiOutput(ns("module_manip_ui")),
    actionButton(ns('test'), 'test ouput')
  )
}

#' manip_choice Server Functions
#'
#' @noRd
mod_manip_choice_server <- function(id, history_datasets, step_nb, parent_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    tool_result <- reactiveValues()

    out_ui_manip <- reactiveVal()

    output$module_manip_ui <- renderUI({
      req(isTruthy(out_ui_manip))
      out_ui_manip()
    })

    observeEvent(input$select_tool, {
      if (input$select_tool == "Sélectionner des lignes"){
        out_ui_manip(mod_filter_ui(ns("filter")))
        tool_result <- mod_filter_server("filter", history_datasets, step_nb,
                                         parent_session = session,
                                         main_session = parent_session)
      }

      if(exists("tool_result")) {
        observeEvent(tool_result$trigger,{
          cat('manip choice dataset changed')
        })
      }
    })



    return(tool_result)


  })
}
