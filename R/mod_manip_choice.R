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
  ns <- NS(id)
  tagList(
    selectInput(ns("select_tool"), "Sélectionner un outil de manipulation de données", c("Sélectionner des lignes", "other")),
    uiOutput(ns("module_manip_ui"))
  )
}

#' manip_choice Server Functions
#'
#' @noRd
mod_manip_choice_server <- function(id, history_datasets, step_nb, parent_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$select_tool, {

      if (input$select_tool == "Sélectionner des lignes"){
        output$module_manip_ui <- renderUI({
          mod_filter_ui(ns("filter"))
        })

        tool_result <- mod_filter_server(ns("filter"), history_datasets, step_nb, parent_session = session, main_session = parent_session)
      } else {
        output$module_manip_ui <- NULL
      }

      if(exists("tool_result")){
        observeEvent(tool_result$trigger,{
          # print(tool_result$result)
        })
      }

    })
  })
}

