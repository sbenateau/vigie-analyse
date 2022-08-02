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
    br(),
    h2("Etape 3 : Manipulation des données"),
    br(),
    selectInput(ns("select_tool"), "Sélectionner un outil de manipulation de données", c("votre sélection", "Sélectionner des lignes", "other")),
    uiOutput(ns("module_manip_ui"))
  )
}

#' manip_choice Server Functions
#'
#' @noRd
mod_manip_choice_server <- function(id, analysis_history, step_nb_react, parent_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    out_ui_manip <- reactiveVal()

    output$module_manip_ui <- renderUI({
      req(isTruthy(out_ui_manip))
      out_ui_manip()
    })
    observeEvent(input$select_tool, {
      if (input$select_tool == "Sélectionner des lignes"){

        out_ui_manip(mod_filter_ui(ns("filter")))
        mod_filter_server("filter", analysis_history, step_nb_react,
                          parent_session = session,
                          main_session = parent_session)

      }
    })


  })
}
