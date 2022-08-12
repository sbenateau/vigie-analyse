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
    selectInput(ns("select_tool"),
                "Sélectionner un outil de manipulation de données",
                c("votre sélection", "Regrouper des lignes", "Sélectionner des lignes", "other")
    ),
    div(id = "manip_tool")
  )
}

#' manip_choice Server Functions
#'
#' @noRd
mod_manip_choice_server <- function(id, analysis_history, step_nb_react, parent_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$select_tool, {
      if (input$select_tool == "Sélectionner des lignes"){
        insertUI(selector = "#manip_tool",
                 where = "afterEnd",
                 ui = mod_filter_ui(ns("filter")),
                 immediate = TRUE
        )
        mod_filter_server("filter", analysis_history, step_nb_react,
                          parent_session = session,
                          main_session = parent_session)

      } else if (input$select_tool == "Regrouper des lignes"){
        insertUI(selector = "#manip_tool",
                 where = "afterEnd",
                 ui = mod_manip_group_by_ui(ns("group_by")),
                 immediate = TRUE
        )
        mod_manip_group_by_server("group_by", analysis_history, step_nb_react,
                                  parent_session = session,
                                  main_session = parent_session)

      }
    })


  })
}
