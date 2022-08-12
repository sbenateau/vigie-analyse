#' visu_choice UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_visu_choice_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    h2("Etape 4 : Visualisation des données"),
    br(),
    selectInput(ns("select_tool"), "Sélectionner un outil de visualisation de données", c("votre sélection", "Graphique", "Carte")),
    actionButton("remove_all", "Remove all")
  )
}

#' visu_choice Server Functions
#'
#' @noRd
mod_visu_choice_server <- function(id, analysis_history, step_nb_react, parent_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$remove_all, {
      # remove all other ui
      removeUI('div:has(> #graphiqu)', immediate = TRUE)

    })

    observeEvent(input$select_tool, {
      if (input$select_tool == "Graphique"){
        # insert ui
        insertUI(selector = "#visu_tool",
                 where = "afterEnd",
                 ui = mod_visu_graphique_ui(ns("graphique")),
                 immediate = TRUE
        )

        # run server part
        mod_visu_graphique_server("graphique",
                                  analysis_history,
                                  step_nb_react,
                                  parent_session = session,
                                  main_session = parent_session)
      }
    })

  })
}

## To be copied in the UI
# mod_visu_choice_ui("visu_choice_1")

## To be copied in the server
# mod_visu_choice_server("visu_choice_1")
