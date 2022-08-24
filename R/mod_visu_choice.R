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
    h3("Graphiques"),
    p("Il existe deux outils pour faire des graphiques, le premier traite les tableaux avec plusieurs lignes (la plupart des utilisations)."),
    actionButton(ns("choose_graph"), "Représenter les données",
                 style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%"),
    br(),
    bsCollapse(id = "collapse_graph_choice",
               multiple = TRUE,
               bsCollapsePanel("Afficher l'aide de l'outil",
                               img(src = "http://galaxybricks.vigienature-ecole.fr/media/edito/2020/04/14/histrow.gif", align = "center", width="50%")
               )
    )
  )
}

#' visu_choice Server Functions
#'
#' @noRd
mod_visu_choice_server <- function(id, analysis_history, step_nb_react, parent_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$choose_graph, {
      updateTabsetPanel(session = parent_session, "vigie_nature_analyse",
                        selected = "visu_graph")
    })


  })
}
