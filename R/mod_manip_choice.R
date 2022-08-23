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
    p("Vous disposez de quatre familles d'outils pour manipuler vos données. N'hésitez pas à consulter l'aide pour apprendre à utiliser les outils."),
    h3("Regrouper des lignes"),
    p("Cet outil permet de regrouper des lignes par catégorie (par type d'environnement par exemple) en faisant un calcul (une moyenne par exemple) sur les données d'une autre colonne."),
    bsCollapse(id = "collapse_group_tool",
               multiple = TRUE,
               bsCollapsePanel("Découvrir ou utiliser cet outil",
                               fluidRow(img(src = "http://galaxybricks.vigienature-ecole.fr/media/edito/2020/04/01/resumer.gif", align = "center", width="50%")),
                               p("Cet outil permet de regrouper des lignes par catégorie (par type d'environnement par exemple) en faisant un calcul (une moyenne par exemple) sur les données d'une autre colonne."),
                               actionButton(ns("manip_tool_group_by"), "Utiliser cet outil",
                                            style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%")
               )
    )

  )
}

#' manip_choice Server Functions
#'
#' @noRd
mod_manip_choice_server <- function(id, analysis_history, step_nb_react, parent_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$manip_tool_group_by, {
      # go to next step UI
      updateTabsetPanel(session = parent_session, "vigie_nature_analyse",
                        selected = "manip_group_by")
    })


  })
}
