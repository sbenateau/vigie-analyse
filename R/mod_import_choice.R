#' import_choice UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_import_choice_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("import_vne"), "Importer un jeu de données issu de Vigie-Nature École",
                 style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%"),
    actionButton(ns("import_own_file"), "Importer votre propre jeu de données",
                 style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%"),
  )
}

#' import_choice Server Functions
#'
#' @noRd
mod_import_choice_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$import_vne, {

    })

  })
}

## To be copied in the UI
# mod_import_choice_ui("import_choice_1")

## To be copied in the server
# mod_import_choice_server("import_choice_1")
