#' nav_manip UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_nav_manip_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("go_to_manip"), "Manipuler des données",
                 style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%")
  )
}

#' nav_manip Server Functions
#'
#' @noRd
mod_nav_manip_server <- function(id, main_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$go_to_manip,{
      # go to next step UI
      updateTabsetPanel(session = main_session, "vigie_nature_analyse",
                        selected = "manip")
    })
  })
}

## To be copied in the UI
# mod_nav_manip_ui("nav_manip_1")

## To be copied in the server
# mod_nav_manip_server("nav_manip_1")
