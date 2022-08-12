#' import_datasets UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_import_datasets_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    h2("Etape 2 : Importation des données"),
    br(),
    selectInput(ns("available_datasets"), "Sélectionner le jeu de données à importer", c("Biolit", "Sauvages de ma Rue")),
    actionButton(ns("validate_dataset"), "Valider le jeu de données",
                 style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%"),
    helpText("Prévisualisation du jeu de données"),
    tags$div(style = 'overflow-x: scroll',
             tableOutput(ns("dataset_preview"))
    )
  )
}

#' import_datasets Server Functions
#'
#' @noRd
mod_import_datasets_server <- function(id, analysis_history, step_nb_react, parent_session){
  moduleServer( id, function(input, output, session){
    cat("  start import module\n")
    ns <- session$ns

    # ReactiveValue to return
    to_return <- reactiveValues(type = NULL,
                                dataset = NULL,
                                protocole = NULL,
                                parameters_text = NULL)

    # render short view of the dataset
    observeEvent(input$available_datasets,{

      file_name <- switch (c(unlist(input$available_datasets)),
                           Biolit = "biolit",
                           "Sauvages de ma Rue" = "sauvages"
      )
      output$dataset_preview <- renderTable({
        to_return$dataset <- read.csv(paste0("../../datasets/bricks/", file_name, ".csv"))
        head(to_return$dataset)
      })
    })



    observeEvent(input$validate_dataset,{
      cat("02_validate import\n")
      # record values
      to_return$type <- "dataset"
      to_return$type_precise <- "Importation de données"
      to_return$tool_name <- "importer des données Vigie-Nature École"
      to_return$parameters <- list() # to do : add parameters for report
      to_return$protocole <- input$available_datasets
      to_return$parameters_text <- paste("Importation du jeu de données issu du protocole :", input$available_datasets)


      # store into reactive value
      analysis_history[[paste0("step_", step_nb_react())]] <- to_return
      mod_history_server("question", analysis_history, step_nb_react())


      # go to next step UI
      updateTabsetPanel(session = parent_session, "vigie_nature_analyse",
                        selected = "import_landing")

      step_nb_react(step_nb_react()+1)
    })
  })
}
