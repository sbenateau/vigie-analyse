#' import_own_file UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_import_own_file_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    h2("Etape 2 : Importation des données"),
    br(),
    fileInput(ns("file_upload"), "Importer votre propre jeu de données"),
    actionButton(ns("validate_dataset"), "Valider le jeu de données",
                 style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%"),
    helpText("Prévisualisation du jeu de données"),
    tags$div(style = 'overflow-x: scroll',
             tableOutput(ns("dataset_preview"))
    )
  )
}

#' import_own_file Server Functions
#'
#' @noRd
mod_import_own_file_server <- function(id, analysis_history, step_nb_react, parent_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    to_return <- reactiveValues(dataset = NULL)

    # load loaded dataset
    observeEvent(input$file_upload,{
      dataset_uploaded <- fread(input$file_upload$datapath)
      data.table::setDF(dataset_uploaded)
      names(dataset_uploaded) <- gsub(" ", "_", names(dataset_uploaded))
      to_return$dataset <- dataset_uploaded
    })

    # render short view of the dataset
    output$dataset_preview <- renderTable({
      head(to_return$dataset)
    })

    # go to next step, save in history and record values
    observeEvent(input$validate_dataset,{
      # record values
      to_return$type <- "dataset"
      to_return$type_precise <- "Importation de données"
      to_return$tool_name <- "importer votre jeu de données"
      to_return$parameters <- list() # to do : add parameters for report
      to_return$protocole <- "inconnu"

      to_return$parameters_text <- tags$div(
        "Importation d'un jeu de données personnel", tags$br(),
        "    Nom du jeu de données :", input$file_upload$name, tags$br(),
        "    Type : ",  input$file_upload$type
        )

      # store into reactive value
      analysis_history[[paste0("step_",step_nb_react())]] <- to_return
      mod_history_server("import", analysis_history, step_nb_react())


      # go to next step UI
      updateTabsetPanel(session = parent_session, "vigie_nature_analyse",
                        selected = "import_landing")

      step_nb_react(step_nb_react()+1)
    })

  })
}
