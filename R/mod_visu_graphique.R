#' visu_graphique UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import ggplot2
mod_visu_graphique_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("select_dataset"), label = "Faire un graphique à partir du jeu de données", choices = NULL),
    selectInput(ns("select_column_x"), label = "Axe des ordonnées (X)", choices = NULL),
    selectInput(ns("select_column_y"), label = "Axe des ordonnées (Y)", choices = NULL),
    selectInput(ns("select_type"), label = "Type de graphique", choices = c("points", "lignes")),
    actionButton(ns("valid_graph"), label = "Valider le graphique",
                 style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%"),
    helpText("Prévisualisation du graphique"),
    tags$div(style = 'overflow-x: scroll',
             plotOutput(ns("graph_preview"))
    )
  )
}

#' visu_graphique Server Functions
#'
#' @noRd
mod_visu_graphique_server <- function(id, analysis_history, step_nb_react, parent_session, main_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # add reactive values to store data
    rv <- reactiveValues(active_dataset = NULL,
                         tool_result = NULL)

    # ReactiveValue to return
    to_return <- reactiveValues(result = NULL,
                                trigger = NULL)

    cat("data wrangling : Filter\n")


    # populate select with datasets names

    # filter datasets only
    datasets_names <- names(analysis_history)
    datasets_names_keep <- rep(TRUE, length(datasets_names))
    if(length(datasets_names) > 1) {

      cat("  update dataset list\n")
      for (i in seq_along(datasets_names)){
        datasets_names_keep[i] <- ifelse(analysis_history[[datasets_names[i]]][["type"]] != "dataset", FALSE, TRUE)
      }
      datasets_names <- datasets_names[datasets_names_keep]
      updateSelectInput(session = main_session,
                        inputId = ns("select_dataset"),
                        choices = datasets_names)
    }

    # populate columns with columns names
    observeEvent(input$select_dataset, {
      if (!is.null(input$select_dataset) & input$select_dataset != ""){
        cat("  update columns list\n")
        # allocate active dataset
        rv$active_dataset <- analysis_history[[input$select_dataset]][["dataset"]]
        active_dataset_columns <- colnames(rv$active_dataset)
        updateSelectInput(session = main_session, inputId = ns("select_column_x"), choices = active_dataset_columns)
        updateSelectInput(session = main_session, inputId = ns("select_column_y"), choices = active_dataset_columns)
      }
    })

    observe({
      if(!(is.null(input$select_column_x) & is.null(input$select_column_y))) {
        if(input$select_column_x != "" & input$select_column_y != "") {

          rv$tool_result <- ggplot(rv$active_dataset, aes_string(input$select_column_x,input$select_column_y)) +
            geom_point()

        }
      }
    })


    output$graph_preview <- renderPlot({
      rv$tool_result
    })

    # store data
    observeEvent(input$valid_graph, {
      cat("  validate result and return from tool\n")
      # record values
      to_return$graphique  <- rv$tool_result
      to_return$type <- "graph"
      to_return$parameters <- list() # to do : add parameters for report
      to_return$parameters_text <- paste("Vous avez fait un joli graphique")


      # store into reactive value
      analysis_history[[paste0("step_", step_nb_react())]] <- to_return
      mod_history_server("question", analysis_history, step_nb_react())

      # go to next step UI
      updateTabsetPanel(session = main_session, "vigie_nature_analyse",
                        selected = "visu_landing")

      step_nb_react(step_nb_react()+1)
    })

  })
}
