#' visu_graphique UI Function
#'
#' @description A shiny Module to visualise data as a graph
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param analysis_history an object that contain all the information from the analysis (datasets and information)
#' @param step_nb_react the current step number
#' @param parent_session the parent session internal parameters for {shiny}
#'
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_visu_graphique_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("launch_tool"), label = "lancer l'outil (remplacer par réactive)"),
    selectInput(ns("select_dataset"), label = "Faire un graphique à partir du jeu de données", choices = NULL),
    selectInput(ns("select_column_x"), label = "Axe des ordonnées (X)", choices = NULL),
    selectInput(ns("select_column_y"), label = "Axe des ordonnées (Y)", choices = NULL),
    selectInput(ns("select_type"), label = "Type de graphique", choices = c("points", "lignes")),
    bsCollapse(id = "advanced_option",
               bsCollapsePanel("Options avancées",
                               textInput(ns("x_label"), "Donner un nom personalisé à l'axe des x"),
                               textInput(ns("y_label"), "Donner un nom personalisé à l'axe des y")
               )),
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
mod_visu_graphique_server <- function(id, analysis_history, step_nb_react, parent_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # add reactive values to store data
    rv <- reactiveValues(active_dataset = NULL,
                         tool_result = NULL)

    # ReactiveValue to return
    to_return <- reactiveValues(result = NULL,
                                trigger = NULL)

    cat("data visualisation : graph\n")


    observeEvent(input$launch_tool,{
      # populate select with datasets names
      # filter datasets only and update the select input list
      filter_and_update_datasets(analysis_history, "select_dataset", parent_session, ns)

      # populate columns with columns names
      observeEvent(input$select_dataset, {
        if (!is.null(input$select_dataset) & input$select_dataset != ""){
          cat("  update columns list\n")
          # allocate active dataset
          rv$active_dataset <- analysis_history[[input$select_dataset]][["dataset"]]
          active_dataset_columns <- colnames(rv$active_dataset)
          updateSelectInput(session = parent_session, inputId = ns("select_column_x"), choices = active_dataset_columns)
          updateSelectInput(session = parent_session, inputId = ns("select_column_y"), choices = active_dataset_columns)
        }
      })

      toListen <- reactive({
        list(input$select_column_x,
             input$select_column_y,
             input$select_type,
             input$select_dataset,
             input$y_label,
             input$x_label)
      })

      observeEvent(toListen(), {
        if(!(is.null(input$select_column_x) & is.null(input$select_column_y))) {
          if(input$select_column_x != "" & input$select_column_y != "") {

            print("change type")

            rv$tool_result <- ggplot(rv$active_dataset, aes_string(input$select_column_x,input$select_column_y))

            if(input$select_type == "points") {
              print("points")
              rv$tool_result <- rv$tool_result + geom_point()
            }

            if(input$select_type == "lignes") {
              print("lignes")
              rv$tool_result <- rv$tool_result + geom_line()
            }

            if(input$select_type == "barres") {
              print("barres")
              rv$tool_result <- rv$tool_result + geom_col()
            }

            if(input$x_label != "") {
              rv$tool_result = rv$tool_result + xlab(input$x_label)
            }

            if(input$y_label != "") {
              rv$tool_result = rv$tool_result + ylab(input$y_label)
            }
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
        to_return$graph  <- rv$tool_result
        to_return$type <- "graph"
        to_return$type_precise <- "Visualisation de données"
        to_return$tool_name <- "Faire un graphique"
        to_return$parameters <- list() # to do : add parameters for report
        to_return$parameters_text <- paste("Vous avez fait un joli graphique")


        # store into reactive value
        analysis_history[[paste0("step_", step_nb_react())]] <- to_return
        mod_history_server("graph", analysis_history, step_nb_react())

        # go to next step UI
        updateTabsetPanel(session = parent_session, "vigie_nature_analyse",
                          selected = "visu_landing")
        # increment step
        step_nb_react(step_nb_react() + 1)
      })
    })

  })
}
