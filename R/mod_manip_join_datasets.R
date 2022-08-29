#' manip_join_datasets UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import dplyr
mod_manip_join_datasets_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("launch_tool"), label = "lancer l'outil (remplacer par réactive)"),
    selectInput(ns("select_dataset_1"), label = "Joindre ce jeu de données", choices = NULL),
    selectInput(ns("select_columns_by_1"), label = "en utilisant cette colonne", choices = NULL, multiple = FALSE),
    selectInput(ns("select_dataset_2"), label = "avec ce jeu de donnée", choices = NULL),
    selectInput(ns("select_columns_by_2"), label = "en utilisant cette colonne", choices = NULL, multiple = FALSE),
    actionButton(ns("valid_tool"), label = "Valider le résultat",
                 style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%"),
    helpText("Prévisualisation du jeu de données"),
    tags$div(style = 'overflow-x: scroll',
             tableOutput(ns("dataset_preview"))
    )
  )
}

#' manip_join_datasets Server Functions
#'
#' @noRd
mod_manip_join_datasets_server <- function(id, analysis_history, step_nb_react, parent_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # add reactive values to store data
    rv <- reactiveValues(active_dataset_1 = NULL,
                         active_dataset_2 = NULL,
                         error_text = NULL,
                         trigger = 0)

    # ReactiveValue to return
    to_return <- reactiveValues(result = NULL,
                                trigger = NULL)

    cat("data wrangling : join\n")

    observeEvent(input$launch_tool,{

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
        updateSelectInput(session = parent_session,
                          inputId = ns("select_dataset_1"),
                          choices = datasets_names)
        updateSelectInput(session = parent_session,
                          inputId = ns("select_dataset_2"),
                          choices = datasets_names)
      }



      # populate columns with columns names
      observeEvent(input$select_dataset_1, {
        if (!is.null(input$select_dataset_1) & input$select_dataset_1 != ""){
          cat("  update columns list\n")
          # allocate active dataset
          rv$active_dataset_1 <- analysis_history[[input$select_dataset_1]][["dataset"]]
          active_dataset_columns <- colnames(rv$active_dataset_1)
          updateSelectInput(session = parent_session, inputId = ns("select_columns_by_1"), choices = active_dataset_columns)
        }
      })

      # populate columns with columns names
      observeEvent(input$select_dataset_2, {
        if (!is.null(input$select_dataset_2) & input$select_dataset_2 != ""){
          cat("  update columns list\n")
          # allocate active dataset
          rv$active_dataset_2 <- analysis_history[[input$select_dataset_2]][["dataset"]]
          active_dataset_columns <- colnames(rv$active_dataset_2)
          updateSelectInput(session = parent_session, inputId = ns("select_columns_by_2"), choices = active_dataset_columns)
        }
      })
    })

    toListen <- reactive({
      list(input$select_columns_by_1,
           input$select_columns_by_2,
           input$select_dataset_1,
           input$select_dataset_2)
    })

    observeEvent(toListen(), {
      if(input$select_columns_by_1 != "" &
         input$select_columns_by_2 != "" &
         input$select_dataset_1 != "" &
         input$select_dataset_2 != "" &
         input$select_dataset_1 != input$select_dataset_2
      ) {
        print(input$select_columns_by_2 )
        print(colnames(rv$active_dataset_2))
        print(input$select_columns_by_2 %in% colnames(rv$active_dataset_2))
        if(input$select_columns_by_2 %in% colnames(rv$active_dataset_2)){
          rv$tool_result = merge(rv$active_dataset_1,
                                             rv$active_dataset_2,
                                             by.x = input$select_columns_by_1,
                                             by.y = input$select_columns_by_2)
        }
      }
    })

    # show preview of the filter
    output$dataset_preview <- renderTable({
      head(rv$tool_result, 20)
    })


    # store data
    observeEvent(input$valid_tool, {
      cat("  validate result and return from tool\n")

      if(rv$trigger < input$valid_tool) {
        rv$trigger = rv$trigger + 1

        # record values
        to_return$dataset  <- rv$tool_result
        to_return$type <- "dataset"
        to_return$type_precise <- "Manipulation de données"
        to_return$tool_name <- "Joindre deux jeux données"
        to_return$parameters <- list() # to do : add parameters for report
        to_return$parameters_text <- paste("Vous avez joint le jeu de données" , input$select_dataset_1, "avec le jeux de donnée", input$select_dataset_2, "en utilisant les colonnes", input$select_columns_by_1 , "et", input$select_columns_by_2 )


        # store into reactive value
        analysis_history[[paste0("step_", step_nb_react())]] <- to_return
        mod_history_server("question", analysis_history, step_nb_react())

        # go to next step UI
        updateTabsetPanel(session = parent_session, "vigie_nature_analyse",
                          selected = "manip_landing")
        cat("increment step_nb_react")
        step_nb_react(step_nb_react()+1)
        shinyjs::reset("valid_tool")

        #reset all
        updateSelectInput(session = parent_session, inputId = ns("select_dataset"), selected = "")
        updateSelectInput(session = parent_session, inputId = ns("select_columns_group"), selected = "")
        updateSelectInput(session = parent_session, inputId = ns("select_operation"), selected = "")
        updateSelectInput(session = parent_session, inputId = ns("select_column_operation"), selected = "")
      }
    })

  })


}
