#' manip_group_by UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import dplyr
mod_manip_group_by_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("select_dataset"), label = "Regrouper toutes les lignes du jeu de données", choices = NULL),
    selectInput(ns("select_columns_group"), label = "selon des catégories contenues dans la ou les colonnes suivantes", choices = NULL, multiple = TRUE),
    # textOutput(ns("help_text_column")), # preciser par exemple attention si une valeur quanti
    selectInput(ns("select_operation"), label = "en faisant la ou les opération suivante", choices = c("moyenne", "médiane", "somme", "compte", "compte des valeurs supérieures à zéro", "écart-type", "erreur standard"), multiple = TRUE),
    selectInput(ns("select_column_operation"), label = " sur la colonne", choices = NULL, multiple = FALSE), # TO DO : remove if count
    textOutput(ns("error")),
    actionButton(ns("valid_tool"), label = "Valider le filre",
                 style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%"),
    helpText("Prévisualisation du jeu de données"),
    tags$div(style = 'overflow-x: scroll',
             tableOutput(ns("dataset_preview"))
    )
  )
}

#' manip_group_by Server Functions
#'
#' @noRd
mod_manip_group_by_server <- function(id, analysis_history, step_nb_react, parent_session, main_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # add reactive values to store data
    rv <- reactiveValues(active_dataset = NULL,
                         error_text = NULL)

    # ReactiveValue to return
    to_return <- reactiveValues(result = NULL,
                                trigger = NULL)

    cat("data wrangling : group_by\n")


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
        updateSelectInput(session = main_session, inputId = ns("select_columns_group"), choices = active_dataset_columns)
        updateSelectInput(session = main_session, inputId = ns("select_column_operation"), choices = active_dataset_columns)
      }
    })

    # define functions
    observeEvent(input$select_operation,{
      if (!is.null(input$select_operation)) {
        rv$function_calculation <- c()
        if ("moyenne" %in% input$select_operation) rv$function_calculation = c(rv$function_calculation, "mean")
        if ("médiane" %in% input$select_operation) rv$function_calculation = c(rv$function_calculation, "median")
        if ("somme" %in% input$select_operation) rv$function_calculation = c(rv$function_calculation, "sum")
        if ("compte" %in% input$select_operation) rv$function_calculation = c(rv$function_calculation, "length")
        if ("compte des valeurs supérieures à zéro" %in% input$select_operation) rv$function_calculation = c(rv$function_calculation, "mean")
        if ("écart-type" %in% input$select_operation) rv$function_calculation = c(rv$function_calculation, "sd")
        if ("erreur standard" %in% input$select_operation) rv$function_calculation = c(rv$function_calculation, "mean")

        print(input$select_operation)
        print(rv$function_calculation)
      }
    })

    # calculate dataset
    observe({
      print(input$select_columns_group)
      print(input$select_operation)
      print(input$select_column_operation)
      print(!is.null(input$select_columns_group) & !is.null(input$select_operation) & !is.null(input$select_column_operation))
      if(!is.null(input$select_columns_group) & !is.null(input$select_operation) & !is.null(input$select_column_operation)){
        if(input$select_columns_group != "" && input$select_column_operation != "" && input$select_operation != "") {
          print(input$select_column_operation)

          column_type = class(unlist(rv$active_dataset[input$select_column_operation]))

          if(input$select_column_operation %in% input$select_columns_group) {
            rv$error_text <- "Attention la colonne sur laquelle vous faite le calcul ne peut pas être présente deux fois"
          } else if(column_type != "numeric" & column_type != "integer") {
            rv$error_text <- "Attention la colonne sur laquelle vous faite le calcul ne doit contenir que des nombres"

          } else {
            cat("  calculate result for preview\n")

            rv$tool_result <- rv$active_dataset %>%
              group_by_at(input$select_columns_group) %>%
              summarise_at(.vars = input$select_column_operation, .funs = rv$function_calculation)
            rv$error_text <- NULL
          }
        }
      }
    })

    # show preview of the filter
    output$dataset_preview <- renderTable({
      head(rv$tool_result, 20)
    })

    output$error <- renderText({
      rv$error_text
    })



    # store data
    observeEvent(input$valid_tool, {
      cat("  validate result and return from tool\n")
      # record values
      to_return$dataset  <- rv$tool_result
      to_return$type <- "dataset"
      to_return$parameters <- list() # to do : add parameters for report
      to_return$parameters_text <- paste("Vous avez résumé le jeu de données")


      # store into reactive value
      analysis_history[[paste0("step_", step_nb_react())]] <- to_return
      mod_history_server("question", analysis_history, step_nb_react())

      # go to next step UI
      # updateTabsetPanel(session = main_session, "vigie_nature_analyse",
      #                   selected = "manip_landing")

      step_nb_react(step_nb_react()+1)
      shinyjs::reset("column_manip")
    })


  })
}

## To be copied in the UI
# mod_manip_group_by_ui("manip_group_by_1")

## To be copied in the server
# mod_manip_group_by_server("manip_group_by_1")
