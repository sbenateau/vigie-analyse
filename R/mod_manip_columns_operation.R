#' manip_columns_operation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_manip_columns_operation_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("launch_tool"), label = "lancer l'outil (remplacer par réactive)"),
    selectInput(ns("select_dataset"), label = "Faire des opérations sur les colonnes du jeu de données", choices = NULL),
    selectInput(ns("select_column_left"), label = "en utilisant cette ou ses colonnes", choices = NULL, multiple = FALSE),
    # textOutput(ns("help_text_column")), # preciser par exemple attention si une valeur quanti
    selectInput(ns("select_operation"), label = "pour faire cette opération", choices = c("addition", "soustraction", "multiplication", "division"), multiple = FALSE),
    selectInput(ns("select_column_right"), label = "avec la colonne", choices = NULL, multiple = FALSE), # TO DO : remove if count
    textInput(ns("column_name"), label = "et stocker le résultat dans la colonne"),
    textOutput(ns("error")),
    actionButton(ns("valid_tool"), label = "Valider le résultat",
                 style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%"),
    helpText("Prévisualisation du jeu de données"),

    tags$div(style = 'overflow-x: scroll',
             tableOutput(ns("dataset_preview"))
    )
  )
}

#' manip_columns_operation Server Functions
#'
#' @noRd
mod_manip_columns_operation_server <- function(id, analysis_history, step_nb_react, parent_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # add reactive values to store data
    rv <- reactiveValues(active_dataset = NULL,
                         error_text = NULL,
                         trigger = 0)

    # ReactiveValue to return
    to_return <- reactiveValues(result = NULL,
                                trigger = NULL)

    cat("data wrangling : column operation\n")

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
          updateSelectInput(session = parent_session, inputId = ns("select_column_left"), choices = active_dataset_columns)
          updateSelectInput(session = parent_session, inputId = ns("select_column_right"), choices = active_dataset_columns)
        }
      })

      toListen <- reactive({
        list(input$select_column_right,
             input$select_column_left,
             input$select_operation,
             input$column_name)
      })

      observeEvent(toListen(), {
        if(!is.null(input$select_column_left) & !is.null(input$select_operation) & !is.null(input$select_column_right)){
          if(input$select_column_left != "" && input$select_column_right != "" && input$select_operation != "") {

            column_left_type = class(unlist(rv$active_dataset[input$select_column_left]))
            column_right_type = class(unlist(rv$active_dataset[input$select_column_right]))

            # if(input$select_column_right %in% input$select_columns_group) {
            #   rv$error_text <- "Attention la colonne sur laquelle vous faite le calcul ne peut pas être présente deux fois"
            # } else
            if(column_left_type != "numeric" & column_left_type != "integer" & column_right_type != "numeric" & column_right_type != "integer") {
              rv$error_text <- "Attention la colonne sur laquelle vous faite le calcul ne doit contenir que des nombres"

            } else {
              cat("  calculate result for preview\n")
              # browser()
              if (input$select_operation == "addition") {
                rv$tool_result <- rv$active_dataset %>%
                  mutate(!! input$column_name := (!!as.symbol(input$select_column_left)) + (!!as.symbol(input$select_column_right)))
              } else if (input$select_operation == "soustraction") {
                rv$tool_result <- rv$active_dataset %>%
                  mutate(!! input$column_name := (!!as.symbol(input$select_column_left)) - (!!as.symbol(input$select_column_right)))
              } else if (input$select_operation == "multiplication") {
                rv$tool_result <- rv$active_dataset %>%
                  mutate(!! input$column_name := (!!as.symbol(input$select_column_left)) * (!!as.symbol(input$select_column_right)))
              } else if (input$select_operation == "division") {
                cat("division")
                rv$tool_result <- rv$active_dataset %>%
                  mutate(!! input$column_name := (!!as.symbol(input$select_column_left)) / (!!as.symbol(input$select_column_right)))
              }
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

        if(rv$trigger < input$valid_tool) {
          rv$trigger = rv$trigger + 1

          # record values
          to_return$dataset  <- rv$tool_result
          to_return$type <- "dataset"
          to_return$type_precise <- "Manipulation de données"
          to_return$tool_name <- "Opération sur des colonnes"
          to_return$parameters <- list() # to do : add parameters for report
          # to_return$parameters_text <- paste("Vous avez regroupé toutes les lignes du jeu de données :",
          #                                    input$select_dataset,
          #                                    "selon les catégories contenues dans la ou les colonnes :",
          #                                    paste(input$select_columns_group, collapse = " "),
          #                                    "en faisant la ou les opérations suivantes :",
          #                                    paste(input$select_operation, collapse = " "),
          #                                    "sur la colonne :",
          #                                    input$select_column_operation)
          #
          # print(to_return$parameters_text)


          # store into reactive value
          analysis_history[[paste0("step_", step_nb_react())]] <- to_return
          mod_history_server("manip", analysis_history, step_nb_react())

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
  })
}
