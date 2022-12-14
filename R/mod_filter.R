#' filter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_filter_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("launch_tool"), label = "lancer l'outil (remplacer par réactive)"),
    selectInput(ns("select_dataset"), label = "Garder les lignes du jeu de données", choices = NULL),
    selectInput(ns("select_column"), label = "dont les valeurs de la colonne", choices = NULL),
    textOutput(ns("help_text_column")),
    selectInput(ns("select_type"), label = "sont", choices = NULL),
    textInput(ns("filter_pattern"), label = "à la valeur suivante :"),
    actionButton(ns("valid_tool"), label = "Valider le filre",
                 style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%"),
    helpText("Prévisualisation du jeu de données"),
    tags$div(style = 'overflow-x: scroll',
             tableOutput(ns("dataset_preview"))
    )
  )
}

#' filter Server Functions
#'
#' @noRd
mod_filter_server <- function(id, analysis_history, step_nb_react, parent_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # add reactive values to store data
    rv <- reactiveValues(active_dataset = NULL,
                         tool_result = NULL)

    # ReactiveValue to return
    to_return <- reactiveValues(result = NULL)

    cat("data wrangling : Filter\n")

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
          updateSelectInput(session = parent_session, inputId = ns("select_column"), choices = active_dataset_columns)
        }
      })

      # populate filter type according to column type
      observeEvent(input$select_column, {
        cat("  update type list\n")
        if (input$select_column != "") {
          # get datatype of the column
          column_type = class(unlist(rv$active_dataset[input$select_column]))
          if (column_type == "numeric" | column_type == "integer"){
            column_type_text = "numérique"
            filter_options = c("supérieure", "inférieure", "égale", "supérieure ou égale", "inférieure ou égale")
          } else {
            column_type_text = "chaine de caractères"
            filter_options = c("exactement égale", "partiellement égale")
          }
          updateSelectInput(session = session, inputId = "select_type", choices = filter_options)
          output$help_text_column <- renderText({
            paste("Cette colonne est de type :", column_type_text)
          })
        }
      })

      # filter dataset
      observe({
        if(!is.null(input$select_column)){
          if(input$select_column != "" & input$select_type != "" & input$filter_pattern != "") {
            cat("  calculate result for preview\n")
            if (input$select_type == "supérieure") {
              to_return$dataset <- subset(rv$active_dataset, rv$active_dataset[ , input$select_column] > as.numeric(input$filter_pattern))
            } else if (input$select_type == "inférieure") {
              to_return$dataset <- subset(rv$active_dataset, rv$active_dataset[ , input$select_column] < as.numeric(input$filter_pattern))
            } else if (input$select_type == "égale") {
              to_return$dataset <- subset(rv$active_dataset, rv$active_dataset[ , input$select_column] == as.numeric(input$filter_pattern))
            } else if (input$select_type == "supérieure ou égale") {
              to_return$dataset <- subset(rv$active_dataset, rv$active_dataset[ , input$select_column] >= as.numeric(input$filter_pattern))
            } else if (input$select_type == "inférieure ou égale") {
              to_return$dataset <- subset(rv$active_dataset, rv$active_dataset[ , input$select_column] <= as.numeric(input$filter_pattern))
            } else if (input$select_type == "exactement égale") {
              to_return$dataset <- subset(rv$active_dataset, rv$active_dataset[ , input$select_column] == input$filter_pattern)
            }else if (input$select_type == "partiellement égale") {
              to_return$dataset <- subset(rv$active_dataset, grepl(input$filter_pattern, rv$active_dataset[ , input$select_column]))
            }
          }
        }
      })

      # show preview of the filter
      output$dataset_preview <- renderTable({
        head(to_return$dataset, 20)
      })



      # store data
      observeEvent(input$valid_tool, {
        cat("  validate result and return from tool\n")
        # record values
        to_return$type <- "dataset"
        to_return$type_precise <- "Manipulation de données"
        to_return$tool_name <- "Sélectionner des lignes"
        to_return$parameters <- list() # to do : add parameters for report
        to_return$parameters_text <- paste("Vous avez gardé les lignes du jeu de données",
                                           input$select_dataset,
                                           "dont les valeurs de la colonne",
                                           input$select_column,
                                           "sont",
                                           input$select_type,
                                           "à la valeur suivante :",
                                           input$filter_pattern)


        # store into reactive value
        analysis_history[[paste0("step_", step_nb_react())]] <- to_return
        mod_history_server("question", analysis_history, step_nb_react())

        # go to next step UI
        updateTabsetPanel(session = parent_session, "vigie_nature_analyse",
                          selected = "manip_landing")

        step_nb_react(step_nb_react()+1)
        print(input$valid_tool)
        shinyjs::reset("valid_tool", asis = TRUE)
        print(input$valid_tool)
      })

    })
  })
}

