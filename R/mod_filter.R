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
    selectInput(ns("select_dataset"), label = "Garder les lignes du jeu de données", choices = NULL),
    selectInput(ns("select_column"), label = "dont les valeurs de la colonne", choices = NULL),
    textOutput(ns("help_text_column")),
    selectInput(ns("select_type"), label = "sont", choices = NULL),
    textInput(ns("filter_pattern"), label = "à la valeur suivante :"),
    actionButton(ns("valid_filter"), label = "Valider le filre",
                 style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%"),
    helpText("Prévisualisation du jeu de données"),
    tableOutput(ns("dataset_preview")),
    actionButton(ns("print_input_mod"), "print ui input (to be deleted")
  )
}

#' filter Server Functions
#'
#' @noRd
mod_filter_server <- function(id, history_datasets, step_nb, parent_session, main_session){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    rv <- reactiveValues(active_dataset = NULL,
                         tool_result = NULL)

    cat("data wrangling : Filter\n")
    # populate select with datasets names
    datasets_names <- names(history_datasets)
    print(datasets_names)
    updateSelectInput(session = main_session,
                      inputId = "manip_dataset-filter-select_dataset",
                      choices = datasets_names)

    # populate columns with columns names
    observeEvent(ns(input$select_dataset), {
      print("select_col")
      print("name")
      print(ns(input$select_dataset))
      print("test [[]]")
      print(input[["manip_dataset-filter-select_dataset"]] )
      print("new format $` `")
      print(input$`manip_dataset-filter-select_dataset`)
      print("try input")
      print(input)
     if (!is.null(input[["filter-select_dataset"]] )){
          cat("update column\n\n")
          # allocate active dataset
          rv$active_dataset <- history_datasets[[input$select_dataset]]
          active_dataset_columns <- colnames(rv$active_dataset)
          updateSelectInput(session = main_session, inputId = "manip_dataset-filter-select_column", choices = active_dataset_columns)
      }
    })

    observeEvent(input$select_column, {
      if (input$select_column != "") {
        # get datatype of the column
        column_type = class(unlist(rv$active_dataset[input$select_column]))
        if (column_type == "numeric"){
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

    observe({
      if(!is.null(input$select_column)){
        if(input$select_column != "" & input$select_type != "" & input$filter_pattern != "") {
          if (input$select_type == "supérieure") {
            rv$tool_result <- subset(rv$active_dataset, rv$active_dataset[ , input$select_column] > input$filter_pattern)
          } else if (input$select_type == "inférieure") {
            rv$tool_result <- subset(rv$active_dataset, rv$active_dataset[ , input$select_column] < input$filter_pattern)
          } else if (input$select_type == "égale") {
            rv$tool_result <- subset(rv$active_dataset, rv$active_dataset[ , input$select_column] == input$filter_pattern)
          } else if (input$select_type == "supérieure ou égale") {
            rv$tool_result <- subset(rv$active_dataset, rv$active_dataset[ , input$select_column] >= input$filter_pattern)
          } else if (input$select_type == "inférieure ou égale") {
            rv$tool_result <- subset(rv$active_dataset, rv$active_dataset[ , input$select_column] <= input$filter_pattern)
          } else if (input$select_type == "exactement égale") {
            rv$tool_result <- subset(rv$active_dataset, rv$active_dataset[ , input$select_column] == input$filter_pattern)
          }else if (input$select_type == "partiellement égale") {
            rv$tool_result <- subset(rv$active_dataset, grepl(input$filter_pattern, rv$active_dataset[ , input$select_column]))
          }
        }
      }
    })

    output$dataset_preview <- renderTable({
      head(rv$tool_result)
    })

    # ReactiveValue to return
    toReturn <- reactiveValues(result = NULL, trigger = NULL)

    # Apply function on variable
    observeEvent(input$valid_filter, {
      # record values
      toReturn$trigger <- ifelse(is.null(toReturn$trigger), 0, toReturn$trigger) + 1
      toReturn$result  <- rv$tool_result
      toReturn$parameters <- list() # to do : add parameters for report
      # remove UI
      hide("select_dataset")
      hide("select_column")
    })

    # test
    observeEvent(input$print_input_mod,{
      print(reactiveValuesToList(input))
    })

    return(toReturn)

  })
}

