jscode <- "shinyjs.collapse = function(boxid) {$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();}"


#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @import shiny
#' @noRd
#'
app_server <- function(input, output, session) {

  history <- reactiveValues()

  # navigate between pages at the start of the application
  observeEvent(input$start_analysis,{
    updateTabsetPanel(session, "vigie_nature_analyse",
                      selected = "question")
  })

  # import modules
  question_result <- mod_question_server("question", parent_session = session)
  import_result <- mod_import_datasets_server("import_dataset", parent_session = session)


  # store question result and add it to the history
  if(exists("question_result")){
    observeEvent(question_result$trigger,{
      history[["step_1"]] <- question_result
      mod_history_server("question", history, 1)
    })
  }

  # store import result and add it to the history
  if(exists("import_result")){
    observeEvent(import_result$trigger,{
      cat("imported values\n")
      step_nb <- length(reactiveValuesToList(history)) + 1
      history[[paste0("step_", step_nb)]] <- import_result
      mod_history_server("manip", history, step_nb)
      mod_manip_choice_server("manip_dataset", history, 3, parent_session = session) # A MODIFIER !!!!!!!
    })

  }

}
