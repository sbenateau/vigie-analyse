jscode <- "shinyjs.collapse = function(boxid) {$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();}"


#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @import shiny
#' @noRd
#'
app_server <- function(input, output, session) {

  cat("start application\n")

  history <- reactiveValues()

  # navigate between pages at the start of the application
  observeEvent(input$start_analysis,{
    updateTabsetPanel(session, "vigie_nature_analyse",
                      selected = "question")
  })

  # initiate modules
  question_result <- mod_question_server("question", parent_session = session)
  import_result <- mod_import_datasets_server("import_dataset", parent_session = session)



  # store question result and add it to the history
  if(exists("question_result")){
    observeEvent(question_result$trigger,{
      cat("  store question\n")
      history[["step_1"]] <- question_result
      mod_history_server("question", history, 1)
    })
  }

  # store import result and add it to the history
  if(exists("import_result")){
    observeEvent(import_result$trigger,{
      cat("  store import\n")
      step_nb <- length(reactiveValuesToList(history)) + 1
      history[[paste0("step_", step_nb)]] <- import_result
      mod_history_server("import", history, step_nb)
      manip_result <- mod_manip_choice_server("manip_dataset", history, step_nb, parent_session = session) # A MODIFIER !!!!!!!
    })

  }


  observeEvent(input$test,{
    print(ifelse(exists("manip_result"), manip_result, "not found"))
  })


  if(exists("manip_result")){
    observeEvent(manip_result$trigger,{
      cat("store manip\n")
      step_nb <- length(reactiveValuesToList(history)) + 1
      history[[paste0("step_", step_nb)]] <- manip_result
      mod_history_server("import", history, step_nb)

      if(exists("manip_result")) {
        observeEvent(manip_result$trigger,{
          print("Server output !!!!")
          print(manip_result)

        })
      }
    })
  }

}
