#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @import shiny
#' @noRd
#'
app_server <- function(input, output, session) {

  cat("start application\n")

  # define values

  # all information about the analysis will be stored here :
  # - questions
  # - datasets + parameters + comments
  # - figures
  # - conclusions
  history <- reactiveValues()
  # used as parameter for a lot of functions
  step_nb_react <- reactiveVal()

  # navigate between pages at the start of the application
  observeEvent(input$start_analysis,{
    updateTabsetPanel(session, "vigie_nature_analyse",
                      selected = "question")
  })

  # initiate modules
  question_result <- mod_question_server("question", parent_session = session)
  import_result <- mod_import_datasets_server("import_dataset", parent_session = session)

  # choose module and dataset
  manip_info <- mod_manip_choice_server("manip_dataset", history, step_nb, parent_session = session)

  filter_result <- mod_filter_server("filter", history, step_nb, parent_session = session)
    # store import result and add it to the history
    if(exists("filter_result")){
      observeEvent(filter_result$trigger,{
        cat("  store import\n")
        step_nb <- length(reactiveValuesToList(history)) + 1
        history[[paste0("step_", step_nb)]] <- filter_result
        mod_history_server("import", history, step_nb)
      })

      # store question result and add it to the history
      if(exists("question_result")){
        observeEvent(question_result$trigger,{
          cat("  store question\n")
          history[["step_1"]] <- question_result
          mod_history_server("question", history, 1)
        })
      }

    }

  # choose visualisation tool and dataset

  # choose statistical test and dataset

  # write conclusion

  # edit report



  # store import result and add it to the history
  if(exists("import_result")){
    observeEvent(import_result$trigger,{
      cat("  store import\n")
      step_nb <- length(reactiveValuesToList(history)) + 1
      history[[paste0("step_", step_nb)]] <- import_result
      mod_history_server("import", history, step_nb)
    })

  }

  #   if(exists("manip_result")){
  #     observeEvent(manip_result$trigger,{
  #       cat("store manip\n")
  #       step_nb <- length(reactiveValuesToList(history)) + 1
  #       history[[paste0("step_", step_nb)]] <- manip_result
  #       mod_history_server("import", history, step_nb)
  #     })
  #   }

  observeEvent(input$browser,{
    browser()
  })

}
