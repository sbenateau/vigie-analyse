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
  analysis_history <- reactiveValues()
  # used as parameter for a lot of functions
  step_nb_react <- reactiveVal(1)

  # navigate between pages at the start of the application
  observeEvent(input$start_analysis,{
    updateTabsetPanel(session, "vigie_nature_analyse",
                      selected = "question")
  })

  # launch modules
  mod_question_server("question", analysis_history, step_nb_react, parent_session = session)
  mod_import_datasets_server("import_dataset", analysis_history, step_nb_react, parent_session = session)
  mod_manip_choice_server("manip_dataset", analysis_history, step_nb_react, parent_session = session)
  mod_visu_choice_server("visu_dataset", analysis_history, step_nb_react, parent_session = session)
  mod_report_server("report", analysis_history)

  # choose statistical test and dataset

  # write conclusion

  # edit report

  # navigation
  observeEvent(input$import_nav_import,{
    updateTabsetPanel(session, "vigie_nature_analyse",
                      selected = "import")
  })

  observeEvent(input$import_nav_manip,{
    updateTabsetPanel(session, "vigie_nature_analyse",
                      selected = "import")
  })

  observeEvent(input$import_nav_visu,{
    updateTabsetPanel(session, "vigie_nature_analyse",
                      selected = "import")
  })

  observeEvent(input$manip_nav_import,{
    updateTabsetPanel(session, "vigie_nature_analyse",
                      selected = "manip")
  })

  observeEvent(input$manip_nav_manip,{
    updateTabsetPanel(session, "vigie_nature_analyse",
                      selected = "manip")
  })

  observeEvent(input$manip_nav_visu,{
    updateTabsetPanel(session, "vigie_nature_analyse",
                      selected = "manip")
  })

  observeEvent(input$visu_nav_import,{
    updateTabsetPanel(session, "vigie_nature_analyse",
                      selected = "visu")
  })

  observeEvent(input$visu_nav_manip,{
    updateTabsetPanel(session, "vigie_nature_analyse",
                      selected = "visu")
  })

  observeEvent(input$visu_nav_visu,{
    updateTabsetPanel(session, "vigie_nature_analyse",
                      selected = "visu")
  })
}
