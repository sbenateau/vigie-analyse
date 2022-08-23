#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @import shiny
#' @noRd
#'
app_server <- function(input, output, session) {

  # increase max dowload size for manual import
  options(shiny.maxRequestSize=60*1024^2)

  # define values

  # all information about the analysis will be stored here :
  # - questions
  # - datasets + parameters + comments
  # - figures
  # - conclusions
  analysis_history <- reactiveValues()

  # used as parameter for a lot of functions
  # define the step number
  step_nb_react <- reactiveVal(1)

  # launch modules

  # module to write question
  mod_question_server("question", analysis_history, step_nb_react, parent_session = session)

  # module to import dataset
  #mod_import_datasets_server("import_dataset", analysis_history, step_nb_react, parent_session = session)
  mod_import_choice_server("import_choice", analysis_history, step_nb_react, parent_session = session)
  mod_import_own_file_server("import_own_file", analysis_history, step_nb_react, parent_session = session)

  # module to choose between the different tools
  mod_manip_choice_server("manip_dataset", analysis_history, step_nb_react, parent_session = session)
  mod_manip_group_by_server("manip_group_by", analysis_history, step_nb_react, parent_session = session)

  # module to choose betwen visualisation
  mod_visu_choice_server("visu_dataset", analysis_history, step_nb_react, parent_session = session)

  # module to download the report
  mod_report_server("report", analysis_history)

  # choose statistical test and dataset

  # write conclusion


  # navigation

  # navigate between pages at to start the application
  observeEvent(input$start_analysis,{
    updateTabsetPanel(session, "vigie_nature_analyse",
                      selected = "question")
  })

  # navigation from landing pages to others
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
