#' question UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_question_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    h2("Etape 1 : Question de recherche"),
    br(),
    textAreaInput(ns("question"), "Écrire votre question de recherche", width = "90%", rows = 5, resize = "none"),
    column(width = 4, offset = 8,
    actionButton(ns("validate_question"), "Valider la question", style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%"),
    ),
    textOutput(ns("question_output"))
  )
}

#' question Server Functions
#'
#' @noRd
mod_question_server <- function(id, analysis_history, step_nb_react, parent_session){
  moduleServer( id, function(input, output, session){

    cat("  start question module\n")
    ns <- session$ns

    # ReactiveValue to return
    to_return <- reactiveValues(type = NULL,
                                question_text = NULL)

    # When user press validate question
    # Record values, show output, go to next step
    observeEvent(input$validate_question, {
      cat("01_validate question\n")

      # record values
      to_return$type <- "question"
      to_return$question_text  <- input$question

      # store into reactive value
      analysis_history[["step_1"]] <- to_return
      mod_history_server("question", analysis_history, step_nb_react())


      # hide UI
      hide("question")
      hide("validate_question")

      # go to next step UI
      updateTabsetPanel(session = parent_session, "vigie_nature_analyse",
                        selected = "import")
      # shinyjs::reset(ns("validate_question"))
      step_nb_react(step_nb_react()+1)
    })

    # render question
    output$question_output <- renderText({
      to_return$question_text
    })
  })


}
