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
mod_question_server <- function(id, parent_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # ReactiveValue to return
    to_return <- reactiveValues(question_text = NULL,
                               trigger = NULL)

    # When user press validate question
    # Record values, show output, go to next step
    observeEvent(input$validate_question, {
      cat("Validate question\n")

      # record values
      to_return$trigger <- ifelse(is.null(to_return$trigger), 0, to_return$trigger) + 1
      to_return$type <- "question"
      to_return$question_text  <- input$question

      # render question
      output$question_output <- renderText({
        input$question
      })

      # hide UI
      hide("question")
      hide("validate_question")

      # go to next step UI
      updateTabsetPanel(session = parent_session, "vigie_nature_analyse",
                        selected = "import")
      # shinyjs::reset(ns("validate_question"))

    })
    return(to_return)
  })


}
