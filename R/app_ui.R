#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyWidgets
#' @import shinyjs
#' @import shinydashboardPlus
#' @noRd
app_ui <- function(request) {
  tagList(
    useShinyjs(),
    useShinydashboardPlus(),
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      setBackgroundColor(color = "#037971"),
      #have diferent pages for each steps
      navbarPage("Vigie-Nature Analyse", id = "vigie_nature_analyse",
                 # home page
                 tabPanel("Start",
                          column(style='min-height:500px; border: 10px; background: #FFFFFF', width = 10, offset = 1, align="center",
                                 #h1("Vigie-Nature École Analyse"),
                                 h2("Bienvenue dans Vigie-Nature École Analyse !"),
                                 tags$div(class = "classic-button",
                                          actionButton(
                                            inputId = "start_analysis",
                                            label = "Commencer l'analyse de données",
                                            style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%"
                                          )
                                 )
                          )
                 ),
                 # question page
                 tabPanel(title = "Question de recherche", value = "question",

                          column(style='min-height:500px; border: 10px; background: #FFFFFF', width = 10, offset = 1, align="left",
                                 br(),
                                 mod_question_ui("question")
                          )
                 ),
                 # import page
                 tabPanel("Importation des données", value = "import",

                          column(style='min-height:500px; border: 10px; background: #FFFFFF', width = 10, offset = 1, align="left",
                                 rep_br(1),
                                 mod_import_datasets_ui("import_dataset")
                          )
                 ),
                 # data wrangling page
                 tabPanel("Manipulation des données", value = "manip",

                          column(style='min-height:500px; border: 10px; background: #FFFFFF', width = 10, offset = 1, align="left",
                                 rep_br(1),
                                 mod_manip_choice_ui("manip_dataset")
                          )
                 )
      ),
      # had reference to create hitory below the app
      br(),br(),
      div(id="history_reference")
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "vigieanalyse"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
