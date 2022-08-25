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
    # Use js and dashboard function - for boxes and show / hide content
    useShinyjs(),
    useShinydashboardPlus(),
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Our application UI logic
    fluidPage(
      setBackgroundColor(color = "#037971"),
      # Have different pages for each steps
      navbarPage("Vigie-Nature Analyse", id = "vigie_nature_analyse",
                 # home page
                 tabPanel("Start",
                          column(style='min-height:500px; border: 10px; background: #FFFFFF', width = 10, offset = 1, align="center",
                                 #h1("Vigie-Nature École Analyse"),
                                 rep_br(3),
                                 h2("Bienvenue dans Vigie-Nature École Analyse !"),
                                 rep_br(2),
                                 actionButton(
                                   inputId = "start_analysis",
                                   label = "Commencer l'analyse de données",
                                   style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%"
                                 )
                          )
                 ), # end tab start
                 # question page
                 tabPanel(title = "Question de recherche", value = "question",

                          column(style='min-height:500px; border: 10px; background: #FFFFFF', width = 10, offset = 1, align="left",
                                 br(),
                                 mod_question_ui("question")
                          )
                 ), # end tab question
                 # import page
                 navbarMenu("Importation des données",
                            tabPanel("Importation des données", value = "import",
                                     column(style='min-height:500px; border: 10px; background: #FFFFFF', width = 10, offset = 1, align="left",
                                            rep_br(1),
                                            mod_import_choice_ui("import_choice")
                                     )
                            ),
                            tabPanel("votre fichier", value = "import_own_file",
                                     column(style='min-height:500px; border: 10px; background: #FFFFFF', width = 10, offset = 1, align="left",
                                            mod_import_own_file_ui("import_own_file")
                                     )
                            ),
                            tabPanel("Landing importation des données", value = "import_landing",
                                     column(style='min-height:200px; border: 10px; background: #FFFFFF', width = 10, offset = 1, align="left",
                                            rep_br(1),
                                            p("Votre jeu de données a bien été importé. Il est disponible dans votre historique en dessous de ce paragraphe"),
                                            p("Vous pouvez continuer en important un nouveau jeu de donnée, en manipulant les données ou en les visualisant"),
                                            actionButton("import_nav_import", "Importer un nouveau jeu de données",
                                                         style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%"),
                                            actionButton("manip_nav_import", "Manipuler les données",
                                                         style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%"),
                                            actionButton("visu_nav_import", "Visualiser les données",
                                                         style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%"),
                                     )
                            )
                 ),
                 # data wrangling page
                 navbarMenu("Manipulation des données",
                            tabPanel("Manipulation des données", value = "manip",

                                     column(id = "column_manip", style='min-height:500px; border: 10px; background: #FFFFFF', width = 10, offset = 1, align="left",
                                            rep_br(1),
                                            mod_manip_choice_ui("manip_dataset")
                                     )
                            ),
                            tabPanel("Résumer des données", value = "manip_group_by",
                                     column(id = "column_manip", style='min-height:500px; border: 10px; background: #FFFFFF', width = 10, offset = 1, align="left",
                                            rep_br(1),
                                            mod_manip_group_by_ui("manip_group_by")
                                     )
                            ),
                            tabPanel("Sélectionner des lignes", value = "manip_filter",
                                     column(id = "column_manip", style='min-height:500px; border: 10px; background: #FFFFFF', width = 10, offset = 1, align="left",
                                            rep_br(1),
                                            mod_filter_ui("manip_filter")
                                     )
                            ),
                            tabPanel("Joindre deux jeux de données", value = "manip_join",
                                     column(id = "column_manip", style='min-height:500px; border: 10px; background: #FFFFFF', width = 10, offset = 1, align="left",
                                            rep_br(1),
                                            mod_manip_join_datasets_ui("manip_join")
                                     )
                            ),
                            tabPanel("Landing manipulation des données", value = "manip_landing",
                                     column(style='min-height:200px; border: 10px; background: #FFFFFF', width = 10, offset = 1, align="left",
                                            rep_br(1),
                                            p("Votre calcul a bien été effectué. Le jeu de données calculé est disponible dans votre historique en dessous de ce paragraphe"),
                                            p("Vous pouvez continuer en important un nouveau jeu de donnée, en manipulant les données ou en les visualisant"),
                                            actionButton("import_nav_manip", "Importer un nouveau jeu de données",
                                                         style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%"),
                                            actionButton("manip_nav_manip", "Manipuler les données",
                                                         style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%"),
                                            actionButton("visu_nav_manip", "Visualiser les données",
                                                         style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%"),
                                     )
                            )
                 ),
                 # data visualisation page
                 navbarMenu("Visualisation des données",
                            tabPanel("Visualisation des données", value = "visu",

                                     column(style='min-height:500px; border: 10px; background: #FFFFFF', width = 10, offset = 1, align="left",
                                            rep_br(1),
                                            mod_visu_choice_ui("visu_dataset"),
                                            div(id="visu_tool")
                                     ),

                            ),
                            tabPanel("Sélectionner des lignes", value = "visu_graph",
                                     column(id = "column_visu", style='min-height:500px; border: 10px; background: #FFFFFF', width = 10, offset = 1, align="left",
                                            rep_br(1),
                                            mod_visu_graphique_ui("visu_graph")
                                     )
                            ),
                            tabPanel("Landing visualisation des données", value = "visu_landing",
                                     column(style='min-height:200px; border: 10px; background: #FFFFFF', width = 10, offset = 1, align="left",
                                            rep_br(1),
                                            p("Votre réprésentation a bien été effectuée. La représentation calculée est disponible dans votre historique en dessous de ce paragraphe"),
                                            p("Vous pouvez continuer en important un nouveau jeu de donnée, en manipulant les données ou en les visualisant"),
                                            actionButton("import_nav_visu", "Importer un nouveau jeu de données",
                                                         style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%"),
                                            actionButton("manip_nav_visu", "Manipuler les données",
                                                         style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%"),
                                            actionButton("visu_nav_visu", "Visualiser les données",
                                                         style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%"),
                                     )
                            )
                 ),
                 tabPanel("Télécharger un rapport", value = "report",
                          column(style='min-height:200px; border: 10px; background: #FFFFFF', width = 10, offset = 1, align="left",
                                 rep_br(1),
                                 mod_report_ui("report")
                          )
                 )
      ),
      # had reference to create hitory below the app
      rep_br(2),
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
