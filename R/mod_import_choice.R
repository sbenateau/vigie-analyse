#' import_choice UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinyBS
mod_import_choice_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Etape 2 : Importation des données"),
    p("Vous êtes prêt à commencer votre analyse ! Il vous faut maintenant charger des données. Vous pouvez accéder à des jeux de données issus des programmes du Muséum national d'Histoire naturelle ou importer votre propore fichier de données (au format csv)."),
    bsCollapse(id = "collapseExample",
               multiple = TRUE,
               bsCollapsePanel(title = "Importer des données sur les oiseaux",
                               tagList(
                                 column(6,
                                        h3("Données issues de Vigie-Nature École (données protocolées)"),
                                        rep_br(2),
                                        fluidRow(img(src='http://bricks.vigienature-ecole.fr/media/edito/2021/04/01/donneevne_JrAsU7r.png', align = "center", width="95%")),
                                        br(),
                                        p("Les données issues de Vigie-Nature École sont organisées d'une manière particulière : chaque ligne représente une espèce. Pour une session d'observation, on aura donc plusieurs lignes (ayant toutes le même numéro d'observation)."),
                                        actionButton(ns("import_vne_birds"), "Importer un jeu de données issu de Vigie-Nature École",
                                                     style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%")
                                 ),
                                 column(6,
                                        h3("Importer des données regroupées par départements et par années"),
                                        rep_br(2),
                                        fluidRow(img(src = "http://bricks.vigienature-ecole.fr/media/edito/2021/04/01/donneevn_NGWYIuF.png", align = "center", width="95%")),
                                        br(),
                                        p("Les données issues de ce jeu de données sont issues de Vigie-Nature et Vigie-Nature École. Les données ont été pré-traitées : mois par mois depuis 2012, pour chaque espèce d'oiseaux, vous trouverez le nombre d'individus comptés (l'abondance), le nombre de fois où l'espèce a été vue et le nombre totale d'observations réalisées ce mois dans le département."),
                                        actionButton(ns("import_vn_birds"), "Importer des données regroupées par départements et par années",
                                                     style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%"),
                                        rep_br(2)
                                 ),

                                 column(6,
                                        h3("Importer les données issues de l'INPN"),
                                        rep_br(2),
                                        fluidRow(img(src = "http://bricks.vigienature-ecole.fr/media/edito/2021/04/01/donneeinpn.png", align = "center", width="95%")),
                                        br(),
                                        p("Pour ces données, la France a été découpée en carrés de 10 km sur 10 km, ces carrés sont appelés des mailles. L'INPN a ensuite synthétisé dans chaque maille les nombre d'espèces qui ont été vues dans de très nombreuses observations. Ainsi, pour chaque maille, ce jeu de données indique le nombre d'espèces qui ont été déjà signalées toutes dates confondues."),
                                        actionButton(ns("import_INPN_birds"), "Importer les données issues de l'INPN",
                                                     style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%"),
                                        rep_br(2)
                                 )
                               )
               ),
               bsCollapsePanel("Importer votre propre jeu de données",
                               tagList(
                                 column(6,
                                        h3("Importer votre propre jeu de données"),
                                        br(),
                                        p("Cet outil permet d'importer un fichier de données au format CSV. Ce format est accessible dans tous les tableurs, il suffit de choisir ce format lors de l'enregistrement du document. Il est nécessaire de choisir la virgule comme séparateur de colonne. Le contenu de chaque cellule doit être encadré par des guillemets simples (') ou doubles, si ce contenu contient le caractère séparateur de colonne (une virgule donc). Par exemple si un contenu contient une virgule, il faut donc encadrer ce contenu par des guillemets.  aaa, bbb devient donc 'aaa, bbb' ."),
                                        actionButton(ns("import_own_file"), "Importer votre propre jeu de données",
                                                     style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%"),
                                        rep_br(2)
                                 )
                               )
               ),
               bsCollapsePanel("Importer des données complémentaires",
                               tagList(
                                 column(6,
                                        h3("Importer des données pour analyser les résultats Vigie-Chiro"),
                                        br(),
                                        p("Ce fichier contient les correspondances entre les codes taxons et les espèces suivies dans le cadre de Vigie-Chiro. Pour faire la correspondance avec les deux fichiers, il faut utiliser l'outil de manipulation de données : joindre deux fichiers en fonction d'une colonne"),
                                        actionButton(ns("import_chiro_specie_list"), "Importer le fichier de données complémentaires Vigie-Chiro",
                                                     style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%"),
                                        rep_br(2)
                                 )
                               )
               )
    )
  )
}

#' import_choice Server Functions
#'
#' @noRd
mod_import_choice_server <- function(id, analysis_history, step_nb_react, parent_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # ReactiveValue to return
    to_return <- reactiveValues(type = NULL,
                                dataset = NULL,
                                protocole = NULL,
                                parameters_text = NULL)

    # Importation directe des jeux de données
    observeEvent(input$import_vne_birds, {
      # record values
      to_return$type <- "dataset"
      to_return$type_precise <- "Importation de données"
      to_return$tool_name <- "importer des données Vigie-Nature École"
      to_return$parameters <- list() # to do : add parameters for report
      to_return$protocole <- "Oiseaux des jardins"
      to_return$parameters_text <- paste("Importation du jeu de données issu du protocole :  Oiseaux des jardins")

      to_return$dataset <- read.csv(paste0("../../datasets/bricks/oiseaux.csv"))


      # store into reactive value
      analysis_history[[paste0("step_", step_nb_react())]] <- to_return
      mod_history_server("import", analysis_history, step_nb_react())


      # go to next step UI
      updateTabsetPanel(session = parent_session, "vigie_nature_analyse",
                        selected = "import_landing")

      cat("increment step_nb_react")
      step_nb_react(step_nb_react()+1)
    })

    # Importation directe des jeux de données
    observeEvent(input$import_vn_birds, {
      # record values
      to_return$type <- "dataset"
      to_return$type_precise <- "Importation de données"
      to_return$tool_name <- "importer des données Vigie-Nature École et Vigie-Nature"
      to_return$parameters <- list() # to do : add parameters for report
      to_return$protocole <- "Oiseaux des jardins"
      to_return$parameters_text <- paste("Importation du jeu de données issu du protocole : Oiseaux des jardins")

      to_return$dataset <- read.csv(paste0("../../datasets/bricks/oiseaux_odj.csv"))


      # store into reactive value
      analysis_history[[paste0("step_", step_nb_react())]] <- to_return
      mod_history_server("import", analysis_history, step_nb_react())


      # go to next step UI
      updateTabsetPanel(session = parent_session, "vigie_nature_analyse",
                        selected = "import_landing")

      cat("increment step_nb_react")
      step_nb_react(step_nb_react()+1)
    })

    observeEvent(input$import_chiro_specie_list, {
      # record values
      to_return$type <- "dataset"
      to_return$type_precise <- "Importation de données"
      to_return$tool_name <- "importer le fichier Vigie-Chiro pour la correspondance des espèces"
      to_return$parameters <- list() # to do : add parameters for report
      to_return$protocole <- "Vigie-Chiro"
      to_return$parameters_text <- paste("Importation du jeu de données Vigie-Chiro pour la correspondance des espèces")
      to_return$dataset <- read.csv(paste0("../../datasets/bricks/SpeciesList.csv"))

      # store into reactive value
      analysis_history[[paste0("step_", step_nb_react())]] <- to_return
      mod_history_server("import", analysis_history, step_nb_react())


      # go to next step UI
      updateTabsetPanel(session = parent_session, "vigie_nature_analyse",
                        selected = "import_landing")

      cat("increment step_nb_react")
      step_nb_react(step_nb_react()+1)
    })


    # navigation vers les pages d'importation des données
    observeEvent(input$import_own_file, {
      # go to next step UI
      updateTabsetPanel(session = parent_session, "vigie_nature_analyse",
                        selected = "import_own_file")

    })

  })
}

