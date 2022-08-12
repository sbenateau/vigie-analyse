#' import_choice UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_import_choice_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Etape 2 : Importation des données"),
    p("Vous êtes prêt à commencer votre analyse ! Il vous faut maintenant charger des données. Vous pouvez accéder à des jeux de données issus des programmes du Muséum national d'Histoire naturelle ou importer votre propore fichier de données (au format csv)."),

    box(
      title = p(dashboardLabel(paste("Étape"), status = "primary"), "Importer des données sur les oiseaux"),
      closable = FALSE,
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      collapsible = TRUE,
      collapsed = TRUE,
      tagList(
        column(6,
               h3("Données issues de Vigie-Nature École (données protocolées)"),
               rep_br(2),
               fluidRow(img(src='http://galaxybricks.vigienature-ecole.fr/media/edito/2021/04/01/donneevne_JrAsU7r.png', align = "center", width="95%")),
               br(),
               p("Les données issues de Vigie-Nature École sont organisées d'une manière particulière : chaque ligne représente une espèce. Pour une session d'observation, on aura donc plusieurs lignes (ayant toutes le même numéro d'observation)."),
               actionButton(ns("import_vne"), "Importer un jeu de données issu de Vigie-Nature École",
                            style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%")
        ),
        column(6,
               h3("Importer des données regroupées par départements et par années"),
               rep_br(2),
               fluidRow(img(src = "http://galaxybricks.vigienature-ecole.fr/media/edito/2021/04/01/donneevn_NGWYIuF.png", align = "center", width="95%")),
               br(),
               p("Les données issues de ce jeu de données sont issues de Vigie-Nature et Vigie-Nature École. Les données ont été pré-traitées : mois par mois depuis 2012, pour chaque espèce d'oiseaux, vous trouverez le nombre d'individus comptés (l'abondance), le nombre de fois où l'espèce a été vue et le nombre totale d'observations réalisées ce mois dans le département."),
               actionButton(ns("import_vn"), "Importer des données regroupées par départements et par années",
                            style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%"),
               rep_br(2)
        ),

        column(6,
               h3("Importer les données issues de l'INPN"),
               rep_br(2),
               fluidRow(img(src = "http://galaxybricks.vigienature-ecole.fr/media/edito/2021/04/01/donneeinpn.png", align = "center", width="95%")),
               br(),
               p("Pour ces données, la France a été découpée en carrés de 10 km sur 10 km, ces carrés sont appelés des mailles. L'INPN a ensuite synthétisé dans chaque maille les nombre d'espèces qui ont été vues dans de très nombreuses observations. Ainsi, pour chaque maille, ce jeu de données indique le nombre d'espèces qui ont été déjà signalées toutes dates confondues."),
               actionButton(ns("import_INPN"), "Importer les données issues de l'INPN",
                            style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%"),
               rep_br(2)
        )
      )
    ), # end bird box
    box(
      title = p(dashboardLabel(paste("Étape"), status = "primary"), "Importer votre propre jeu de données"),
      closable = FALSE,
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      collapsible = TRUE,
      collapsed = TRUE,
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
    ) # end box import own file

  )
}

#' import_choice Server Functions
#'
#' @noRd
mod_import_choice_server <- function(id, parent_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # navigation vers les pages d'importation des données
    observeEvent(input$import_vne_birds, {

    })

    # navigation vers les pages d'importation des données
    observeEvent(input$import_own_file, {
      # go to next step UI
      updateTabsetPanel(session = parent_session, "vigie_nature_analyse",
                        selected = "import_own_file")

    })

  })
}

## To be copied in the UI
# mod_import_choice_ui("import_choice_1")

## To be copied in the server
# mod_import_choice_server("import_choice_1")
