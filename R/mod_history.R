#' history UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import kableExtra
#' @import shinydashboardPlus
mod_history_ui <- function(id){
  ns <- NS(id)
  tagList(
    # not nessary ?
  )
}

#' history Server Functions
#'
#' @noRd
mod_history_server <- function(id, history_datasets, step_nb){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # get values of current step
    history_names <- names(history_datasets)
    step_id <- as.numeric(sub(".*_", "", history_names))
    index_current_step <- which(step_id == step_nb)

    # add history step box according to data type
    if (history_datasets[[history_names[index_current_step]]][["type"]] == "dataset"){

      insertUI(selector = "#history_reference", where = "afterEnd",
               tagList(
                 tags$style(HTML("
.box.box-solid.box-primary>.box-header {
  color:#fff;
  background:#ffc455;
}

.box.box-solid.box-primary{
border-bottom-color:#ffc455;
border-left-color:#666666;
border-right-color:#666666;
border-top-color:#ffc455;
}")),
                 br(), br(),


                 box(
                   title = p(dashboardLabel(paste("Étape", step_nb), status = "primary"), "Jeu de données"),
                   id = paste0("history_", step_nb),
                   closable = FALSE,
                   width = 12,
                   solidHeader = TRUE,
                   status = "primary",
                   collapsible = TRUE,
                   dropdownMenu = boxDropdown(
                     boxDropdownItem(id = ns("view_table"), "Voir le jeu de données"),
                     boxDropdownItem("Télécharger le jeu de données")
                   ),
                   tagList(
                     tabsetPanel(
                       tabPanel("Données",
                                helpText("Voici un extrait des premières lignes du jeu de données :"),
                                tags$div(style = 'overflow-x: scroll',
                                         HTML(
                                           kbl(head(history_datasets[[history_names[index_current_step]]][["dataset"]])) %>%
                                             kable_paper() %>%
                                             kable_styling(bootstrap_options = c("striped", "hover")) %>%
                                             row_spec(1, bold = T, color = "white", background = "grey")%>%
                                             add_indent(c(2:5))

                                         )
                                )


                       ),
                       tabPanel("Paramètres utilisés",
                                helpText("Voici les paramètres que vous avez utilisés :"),
                                p(history_datasets[[history_names[index_current_step]]][["parameters_text"]])
                       )
                     )
                   )
                 )
               )
      )
    } else if (history_datasets[[history_names[index_current_step]]][["type"]] == "question"){
      insertUI(selector = "#history_reference", where = "afterEnd",
               tagList(
                 br(), br(),


                 box(
                   title = p(dashboardLabel(paste("Étape", step_nb), status = "primary"), "Question de recherche"),
                   id = paste0("history_", step_nb),
                   closable = FALSE,
                   width = 12,
                   solidHeader = TRUE,
                   status = "primary",
                   collapsible = TRUE,
                   tagList(
                     p(history_datasets[[history_names[index_current_step]]][["question_text"]])

                   )
                 )
               )

      )

    } else if (history_datasets[[history_names[index_current_step]]][["type"]] == "graphique"){

      insertUI(selector = "#history_reference", where = "afterEnd",
               tagList(
                 tags$style(HTML("
.box.box-solid.box-primary>.box-header {
  color:#fff;
  background:#ffc455;
}

.box.box-solid.box-primary{
border-bottom-color:#ffc455;
border-left-color:#666666;
border-right-color:#666666;
border-top-color:#ffc455;
}")),
                 br(), br(),


                 box(
                   title = p(dashboardLabel(paste("Étape", step_nb), status = "primary"), "Graphique"),
                   id = paste0("history_", step_nb),
                   closable = FALSE,
                   width = 12,
                   solidHeader = TRUE,
                   status = "primary",
                   collapsible = TRUE,
                   dropdownMenu = boxDropdown(
                     boxDropdownItem(id = ns("view_table"), "Voir le graphique"),
                     boxDropdownItem("Télécharger le graphique")
                   ),
                   tagList(
                     tabsetPanel(
                       tabPanel("Graphique",

                                renderPlot({
                                  history_datasets[[history_names[index_current_step]]][["graphique"]]
                                })


                       ),
                       tabPanel("Paramètres utilisés",
                                helpText("Voici les paramètres que vous avez utilisés :"),
                                p(history_datasets[[history_names[index_current_step]]][["parameters_text"]])
                       )
                     )
                   )
                 )
               )
      )
    }


  })

  if (step_nb > 1){
    cat('collapse box\n')
    # add code to collapse the box before
  }

}
