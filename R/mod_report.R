#' report UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_report_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Vous avez maintenant terminé votre analyse, vous pouvez télécharger un rapport contenant toutes les étapes, depuis la questions jusqu'au test statistique"),
    downloadButton(ns("report"), "Télécharger le rapport",
                   style = "color: #FFFFFF; background-color: #037971; border-color: #037971; font-size:120%")
  )
}

#' report Server Functions
#'
#' @noRd
mod_report_server <- function(id, analysis_history){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.pdf",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("R/report.Rmd", tempReport, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list(analysis_history = analysis_history)

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
  })
}

## To be copied in the UI
# mod_report_ui("report_1")

## To be copied in the server
# mod_report_server("report_1")
