#' Inverted versions of in, is.null and is.na
#'
#' @noRd
#'
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#'
#' @noRd
#'
#' @example
#' drop_nulls(list(1, NULL, 2))
drop_nulls <- function(x) {
  x[!sapply(x, is.null)]
}

#' If x is `NULL`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NULL`
#'
#' @noRd
#'
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NA`
#'
#' @noRd
#'
#' @examples
#' NA %|NA|% 1
"%|NA|%" <- function(x, y) {
  if (is.na(x)) {
    y
  } else {
    x
  }
}

#' Typing reactiveValues is too long
#'
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#'
#' @noRd
rv <- function(...) shiny::reactiveValues(...)
rvtl <- function(...) shiny::reactiveValuesToList(...)

# add counting function
count_sup_zero <- function(x) length(x[x>0])


#' filter dataset from analysis report and update the select input
#'
#' @param analysis_history reactive values containing all information about the objects and objects themself
#' @param input_to_update vector of the input to update
#'
#' @export
#'
#' @examples
#' filter_and_update_datasets()
filter_and_update_datasets <- function(analysis_history, input_to_update, session, ns){
  datasets_names <- names(analysis_history)
  datasets_names_keep <- rep(TRUE, length(datasets_names))
  if(length(datasets_names) > 1) {

    cat("  update dataset list\n")
    for (i in seq_along(datasets_names)){
      datasets_names_keep[i] <- ifelse(analysis_history[[datasets_names[i]]][["type"]] != "dataset", FALSE, TRUE)
    }
    datasets_names <- datasets_names[datasets_names_keep]
    for (i in seq_along(input_to_update)){
      updateSelectInput(session = session,
                        inputId = ns(input_to_update[i]),
                        choices = datasets_names)
    }
  }
}
