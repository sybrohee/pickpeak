


#' @title   sampleSelector and sampleSelectorUI
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname sampleSelector
#' @export
#' @importFrom shiny NS fileInput tagList
#' @keywords internal
sampleSelectorUI <- function(id){
  ns <- NS(id)

  tagList(
      fileInput( ns("fileList"), "Select FSA file(s)",
                 multiple = TRUE,
                 buttonLabel = "Add samples...",
                 accept = c("fsa"))
    )
}

# module server function
#' @rdname sampleSelector
#' @export
#' @keywords internal

sampleSelector <- function(input,output,session) {
  return(
	list(
		selectedSamples = reactive(input$fileList)
		)
	)
}
