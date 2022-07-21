fileInput <- function(inputId, label, multiple = FALSE, accept = NULL,
  width = NULL, buttonLabel = "Browse...", placeholder = "No file selected",
  capture = NULL) {

  restoredValue <- restoreInput(id = inputId, default = NULL)

  # Catch potential edge case - ensure that it's either NULL or a data frame.
  if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
    warning("Restored value for ", inputId, " has incorrect format.")
    restoredValue <- NULL
  }

  if (!is.null(restoredValue)) {
    restoredValue <- toJSON(restoredValue, strict_atomic = FALSE)
  }

  inputTag <- tags$input(
    id = inputId,
    name = inputId,
    type = "file",
    # Don't use "display: none;" style, which causes keyboard accessibility issue; instead use the following workaround: https://css-tricks.com/places-its-tempting-to-use-display-none-but-dont/
    #style = "position: absolute !important; top: -99999px !important; left: -99999px !important;",
    `data-restore` = restoredValue
  )

  if (multiple)
    inputTag$attribs$multiple <- "multiple"
  if (length(accept) > 0)
    inputTag$attribs$accept <- paste(accept, collapse=',')

  if (!is.null(capture)) {
    inputTag$attribs$capture <- capture
  }

  div(class = "form-group shiny-input-container",

    div(class = "input-group",
      # input-group-prepend is for bootstrap 4 compat
        inputTag,
    ),
  )
}