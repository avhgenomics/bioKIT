# Module UI function


#' csvFileInput
#'
#' shiny UI module for loading csv file.  Needs csvFile() in the server function.
#'
#' @param id the shiny id to be called internally, as a string.
#' @param label The label that is displayed in the app.
#' @export
#' @return Returns a load file button in a shiny app.
#' @examples
#' #UI section
#' #csvFileInput(id = "csv1",label= "load csv file")
#' #Server Section
#' #X <- callModule(csvFile,"csv1",stringsAsFactors = F)
#'
csvFileInput <- function(id, label = "CSV file") {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    fileInput(ns("file"), label),
    checkboxInput(ns("heading"), "Has heading",value = TRUE),
    selectInput(ns("quote"), "Quote", c(
      "None" = "",
      "Double quote" = "\"",
      "Single quote" = "'"
    ),selected = "None")
  )
}

# Module server function

#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param input DESCRIPTION.
#' @param output DESCRIPTION.
#' @param session DESCRIPTION.
#' @param stringsAsFactors treat strings as factors? Logical.
#'
#' @return returns a loaded csv file.
#' @examples
#' #UI section
#' #csvFileInput(id = "csv1",label= "load csv file")
#' #Server Section
#' #X <- callModule(csvFile,"csv1",stringsAsFactors = F)
#'
csvFile <- function(input, output, session, stringsAsFactors) {
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })

  # The user's data, parsed into a data frame
  dataframe <- reactive({
    read.csv(userFile()$datapath,
             header = input$heading,
             quote = input$quote,
             stringsAsFactors = stringsAsFactors)
  })

  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })

  # Return the reactive that yields the data frame
  return(dataframe)
}
