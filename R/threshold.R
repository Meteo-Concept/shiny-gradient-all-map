library(shiny)
library(colourpicker)

thresholdUI <- function(id, colour, value) {
	ns <- NS(id)
	tagList(
		colourpicker::colourInput(ns("colour"), NULL, value=colour),
		numericInput(ns("value"), NULL, value=value),
		actionButton(ns("remove"), "Retirer", icon=icon("trash"))
	)
}

thresholdServer <- function(id) {
	moduleServer(
		id,
		function(input, output, session) {
			value <- reactive({
				#print(file=stderr(), "Triggered refresh")
				list(colour=input$colour, value=input$value, removal=input$remove)
			})

			return(value)
		}
	)
}

