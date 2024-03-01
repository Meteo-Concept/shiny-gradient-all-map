library(shiny)

cityInputUI <- function(id, value, name) {
	ns <- NS(id)
	tagList(
	  tags$label(name, `for`=ns("value")),
		numericInput(ns("value"), NULL, value=value),
		actionButton(ns("remove"), "Retirer", icon=icon("trash"))
	)
}

cityInputServer <- function(id, cityId) {
	moduleServer(
		id,
		function(input, output, session) {
			list(
			    value=reactive({ input$value }),
			    removal=reactive({ input$remove }),
			    id=cityId # this one is not reactive
			)
		}
	)
}

