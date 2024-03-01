library(shiny)

extentInputUI <- function(id, ext) {
	ns <- NS(id)
	if (is.null(ext)) {
		tagList(
		  selectInput(ns("map"), "Cartes prédéfinies",  NULL),
			numericInput(ns("minlat"), "Sud", value=NULL, min=-85, max=85, step=0.1),
			numericInput(ns("maxlat"), "Nord", value=NULL, min=-85, max=85, step=0.1),
			numericInput(ns("minlon"), "Ouest", value=NULL, min=-180, max=180, step=0.1),
			numericInput(ns("maxlon"), "Est", value=NULL, min=-180, max=180, step=0.1)
		)
	} else {
		tagList(
		  selectInput(ns("map"), "Cartes prédéfinies",  NULL),
			numericInput(ns("minlat"), "Sud", value=round(ext$ymin,1), min=-85, max=85, step=0.1),
			numericInput(ns("maxlat"), "Nord", value=round(ext$ymax,1), min=-85, max=85, step=0.1),
			numericInput(ns("minlon"), "Ouest", value=round(ext$xmin,1), min=-180, max=180, step=0.1),
			numericInput(ns("maxlon"), "Est", value=round(ext$xmax,1), min=-180, max=180, step=0.1)
		)
	}
}


extentInputServer <- function(id, targetCRS) {
	moduleServer(
		id,
		function(input, output, session) {
		  updateSelectInput(session, 'map', choices={
		    files <- list.files(path="./maps", pattern=".*\\.json")
		    names(files) <- sub('.json', '', files)
		  })

		  map4326 <- reactive({
		    m <- input$map
		    req(m)
		    print(file=stderr(), paste("New map selected: ", m))

		    st_read(dsn=paste0("maps/",m,".json"))
		  })
		    
		  map <- reactive({
		    req(map4326)
		    # convert the map to the target CRS
		    st_transform(map4326(), targetCRS)
		  })
		  
		  observe({
		    req(map4326)
		    
		    # display and get the extent in lat/lng for the user
		    ext4326 <- st_bbox(map4326())
		    updateNumericInput(session, "minlat", value=round(as.numeric(ext4326$ymin), 1))
		    updateNumericInput(session, "maxlat", value=round(as.numeric(ext4326$ymax), 1))
		    updateNumericInput(session, "minlon", value=round(as.numeric(ext4326$xmin), 1))
		    updateNumericInput(session, "maxlon", value=round(as.numeric(ext4326$xmax), 1))
		  })

			ext <- reactive({
			  newExt <- c(xmin=input$minlon, xmax=input$maxlon, ymin=input$minlat, ymax=input$maxlat)
				print(file=stderr(), "Triggered extent refresh")
				req(newExt['ymin'], newExt['xmin'], newExt['ymax'], newExt['xmax'],
				    newExt['xmin'] <= newExt['xmax'], newExt['ymin'] <= newExt['ymax'])
				bbox <- st_bbox(newExt, crs=st_crs(4326))
				
				# transform the corners into a polygon in the target CRS
				st_transform(st_as_sfc(bbox), crs=targetCRS)
			})


			return(list(extent=debounce(ext, 1000), map=debounce(map, 1000)))
		}
	)
}

