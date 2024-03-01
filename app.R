library(gstat)
library(sf)
library(stars)
library(RColorBrewer)
library(sysfonts)
library(colourpicker)
library(shiny)
library(shinyjs)
library(shinyalert)
library(DBI)
library(RSQLite)
library(RMariaDB)

ui <- fluidPage(
	useShinyjs(),
	titlePanel("Carte"),
	sidebarLayout(
		sidebarPanel(
			h1("Paramètres"),
			tags$div(id="accordion",
				tags$div(class="panel panel-primary",
					tags$div(class="panel-heading", id="panel-heading-1",
						tags$h2(role="button", `data-toggle`="collapse", `data-target`="#collapse-1", "Couleurs")
						),
					tags$div(id="collapse-1", class="panel-body collapse in", `data-parent`="#accordion",
						paletteUI("palette")
					)
					),
				tags$div(class="panel panel-primary",
					tags$div(class="panel-heading", id="panel-heading-2",
						tags$h2(role="button", `data-toggle`="collapse", `data-target`="#collapse-2", "Carte")
						),
					tags$div(id="collapse-2", class="panel-body collapse", `data-parent`="#accordion",
						p("Choisissez la carte que vous désirez."),
						(function (e) {
							tagList(
								column(12, e[[1]]),
								column(3, e[[2]]),
								column(3, e[[3]]),
								column(3, e[[4]]),
								column(3, e[[5]])
							)
})(extentInputUI("extent", NULL)),
						p("Cadre : ", textOutput("extent_frame", inline=T))
					)
					),
				tags$div(class="panel panel-primary",
					tags$div(class="panel-heading", id="panel-heading-3",
						tags$h2(role="button", `data-toggle`="collapse", `data-target`="#collapse-3", "Villes")
						),
					tags$div(id="collapse-3", class="panel-body collapse", `data-parent`="#accordion",
						citiesUI("cities")
					)
				)
				),
			fluidRow(
				column(6,
					actionButton(inputId="recompute", label="Lancer le calcul de la carte", class="btn btn-primary",
						width="100%", icon=icon("map"), style="font-size: larger;")
					),
				column(6, downloadButton(outputId="download", label="Télécharger l'image"))
			)
			),
		mainPanel(
			h1("Carte de température"),
			p("L'ancien outil est disponible ", a("ici", href="/gradient_map_v1"), "."),
			fluidRow(
				column(12, plotOutput(outputId="map", height="auto"), class="m-2") #, height="600px"
				#column(12, plotOutput(outputId="mapvalues", height="auto")) #, height="600px"
			)
		)
	)
)

server <- function(input, output, session) {

	paletteInput <- paletteServer("palette")

	reactiveMap <- reactiveValues(
		contour=NULL,
		last_id=0,
		grd=NULL,
		corners=NULL,
		map=NULL,
		limits=NULL,
		communesInput=NULL,
		communesValues=NULL
	)
	reactiveMap$map <- extentInputServer("extent", "EPSG:3857")

	communesInput <- citiesServer("cities", reactiveMap$map$extent)

	output$extent_frame <- renderText({
		ext <- st_bbox(reactiveMap$map$extent())
		if (is.null(ext)) {
			"-- Invalide --"
		} else {
			sprintf("(%.1f, %.1f, %.1f, %.1f)", ext$xmin, ext$xmax, ext$ymin, ext$ymax)
		}
	})

	reactiveMap$grd <- reactive({
		ext <- st_bbox(reactiveMap$map$extent())
		req(ext)
		st_make_grid(c(ext$xmin-5000, ext$xmax+5000, ext$ymin-5000, ext$ymax+5000), cellsize=500, crs="EPSG:3857", what="centers")
	})

	makePlot <- function() {
		values <- communesInput()
		contour <- reactiveMap$map$map()
		req(values, contour)
		req(nrow(values) > 0)
		palette <- paletteInput()
		print(file=stderr(), paste("Palette for map is null:", is.null(palette)))
		req(length(palette$domain) > 1)
		grd <- reactiveMap$grd()
		print(file=stderr(), paste("Grid:",  st_bbox(grd)))
		corners <- reactiveMap$map$extent()
		print(file=stderr(), paste(st_bbox(corners)))
		print(file=stderr(), paste("Length of palette:", length(palette$domain)))

		values <- st_transform(values, st_crs(grd))
		pred <- idw(TEMP~1, locations=values, newdata=grd)
		par(bg=NA, bty="n", xpd=NA, xaxs='i', xaxt='n', yaxs='i', yaxt='n', plt=c(0,1,0,1), oma=c(0,0,0,0))
		r <- st_crop(st_rasterize(pred), contour)
		plot(corners)
		image(r, col=palette$range, breaks=palette$domain,add=T, axis=F, legend=F)
	}

	observeEvent(input$recompute, {
		output$map <- renderPlot({
			makePlot()
		},
		bg="transparent",
		height=function() {
			corners <- st_bbox(reactiveMap$map$extent())
			ratio <- (corners$ymax - corners$ymin) / (corners$xmax - corners$xmin)
			session$clientData$output_map_width * ratio
		},
		res = 96 * session$clientData$output_map_width/1920
		)
	})

	output$download <- downloadHandler(
		filename = "carte.png",
		content = function(file) {
			plotPNG(makePlot, filename=file, width=1920, height=1080, res=96)
		}
	)
}


shinyApp(ui = ui, server = server)

