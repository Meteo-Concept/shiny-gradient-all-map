library(data.table)
library(sf)

citiesUI <- function(id)
{
  ns <- NS(id)
  tagList(
    tags$div(id=ns("cities_containers"),
      tags$fieldset(
        tags$legend("Liste des villes"),
        textOutput(ns("cities_size")),
        uiOutput(ns("cities"))
      )
    ),
    tags$fieldset(
      tags$legend("Ajouter une ville"),
      fluidRow(
        column(6, selectizeInput(ns("new_city"), NULL, choices=NULL)),
        column(3, numericInput(ns("new_value"), NULL,value=NULL)),
        column(3, actionButton(ns("add_city"), "Ajouter", icon=icon("plus")))
      )
    )
  )
}


citiesServer <- function (id, mapExtent)
{
  frenchCities <- st_read(dsn=".", layer="COMMUNE")
  st_geometry(frenchCities) <- st_sfc(apply(frenchCities, 1, function (city) { st_point(c(city$X_CENTR, city$Y_CENTR)) }, simplify=F), crs="EPSG:2154")
  frenchCities <- st_transform(frenchCities, crs="EPSG:3857")
  frenchCities <- frenchCities[order(frenchCities$NOM_COM),]
  frenchCities$ID <- 1:nrow(frenchCities)
  
  refreshCitiesSelector <- function(session, ext, cities) {
    ext <- st_transform(ext, st_crs(cities))
    geometries <- st_coordinates(cities)
    relevantCities <- as.data.frame(st_intersection(cities, ext))
    relevantCities$label <- sprintf("%s - %s (%s)", relevantCities$NOM_COM, relevantCities$CODE_PO, relevantCities$INSEE_C)
    relevantCities$value <- relevantCities$ID
    updateSelectizeInput(session, 'new_city', choices=relevantCities, server=TRUE)
  }

  moduleServer(
    id,
    function (input, output, session) {
      ns <- session$ns
      
      observe({
        ext <- debounce(mapExtent, 1000)()
        req(ext)
        refreshCitiesSelector(session, ext, frenchCities)
      })

      reactiveCities <- reactiveVal(value=list())
      
      observeEvent(input$add_city, {
        cityId <- input$new_city
        value <- input$new_value
        currentCities <- reactiveCities()
        req(cityId, value)
        req(all(sapply(currentCities, function (ci) ci$id != cityId)))
        newModule <- cityInputServer(paste0("city_", cityId), cityId)
        reactiveCities(c(currentCities, list(list(module=newModule, id=cityId, value=value))))
        observeEvent(newModule$removal(), {
          cities <- reactiveCities()
          notMe <- sapply(cities, function (ci) ci$id != cityId)
          req(notMe) # this is not supposed to be empty
          reactiveCities(cities[notMe])
        }, ignoreInit=TRUE)
      }, ignoreInit=TRUE)

      
      citiesInput <- debounce(reactive({
          currentCities <- reactiveCities()
          values <- lapply(currentCities, function (city) city$module$value())
          req(values)
          req(length(values) > 0)
          isolate({
            ids <- sapply(currentCities, function (city) city$id)
            val <- sapply(values, function (v) {
              if (is.null(v) || is.na(v) || !is.numeric(v))
                return(NA)
              else
                return(as.numeric(v))
            })
            communes <- frenchCities[as.integer(ids),]
            communes$TEMP <- val
            return (communes)
          })
        }), 1000)

      
      output$cities_size <- renderText({
        cities <- citiesInput()
        if (isTruthy(cities))
          paste(nrow(cities), " ville(s) avec une valeur")
        else
          "aucune ville"
      })


      output$cities <- renderUI({
        currentCities <- reactiveCities()
        ids <- lapply(currentCities, function (city) city$id)
        req(ids)
        req(length(ids) > 0)
        tagList(
          lapply(1:length(ids), function(n) {
            cityId <- ids[[n]]

            moduleValue <- isolate(currentCities[[n]]$module$value())
            value <- if (is.null(moduleValue)) currentCities[[n]]$value else moduleValue
            
            cc <- cityInputUI(id=ns(paste0("city_",cityId)), name=frenchCities[as.integer(cityId),]$NOM_COM, value=value)
            fluidRow(
              column(6, cc[[1]]),
              column(3, cc[[2]]),
              column(3, cc[[3]])
            )
          })
        )
      })
      
      return(citiesInput)
    }
  )
}