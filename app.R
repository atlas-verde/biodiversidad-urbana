#
# Dashboard for "Biodiversidad urbana"
#


# PACKAGES
library(dplyr)
library(sf)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(shiny)
library(shinythemes)
library(shinycssloaders)


# CONSTANTS

# Data sources

# Cantons
DSN_CANTONS <- "data/cantones.geojson"
COLUMN_CANTON_NAME <- "canton"
cantons <- st_read(dsn = DSN_CANTONS, quiet = TRUE)

# Birds
DSN_BIRDS <- "data/aves-gam.csv"

birds <-
  st_read(
    DSN_BIRDS,
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    ),
    quiet = TRUE
  )

# Plants
DSN_PLANTS <- "data/plantae-gam.csv"

plants <-
  st_read(
    DSN_PLANTS,
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    ),
    quiet = TRUE
  )

# Reptils
DSN_REPTILS <- "data/reptiles-gam.csv"

reptils <-
  st_read(
    DSN_REPTILS,
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    ),
    quiet = TRUE
  )

# Mammals
DSN_MAMMALS <- "data/mamiferos-gam.csv"

mammals <-
  st_read(
    DSN_MAMMALS,
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    ),
    quiet = TRUE
  )

# Amphibians
DSN_AMPHIBIANS <- "data/anfibios-gam.csv"

amphibians <-
  st_read(
    DSN_AMPHIBIANS,
    options = c(
      "X_POSSIBLE_NAMES=decimalLongitude",
      "Y_POSSIBLE_NAMES=decimalLatitude"
    ),
    quiet = TRUE
  )

# FUNCTIONS

# Create map
create_map <-
  function() {
    leaflet() |>
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Mapa de calles (OpenStreetMap)") |>
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Mapa oscuro (CartoDB Dark Matter)") |>
      addProviderTiles(providers$Stamen.TonerLite, group = "Mapa claro (Stamen Toner Lite)") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Imágenes satelitales (ESRI World Imagery)") |>
      addPolygons(
        data = cantons,
        layerId = ~canton,
        fillOpacity = 0.0,
        stroke = TRUE,
        color = "black",
        weight = 2,
        popup = paste(
          paste("<strong>Cantón:</strong>",  cantons[[COLUMN_CANTON_NAME]])
        ),
        label = paste(
          paste("Cantón:",  cantons[[COLUMN_CANTON_NAME]])
        ),        
        group = "Límite cantonal"
      ) |>
      addCircleMarkers(
        data = birds,
        clusterOptions = markerClusterOptions(),
        radius = 1,
        popup = paste("Aves",
                      birds$species,
                      birds$year, 
                      sep = '<br/>'),        
        group = "Aves"
      ) |>
      addCircleMarkers(
        data = reptils,
        clusterOptions = markerClusterOptions(),
        radius = 1,
        popup = paste("Reptiles",
                      reptils$species,
                      reptils$year, 
                      sep = '<br/>'),        
        group = "Reptiles"
      ) |>
      addCircleMarkers(
        data = mammals,
        clusterOptions = markerClusterOptions(),
        radius = 1,
        popup = paste("Mamíferos",
                      mammals$species,
                      mammals$year, 
                      sep = '<br/>'),        
        group = "Mamíferos"
      ) |>
      addCircleMarkers(
        data = amphibians,
        clusterOptions = markerClusterOptions(),
        radius = 1,
        popup = paste("Anfibios",
                      amphibians$species,
                      amphibians$year, 
                      sep = '<br/>'),        
        group = "Anfibios"
      ) |>
      addCircleMarkers(
        data = plants,
        clusterOptions = markerClusterOptions(),
        radius = 1,
        popup = paste("Plantas",
                      plants$species,
                      plants$year, 
                      sep = '<br/>'),        
        group = "Plantas"
      ) |>
      addLayersControl(
        baseGroups = c(
          "Mapa de calles (OpenStreetMap)",
          "Mapa oscuro (CartoDB Dark Matter)",
          "Mapa claro (Stamen Toner Lite)",
          "Imágenes satelitales (ESRI World Imagery)"
        ),
        overlayGroups = c(
          "Límite cantonal",
          "Aves",
          "Reptiles",
          "Mamíferos",
          "Anfibios",
          "Plantas"          
        ),
        position = "topright",
        options = layersControlOptions(collapsed = FALSE)
      ) |>      
      addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE)) |>
      addMouseCoordinates() |>
      addSearchOSM() |>
      addResetMapButton() |>
      addFullscreenControl()
  }


# USER INTERFACE
ui <- fluidPage(
  theme = "bootstrap",
  tags$head(
    tags$style(
      HTML(
        '
        .texto_agradecimiento_logos_1 {
          text-align: center;
        }        
        .texto_agradecimiento_logos_2 {
          text-align: center;
        }'
      )
    )
  ),
  
  navbarPage(
    title = tags$span(
      tags$a(href = "http://atlasverde.net/", target = "_blank", "Atlas de servicios ecosistémicos de la GAM"),
      " - ",
      "Biodiversidad urbana"
    ),
    theme = shinytheme("lumen"),

    fluidRow(withSpinner(leafletOutput("map_biodiversity"))),
    fluidRow(h1(column(width = 12))),
    h3(class = "texto_agradecimiento_logos_1", strong("Variación en la diversidad de especies de flora y fauna nativas o naturalizadas")),
    fluidRow(
      column(width = 2,
        wellPanel(
          style = "background-color: #d9edf7; border-color: #bce8f1;",
          h4(strong("Aves 2023"), style = "margin-top: 0;"),
          h4(strong("696 especies"))
        )
      ),
      column(width = 2,
        wellPanel(
          style = "background-color: #d9edf7; border-color: #bce8f1;",
          h4(strong("Anfibios 2023"), style = "margin-top: 0;"),
          h4(strong("52 especies"))
        )
      ),
      column(width = 2,
        wellPanel(
          style = "background-color: #d9edf7; border-color: #bce8f1;",
          h4(strong("Reptiles 2023"), style = "margin-top: 0;"),
          h4(strong("148 especies"))
        )
      ),
      column(width = 2,
        wellPanel(
          style = "background-color: #d9edf7; border-color: #bce8f1;",
          h4(strong("Mamíferos 2023"), style = "margin-top: 0;"),
          h4(strong("61 especies"))
        )
      ),
      column(width = 2,
        wellPanel(
          style = "background-color: #d9edf7; border-color: #bce8f1;",
          h4(strong("Plantas 2023"), style = "margin-top: 0;"),
          h4(strong("3489 especies"))
        )
      ),
      column(width = 2,
        wellPanel(
          style = "background-color: #d9edf7; border-color: #bce8f1;",
          h4(strong("Total 2023"), style = "margin-top: 0;"),
          h4(strong("4446 especies"))
        )
      ),      
    ),
    fluidRow(
      column(width = 2,
        wellPanel(
          style = "background-color: #d9edf7; border-color: #bce8f1;",
          h4(strong("Aves 2020"), style = "margin-top: 0;"),
          h4(strong("668 especies"))
        )
      ),
      column(width = 2,
        wellPanel(
          style = "background-color: #d9edf7; border-color: #bce8f1;",
          h4(strong("Anfibios 2020"), style = "margin-top: 0;"),
          h4(strong("35 especies"))
        )
      ),
      column(width = 2,
        wellPanel(
          style = "background-color: #d9edf7; border-color: #bce8f1;",
          h4(strong("Reptiles 2020"), style = "margin-top: 0;"),
          h4(strong("47 especies"))
        )
      ),
      column(width = 2,
        wellPanel(
          style = "background-color: #d9edf7; border-color: #bce8f1;",
          h4(strong("Mamíferos 2020"), style = "margin-top: 0;"),
          h4(strong("30 especies"))
        )
      ),
      column(width = 2,
        wellPanel(
          style = "background-color: #d9edf7; border-color: #bce8f1;",
          h4(strong("Plantas 2020"), style = "margin-top: 0;"),
          h4(strong("2966 especies"))
        )
      ),
      column(width = 2,
        wellPanel(
          style = "background-color: #d9edf7; border-color: #bce8f1;",
          h4(strong("Total 2020"), style = "margin-top: 0;"),
          h4(strong("3746 especies"))
        )
      ),      
    ),
    fluidRow(
      column(width = 2,
        wellPanel(
          style = "background-color: #d9edf7; border-color: #bce8f1;",
          h4(strong("Variación en aves"), style = "margin-top: 0;"),
          h4(strong("4.0 %"))
        )
      ),
      column(width = 2,
        wellPanel(
          style = "background-color: #d9edf7; border-color: #bce8f1;",
          h4(strong("Variación en anfibios"), style = "margin-top: 0;"),
          h4(strong("32.7 %"))
        )
      ),
      column(width = 2,
        wellPanel(
          style = "background-color: #d9edf7; border-color: #bce8f1;",
          h4(strong("Variación en reptiles"), style = "margin-top: 0;"),
          h4(strong("68.2 %"))
        )
      ),
      column(width = 2,
        wellPanel(
          style = "background-color: #d9edf7; border-color: #bce8f1;",
          h4(strong("Variación en mamíferos"), style = "margin-top: 0;"),
          h4(strong("50.8 %"))
        )
      ),
      column(width = 2,
        wellPanel(
          style = "background-color: #d9edf7; border-color: #bce8f1;",
          h4(strong("Variación en plantas"), style = "margin-top: 0;"),
          h4(strong("15.0 %"))
        )
      ),
      column(width = 2,
        wellPanel(
          style = "background-color: #d9edf7; border-color: #bce8f1;",
          h4(strong("Variación total"), style = "margin-top: 0;"),
          h4(strong("15.7 %"))
        )
      ),      
    ),    
  ),
  
  
  fluidRow(h1(column(width = 12))),
  fluidRow(h1(column(width = 12))),  
  h4(class = "texto_agradecimiento_logos_1", strong("Acerca del Atlas de Servicios Ecosistémicos de la GAM")),
  h4(class = "texto_agradecimiento_logos-2", "El Atlas de Servicios Ecosistémicos de la GAM es producto de la cooperación entre los Gobiernos de Alemania y Costa Rica en el marco del proyecto Biodiver_City – Establecimiento de Corredores Biológicos Interurbanos con el fin de promover el desarrollo urbano centrado en los beneficios de la naturaleza. El instrumento fue desarrollado por el CATIE, por encargo de la Cooperación alemana para el desarrollo GIZ, bajo una estrecha articulación con el MINAE, CENIGA, SINAC y con el apoyo técnico del Instituto de Estudios Ambientales Helmholtz, UFZ."),
  fluidRow(h1(column(width = 12))),
  fluidRow(
    column(width = 4, img(src = "logo-gcr20222026.png", height = 90)),
    column(width = 4, img(src = "logo-minae.png", height = 90)),
    column(width = 4, img(src = "logo-sinac.jpg", height = 90)),
    class = "text-center"
  ),
  fluidRow(h1(column(width = 12))),
  fluidRow(
    column(width = 4, img(src = "logo-catie.jpeg", height = 90)),
    column(width = 4, img(src = "logo-giz.png", height = 90)),
    column(
      width = 4,
      img(src = "logo-minambientealemania-iki.png", height = 90)
    ),
    class = "text-center"
  ),
  fluidRow(h1(column(width = 12))),
  fluidRow(h1(column(width = 12)))  
)


# SERVER LOGIC
server <- function(input, output, session) {
  # Map
  output$map_biodiversity <- renderLeaflet({
    create_map()
  })
  
  # Initial "selected" canton
  selected_canton <- reactiveVal("San José")  
  
  # Capture click event in cantons layer for zooming and changing styles
  observeEvent(input$map_biodiversity_shape_click, {
    click_data <- input$map_biodiversity_shape_click
    
    if (!is.null(click_data)) {
      selected_canton(click_data$id)
      
      # Zoom to selected polygon
      selected_canton_polygon <- cantons |> filter(canton == click_data$id)
      leafletProxy("map_biodiversity") |>
        fitBounds(
          lng1 = min(st_bbox(selected_canton_polygon)[["xmin"]]),
          lat1 = min(st_bbox(selected_canton_polygon)[["ymin"]]),
          lng2 = max(st_bbox(selected_canton_polygon)[["xmax"]]),
          lat2 = max(st_bbox(selected_canton_polygon)[["ymax"]])
        )      
    }
  })  
  
}


# RUN APPLICATION
shinyApp(ui = ui, server = server)