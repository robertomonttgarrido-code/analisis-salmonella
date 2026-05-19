options(encoding = "UTF-8")

library(shiny)
library(bslib)
library(dplyr)
library(sf)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(ggplot2)
library(DT)
library(htmltools)
library(htmlwidgets)
library(lubridate)

Consolidado_salmonella_v_shiny <- Consolidado_salmonella_v_shiny %>%
  mutate(
    `Fecha Muestreo` = as.Date(`Fecha Muestreo`),
    `N° muestreos` = as.numeric(`N° muestreos`),
    `N° resultados positivos` = as.numeric(`N° resultados positivos`),
    lat = as.numeric(lat),
    long = as.numeric(long),
    `Centro Productivo` = as.character(`Centro Productivo`),
    sector_st = as.character(sector_st)
  ) %>%
  filter(
    !is.na(`Fecha Muestreo`),
    !is.na(lat),
    !is.na(long)
  )

Sectores_Cerdos <- Sectores_Cerdos %>%
  mutate(
    Sector = as.character(Sector),
    Etapa = as.character(Etapa),
    Longitud = as.numeric(Longitud),
    Latitud = as.numeric(Latitud)
  ) %>%
  filter(
    !is.na(Longitud),
    !is.na(Latitud)
  )

paleta_riesgo <- colorNumeric(
  palette = c("#1a9850", "#91cf60", "#ffffbf", "#fc8d59", "#d73027"),
  domain = c(0, 1),
  na.color = "#808080"
)


legend_df <- data.frame(prevalencia_periodo = c(0, 1))

# Valores dinámicos para filtros
fecha_min <- min(Consolidado_salmonella_v_shiny$`Fecha Muestreo`, na.rm = TRUE)
fecha_max <- max(Consolidado_salmonella_v_shiny$`Fecha Muestreo`, na.rm = TRUE)

centros_disponibles <- sort(unique(Consolidado_salmonella_v_shiny$`Centro Productivo`))


ui <- page_navbar(
  title = HTML("Análisis <i>Salmonella sp</i>"),
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  sidebar = sidebar(
    title = "Controles",
    
    sliderInput(
      "rango_fechas",
      "Fecha de muestreo:",
      min = fecha_min,
      max = fecha_max,
      value = c(fecha_min, fecha_max),
      timeFormat = "%b %Y",
      step = 30,
      animate = TRUE
    ),
    
    checkboxGroupInput(
      "etapa",
      "Centro Productivo:",
      choices = centros_disponibles,
      selected = centros_disponibles
    ),
    
    hr(),
    
    card(
      card_header("Resumen Dinámico"),
      textOutput("conteo_total"),
      span(textOutput("conteo_pos"), style = "color: red; font-weight: bold;"),
      span(textOutput("conteo_neg"), style = "color: green; font-weight: bold;"),
      span(textOutput("prevalencia_global"), style = "color: #444; font-weight: bold;")
    )
  ),
  
  nav_panel(
    "Mapa de prevalencia",
    leafletOutput("mapa_cv", height = "750px")
  ),
  
  nav_panel(
    "Tabla de Registros",
    card(
      DT::dataTableOutput("tabla_datos")
    )
  )
)

server <- function(input, output, session) {
  
  
  datos_filtrados <- reactive({
    req(input$rango_fechas)
    req(input$etapa)
    
    Consolidado_salmonella_v_shiny %>%
      filter(
        `Fecha Muestreo` >= input$rango_fechas[1],
        `Fecha Muestreo` <= input$rango_fechas[2],
        `Centro Productivo` %in% input$etapa
      )
  })
  
  
  output$mapa_cv <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      
      addCircles(
        data = Sectores_Cerdos,
        lng = ~Longitud,
        lat = ~Latitud,
        radius = 120,
        color = "#BF3EFF",
        fillColor = "#BF3EFF",
        opacity = 0.9,
        fillOpacity = 0.6,
        stroke = TRUE,
        group = "Cerdos AS",
        popup = ~paste0(
          "<strong>Sector cerdo:</strong> ", Sector,
          "<br><strong>Etapa:</strong> ", Etapa
        ),
        label = ~paste0(Sector, " - ", Etapa)
      ) %>%
      
      addLegend(
        data = legend_df,
        pal = paleta_riesgo,
        values = ~prevalencia_periodo,
        opacity = 0.7,
        title = "Prevalencia (%)",
        position = "bottomright",
        labFormat = labelFormat(
          suffix = "%",
          transform = function(x) 100 * x
        )
      ) %>%
      
      addLayersControl(
        overlayGroups = c("Sectores AS", "Cerdos AS"),
        options = layersControlOptions(collapsed = FALSE),
        position = "topright"
      ) %>%
      
      hideGroup("Cerdos AS") %>%
      
      addControl(
        html = paste0(
          '<div style="background: white; padding: 10px; border: 1px solid #aaa;">',
          '<img src="https://i.imgur.com/cFIO3uG.png" style="width: 100px; margin-bottom: 5px;"><br>',
          '<img src="https://i.imgur.com/TFghkGX.jpeg" style="width: 100px;"><br>',
          '</div>'
        ),
        position = "topleft"
      ) %>%
      
      addScaleBar(
        position = "bottomleft",
        options = scaleBarOptions()
      )
  })
  
  observe({
    df <- datos_filtrados()
    
    proxy <- leafletProxy("mapa_cv")
    
    if (nrow(df) == 0) {
      proxy %>%
        clearGroup("Sectores AS")
      return()
    }
    
    df_agrupado <- df %>%
      group_by(sector_st, lat, long, `Centro Productivo`) %>%
      summarise(
        suma_positivos = sum(`N° resultados positivos`, na.rm = TRUE),
        suma_muestreos = sum(`N° muestreos`, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        prevalencia_periodo = ifelse(
          suma_muestreos > 0,
          suma_positivos / suma_muestreos,
          0
        )
      )
    
    proxy %>%
      clearGroup("Sectores AS") %>%
      addCircles(
        data = df_agrupado,
        lng = ~long,
        lat = ~lat,
        radius = 120,
        color = ~paleta_riesgo(prevalencia_periodo),
        fillColor = ~paleta_riesgo(prevalencia_periodo),
        opacity = 0.9,
        fillOpacity = 0.6,
        group = "Sectores AS",
        popup = ~paste0(
          "<strong>Sector:</strong> ", sector_st,
          "<br><strong>Centro Productivo:</strong> ", `Centro Productivo`,
          "<br><strong>Positivos periodo:</strong> ", suma_positivos,
          "<br><strong>Muestreos periodo:</strong> ", suma_muestreos,
          "<br><strong>Prevalencia periodo:</strong> ",
          round(prevalencia_periodo * 100, 2), "%"
        ),
        label = ~paste0(
          "Sector: ", sector_st,
          " | Prevalencia: ", round(prevalencia_periodo * 100, 1), "%"
        )
      )
  })
  
  
  output$tabla_datos <- DT::renderDataTable({
    datos_filtrados()
  },
  options = list(
    pageLength = 10,
    scrollX = TRUE,
    autoWidth = TRUE
  ))
  
  
  output$conteo_total <- renderText({
    total_v <- sum(datos_filtrados()$`N° muestreos`, na.rm = TRUE)
    paste("Total de muestreos:", format(total_v, big.mark = ","))
  })
  
  output$conteo_pos <- renderText({
    positivos_v <- sum(datos_filtrados()$`N° resultados positivos`, na.rm = TRUE)
    paste("N° resultados positivos:", format(positivos_v, big.mark = ","))
  })
  
  output$conteo_neg <- renderText({
    total_v <- sum(datos_filtrados()$`N° muestreos`, na.rm = TRUE)
    positivos_v <- sum(datos_filtrados()$`N° resultados positivos`, na.rm = TRUE)
    paste("N° resultados negativos:", format(total_v - positivos_v, big.mark = ","))
  })
  
  output$prevalencia_global <- renderText({
    total_v <- sum(datos_filtrados()$`N° muestreos`, na.rm = TRUE)
    positivos_v <- sum(datos_filtrados()$`N° resultados positivos`, na.rm = TRUE)
    
    prevalencia <- ifelse(total_v > 0, positivos_v / total_v, 0)
    
    paste("Prevalencia global:", round(prevalencia * 100, 2), "%")
  })
}

shinyApp(ui, server)
