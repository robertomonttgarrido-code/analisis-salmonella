options(encoding = "UTF-8")

library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(leaflet)
library(DT)
library(htmltools)

# ============================================================
# 1. CARGA DE DATOS
# ============================================================

consolidado_salmonella <- readxl::read_excel(
  "Consolidado salmonella v.shiny.xlsx",
  sheet = "Hoja1"
)

sectores_cerdos <- readxl::read_excel(
  "Sectores Cerdos.xlsx",
  sheet = "Sectores Cerdos"
)

# ============================================================
# 2. RENOMBRAR COLUMNAS PARA EVITAR PROBLEMAS DE ENCODING
# ============================================================

names(consolidado_salmonella) <- c(
  "id",
  "centro_productivo",
  "sector_st",
  "n_pabellones",
  "fecha_muestreo",
  "n_muestreos",
  "n_resultados_positivos",
  "pct_muestreos_positivos",
  "lat",
  "long"
)

names(sectores_cerdos) <- c(
  "sector",
  "etapa",
  "longitude",
  "latitude",
  "longitud",
  "latitud"
)

# ============================================================
# 3. LIMPIEZA DE DATOS
# ============================================================

consolidado_salmonella <- consolidado_salmonella %>%
  mutate(
    fecha_muestreo = as.Date(fecha_muestreo),
    n_muestreos = as.numeric(n_muestreos),
    n_resultados_positivos = as.numeric(n_resultados_positivos),
    lat = as.numeric(lat),
    long = as.numeric(long),
    centro_productivo = as.character(centro_productivo),
    sector_st = as.character(sector_st)
  ) %>%
  filter(
    !is.na(fecha_muestreo),
    !is.na(lat),
    !is.na(long)
  )

sectores_cerdos <- sectores_cerdos %>%
  mutate(
    sector = as.character(sector),
    etapa = as.character(etapa),
    longitud = as.numeric(longitud),
    latitud = as.numeric(latitud)
  ) %>%
  filter(
    !is.na(longitud),
    !is.na(latitud)
  )

# ============================================================
# 4. PARAMETROS GENERALES
# ============================================================

paleta_riesgo <- leaflet::colorNumeric(
  palette = c("#1a9850", "#91cf60", "#ffffbf", "#fc8d59", "#d73027"),
  domain = c(0, 1),
  na.color = "#808080"
)

legend_df <- data.frame(prevalencia_periodo = c(0, 1))

fecha_min <- min(consolidado_salmonella$fecha_muestreo, na.rm = TRUE)
fecha_max <- max(consolidado_salmonella$fecha_muestreo, na.rm = TRUE)

centros_disponibles <- sort(unique(consolidado_salmonella$centro_productivo))

# ============================================================
# 5. UI
# ============================================================

ui <- page_navbar(
  title = HTML("Analisis <i>Salmonella sp</i>"),
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  sidebar = sidebar(
    title = "Controles",
    
    sliderInput(
      inputId = "rango_fechas",
      label = "Fecha de muestreo:",
      min = fecha_min,
      max = fecha_max,
      value = c(fecha_min, fecha_max),
      timeFormat = "%b %Y",
      step = 30,
      animate = TRUE
    ),
    
    checkboxGroupInput(
      inputId = "etapa",
      label = "Centro Productivo:",
      choices = centros_disponibles,
      selected = centros_disponibles
    ),
    
    hr(),
    
    card(
      card_header("Resumen dinamico"),
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
    "Tabla de registros",
    card(
      DT::dataTableOutput("tabla_datos")
    )
  )
)

# ============================================================
# 6. SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ------------------------------------------------------------
  # Datos filtrados
  # ------------------------------------------------------------
  
  datos_filtrados <- reactive({
    req(input$rango_fechas)
    req(input$etapa)
    
    consolidado_salmonella %>%
      filter(
        fecha_muestreo >= input$rango_fechas[1],
        fecha_muestreo <= input$rango_fechas[2],
        centro_productivo %in% input$etapa
      )
  })
  
  # ------------------------------------------------------------
  # Mapa base
  # ------------------------------------------------------------
  
  output$mapa_cv <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      
      addCircles(
        data = sectores_cerdos,
        lng = ~longitud,
        lat = ~latitud,
        radius = 120,
        color = "#BF3EFF",
        fillColor = "#BF3EFF",
        opacity = 0.9,
        fillOpacity = 0.6,
        stroke = TRUE,
        group = "Cerdos AS",
        popup = ~paste0(
          "<strong>Sector cerdo:</strong> ", sector,
          "<br><strong>Etapa:</strong> ", etapa
        ),
        label = ~paste0(sector, " - ", etapa)
      ) %>%
      
      addLegend(
        data = legend_df,
        pal = paleta_riesgo,
        values = ~prevalencia_periodo,
        opacity = 0.7,
        title = "Prevalencia (%)",
        position = "bottomright",
        labFormat = leaflet::labelFormat(
          suffix = "%",
          transform = function(x) {
            100 * x
          }
        )
      ) %>%
      
      addLayersControl(
        overlayGroups = c("Sectores AS", "Cerdos AS"),
        options = layersControlOptions(collapsed = FALSE),
        position = "topright"
      ) %>%
      
      hideGroup("Cerdos AS") %>%
      
      addScaleBar(
        position = "bottomleft",
        options = scaleBarOptions()
      )
  })
  
  # ------------------------------------------------------------
  # Actualizacion dinamica del mapa
  # ------------------------------------------------------------
  
  observe({
    df <- datos_filtrados()
    
    proxy <- leafletProxy("mapa_cv")
    
    if (nrow(df) == 0) {
      proxy %>%
        clearGroup("Sectores AS")
      return()
    }
    
    df_agrupado <- df %>%
      group_by(sector_st, lat, long, centro_productivo) %>%
      summarise(
        suma_positivos = sum(n_resultados_positivos, na.rm = TRUE),
        suma_muestreos = sum(n_muestreos, na.rm = TRUE),
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
          "<br><strong>Centro Productivo:</strong> ", centro_productivo,
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
  
  # ------------------------------------------------------------
  # Tabla de datos
  # ------------------------------------------------------------
  
  output$tabla_datos <- DT::renderDataTable({
    datos_filtrados()
  },
  options = list(
    pageLength = 10,
    scrollX = TRUE,
    autoWidth = TRUE
  ))
  
  # ------------------------------------------------------------
  # Resumen lateral
  # ------------------------------------------------------------
  
  output$conteo_total <- renderText({
    total_v <- sum(datos_filtrados()$n_muestreos, na.rm = TRUE)
    paste("Total de muestreos:", format(total_v, big.mark = ","))
  })
  
  output$conteo_pos <- renderText({
    positivos_v <- sum(datos_filtrados()$n_resultados_positivos, na.rm = TRUE)
    paste("Resultados positivos:", format(positivos_v, big.mark = ","))
  })
  
  output$conteo_neg <- renderText({
    total_v <- sum(datos_filtrados()$n_muestreos, na.rm = TRUE)
    positivos_v <- sum(datos_filtrados()$n_resultados_positivos, na.rm = TRUE)
    paste("Resultados negativos:", format(total_v - positivos_v, big.mark = ","))
  })
  
  output$prevalencia_global <- renderText({
    total_v <- sum(datos_filtrados()$n_muestreos, na.rm = TRUE)
    positivos_v <- sum(datos_filtrados()$n_resultados_positivos, na.rm = TRUE)
    
    prevalencia <- ifelse(total_v > 0, positivos_v / total_v, 0)
    
    paste("Prevalencia global:", round(prevalencia * 100, 2), "%")
  })
}

# ============================================================
# 7. EJECUTAR APP
# ============================================================

shinyApp(ui, server)
