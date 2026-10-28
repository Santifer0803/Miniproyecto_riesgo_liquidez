# Librerías necesarias
pacman::p_load(janitor, knitr, plotly, readxl, shiny, tidyverse, here, DT)

# Datos necesarios
ruta_datos <- here("data", "miniLiquidez_DATOS.xlsx")

datos_contables <- read_excel(ruta_datos, sheet = "Datos_Contables")
datos_contrato <- read_excel(ruta_datos, sheet = "Pagos_Contractuales")
datos_observados <- read_excel(ruta_datos, sheet = "Observaciones")
datos_tc <- read_excel(ruta_datos, sheet = "Tipo_Cambio")
datos_catalogo <- read_excel(ruta_datos, sheet = "Catalogo_Usado")

# Se limpia el nombre de las variables
datos_contables <- clean_names(datos_contables)
datos_contrato <- clean_names(datos_contrato)
datos_observados <- clean_names(datos_observados)
datos_tc <- clean_names(datos_tc)
datos_catalogo <- clean_names(datos_catalogo)

# Variables tipo factor de datos del catálogo
datos_catalogo$cuenta_contable <- as.factor(datos_catalogo$cuenta_contable)

# Variables tipo factor de datos contables
datos_contables$c_entidad <- as.factor(datos_contables$c_entidad)
datos_contables$alias <- as.factor(datos_contables$alias)
datos_contables$moneda <- as.factor(datos_contables$moneda)
datos_contables$cuenta_contable <- as.factor(datos_contables$cuenta_contable)
datos_contables$nombre_cuenta_catalogo <- as.factor(datos_contables$nombre_cuenta_catalogo)
datos_contables$sector_contraparte <- as.factor(datos_contables$sector_contraparte)
datos_contables$relacion_comercial_estable <- as.factor(datos_contables$relacion_comercial_estable)
datos_contables$retiro_anticipado_sin_penalidad <- as.factor(datos_contables$retiro_anticipado_sin_penalidad)
datos_contables$calificacion_internacional <- as.factor(datos_contables$calificacion_internacional)
datos_contables$mercado_activo <- as.factor(datos_contables$mercado_activo)
datos_contables$negociacion_disponible <- as.factor(datos_contables$negociacion_disponible)
datos_contables$ingreso_cliente_misma_moneda <- as.factor(datos_contables$ingreso_cliente_misma_moneda)
datos_contables$destino_credito <- as.factor(datos_contables$destino_credito)

# Variables tipo factor de datos observados
datos_observados$c_entidad <- as.factor(datos_observados$c_entidad)
datos_observados$alias <- as.factor(datos_observados$alias)
datos_observados$moneda <- as.factor(datos_observados$moneda)

# Variables tipo factor de datos del tipo de cambio
datos_tc$moneda <- as.factor(datos_tc$moneda)

# Se renombran los nombres de las entidades
levels(datos_contables$alias) <- c("A", "B", "C", "D", "E")
levels(datos_observados$alias) <- c("A", "B", "C", "D", "E")

# Se juntan los datos contables con los pagos de contrato
datos_contables <- datos_contables |>
  left_join(datos_contrato |>
              select(-c("id_pago", "periodo")), "id_registro")

# Se añade el tipo de cambio correspondiente a cada periodo
datos_contables <- datos_contables |>
  left_join(datos_tc, c("periodo", "moneda"))

# Se añade el tipo de cambio a los datos observados
datos_observados <- datos_observados |>
  left_join(datos_tc, c("periodo", "moneda"))

# Parte 2.1, 2.2, 2.3 y 2.4 del miniproyecto de riesgo de liquidez
source(here("R", "liquidez_funciones.R"), encoding = "UTF-8")

modelo <- crear_modelo_liquidez(datos_contables,
                                datos_contrato,
                                datos_observados,
                                datos_catalogo)

# Las líneas contingentes se conservan con sus saldos originales en modelo.
# Para el punto 4 se usa una copia donde su importe no aporta recursos de cobertura.
# De esta manera no se modifican las funciones del módulo 4 ni las posiciones originales.
modelo_para_cobertura <- modelo
modelo_para_cobertura$saldos <- modelo_para_cobertura$saldos |>
  dplyr::mutate(
    saldo_contable = dplyr::if_else(
      clase_contable == "Contingente" |
        rubro_liquidez == "Líneas concedidas no utilizadas",
      0,
      saldo_contable
    )
  )

coberturas <- cobertura_faltantes(modelo_para_cobertura)

# Las columnas de líneas del módulo 4 son meramente auxiliares.
# No deben mostrar recursos ni remanentes, pues no participan en la cobertura.
coberturas$bandas <- coberturas$bandas |>
  dplyr::mutate(
    lineas_cred = 0,
    lineas_cred_rem = 0
  )

# Se agrega el detalle de las bandas al modelo
modelo$bandas <- coberturas$bandas

# Activos liquidados por entidad
act_liquidados <- coberturas$act_liquidados

# Formato de los montos para tablas
formato_monto <- function(x) {
  if (length(x) == 0 || is.na(x))
    return("No disponible")
  paste0(format(
    round(x / 1e6, 2),
    big.mark = ".",
    decimal.mark = ",",
    trim = TRUE,
    nsmall = 2
  ),
  " millones")
}

# Función para crear tablas interactivas
tabla_interactiva <- function(datos, pagina = 12) {
  DT::datatable(
    datos,
    rownames = FALSE,
    filter = "top",
    options = list(
      pageLength = pagina,
      scrollX = TRUE,
      autoWidth = TRUE,
      dom = "ftip"
    )
  )
}

# Diseño de la aplicación
ui <- fluidPage(
  tags$head(tags$style(HTML(
    paste0(
      "body { background-color: #f7f9fc; color: #1d2939; }",
      ".well { background-color: white; border: 1px solid #e3e8ef; }",
      ".metric { padding: 12px; background-color: white; border: 1px solid #e3e8ef; ",
      "border-radius: 6px; min-height: 94px; }",
      ".metric b { display: block; color: #526071; font-size: 12px; }",
      ".metric span { font-size: 18px; font-weight: 600; }"
    )
  ))),
  titlePanel("Riesgo de liquidez | Módulos 2.1 a 2.3"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("entidad", "Entidad", choices = sort(unique(
        modelo$saldos$alias
      ))),
      selectInput("corte", "Fecha de corte", choices = format(sort(
        unique(modelo$saldos$periodo)
      ), "%Y-%m-%d")),
      selectInput("moneda", "Moneda", choices = c("MN", "ME")),
      selectInput(
        "escenario",
        "Vista de flujos",
        choices = c("Contractual", "Ajustado por comportamiento")
      ),
      helpText("MN: colones; ME: dólares. Montos sin conversión cruzada."),
      helpText(
        "La vista ajustada usa observaciones del corte. No es un escenario de tensión regulatoria."
      ),
      hr(),
      downloadButton("descargar_bandas", "Descargar brechas (CSV)"),
      br(),
      br(),
      downloadButton("descargar_rubros", "Descargar rubros (CSV)"),
      br(),
      br(),
      downloadButton("descargar_saldos", "Descargar saldos (CSV)"),
      br(),
      br(),
      downloadButton("descargar_maestra", "Descargar tabla maestra (CSV)")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "Resumen 2.1",
          br(),
          fluidRow(column(4, div(
            class = "metric", tags$b("Activos contables"), span(textOutput("kpi_activos", inline = TRUE))
          )), column(4, div(
            class = "metric", tags$b("Pasivos contables"), span(textOutput("kpi_pasivos", inline = TRUE))
          )), column(4, div(
            class = "metric", tags$b("Brecha acumulada a 30 días"), span(textOutput("kpi_30", inline = TRUE))
          ))),
          br(),
          strong(textOutput("alerta")),
          p(
            "La brecha es anterior al uso de caja, encaje, venta de inversiones y otras medidas de cobertura."
          ),
          plotlyOutput("grafico_brecha", height = "290px"),
          h4("Brechas por plazo"),
          DTOutput("tabla_bandas"),
          h4("Comparación entre entidades (misma moneda y corte)"),
          DTOutput("tabla_comparativo")
        ),
        tabPanel(
          "Estructura 2.2",
          br(),
          h4("Composición de activos y pasivos"),
          plotlyOutput("grafico_estructura", height = "450px"),
          h4("Detalle del catálogo y disponibilidad"),
          DTOutput("tabla_estructura"),
          h4("Concentración de vencimientos de posiciones contables"),
          DTOutput("tabla_vencimientos"),
          p(
            "Los vencimientos de saldos no son flujos sumables: el calendario de pagos se analiza en la siguiente pestaña."
          )
        ),
        tabPanel(
          "Flujos y brechas 2.3",
          br(),
          plotlyOutput("grafico_flujos", height = "340px"),
          h4("Detalle por cuenta contable y banda"),
          DTOutput("tabla_rubros"),
          h4("Cobros vencidos y no confirmados"),
          DTOutput("tabla_atrasos"),
          h4("Supuestos históricos del corte"),
          DTOutput("tabla_tasas"),
          textOutput("advertencia_tasas"),
          h4("Trazabilidad de pagos y ajustes"),
          DTOutput("tabla_flujos_detallados")
        ),
        tabPanel(
          "Tabla maestra",
          br(),
          p(
            "Una fila por entidad, corte, moneda y cuenta contable. El saldo no se repite por banda; las columnas de flujos tienen sufijos d1_8, d9_15, d16_30, d31_89, d90_180 y d181_."
          ),
          DTOutput("tabla_maestra")
        ),
        tabPanel(
          "Metodología y controles",
          br(),
          h4("Catálogo clasificado"),
          DTOutput("tabla_catalogo"),
          h4("Verificaciones de integridad"),
          DTOutput("tabla_controles"),
          h4("Reglas de interpretación"),
          tags$ul(
            tags$li(
              "Clases contables: activo (1), pasivo (2), patrimonio (3), contingente (6)."
            ),
            tags$li("Bandas: 1–8, 9–15, 16–30, 31–89, 90–180 y 181+ días."),
            tags$li(
              "Pasivos vencidos: primera banda; cobros vencidos: identificados aparte, no reconocidos como cobrados."
            ),
            tags$li(
              "El escenario ajustado usa tasas históricas del corte solamente hasta 30 días."
            ),
            tags$li(
              "Líneas concedidas: solo registro contingente, sin salidas ni cobertura; líneas recibidas: recurso potencial, no entrada contractual."
            ),
            tags$li(
              "Cuentas sin fecha, patrimonio, caja y encaje: no se inventan flujos contractuales."
            ),
            tags$li(
              "No se mezclan fechas de corte, monedas ni saldos contables con movimientos de efectivo."
            )
          ),
          p(
            "Fuente de las clases: CONASSIF 6-18, artículo 26. Los tratamientos de liquidez son supuestos académicos y no equivalen al ICL/IFNE."
          ),
          tags$a(href = "https://www.sugef.fi.cr/normativa/NormativaTransversal.aspx", target = "_blank", "Reglamento y anexos SUGEF")
        )
      )
    )
  )
)

# Servidor que define, internamente, qué hace la aplicación
server <- function(input, output, session) {
  filtros <- reactive({
    list(
      alias = input$entidad,
      periodo = as.Date(input$corte),
      moneda = input$moneda
    )
  })
  filtrar <- function(tabla, f) {
    tabla |>
      dplyr::filter(alias == f$alias, periodo == f$periodo, moneda == f$moneda)
  }
  saldos_actuales <- reactive(filtrar(modelo$saldos, filtros()))
  estructura_actual <- reactive(filtrar(modelo$estructura, filtros()))
  bandas_actuales <- reactive(filtrar(modelo$bandas, filtros()))
  rubros_actuales <- reactive(filtrar(modelo$rubros, filtros()))
  vencimientos_actuales <- reactive(filtrar(modelo$vencimientos, filtros()))
  maestra_actual <- reactive(filtrar(modelo$tabla_maestra, filtros()))
  tasas_actuales <- reactive(filtrar(modelo$tasas, filtros()))
  flujos_actuales <- reactive(filtrar(modelo$flujos, filtros()))
  columnas_vista <- reactive({
    if (input$escenario == "Contractual") {
      c(
        "entradas_contrato",
        "salidas_contrato",
        "brecha_contrato",
        "acumulada_contrato"
      )
    } else {
      c(
        "entradas_ajustadas",
        "salidas_ajustadas",
        "brecha_ajustada",
        "acumulada_ajustada"
      )
    }
  })
  bandas_vista <- reactive({
    d <- bandas_actuales()
    nombres <- columnas_vista()
    dplyr::transmute(
      d,
      banda = as.character(banda),
      entradas = .data[[nombres[1]]],
      salidas = .data[[nombres[2]]],
      brecha = .data[[nombres[3]]],
      acumulada = .data[[nombres[4]]]
    )
  })
  
  output$kpi_activos <- renderText({
    formato_monto(sum(saldos_actuales()$saldo_contable[saldos_actuales()$clase_contable == "Activo"], na.rm = TRUE))
  })
  output$kpi_pasivos <- renderText({
    formato_monto(sum(saldos_actuales()$saldo_contable[saldos_actuales()$clase_contable == "Pasivo"], na.rm = TRUE))
  })
  output$kpi_30 <- renderText({
    d <- bandas_vista()
    formato_monto(d$acumulada[which(d$banda == "16-30")])
  })
  output$alerta <- renderText({
    d <- bandas_vista()
    primer <- which(d$acumulada < 0)[1]
    if (is.na(primer)) {
      "No se observa brecha acumulada negativa en las seis bandas seleccionadas."
    } else {
      paste0(
        "Primer faltante acumulado en la banda ",
        d$banda[primer],
        " días: ",
        formato_monto(-d$acumulada[primer]),
        " de déficit."
      )
    }
  })
  
  output$grafico_brecha <- renderPlotly({
    d <- bandas_vista()
    
    plotly::plot_ly(
      data = d,
      x = ~ banda,
      y = ~ acumulada / 1e6,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "#155b75"),
      name = "Brecha acumulada"
    ) |>
      plotly::layout(
        xaxis = list(
          title = "Banda (días)",
          categoryorder = "array",
          categoryarray = bandas_liquidez
        ),
        yaxis = list(title = "Millones de la moneda seleccionada"),
        shapes = list(list(
          type = "line",
          x0 = -0.5,
          x1 = 5.5,
          y0 = 0,
          y1 = 0,
          line = list(dash = "dot")
        ))
      )
  })
  output$tabla_bandas <- DT::renderDT({
    d <- bandas_actuales() |>
      dplyr::select(
        banda,
        entradas_contrato,
        salidas_contrato,
        brecha_contrato,
        acumulada_contrato,
        entradas_ajustadas,
        salidas_ajustadas,
        brecha_ajustada,
        acumulada_ajustada
      )
    tabla_interactiva(d, 6)
  })
  output$tabla_comparativo <- DT::renderDT({
    f <- filtros()
    compare <- modelo$bandas |>
      dplyr::filter(periodo == f$periodo,
                    moneda == f$moneda,
                    as.character(banda) == "16-30") |>
      dplyr::select(alias, acumulada_contrato, acumulada_ajustada) |>
      dplyr::left_join(
        modelo$saldos |>
          dplyr::filter(
            periodo == f$periodo,
            moneda == f$moneda,
            clase_contable == "Activo"
          ) |>
          dplyr::group_by(alias) |>
          dplyr::summarise(activos = sum(saldo_contable), .groups = "drop"),
        by = "alias"
      ) |>
      dplyr::mutate(
        brecha_30_sobre_activos_pct =
          dplyr::if_else(activos > 0, 100 * acumulada_ajustada / activos, NA_real_)
      )
    tabla_interactiva(compare, 5)
  })
  
  output$grafico_estructura <- renderPlotly({
    d <- estructura_actual() |>
      dplyr::filter(clase_contable %in% c("Activo", "Pasivo")) |>
      dplyr::group_by(clase_contable, rubro_liquidez) |>
      dplyr::summarise(saldo = sum(saldo), .groups = "drop")
    p <- ggplot2::ggplot(d,
                         ggplot2::aes(
                           x = reorder(rubro_liquidez, saldo),
                           y = saldo / 1e6,
                           fill = clase_contable,
                           text = paste0(rubro_liquidez, ": ", round(saldo / 1e6, 2), " millones")
                         )) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::coord_flip() +
      ggplot2::facet_wrap(~ clase_contable, scales = "free_y") +
      ggplot2::labs(x = "Rubro", y = "Millones de la moneda seleccionada") +
      ggplot2::theme_minimal()
    plotly::ggplotly(p, tooltip = "text")
  })
  output$tabla_estructura <- DT::renderDT({
    tabla_interactiva(
      estructura_actual() |>
        dplyr::select(
          clase_contable,
          cuenta_contable,
          denominacion_oficial,
          rubro_liquidez,
          disponibilidad,
          saldo,
          operaciones
        )
    )
  })
  output$tabla_vencimientos <- DT::renderDT({
    tabla_interactiva(
      vencimientos_actuales() |>
        dplyr::select(
          clase_contable,
          cuenta_contable,
          rubro_liquidez,
          banda_vencimiento,
          disponibilidad,
          saldo,
          operaciones
        )
    )
  })
  
  output$grafico_flujos <- renderPlotly({
    d <- bandas_vista()
    plotly::plot_ly(
      data = d,
      x = ~ banda,
      y = ~ entradas / 1e6,
      type = "bar",
      name = "Entradas",
      marker = list(color = "#247f79")
    ) |>
      plotly::add_trace(
        y = ~ salidas / 1e6,
        name = "Salidas",
        marker = list(color = "#c07c43")
      ) |>
      plotly::layout(
        barmode = "group",
        xaxis = list(
          title = "Banda (días)",
          categoryorder = "array",
          categoryarray = bandas_liquidez
        ),
        yaxis = list(title = "Millones")
      )
  })
  output$tabla_rubros <- DT::renderDT({
    tabla_interactiva(
      rubros_actuales() |>
        dplyr::select(
          banda,
          cuenta_contable,
          denominacion_oficial,
          rubro_liquidez,
          sentido,
          contractual,
          ajustado
        ),
      15
    )
  })
  output$tabla_atrasos <- DT::renderDT({
    tabla_interactiva(
      flujos_actuales() |>
        dplyr::filter(estado_plazo == "Cobro vencido/no confirmado") |>
        dplyr::select(
          id_registro,
          cuenta_contable,
          fecha_pago_contractual,
          monto_capital,
          monto_interes,
          nota_ajuste
        )
    )
  })
  output$tabla_tasas <- DT::renderDT({
    tabla_interactiva(
      tasas_actuales() |>
        dplyr::select(
          tasa_retiro,
          tasa_renovacion,
          tasa_cobro,
          tasa_uso_lineas,
          linea_recibida_no_dispuesta,
          fecha_disponible_linea_recibida,
          fecha_fin_linea_recibida
        ),
      5
    )
  })
  output$tabla_maestra <- DT::renderDT({
    tabla_interactiva(maestra_actual(), 10)
  })
  output$advertencia_tasas <- renderText({
    t <- tasas_actuales()
    cols <- c("tasa_retiro",
              "tasa_renovacion",
              "tasa_cobro",
              "tasa_uso_lineas")
    if (anyNA(t[, cols])) {
      "Advertencia: hay tasas históricas no observables. Revisar el detalle antes de interpretar el ajuste."
    } else
      "Las cuatro tasas históricas están disponibles para este corte."
  })
  output$tabla_flujos_detallados <- DT::renderDT({
    tabla_interactiva(flujos_actuales(), 10)
  })
  output$tabla_catalogo <- DT::renderDT({
    tabla_interactiva(
      modelo$catalogo |>
        dplyr::select(
          cuenta_contable,
          denominacion_oficial,
          clase_contable,
          rubro_liquidez,
          tratamiento_general
        ),
      10
    )
  })
  output$tabla_controles <- DT::renderDT(tabla_interactiva(modelo$verificaciones, 5))
  
  exportar_csv <- function(datos, archivo) {
    readr::write_excel_csv(datos, archivo, na = "")
  }
  output$descargar_bandas <- downloadHandler(
    filename = function()
      paste0(
        "brechas_",
        input$entidad,
        "_",
        input$corte,
        "_",
        input$moneda,
        ".csv"
      ),
    content = function(file)
      exportar_csv(bandas_actuales(), file)
  )
  output$descargar_rubros <- downloadHandler(
    filename = function()
      paste0(
        "rubros_",
        input$entidad,
        "_",
        input$corte,
        "_",
        input$moneda,
        ".csv"
      ),
    content = function(file)
      exportar_csv(rubros_actuales(), file)
  )
  output$descargar_maestra <- downloadHandler(
    filename = function()
      paste0(
        "tabla_maestra_",
        input$entidad,
        "_",
        input$corte,
        "_",
        input$moneda,
        ".csv"
      ),
    content = function(file)
      exportar_csv(maestra_actual(), file)
  )
  output$descargar_saldos <- downloadHandler(
    filename = function()
      paste0(
        "saldos_",
        input$entidad,
        "_",
        input$corte,
        "_",
        input$moneda,
        ".csv"
      ),
    content = function(file)
      exportar_csv(estructura_actual(), file)
  )
}

shinyApp(ui = ui, server = server)
