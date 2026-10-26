# Aplicación Shiny de riesgo de liquidez, apartados 2.1 a 2.3.
# Este archivo tiene tres responsabilidades diferentes:
# 1) Datos y preparación: lectura de Excel y transformaciones originales del equipo.
# 2) Presentación (ui): diseño, filtros, tablas, gráficas y enlaces de descarga.
# 3) Servidor (server): reacción a filtros y entrega de tablas/gráficos.
# Los cálculos financieros reutilizables viven en R/liquidez_funciones.R.
# La ruta a los archivos usa la raíz del proyecto (here), no rutas personales.
# La vista Contractual/Ajustada selecciona resultados YA calculados; no vuelve a
# estimar las tasas ni constituye un escenario regulatorio de estrés.

# Preparación | pacman::p_load carga paquetes y, si falta alguno, intenta instalarlo.
# Librerias necesarias
# janitor: clean_names; knitr: informes; plotly: gráficos; readxl: Excel;
pacman::p_load(janitor,
               knitr,
               plotly,
               readxl,
# shiny: UI/servidor; tidyverse: dplyr/ggplot2/readr; here: rutas; DT: tablas.
               shiny,
               tidyverse,
               here,
               DT)

# Datos | El archivo contiene cinco hojas indispensables para el modelo.
# Se lee una sola vez cuando la aplicación arranca, NO por cada movimiento de un filtro.
# Cuidado: todos los montos permanecen en moneda original (MN o ME); datos_tc se
# une al balance para uso posterior pero NO convierte automáticamente los flujos.
# Datos necesarios
# here() construye ruta desde el proyecto; requiere data/miniLiquidez_DATOS.xlsx.
ruta_datos <- here("data", "miniLiquidez_DATOS.xlsx")
# Datos_Contables: posiciones (saldos) y características por operación/corte.
datos_contables <- read_excel(ruta_datos, sheet = "Datos_Contables")
# Pagos_Contractuales: fechas e importes de capital/interés; base de flujos.
datos_contrato <- read_excel(ruta_datos, sheet = "Pagos_Contractuales")
# Observaciones: retiros, renovaciones, cobros y líneas durante 30 días anteriores.
datos_observados <- read_excel(ruta_datos, sheet = "Observaciones")
# Tipo_Cambio: CRC por unidad; aquí se incorpora, no se aplica conversión.
datos_tc <- read_excel(ruta_datos, sheet = "Tipo_Cambio")
# Catalogo_Usado: cuentas y denominaciones para clasificación normativa.
datos_catalogo <- read_excel(ruta_datos, sheet = "Catalogo_Usado")

# Limpieza | janitor::clean_names convierte p. ej. SALDO_CONTABLE en saldo_contable.
# Se limpia el nombre de las variables
# Se aplica a cada hoja, manteniendo sus filas y cifras originales.
datos_contables <- clean_names(datos_contables)
datos_contrato <- clean_names(datos_contrato)
datos_observados <- clean_names(datos_observados)
datos_tc <- clean_names(datos_tc)
datos_catalogo <- clean_names(datos_catalogo)

# Tipos de datos | factor indica variable categórica, no cantidad continua.
# Variables tipo factor de datos del catálogo
# Código de cuenta no se suma; se tratará como etiqueta/clave de unión.
datos_catalogo$cuenta_contable <- as.factor(datos_catalogo$cuenta_contable)

# Transformaciones ORIGINALES del proyecto, conservadas para compatibilidad.
# Variables tipo factor de datos contables
# c_entidad identifica banco; alias es etiqueta A–E; moneda es MN/ME.
datos_contables$c_entidad <- as.factor(datos_contables$c_entidad)
datos_contables$alias <- as.factor(datos_contables$alias)
datos_contables$moneda <- as.factor(datos_contables$moneda)
# cuenta_contable se almacena como factor en esta etapa, luego función lo pasa a texto.
datos_contables$cuenta_contable <- as.factor(datos_contables$cuenta_contable)
datos_contables$nombre_cuenta_catalogo <- as.factor(datos_contables$nombre_cuenta_catalogo)
datos_contables$sector_contraparte <- as.factor(datos_contables$sector_contraparte)
# Variables comerciales y de riesgo se conservan aunque no todas afecten 2.1–2.3.
datos_contables$relacion_comercial_estable <- as.factor(datos_contables$relacion_comercial_estable)
datos_contables$retiro_anticipado_sin_penalidad <- as.factor(datos_contables$retiro_anticipado_sin_penalidad)
datos_contables$calificacion_internacional <- as.factor(datos_contables$calificacion_internacional)
datos_contables$mercado_activo <- as.factor(datos_contables$mercado_activo)
datos_contables$negociacion_disponible <- as.factor(datos_contables$negociacion_disponible)
datos_contables$ingreso_cliente_misma_moneda <- as.factor(datos_contables$ingreso_cliente_misma_moneda)
datos_contables$destino_credito <- as.factor(datos_contables$destino_credito)

# Observaciones también se indexan por entidad, alias y moneda.
# Variables tipo factor de datos observados
datos_observados$c_entidad <- as.factor(datos_observados$c_entidad)
datos_observados$alias <- as.factor(datos_observados$alias)
datos_observados$moneda <- as.factor(datos_observados$moneda)

# Moneda del tipo de cambio es un factor para facilitar uniones.
# Variables tipo factor de datos del tipo de cambio
datos_tc$moneda <- as.factor(datos_tc$moneda)

# Los levels() reemplazan las etiquetas originales respetando el orden de niveles.
# Se renombran los nombres de las entidades
# 'A'–'E' son alias del ejercicio; no se modifica c_entidad.
levels(datos_contables$alias) <- c("A", "B", "C", "D", "E")
levels(datos_observados$alias) <- c("A", "B", "C", "D", "E")

# Unión original | Pega a cada registro contable las columnas del pago vinculado.
# Se juntan los datos contables con los pagos de contrato
# Si un ID tiene más de un pago, esta unión podría repetir saldo_contable.
datos_contables <- datos_contables |>
# Se excluyen ID_PAGO y PERIODO del lado derecho; id_registro es la llave.
  left_join(datos_contrato |>
              select(-c("id_pago", "periodo")), "id_registro")

# Unión original | Añade CRC por unidad usando PERIODO + MONEDA como llaves.
# Se añade el tipo de cambio correspondiente a cada periodo
# No multiplica saldo por tipo de cambio: esa conversión no se hace aquí.
datos_contables <- datos_contables |>
  left_join(datos_tc, c("periodo", "moneda"))

# Motor de cálculo | Lo que sigue no dibuja la interfaz: ejecuta la metodología.
# source carga definiciones de funciones; crear_modelo_liquidez las ejecuta.
# modelo es una lista de tablas normales. Cambiar una selección en Shiny no
# ejecuta de nuevo esta construcción; solo filtra esas tablas ya calculadas.
# parte 2.1, 2.2 y 2.3 del miniproyecto de riesgo de liquidez.
# Busca la función auxiliar en la carpeta R/ de la raíz del repositorio.
source(here("R", "liquidez_funciones.R"), encoding = "UTF-8")
# Genera catálogo, saldos, tasas, flujos, bandas, rubros, estructura y controles.
modelo <- crear_modelo_liquidez(datos_contables, datos_contrato,
                                datos_observados, datos_catalogo)

# Presentación | formato_monto(x)
# Convierte un monto a texto legible para las tarjetas y alertas del dashboard.
# Divide entre 1e6 SOLO para mostrar 'millones'. No cambia los montos del modelo.
# El número sigue en MN o ME, según el filtro; no es conversión entre monedas.
formato_monto <- function(x) {
# Si el cálculo está vacío/no disponible, se muestra una explicación, no 0.
  if (length(x) == 0 || is.na(x)) return("No disponible")
# round redondea a 2 decimales; format cambia separadores y espacios.
  paste0(format(round(x / 1e6, 2), big.mark = ".", decimal.mark = ",",
                trim = TRUE, nsmall = 2), " millones")
}

# Presentación | tabla_interactiva(datos, pagina=12)
# Genera una tabla DT con búsqueda, filtros por columna, paginación y barra horizontal.
# No aplica fórmulas financieras; recibe una tabla que ya viene calculada.
tabla_interactiva <- function(datos, pagina = 12) {
# rownames=FALSE quita numeración técnica; filter='top' permite filtrar columnas.
  DT::datatable(datos, rownames = FALSE, filter = "top",
# pageLength define filas por página; scrollX permite tablas anchas.
                options = list(pageLength = pagina, scrollX = TRUE,
# dom='ftip' configura búsqueda, tabla, información y paginación.
                               autoWidth = TRUE, dom = "ftip"))
}

# Interfaz (ui) | Estructura visual. TODO lo que está dentro de fluidPage() hasta
# el cierre correspondiente define presentación: textos, filtros, pestañas,
# contenedores, colores y espacios. No se calculan brechas en esta sección.
ui <- fluidPage(
# CSS del navegador: estilo de fondo, tarjetas y tamaños de letra.
  tags$head(tags$style(HTML(paste0(
# Los estilos no afectan valores ni clasificación financiera.
    "body { background-color: #f7f9fc; color: #1d2939; }",
    ".well { background-color: white; border: 1px solid #e3e8ef; }",
    ".metric { padding: 12px; background-color: white; border: 1px solid #e3e8ef; ",
    "border-radius: 6px; min-height: 94px; }",
    ".metric b { display: block; color: #526071; font-size: 12px; }",
    ".metric span { font-size: 18px; font-weight: 600; }"
  )))),
# Título visible de la aplicación.
  titlePanel("Riesgo de liquidez | Módulos 2.1 a 2.3"),
# sidebarLayout distribuye los filtros a la izquierda y resultados a la derecha.
  sidebarLayout(
# sidebarPanel(width=3) reserva tres columnas de la grilla Bootstrap.
    sidebarPanel(width = 3,
# Control de entidad; choices se extrae de los alias presentes en el modelo.
      selectInput("entidad", "Entidad", choices = sort(unique(modelo$saldos$alias))),
# Control de cierre; fecha se ofrece en formato legible AAAA-MM-DD.
      selectInput("corte", "Fecha de corte",
                  choices = format(sort(unique(modelo$saldos$periodo)), "%Y-%m-%d")),
# Control de moneda: impide sumar MN y ME dentro de un mismo resultado.
      selectInput("moneda", "Moneda", choices = c("MN", "ME")),
# Vista de flujos: alterna columnas contractuales/ajustadas, no escenarios de tensión.
      selectInput("escenario", "Vista de flujos",
                  choices = c("Contractual", "Ajustado por comportamiento")),
# Mensajes explicativos siempre visibles para evitar confusión de unidades.
      helpText("MN: colones; ME: dólares. Montos sin conversión cruzada."),
      helpText("La vista ajustada usa observaciones del corte. No es un escenario de tensión regulatoria."),
      hr(),
# Botones de descarga: aquí solo aparecen; downloadHandler implementa la descarga.
      downloadButton("descargar_bandas", "Descargar brechas (CSV)"),
      br(), br(),
      downloadButton("descargar_rubros", "Descargar rubros (CSV)"),
      br(), br(),
      downloadButton("descargar_saldos", "Descargar saldos (CSV)"),
      br(), br(),
      downloadButton("descargar_maestra", "Descargar tabla maestra (CSV)")
    ),
# mainPanel(width=9) ocupa el espacio de visualizaciones.
    mainPanel(width = 9,
# tabsetPanel separa pantallas funcionales para facilitar la navegación.
      tabsetPanel(
# Resumen 2.1: KPI y alertas. Sus cifras se producirán más abajo en server.
        tabPanel("Resumen 2.1",
          br(),
# fluidRow/column distribuyen tres tarjetas de ancho igual.
          fluidRow(
# tags$b da formato en negrita; textOutput es un hueco para valor dinámico.
            column(4, div(class = "metric", tags$b("Activos contables"), span(textOutput("kpi_activos", inline = TRUE)))),
            column(4, div(class = "metric", tags$b("Pasivos contables"), span(textOutput("kpi_pasivos", inline = TRUE)))),
            column(4, div(class = "metric", tags$b("Brecha acumulada a 30 días"), span(textOutput("kpi_30", inline = TRUE))))
          ),
# 'alerta' es texto dinámico: servidor detecta la primera brecha negativa.
          br(), strong(textOutput("alerta")),
# Texto metodológico: no se cuenta caja como entrada en las brechas.
          p("La brecha es anterior al uso de caja, encaje, venta de inversiones y otras medidas de cobertura."),
# plotlyOutput reserva espacio para gráfico; no dibuja ni calcula todavía.
          plotlyOutput("grafico_brecha", height = "290px"),
# DTOutput también reserva espacio; el contenido se asigna en renderDT.
          h4("Brechas por plazo"), DTOutput("tabla_bandas"),
          h4("Comparación entre entidades (misma moneda y corte)"),
          DTOutput("tabla_comparativo")
        ),
# Apartado 2.2: estructura de saldos y vencimientos de POSICIONES.
        tabPanel("Estructura 2.2",
# Gráfico de composición por rubro y clase contable.
          br(), h4("Composición de activos y pasivos"),
          plotlyOutput("grafico_estructura", height = "450px"),
# Tabla: códigos, nombres y etiquetas de disponibilidad.
          h4("Detalle del catálogo y disponibilidad"),
          DTOutput("tabla_estructura"),
# Tabla de bandas de vencimiento de saldos, distinta del calendario de pagos.
          h4("Concentración de vencimientos de posiciones contables"),
          DTOutput("tabla_vencimientos"),
          p("Los vencimientos de saldos no son flujos sumables: el calendario de pagos se analiza en la siguiente pestaña.")
        ),
# Apartado 2.3: flujos contractuales y ajustados, agregados y detallados.
        tabPanel("Flujos y brechas 2.3",
          br(),
# Gráfico de barras de entradas/salidas por banda temporal.
          plotlyOutput("grafico_flujos", height = "340px"),
# Tablas: agregado por cuenta, atrasos, tasas e inspección de cada pago.
          h4("Detalle por cuenta contable y banda"), DTOutput("tabla_rubros"),
          h4("Cobros vencidos y no confirmados"), DTOutput("tabla_atrasos"),
          h4("Supuestos históricos del corte"), DTOutput("tabla_tasas"),
          textOutput("advertencia_tasas"),
          h4("Trazabilidad de pagos y ajustes"), DTOutput("tabla_flujos_detallados")
        ),
# Tabla maestra: saldo contable único + columnas separadas para cada banda.
        tabPanel("Tabla maestra",
          br(),
# Esta explicación de formato ancho evita sumar seis veces el saldo.
          p("Una fila por entidad, corte, moneda y cuenta contable. El saldo no se repite por banda; las columnas de flujos tienen sufijos d1_8, d9_15, d16_30, d31_89, d90_180 y d181_."),
          DTOutput("tabla_maestra")
        ),
# Esta pantalla documenta supuestos y da trazabilidad regulatoria/operativa.
        tabPanel("Metodología y controles",
# Muestra la clasificación por cuentas; no crea nueva clasificación aquí.
          br(), h4("Catálogo clasificado"), DTOutput("tabla_catalogo"),
# Muestra comprobaciones de integridad creadas por el motor.
          h4("Verificaciones de integridad"), DTOutput("tabla_controles"),
          h4("Reglas de interpretación"),
# Lista HTML de reglas que el lector debe conocer para interpretar cifras.
          tags$ul(
            tags$li("Clases contables: activo (1), pasivo (2), patrimonio (3), contingente (6)."),
            tags$li("Bandas: 1–8, 9–15, 16–30, 31–89, 90–180 y 181+ días."),
            tags$li("Pasivos vencidos: primera banda; cobros vencidos: identificados aparte, no reconocidos como cobrados."),
            tags$li("El escenario ajustado usa tasas históricas del corte solamente hasta 30 días."),
            tags$li("Líneas concedidas: desembolso contingente estimado; líneas recibidas: recurso potencial, no entrada contractual."),
            tags$li("Cuentas sin fecha, patrimonio, caja y encaje: no se inventan flujos contractuales."),
            tags$li("No se mezclan fechas de corte, monedas ni saldos contables con movimientos de efectivo.")
          ),
# Diferencia explícita entre norma de clase contable y supuestos financieros.
          p("Fuente de las clases: CONASSIF 6-18, artículo 26. Los tratamientos de liquidez son supuestos académicos y no equivalen al ICL/IFNE."),
# Enlace a regulación; abrir web no modifica cálculos ni datos.
          tags$a(href = "https://www.sugef.fi.cr/normativa/NormativaTransversal.aspx",
                 target = "_blank", "Reglamento y anexos SUGEF")
        )
      )
    )
  )
)

# Servidor (server) | Conecta controles de la UI con resultados visibles.
# Los cálculos financieros ya se hicieron en modelo; aquí principalmente se
# FILTRAN y AGREGAN para dibujarlos. Algunas sumas de KPI/comparación se ejecutan
# aquí por el filtro elegido; se etiquetan como 'Resumen' cuando corresponda.
# input$... lee un control; output$... escribe su resultado; reactive() memoriza
# dependencias y se invalida automáticamente cuando cambia un input utilizado.
server <- function(input, output, session) {
# Estado reactivo: criterios actuales de entidad, cierre y moneda.
  filtros <- reactive({
# Las tres llaves permiten seleccionar una sola 'fotografía' financiera.
    list(alias = input$entidad, periodo = as.Date(input$corte), moneda = input$moneda)
  })
# Función auxiliar LOCAL: filtrar una tabla por las tres llaves.
  filtrar <- function(tabla, f) {
# No modifica el modelo; devuelve una vista con el filtro solicitado.
    tabla |>
      dplyr::filter(alias == f$alias, periodo == f$periodo, moneda == f$moneda)
  }
# Cada '..._actual' es una tabla reactiva del mismo corte/entidad/moneda.
  saldos_actuales <- reactive(filtrar(modelo$saldos, filtros()))
# Estructura, bandas, rubros y vencimientos reaccionan a la selección.
  estructura_actual <- reactive(filtrar(modelo$estructura, filtros()))
  bandas_actuales <- reactive(filtrar(modelo$bandas, filtros()))
  rubros_actuales <- reactive(filtrar(modelo$rubros, filtros()))
  vencimientos_actuales <- reactive(filtrar(modelo$vencimientos, filtros()))
# Tabla maestra, tasas y flujos detallados usan exactamente el mismo filtro.
  maestra_actual <- reactive(filtrar(modelo$tabla_maestra, filtros()))
  tasas_actuales <- reactive(filtrar(modelo$tasas, filtros()))
  flujos_actuales <- reactive(filtrar(modelo$flujos, filtros()))
# Selector reactivo de NOMBRES de columnas, no de nuevas tasas ni fórmulas.
  columnas_vista <- reactive({
# Si elige contractual, tomamos las cuatro columnas con sufijo contrato.
    if (input$escenario == "Contractual") {
      c("entradas_contrato", "salidas_contrato", "brecha_contrato", "acumulada_contrato")
# Si elige ajustado, tomamos columnas ya construidas en el motor.
    } else {
      c("entradas_ajustadas", "salidas_ajustadas", "brecha_ajustada", "acumulada_ajustada")
    }
  })
# Construye un esquema común de salida para que los gráficos sirvan a ambos modos.
  bandas_vista <- reactive({
# bandas_actuales() invoca un valor REACTIVO: por eso necesita paréntesis.
    d <- bandas_actuales()
# nombres almacena el conjunto de columnas elegido según input$escenario.
    nombres <- columnas_vista()
# transmute conserva sólo banda, entradas, salidas, brecha y acumulada.
    dplyr::transmute(d, banda = as.character(banda),
# .data[[nombre]] accede dinámicamente a la columna seleccionada por texto.
                     entradas = .data[[nombres[1]]], salidas = .data[[nombres[2]]],
                     brecha = .data[[nombres[3]]], acumulada = .data[[nombres[4]]])
  })

# KPI de resumen: suma saldos contables de activos del corte filtrado.
  output$kpi_activos <- renderText({
# Esto es un stock al cierre; no son entradas disponibles en 1-8 días.
    formato_monto(sum(saldos_actuales()$saldo_contable[
      saldos_actuales()$clase_contable == "Activo"], na.rm = TRUE))
  })
# KPI de resumen: suma saldos de pasivos; no equivale a salidas inmediatas.
  output$kpi_pasivos <- renderText({
    formato_monto(sum(saldos_actuales()$saldo_contable[
      saldos_actuales()$clase_contable == "Pasivo"], na.rm = TRUE))
  })
# KPI de 30 días: lee la brecha acumulada hasta 16-30, ya calculada.
  output$kpi_30 <- renderText({
    d <- bandas_vista()
# No hace cumsum aquí: toma la columna generada por resumir_bandas().
    formato_monto(d$acumulada[which(d$banda == "16-30")])
  })
# Alerta de resumen: primera banda cuya acumulada sea negativa.
  output$alerta <- renderText({
    d <- bandas_vista()
# which() retorna índices de valores verdaderos; [1] el primero.
    primer <- which(d$acumulada < 0)[1]
# No hay déficit acumulado negativo en ninguna banda: mensaje explicativo.
    if (is.na(primer)) {
      "No se observa brecha acumulada negativa en las seis bandas seleccionadas."
    } else {
# Se muestra la magnitud positiva del faltante: -acumulada[primer].
      paste0("Primer faltante acumulado en la banda ", d$banda[primer],
             " días: ", formato_monto(-d$acumulada[primer]), " de déficit.")
    }
  })

# Visualización | Línea de brecha acumulada. NO recalcula la brecha:
# recibe bandas_vista() y divide entre 1e6 sólo para visualizar millones.
# Plotly reconoce las bandas como categorías, así que se fija su orden manualmente.
  output$grafico_brecha <- renderPlotly({
    d <- bandas_vista()
    
# plot_ly crea objeto gráfico a partir del data.frame d.
    plotly::plot_ly(
      data = d,
# Eje horizontal: bandas temporales (categorías, NO fechas calendario).
      x = ~banda,
# Eje vertical: brecha acumulada expresada en millones.
      y = ~acumulada / 1e6,
# 'scatter' con lines+markers dibuja línea y puntos.
      type = "scatter",
      mode = "lines+markers",
# Color meramente visual; no comunica un umbral regulatorio.
      line = list(color = "#155b75"),
      name = "Brecha acumulada"
    ) |>
# layout controla ejes y línea de referencia, sin transformar datos.
      plotly::layout(
        xaxis = list(
          title = "Banda (días)",
# categoryorder/array fuerzan 1-8 -> 9-15 -> ... -> 181+.
          categoryorder = "array",
          categoryarray = bandas_liquidez
        ),
        yaxis = list(
          title = "Millones de la moneda seleccionada"
        ),
# shapes dibuja una línea de referencia horizontal en cero.
        shapes = list(
          list(
# x0/x1 son posiciones gráficas; y0=y1=0 define el nivel neutro de brecha.
            type = "line",
            x0 = -0.5,
            x1 = 5.5,
            y0 = 0,
            y1 = 0,
            line = list(dash = "dot")
          )
        )
      )
  })
# Tabla dinámica de las seis bandas. Aquí se muestran AMBAS vistas completas.
  output$tabla_bandas <- DT::renderDT({
# select controla columnas visibles y su orden, sin alterar las cifras.
    d <- bandas_actuales() |>
      dplyr::select(banda, entradas_contrato, salidas_contrato, brecha_contrato,
                    acumulada_contrato, entradas_ajustadas, salidas_ajustadas,
                    brecha_ajustada, acumulada_ajustada)
# La paginación se fija en seis filas (una por banda).
    tabla_interactiva(d, 6)
  })
# Resumen comparativo | Usa brecha 30 días (corte y moneda actuales) para
# comparar TODAS las entidades; por eso no filtra por input$entidad.
# Métrica relativa = 100 × brecha acumulada ajustada / total de activos contables.
# Es una medida didáctica de escala; NO es el ICL ni el IFNE.
  output$tabla_comparativo <- DT::renderDT({
# Extrae el cierre y moneda del panel de filtros.
    f <- filtros()
# La base comparativa usa la tabla completa de bandas, no la entidad filtrada.
    compare <- modelo$bandas |>
      dplyr::filter(periodo == f$periodo, moneda == f$moneda,
# La banda 16-30 expresa el acumulado hasta el día 30, no sólo esa banda.
                    as.character(banda) == "16-30") |>
      dplyr::select(alias, acumulada_contrato, acumulada_ajustada) |>
# left_join añade el total de activos calculado para cada entidad.
      dplyr::left_join(
# Origen de activos: saldos únicos, sin duplicación por pagos.
        modelo$saldos |>
          dplyr::filter(periodo == f$periodo, moneda == f$moneda,
                        clase_contable == "Activo") |>
# Agrupa por entidad para crear una base comparable.
          dplyr::group_by(alias) |>
          dplyr::summarise(activos = sum(saldo_contable), .groups = "drop"),
        by = "alias") |>
# Fórmula financiera de escala: resultado neto como porcentaje de activos.
      dplyr::mutate(brecha_30_sobre_activos_pct =
# Con activos cero o faltantes se evita dividir por cero: devuelve NA.
                      dplyr::if_else(activos > 0,
                        100 * acumulada_ajustada / activos, NA_real_))
    tabla_interactiva(compare, 5)
  })

# Visualización | Composición contable. Los montos se SUMAN aquí por rubro sólo
# para dibujar el gráfico; la fuente sigue siendo modelo$estructura.
# Se excluye patrimonio/contingentes de este gráfico concreto, pero no del catálogo.
  output$grafico_estructura <- renderPlotly({
# Tabla filtrada antes de producir la figura.
    d <- estructura_actual() |>
# Solamente activos y pasivos para esta visualización.
      dplyr::filter(clase_contable %in% c("Activo", "Pasivo")) |>
# Grupo por clase y rubro para tener una barra por combinación.
      dplyr::group_by(clase_contable, rubro_liquidez) |>
# Saldo contable agregado por rubro; aquí NO sumamos flujos.
      dplyr::summarise(saldo = sum(saldo), .groups = "drop")
# ggplot define el sistema gráfico en R; plotly luego lo hace interactivo.
    p <- ggplot2::ggplot(d,
# reorder ordena rubros según saldo; dividir por 1e6 sólo cambia unidad visual.
          ggplot2::aes(x = reorder(rubro_liquidez, saldo), y = saldo / 1e6,
                       fill = clase_contable,
# El texto de hover describe el rubro y su monto.
                       text = paste0(rubro_liquidez, ": ", round(saldo / 1e6, 2), " millones"))) +
# geom_col usa alturas ya calculadas; no vuelve a contar registros.
      ggplot2::geom_col(show.legend = FALSE) +
# coord_flip muestra categorías en vertical para nombres largos.
      ggplot2::coord_flip() +
# facet_wrap crea paneles separados de Activos y Pasivos.
      ggplot2::facet_wrap(~clase_contable, scales = "free_y") +
      ggplot2::labs(x = "Rubro", y = "Millones de la moneda seleccionada") +
      ggplot2::theme_minimal()
# ggplotly transforma la figura en interactiva; tooltip solo presentación.
    plotly::ggplotly(p, tooltip = "text")
  })
# Tabla detallada de estructura: no aplica nuevas fórmulas.
  output$tabla_estructura <- DT::renderDT({
    tabla_interactiva(estructura_actual() |>
      dplyr::select(clase_contable, cuenta_contable, denominacion_oficial,
                    rubro_liquidez, disponibilidad, saldo, operaciones))
  })
# Tabla de vencimientos de SALDOS, distinta de flujos de efectivo.
  output$tabla_vencimientos <- DT::renderDT({
    tabla_interactiva(vencimientos_actuales() |>
      dplyr::select(clase_contable, cuenta_contable, rubro_liquidez,
                    banda_vencimiento, disponibilidad, saldo, operaciones))
  })

# Visualización | Entradas y salidas por banda; lectura del resumen bandas_vista().
# El eje Y se expresa en millones por presentación, sin conversión MN/ME.
# Nota técnica: en ESTA versión no se fija 'categoryarray' para esta figura;
# si Plotly reordena alfabéticamente las bandas se puede aplicar la misma
# opción de xaxis ya utilizada en grafico_brecha, sin cambiar fórmulas.
  output$grafico_flujos <- renderPlotly({
    d <- bandas_vista()
# La primera traza dibuja ENTRADAS; barras agrupadas por categoría.
    plotly::plot_ly(data = d, x = ~banda, y = ~entradas / 1e6,
                    type = "bar", name = "Entradas", marker = list(color = "#247f79")) |>
# add_trace incorpora SALIDAS del mismo dataframe y las mismas bandas.
      plotly::add_trace(y = ~salidas / 1e6, name = "Salidas",
                        marker = list(color = "#c07c43")) |>
# barmode='group' muestra ambas barras lado a lado (no apiladas).
      plotly::layout(barmode = "group", xaxis = list(title = "Banda (días)"),
                     yaxis = list(title = "Millones"))
  })
# Tabla de flujos por cuenta y banda; resume contractual vs ajustado.
  output$tabla_rubros <- DT::renderDT({
    tabla_interactiva(rubros_actuales() |>
      dplyr::select(banda, cuenta_contable, denominacion_oficial,
                    rubro_liquidez, sentido, contractual, ajustado), 15)
  })
# Cobros vencidos: permanecen consultables aun cuando no entren en las seis bandas.
  output$tabla_atrasos <- DT::renderDT({
    tabla_interactiva(flujos_actuales() |>
      dplyr::filter(estado_plazo == "Cobro vencido/no confirmado") |>
      dplyr::select(id_registro, cuenta_contable, fecha_pago_contractual,
                    monto_capital, monto_interes, nota_ajuste))
  })
# Supuestos históricos por corte: tasas y líneas recibidas potenciales.
  output$tabla_tasas <- DT::renderDT({
    tabla_interactiva(tasas_actuales() |>
      dplyr::select(tasa_retiro, tasa_renovacion, tasa_cobro,
                    tasa_uso_lineas, linea_recibida_no_dispuesta,
                    fecha_disponible_linea_recibida, fecha_fin_linea_recibida), 5)
  })
# Tabla maestra: una fila por cuenta del corte, con saldos y bandas en columnas.
  output$tabla_maestra <- DT::renderDT({
    tabla_interactiva(maestra_actual(), 10)
  })
# Aviso sobre NA históricos; una tasa no observable requiere interpretación.
  output$advertencia_tasas <- renderText({
    t <- tasas_actuales()
# Lista de cuatro tasas cuyo faltante se analiza.
    cols <- c("tasa_retiro", "tasa_renovacion", "tasa_cobro", "tasa_uso_lineas")
    if (anyNA(t[, cols])) {
      "Advertencia: hay tasas históricas no observables. Revisar el detalle antes de interpretar el ajuste."
    } else "Las cuatro tasas históricas están disponibles para este corte."
  })
# Mayor detalle de trazabilidad: cada pago/proyección antes de la agregación.
  output$tabla_flujos_detallados <- DT::renderDT({
    tabla_interactiva(flujos_actuales(), 10)
  })
# Catálogo normativo y analítico, común para todas las entidades.
  output$tabla_catalogo <- DT::renderDT({
    tabla_interactiva(modelo$catalogo |>
      dplyr::select(cuenta_contable, denominacion_oficial, clase_contable,
                    rubro_liquidez, tratamiento_general), 10)
  })
# Verificaciones son conteos de integridad, NO métricas de suficiencia.
  output$tabla_controles <- DT::renderDT(tabla_interactiva(modelo$verificaciones, 5))

# Exportación | Los siguientes bloques sólo escriben vistas filtradas a CSV.
# readr::write_excel_csv añade marcas compatibles con Excel; no modifica R objects.
# downloadHandler() tiene dos funciones: filename arma el nombre visible,
# content escribe los datos en el archivo temporal provisto por Shiny.
  exportar_csv <- function(datos, archivo) {
# Los NA se exportan como celdas vacías, en vez de la cadena 'NA'.
    readr::write_excel_csv(datos, archivo, na = "")
  }
# Descarga: seis bandas contractuales y ajustadas de la selección.
  output$descargar_bandas <- downloadHandler(
    filename = function() paste0("brechas_", input$entidad, "_", input$corte,
                                 "_", input$moneda, ".csv"),
    content = function(file) exportar_csv(bandas_actuales(), file))
# Descarga: flujos agregados por cuenta contable y banda.
  output$descargar_rubros <- downloadHandler(
    filename = function() paste0("rubros_", input$entidad, "_", input$corte,
                                 "_", input$moneda, ".csv"),
    content = function(file) exportar_csv(rubros_actuales(), file))
# Descarga: tabla maestra ancha con saldos y columnas temporales.
  output$descargar_maestra <- downloadHandler(
    filename = function() paste0("tabla_maestra_", input$entidad, "_", input$corte,
                                 "_", input$moneda, ".csv"),
    content = function(file) exportar_csv(maestra_actual(), file))
# Descarga: resumen de posiciones; no es archivo de flujos.
  output$descargar_saldos <- downloadHandler(
    filename = function() paste0("saldos_", input$entidad, "_", input$corte,
                                 "_", input$moneda, ".csv"),
    content = function(file) exportar_csv(estructura_actual(), file))
}

# Arranque final: Shiny conecta la interfaz definida por ui con su servidor.
shinyApp(ui = ui, server = server)
