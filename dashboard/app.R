
# Librerías necesarias
pacman::p_load(bslib, janitor, knitr, plotly, readxl, shiny, tidyverse, here, DT)

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

# Se cargan las funciones para hacer las partes 2.1, 2.2, 2.3 y 2.4 y 2.5
source(here("R", "liquidez_funciones.R"), encoding = "UTF-8")

# Modelo inicial de las partes 2 y 3
modelo <- crear_modelo_liquidez(datos_contables,
                                datos_contrato,
                                datos_observados,
                                datos_catalogo)

# Punto 2.3: esta función reutiliza SIN cambiar las operaciones de los módulos 2.4 y 2.5.
# Al recalcular tasas, todas las tablas posteriores parten del mismo modelo actualizado.
recalcular_resultados <- function(modelo) {
  # Se ejecuta la función para las coberturas de faltantes de liquidez
  coberturas_contrato <- cobertura_faltantes(modelo, "contrato")
  coberturas_ajustado <- cobertura_faltantes(modelo, "ajustado")
  
  # Se agrega la nueva tabla con las coberturas al modelo
  modelo$cobertura_contrato <- coberturas_contrato$bandas
  modelo$cobertura_ajustado <- coberturas_ajustado$bandas
  
  # Activos liquidados por entidad
  act_liquidados_contrato <- coberturas_contrato$act_liquidados
  act_liquidados_ajustado <- coberturas_ajustado$act_liquidados
  
  # Se agrega el coste por liquidez
  act_liquidados_contrato <- act_liquidados_contrato |>
    mutate(coste_liq = valor_mercado - valor_cotizado_venta_urgente)
  
  act_liquidados_ajustado <- act_liquidados_ajustado |>
    mutate(coste_liq = valor_mercado - valor_cotizado_venta_urgente)
  
  # Se agrega el coste por liquidez a las bandas
  modelo$cobertura_contrato <- act_liquidados_contrato |>
    group_by(periodo, alias, moneda, banda) |>
    summarise(salidas_liquidez = sum(coste_liq), .groups = "drop_last") |>
    right_join(modelo$cobertura_contrato,
               by = c("periodo", "alias", "moneda", "banda"))
  
  modelo$cobertura_ajustado <- act_liquidados_ajustado |>
    group_by(periodo, alias, moneda, banda) |>
    summarise(salidas_liquidez = sum(coste_liq), .groups = "drop_last") |>
    right_join(modelo$cobertura_ajustado,
               by = c("periodo", "alias", "moneda", "banda"))
  
  # Ordenar la tabla contractual
  modelo$cobertura_contrato <- modelo$cobertura_contrato |>
    dplyr::arrange(
      periodo,
      alias,
      factor(moneda, levels = c("ME", "MN")),
      factor(as.character(banda), levels = bandas_liquidez)
    )
  
  # Ordenar la tabla ajustada por comportamiento
  modelo$cobertura_ajustado <- modelo$cobertura_ajustado |>
    dplyr::arrange(
      periodo,
      alias,
      factor(moneda, levels = c("ME", "MN")),
      factor(as.character(banda), levels = bandas_liquidez)
    )
  
  # Se pone la columna de coste de liquidar inversiones en la penúltima posición
  modelo$cobertura_contrato <- modelo$cobertura_contrato |>
    dplyr::relocate(salidas_liquidez, .before = dplyr::last_col())
  
  modelo$cobertura_ajustado <- modelo$cobertura_ajustado |>
    dplyr::relocate(salidas_liquidez, .before = dplyr::last_col())
  
  # Base sujeta a encaje, requerimiento, reservas mantenidas y exceso o faltante
  encaje_reservs <- encaje_reserv(modelo$saldos)
  
  # Análisis del posible apoyo entre monedas
  bandas_tc <- liquidez_monedas(modelo)
  bandas_tc <- bandas_tc |> dplyr::relocate(tipo_cambio, .before = 5)
  
  list(
    modelo = modelo,
    act_liquidados_contrato = act_liquidados_contrato,
    act_liquidados_ajustado = act_liquidados_ajustado,
    encaje_reservs = encaje_reservs,
    bandas_tc = bandas_tc
  )
}

# Resultado base: es exactamente el modelo histórico usado antes de añadir controles.
resultados_base <- recalcular_resultados(modelo)
modelo <- resultados_base$modelo
act_liquidados_contrato <- resultados_base$act_liquidados_contrato
act_liquidados_ajustado <- resultados_base$act_liquidados_ajustado
encaje_reservs <- resultados_base$encaje_reservs
bandas_tc <- resultados_base$bandas_tc

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
  # Máximo de dos decimales
  datos <- datos |>
    dplyr::mutate(dplyr::across(
      where(~ is.double(.x) && !inherits(.x, c("Date", "POSIXct", "POSIXt", "difftime"))),
      ~ round(.x, 2)
    ))
  
  # Formato de tabla interactiva
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

# Aplica los filtros del dataframe a la tabla a descargar
filtrar_como_dt <- function(datos, filas) {
  if (is.null(filas))
    datos
  else
    datos[filas, , drop = FALSE]
}

# Tema de la aplicación
tema <- bs_theme(
  version = 5,
  bootswatch = "cosmo",        # tema predefinido (cosmo, darkly, minty...)
  primary = "#2C7BE5",
  base_font = font_google("Inter"),
  heading_font = font_google("Poppins")
)

# Diseño de la aplicación
ui <- fluidPage(
  # Se aplica el tema a la aplicación
  theme = tema,
  
  # Estilo por CSS
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
  
  # Título de la aplicación
  titlePanel("Riesgo de liquidez"),
  
  # Formato de cada página
  sidebarLayout(
    # Panel lateral
    sidebarPanel(
      width = 3,
      conditionalPanel(
        condition = paste0(
          "input.pestanas_principales != 'cobertura' && ",
          "input.pestanas_principales != 'encaje_moneda'"
        ),
        
        # Formato para la selección de la entidad
        selectInput("entidad", "Entidad", choices = sort(unique(
          modelo$saldos$alias
        ))),
        
        # Formato para la selección de la fecha de corte
        selectInput("corte", "Fecha de corte", choices = format(sort(
          unique(modelo$saldos$periodo)
        ), "%Y-%m-%d")),
        
        # Formato para la selección de la moneda
        selectInput("moneda", "Moneda", choices = c("MN", "ME")),
        
        # Formato para la selección del escenario
        selectInput(
          "escenario",
          "Vista de flujos",
          choices = c("Contractual", "Ajustado por comportamiento")
        ),
        
        # Formato para la selección de factor para coste de liquidez
        sliderInput(
          "factor",
          "Factor de liquidez",
          min = 0.5,
          max = 2,
          value = 1,
          step = 0.5
        ),
        
        # Punto 2.3: controles de comportamiento que recalculan todos los módulos.
        hr(),
        h4("Escenario de comportamiento"),
        selectInput(
          "modo_tasas",
          "Tasas utilizadas",
          choices = c("Base (tasas históricas)" = "base",
                      "Personalizado" = "personalizado"),
          selected = "base"
        ),
        conditionalPanel(
          condition = "input.modo_tasas == 'personalizado'",
          numericInput("tasa_retiros_param", "Retiros de depósitos (%)",
                       value = 50, min = 0, max = 100, step = 1),
          numericInput("tasa_renovacion_param", "Renovaciones a plazo (%)",
                       value = 50, min = 0, max = 100, step = 1),
          numericInput("tasa_cobro_param", "Recuperación de cobros (%)",
                       value = 80, min = 0, max = 100, step = 1),
          helpText("En personalizado, los tres porcentajes se aplican a todas las entidades, fechas y monedas; no se modifica la tasa de uso de líneas."),
          actionButton("restaurar_tasas", "Volver a tasas históricas")
        ),
        textOutput("tasas_base_seleccion")
      ),
      
      # Filtros de Cobertura de faltantes
      conditionalPanel(
        condition = "input.pestanas_principales == 'cobertura'",
        h4("Filtros de cobertura"),
        selectInput(
          "cobertura_entidad",
          "Entidad",
          choices = c("Todas", sort(unique(modelo$saldos$alias))),
          selected = "B",
          multiple = TRUE
        ),
        selectInput(
          "cobertura_corte",
          "Fecha de corte",
          choices = c("Todas", format(sort(unique(modelo$saldos$periodo)), "%Y-%m-%d")),
          selected = "2026-04-30",
          multiple = TRUE
        ),
        selectInput(
          "cobertura_moneda",
          "Moneda",
          choices = c("Todas", "MN", "ME"),
          selected = "Todas",
          multiple = TRUE
        ),
        selectInput(
          "tipo_flujo",
          "Tipo de flujo",
          choices = c("Contractual", "Ajustado por comportamiento"),
          selected = "Contractual",
          multiple = FALSE
        )
      ),
      
      # Filtros de encaje y liquidez por moneda
      conditionalPanel(
        condition = "input.pestanas_principales == 'encaje_moneda'",
        h4("Filtros de encaje y liquidez"),
        selectInput(
          "encaje_entidad",
          "Entidad",
          choices = c("Todas", sort(unique(modelo$saldos$alias))),
          selected = "B",
          multiple = TRUE
        ),
        selectInput(
          "encaje_corte",
          "Fecha de corte",
          choices = c("Todas", format(sort(unique(modelo$saldos$periodo)), "%Y-%m-%d")),
          selected = "2026-04-30",
          multiple = TRUE
        ),
        selectInput(
          "encaje_moneda",
          "Moneda",
          choices = c("Todas", "MN", "ME"),
          selected = "Todas",
          multiple = TRUE
        ),
        sliderInput(
          "tasa_encaje_input",
          "Tasa de encaje",
          min = 0.05,
          max = 0.25,
          value = 0.15,
          step = 0.05,
          ticks = TRUE
        )
      ),

      helpText("MN: colones; ME: dólares."),
      helpText(
        ""
      ),
      
      # Botones de descarga para las tablas
      hr(),
      downloadButton("descargar_bandas", "Descargar flujos por banda (CSV)"),
      br(),
      br(),
      downloadButton("descargar_rubros", "Descargar rubros (CSV)"),
      br(),
      br(),
      downloadButton("descargar_saldos", "Descargar saldos (CSV)"),
      br(),
      br(),
      downloadButton("descargar_cobertura", "Descargar cobertura (CSV)"),
      br(),
      br(),
      downloadButton("descargar_encaje", "Descargar encaje de reservas (CSV)"),
      br(),
      br(),
      downloadButton("descargar_bandas_tc", "Descargar bandas tipo de cambio (CSV)"),
      br(),
      br(),
      downloadButton("descargar_liquidar", "Descargar liquidez (CSV)")
    ),
    
    # Panel principal en donde se ubican los gráficos y las tablas
    mainPanel(
      width = 9,
      
      # Pestañas superiores que funcionan como las distintas páginas
      tabsetPanel(
        id = "pestanas_principales",
        # Pestaña de resumen
        tabPanel(
          "Resumen",
          br(),
          
          # Tarjetas encontradas en la parte superior de la primera pestaña
          fluidRow(column(4, div(
            class = "metric", tags$b("Activos contables"), span(textOutput("kpi_activos", inline = TRUE))
          )), column(4, div(
            class = "metric", tags$b("Pasivos contables"), span(textOutput("kpi_pasivos", inline = TRUE))
          )), column(4, div(
            class = "metric", tags$b("Brecha acumulada a 30 días"), span(textOutput("kpi_30", inline = TRUE))
          ))),
          
          # Texto de faltante acumulado y brecha
          br(),
          strong(textOutput("alerta")),
          p(
            "La brecha es anterior al uso de caja, encaje, venta de inversiones y otras medidas de cobertura."
          ),
          
          # Gráfico de la brecha
          plotlyOutput("grafico_brecha", height = "290px"),
          
          # Tabla de brechas por plazo
          h4("Brechas por plazo"),
          DTOutput("tabla_bandas"),
          
          # Tabla de comparación entre entidades
          h4("Comparación entre entidades (misma moneda y corte)"),
          DTOutput("tabla_comparativo")
        ),
        
        # Pestaña de estructura
        tabPanel(
          "Estructura",
          br(),
          
          # Gráfico de activos y pasivos
          h4("Composición de activos y pasivos"),
          plotlyOutput("grafico_estructura", height = "450px"),
          
          # Tabla de detalles del catálogo
          h4("Detalle del catálogo y disponibilidad"),
          DTOutput("tabla_estructura"),
          
          # Tabla de vencimiento de cada posición
          h4("Concentración de vencimientos de posiciones contables"),
          DTOutput("tabla_vencimientos"),
          
          # Mensaje de advertencia
          p(
            "Los vencimientos de saldos no son flujos sumables: el calendario de pagos se analiza en la siguiente pestaña."
          )
        ),
        
        # Pestaña de flujos y brechas
        tabPanel(
          "Flujos y brechas",
          br(),
          
          # Gráfico de activos y pasivos por banda
          plotlyOutput("grafico_flujos", height = "340px"),
          
          # Tabla de detalle por cuenta contable y banda
          h4("Detalle por cuenta contable y banda"),
          DTOutput("tabla_rubros"),
          
          # Tabla de cobros vencidos y no confirmados
          h4("Cobros vencidos y no confirmados"),
          DTOutput("tabla_atrasos"),
          
          # Tabla de supuestos históricos tomados para el análisis
          h4("Supuestos históricos del corte"),
          DTOutput("tabla_tasas"),
          textOutput("advertencia_tasas"),
          
          # Tabla para hacer seguimiento de los pagos y ajustes realizados
          h4("Trazabilidad de pagos y ajustes"),
          DTOutput("tabla_flujos_detallados")
        ),
        
        # Pestaña de cobertura de faltantes
        tabPanel(
          "Cobertura de faltantes",
          value = "cobertura",
          br(),
          
          # Texto explicativo general
          h4("La siguiente tabla muestra la brecha individual y acumulada por banda y entidad, periodo y moneda seleccionadas."),
          h4("Además, se aprecia el valor de los recursos disponibles para cubrir, en caso de haber, el faltante de liquidez; así como los recursos remanentes en caso de usarse alguno."),
          h4("Por último, se presenta el costo de liquidar inversiones, en caso de que fuera necesario, y el faltante residual, si fuera el caso."),
          h4("Todos los montos están en millones."),
          DTOutput("tabla_cobertura"),
          
          # Texto con los supuestos de esta página
          h4("Supuestos utilizados"),
          tags$ul(
            tags$li(
              "Los recursos utilizados para atender el faltante de liquidez, en orden de utilización, son:",
              tags$br(),
              "1. Disponibilidades utilizables (dinero en caja y bóvedas), de uso inmediato.",
              tags$br(),
              "2. Reservas excedentes, calculadas como la diferencia entre las reservas mantenidas y el encaje requerido.",
              tags$br(),
              "3. Venta de inversiones, las cuales afectan a las bandas siguientes, ya que no se contaría con esa entrada de liquidez. Se venden a un valor cotizado de venta urgente.",
              tags$br(),
              "4. Financiamiento disponible, representado por las líneas de crédito que la entidad puede usar."
            ),
            tags$li("Se utilizó un supuesto de encaje del 15%.")
          ),
        ),
        
        # Pestaña de encaje y liquidez por moneda
        tabPanel(
          "Encaje y liquidez por moneda",
          value = "encaje_moneda",
          br(),
          
          # Texto explicativo general
          h4("A continuación se presenta la base sujeta a encaje, el encaje requerido, las reservas mantenidas y excedentes de la institución, por periodo, entidad y moneda."),
          h4("Todos los montos están en millones."),
          DTOutput("tabla_encaje_reservs"),
          
          br(),
          
          h4("Seguidamente se muestra la situación de liquidez por moneda, con el tipo de cambio respectivo."),
          h4("Aquí se analiza si la brecha para una entidad, una banda, una moneda y un periodo determinados tiene signo contrario a la de la otra moneda."),
          h4("En este caso, el monto de la moneda que presenta una brecha positiva se convierte a la otra moneda, con el objetivo de determinar si puede cubrir la brecha negativa de su contraparte."),
          checkboxInput(
            "ocultar_na_bandas_tc",
            "Mostrar solo filas sin valores NA",
            value = FALSE
          ),
          DTOutput("tabla_bandas_tc"),
          
          # Supuestos de esta página
          h4("Supuestos utilizados"),
          tags$ul(
            tags$li(
              "La base sujeta a encaje se conforma de los pasivos asociados a depósitos a la vista y depósitos a plazo."
            ),
            tags$li("Las reservas mantenidas son de uso inmediato y sin restricciones."),
            tags$li("Las reservas excedentes corresponden a la diferencia entre las reservas mantenidas y el encaje requerido."),
            tags$li("Las reservas excedentes no tienen ninguna restricción adicional y son de uso inmediato."),
            tags$li("Se utilizó un supuesto de encaje del 15%."),
            tags$li("Se utilizó un 1% como supuesto simplificado de costo por conversión entre monedas.")
          ),
        ),
        
        # Pestaña de métricas de liquidez
        tabPanel(
          "Medidas de liquidez",
          br(),
          
          # Texto explicativo inicial
          p(
            "El gráfico muestra la comparación del Indicador de cobertura de liquidez (ICL) y el Indicador de financiamiento neto estable (IFNE). Para cumplir con la normativa, ambos deben ser mayores a 1."
          ),
          
          # Gráfico de las métricas de liquidez
          plotlyOutput("grafico_icl_ifne", height = "340px"),
          
          # Texto explicativo con las reglas de interpretación del modelo
          h4("Supuestos utilizados"),
          tags$ul(
            tags$li(
              "El ICL cambia según la moneda, el IFNE se mantiene constante debido a que se consideraron todos los activos en moneda nacional."
            ),
            tags$li("Se utilizó un supuesto de encaje del 15% para el ICL."),
            tags$li(
              "Para las entradas de efectivo del ICL, se usó el promedio de la tasa de cobro histórica como supuesto."
            ),
            tags$li(
              "Aunque no se refleja en el resultado final, se hicieron comprobaciones adicionales sobre los requisitos de cada categoría antes de calcularlas."
            )
          ),
          
          # Normativa consultada de la SUGEF
          h4("Anexos consultados"),
          tags$a(href = "https://www.sugef.fi.cr/normativa/normativa_vigente/SUGEF%2017-13%20(v18%201°%20SET2024).pdf", target = "_blank", "Acuerdo SUGEF 17-13")
        ),
        
        # Pestaña de valor en riesgo ajustado por liquidez
        tabPanel(
          "Valor en riesgo ajustado por liquidez",
          br(),
          
          # Texto explicativo inicial
          p(
            "El gráfico muestra las medidas de valor en riesgo y su coste por liquidez. El VaR fue estimado con un horizonte de tiempo mensual a partir de los datos dados."
          ),
          
          # Gráfico de activos y pasivos por banda
          plotlyOutput("grafico_lvar", height = "340px"),
          br(),
          
          # Texto explicativo con los supuestos del modelo
          h4("Supuestos utilizados"),
          tags$ul(
            tags$li(
              "Tanto el VaR como el coste por liquidez fueron calculados en colones, usando el tipo de cambio correspondiente en el caso de los activos presentados en moneda extranjera."
            ),
            tags$li(
              "El coste de liquidación fue calculado como la diferencia entre el valor de mercado del activo y el valor cotizado de venta urgente, debido a la falta de datos."
            )
          ),
          
          # Tabla de detalle por cuenta contable y banda
          br(),
          h4("Detalle de los 10 activos con mayor coste por liquidez"),
          DTOutput("tabla_coste_liq"),
          
          br(),
          
          # Texto explicativo inicial
          p(
            "El gráfico compara el Indicador de Financiamiento Neto Estable para cada entidad y periodo, permitiendo ver su evolución a lo largo del tiempo."
          ),
          
          # Gráfico de activos y pasivos por banda
          plotlyOutput("grafico_comp_ifne", height = "340px"),
        ),
        
        # Pestaña de metodología y controles
        tabPanel(
          "Metodología y controles",
          br(),
          
          # Tabla del catálogo clasificado por tratamiento general y rubro de liquidez
          h4("Catálogo clasificado"),
          DTOutput("tabla_catalogo"),
          
          # Tabla con las verificaciones de integridad requeridas
          h4("Verificaciones de integridad"),
          DTOutput("tabla_controles"),
          
          # Texto explicativo con las reglas de interpretación del modelo
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
          
          # Normativa de la SUGEF
          h4("Anexos consultados"),
          tags$a(href = "https://www.sugef.fi.cr/normativa/NormativaTransversal.aspx", target = "_blank", "Reglamento y anexos SUGEF")
        ),
      )
    )
  )
)

# Servidor que define, internamente, qué hace la aplicación
server <- function(input, output, session) {
  # Punto 2.3: los valores base NO se modifican. En personalizado, solamente
  # cambian los tres numeradores observados que originan las tasas históricas.
  # Así calcular_tasas() y preparar_flujos() conservan sus fórmulas originales.
  datos_observados_actuales <- reactive({
    if (is.null(input$modo_tasas) || input$modo_tasas == "base") {
      return(datos_observados)
    }
    porcentajes <- c(input$tasa_retiros_param,
                     input$tasa_renovacion_param,
                     input$tasa_cobro_param)
    shiny::req(length(porcentajes) == 3, all(is.finite(porcentajes)),
               all(porcentajes >= 0), all(porcentajes <= 100))
    datos_observados |>
      dplyr::mutate(
        retiros_vista_30d = saldo_vista_inicio_30d * porcentajes[1] / 100,
        principal_renovado_30d = vencimientos_plazo_30d * porcentajes[2] / 100,
        cobros_recibidos_30d = cobros_programados_30d * porcentajes[3] / 100
      )
  })
  
  resultados_actuales <- reactive({
    if (is.null(input$modo_tasas) || input$modo_tasas == "base") {
      return(resultados_base)
    }
    modelo_parametrizado <- crear_modelo_liquidez(
      datos_contables, datos_contrato,
      datos_observados_actuales(), datos_catalogo
    )
    recalcular_resultados(modelo_parametrizado)
  })
  
  modelo_actual <- reactive(resultados_actuales()$modelo)
  
  # Muestra las tasas históricas correspondientes al filtro de la barra lateral.
  output$tasas_base_seleccion <- renderText({
    t <- modelo$tasas |>
      dplyr::filter(alias == input$entidad,
                    periodo == as.Date(input$corte),
                    moneda == input$moneda)
    if (nrow(t) == 0) return("Sin tasas históricas para este filtro.")
    f <- function(x) if (is.na(x[1])) "NA" else paste0(round(x[1] * 100, 1), "%")
    paste0("Históricas del corte — retiros: ", f(t$tasa_retiro),
           "; renovaciones: ", f(t$tasa_renovacion),
           "; recuperación: ", f(t$tasa_cobro))
  })
  
  observeEvent(input$restaurar_tasas, {
    updateSelectInput(session, "modo_tasas", selected = "base")
  })
  
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
  saldos_actuales <- reactive(filtrar(modelo_actual()$saldos, filtros()))
  estructura_actual <- reactive(filtrar(modelo_actual()$estructura, filtros()))
  bandas_actuales <- reactive(filtrar(modelo_actual()$bandas, filtros()))
  rubros_actuales <- reactive(filtrar(modelo_actual()$rubros, filtros()))
  vencimientos_actuales <- reactive(filtrar(modelo_actual()$vencimientos, filtros()))
  maestra_actual <- reactive(filtrar(modelo_actual()$tabla_maestra, filtros()))
  tasas_actuales <- reactive(filtrar(modelo_actual()$tasas, filtros()))
  flujos_actuales <- reactive(filtrar(modelo_actual()$flujos, filtros()))
  bandas_cobertura_actuales <- reactive({
    filtrar(modelo_actual()$cobertura_contrato, filtros())
  })
  tasa_encaje_actual <- reactive({
    input$tasa_encaje_input
  })
  
  encaje_reservs_actual <- reactive({
    
    # Se recalcula usando la tasa seleccionada
    datos <- encaje_reserv(
      saldos = modelo_actual()$saldos,
      tasa_encaje = tasa_encaje_actual()
    )
    
    # Se filtra por entidad
    if (
      !is.null(input$encaje_entidad) &&
      !"Todas" %in% input$encaje_entidad
    ) {
      datos <- datos |>
        dplyr::filter(
          alias %in% input$encaje_entidad
        )
    }
    
    # Se filtra por fecha de corte
    if (
      !is.null(input$encaje_corte) &&
      !"Todas" %in% input$encaje_corte
    ) {
      datos <- datos |>
        dplyr::filter(
          periodo %in% as.Date(input$encaje_corte)
        )
    }
    
    # Se filtra por moneda
    if (
      !is.null(input$encaje_moneda) &&
      !"Todas" %in% input$encaje_moneda
    ) {
      datos <- datos |>
        dplyr::filter(
          moneda %in% input$encaje_moneda
        )
    }
    
    datos
  })
  
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
    compare <- modelo_actual()$bandas |>
      dplyr::filter(periodo == f$periodo,
                    moneda == f$moneda,
                    as.character(banda) == "16-30") |>
      dplyr::select(alias, acumulada_contrato, acumulada_ajustada) |>
      dplyr::left_join(
        modelo_actual()$saldos |>
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
      modelo_actual()$catalogo |>
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
  output$tabla_controles <- DT::renderDT(tabla_interactiva(modelo_actual()$verificaciones, 5))
  
  # Tabla de cobertura de faltantes con filtros
  output$tabla_cobertura <- DT::renderDT({
    tabla_interactiva(bandas_cobertura_actuales(), 12)
  })
  
  
  # Tabla de encaje y reservas
  output$tabla_encaje_reservs <- DT::renderDT({
    tabla_interactiva(
      encaje_reservs_actual(),
      12
    )
  })
  
  # Tabla de bandas por tipo de cambio
  output$tabla_bandas_tc <- DT::renderDT({
    
    # Obtener los datos filtrados
    datos <- bandas_tc_actual()
    
    # Aplicar el filtro cuando se activa el checkbox
    if (isTRUE(input$ocultar_na_bandas_tc) && nrow(datos) > 0) {
      
      # Columnas que contienen los resultados del análisis cambiario
      columnas_resultado <- c(
        "brecha_contrato_tc",
        "brecha_contrato_residual",
        "brecha_ajustada_tc",
        "brecha_ajustada_residual"
      )
      
      # Identificar las parejas por periodo, entidad y banda
      datos <- datos |>
        dplyr::group_by(periodo, alias, banda) |>
        dplyr::filter(
          any(
            rowSums(
              !is.na(
                dplyr::pick(dplyr::all_of(columnas_resultado))
              )
            ) > 0
          )
        ) |>
        dplyr::ungroup()
      
    }
    
    # Mostrar la tabla
    tabla_interactiva(datos, 12)
  })
  
  # Datos del ICL y el IFNE
  df_icl_ifne <- reactive({
    data.frame(categoria = c("ICL", "IFNE"),
               metrica = c(
                 icl(
                   datos = modelo_actual()$saldos,
                   entidad = input$entidad,
                   fecha = as.Date(input$corte),
                   moneda_var = input$moneda,
                   datos_observados = datos_observados_actuales()
                 ),
                 ifne(
                   datos = modelo_actual()$saldos,
                   entidad = input$entidad,
                   fecha = as.Date(input$corte),
                   datos_observados = datos_observados_actuales()
                 )
               ))
  })
  
  # Gráfico con el ICL e IFNE
  output$grafico_icl_ifne <- renderPlotly({
    # Se llama al dataframe
    d <- df_icl_ifne()
    
    # Se genera el gráfico
    plotly::plot_ly(
      data = d,
      x = ~ categoria,
      y = ~ metrica,
      type = "bar",
      name = "Medida",
      marker = list(color = "#247f79")
    ) |>
      plotly::layout(xaxis = list(title = "Medida"),
                     yaxis = list(title = "Unidades"))
  })
  
  # Datos del LVaR por entidad
  var_entidad <- reactive({
    var_liquidez(resultados_actuales()$act_liquidados_ajustado, factor_liq = input$factor, datos_contables = datos_contables)
  })
  
  # Datos del VaR y LVaR
  df_lvar <- reactive({
    var_entidad()[[1]] |>
      filter(alias == input$entidad)
  })
  
  # Gráfico del VaR y LVaR
  output$grafico_lvar <- renderPlotly({
    # Se llama al dataframe
    d <- df_lvar() |>
      select(var_95, var_99, coste_liq, var_liq_95, var_liq_99) |>
      pivot_longer(cols = everything(),
                   names_to = "metrica",
                   values_to = "valor")
    
    # Se genera el gráfico
    plotly::plot_ly(
      data = d,
      x = ~ metrica,
      y = ~ valor / 1000000,
      type = "bar",
      marker = list(color = "#247f79")
    ) |>
      plotly::layout(
        xaxis = list(
          title = "Medida",
          categoryorder = "array",
          categoryarray = c(
            "var_95",
            "var_99",
            "coste_liq",
            "var_liq_95",
            "var_liq_99"
          ),
          tickvals = c(
            "var_95",
            "var_99",
            "coste_liq",
            "var_liq_95",
            "var_liq_99"
          ),
          ticktext = c(
            "VaR 95%",
            "VaR 99%",
            "Coste por liquidez",
            "LVaR 95%",
            "LVaR 99%"
          )
        ),
        yaxis = list(title = "Millones de colones")
      )
  })
  
  # Tabla con los 10 registros con mayor coste de liquidez
  tabla_liquidez <- reactive({
    var_entidad()[[2]] |>
      filter(alias == input$entidad)
  })
  
  # Se hace tabla de coste de liquidez interactiva
  output$tabla_coste_liq <- DT::renderDT({
    tabla_interactiva(tabla_liquidez(), 10)
  })
  
  # Filtrar la tabla de cobertura de faltantes
  bandas_cobertura_actuales <- reactive({
    
    # Seleccionar la tabla según el tipo de flujo
    if (identical(
      input$tipo_flujo,
      "Ajustado por comportamiento"
    )) {
      datos <- modelo_actual()$cobertura_ajustado
    } else {
      datos <- modelo_actual()$cobertura_contrato
    }
    
    # Filtrar por entidad
    if (!is.null(input$cobertura_entidad) &&
        !"Todas" %in% input$cobertura_entidad) {
      
      datos <- datos |>
        dplyr::filter(
          alias %in% input$cobertura_entidad
        )
    }
    
    # Filtrar por fecha de corte
    if (!is.null(input$cobertura_corte) &&
        !"Todas" %in% input$cobertura_corte) {
      
      datos <- datos |>
        dplyr::filter(
          periodo %in% as.Date(input$cobertura_corte)
        )
    }
    
    # Filtrar por moneda
    if (!is.null(input$cobertura_moneda) &&
        !"Todas" %in% input$cobertura_moneda) {
      
      datos <- datos |>
        dplyr::filter(
          moneda %in% input$cobertura_moneda
        )
    }
    
    datos
  })
  
  # Filtrar datos de bandas_tc
  bandas_tc_actual <- reactive({
    
    datos <- resultados_actuales()$bandas_tc
    
    # Filtrar por entidad
    if (
      !is.null(input$encaje_entidad) &&
      !"Todas" %in% input$encaje_entidad
    ) {
      datos <- datos |>
        dplyr::filter(
          alias %in% input$encaje_entidad
        )
    }
    
    # Filtrar por fecha de corte
    if (
      !is.null(input$encaje_corte) &&
      !"Todas" %in% input$encaje_corte
    ) {
      datos <- datos |>
        dplyr::filter(
          periodo %in% as.Date(input$encaje_corte)
        )
    }
    
    # Filtrar por moneda
    if (
      !is.null(input$encaje_moneda) &&
      !"Todas" %in% input$encaje_moneda
    ) {
      datos <- datos |>
        dplyr::filter(
          moneda %in% input$encaje_moneda
        )
    }
    
    datos
  })
  
  # Mostrar la tabla filtrada
  output$tabla_cobertura <- DT::renderDT({
    tabla_interactiva(bandas_cobertura_actuales(), 12)
  })
  
  # Datos para comparar el ifne entre entidades
  df_ifne_comp <- reactive({
    data.frame(
      periodo = c(
        ymd("2026-04-30"),
        ymd("2026-05-31"),
        ymd("2026-06-30"),
        ymd("2026-07-31"),
        ymd("2026-08-31"),
        ymd("2026-09-30")
      ),
      ifne_a = c(
        ifne(
          modelo_actual()[[2]],
          entidad = "A",
          fecha = ymd("2026-04-30"),
          datos_observados = datos_observados_actuales()
        ),
        ifne(
          modelo_actual()[[2]],
          entidad = "A",
          fecha = ymd("2026-05-31"),
          datos_observados = datos_observados_actuales()
        ),
        ifne(
          modelo_actual()[[2]],
          entidad = "A",
          fecha = ymd("2026-06-30"),
          datos_observados = datos_observados_actuales()
        ),
        ifne(
          modelo_actual()[[2]],
          entidad = "A",
          fecha = ymd("2026-07-31"),
          datos_observados = datos_observados_actuales()
        ),
        ifne(
          modelo_actual()[[2]],
          entidad = "A",
          fecha = ymd("2026-08-31"),
          datos_observados = datos_observados_actuales()
        ),
        ifne(
          modelo_actual()[[2]],
          entidad = "A",
          fecha = ymd("2026-09-30"),
          datos_observados = datos_observados_actuales()
        )
      ),
      ifne_b = c(
        ifne(
          modelo_actual()[[2]],
          entidad = "B",
          fecha = ymd("2026-04-30"),
          datos_observados = datos_observados_actuales()
        ),
        ifne(
          modelo_actual()[[2]],
          entidad = "B",
          fecha = ymd("2026-05-31"),
          datos_observados = datos_observados_actuales()
        ),
        ifne(
          modelo_actual()[[2]],
          entidad = "B",
          fecha = ymd("2026-06-30"),
          datos_observados = datos_observados_actuales()
        ),
        ifne(
          modelo_actual()[[2]],
          entidad = "B",
          fecha = ymd("2026-07-31"),
          datos_observados = datos_observados_actuales()
        ),
        ifne(
          modelo_actual()[[2]],
          entidad = "B",
          fecha = ymd("2026-08-31"),
          datos_observados = datos_observados_actuales()
        ),
        ifne(
          modelo_actual()[[2]],
          entidad = "B",
          fecha = ymd("2026-09-30"),
          datos_observados = datos_observados_actuales()
        )
      ),
      ifne_c = c(
        ifne(
          modelo_actual()[[2]],
          entidad = "C",
          fecha = ymd("2026-04-30"),
          datos_observados = datos_observados_actuales()
        ),
        ifne(
          modelo_actual()[[2]],
          entidad = "C",
          fecha = ymd("2026-05-31"),
          datos_observados = datos_observados_actuales()
        ),
        ifne(
          modelo_actual()[[2]],
          entidad = "C",
          fecha = ymd("2026-06-30"),
          datos_observados = datos_observados_actuales()
        ),
        ifne(
          modelo_actual()[[2]],
          entidad = "C",
          fecha = ymd("2026-07-31"),
          datos_observados = datos_observados_actuales()
        ),
        ifne(
          modelo_actual()[[2]],
          entidad = "C",
          fecha = ymd("2026-08-31"),
          datos_observados = datos_observados_actuales()
        ),
        ifne(
          modelo_actual()[[2]],
          entidad = "C",
          fecha = ymd("2026-09-30"),
          datos_observados = datos_observados_actuales()
        )
      ),
      ifne_d = c(
        ifne(
          modelo_actual()[[2]],
          entidad = "D",
          fecha = ymd("2026-04-30"),
          datos_observados = datos_observados_actuales()
        ),
        ifne(
          modelo_actual()[[2]],
          entidad = "D",
          fecha = ymd("2026-05-31"),
          datos_observados = datos_observados_actuales()
        ),
        ifne(
          modelo_actual()[[2]],
          entidad = "D",
          fecha = ymd("2026-06-30"),
          datos_observados = datos_observados_actuales()
        ),
        ifne(
          modelo_actual()[[2]],
          entidad = "D",
          fecha = ymd("2026-07-31"),
          datos_observados = datos_observados_actuales()
        ),
        ifne(
          modelo_actual()[[2]],
          entidad = "D",
          fecha = ymd("2026-08-31"),
          datos_observados = datos_observados_actuales()
        ),
        ifne(
          modelo_actual()[[2]],
          entidad = "D",
          fecha = ymd("2026-09-30"),
          datos_observados = datos_observados_actuales()
        )
      ),
      ifne_e = c(
        ifne(
          modelo_actual()[[2]],
          entidad = "E",
          fecha = ymd("2026-04-30"),
          datos_observados = datos_observados_actuales()
        ),
        ifne(
          modelo_actual()[[2]],
          entidad = "E",
          fecha = ymd("2026-05-31"),
          datos_observados = datos_observados_actuales()
        ),
        ifne(
          modelo_actual()[[2]],
          entidad = "E",
          fecha = ymd("2026-06-30"),
          datos_observados = datos_observados_actuales()
        ),
        ifne(
          modelo_actual()[[2]],
          entidad = "E",
          fecha = ymd("2026-07-31"),
          datos_observados = datos_observados_actuales()
        ),
        ifne(
          modelo_actual()[[2]],
          entidad = "E",
          fecha = ymd("2026-08-31"),
          datos_observados = datos_observados_actuales()
        ),
        ifne(
          modelo_actual()[[2]],
          entidad = "E",
          fecha = ymd("2026-09-30"),
          datos_observados = datos_observados_actuales()
        )
      )
    )
  })
  
  # Gráfico de comparacion del ifne entre entidades
  output$grafico_comp_ifne <- renderPlotly({
    # Se filtra el gráfico correspondiente
    d <- df_ifne_comp() |>
      pivot_longer(
        cols = c(ifne_a, ifne_b, ifne_c, ifne_d, ifne_e),
        names_to = "serie",
        values_to = "valor"
      ) |>
      mutate(serie = factor(
        serie,
        levels = c("ifne_a", "ifne_b", "ifne_c", "ifne_d", "ifne_e"),
        labels = c("A", "B", "C", "D", "E")
      ))
    
    # Gráfico final
    plotly::plot_ly(
      data = d,
      x = ~ periodo,
      y = ~ valor,
      color = ~ serie,
      type = "scatter",
      mode = "lines+markers"
    ) |>
      plotly::layout(
        xaxis = list(title = "Fecha"),
        yaxis = list(title = "IFNE"),
        legend = list(title = list(text = "Medida"))
      )
  })
  
  # Función para hacer un archivo excel/csv de unos datos
  exportar_csv <- function(datos, archivo) {
    readr::write_excel_csv(datos, archivo, na = "")
  }
  
  # Descarga las brechas por plazo
  output$descargar_bandas <- downloadHandler(
    # Nombre del archivo
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
    
    # Contenido del archivo a descargar
    content = function(file) {
      # Se extraen los filtros actuales
      datos <- filtrar_como_dt(bandas_actuales(), input$tabla_bandas_rows_all)
      
      # Se crea el csv correspondiente
      exportar_csv(datos, file)
    }
  )
  
  # Descarga la tabla de rubros
  output$descargar_rubros <- downloadHandler(
    # Nombre del archivo
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
    
    # Contenido del archivo a descargar
    content = function(file) {
      # Se extraen los filtros actuales
      datos <- filtrar_como_dt(rubros_actuales(), input$tabla_rubros_rows_all)
      
      # Se crea el csv correspondiente
      exportar_csv(datos, file)
    }
  )
  
  # Descarga la tabla de saldos
  output$descargar_saldos <- downloadHandler(
    # Nombre del archivo
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
    
    # Contenido del archivo a descargar
    content = function(file) {
      # Contenido del archivo a descargar
      datos <- filtrar_como_dt(estructura_actual(), input$tabla_estructura_rows_all)
      
      # Se crea el csv correspondiente
      exportar_csv(datos, file)
    }
  )
  
  # Descarga la tabla de cobertura de faltantes
  output$descargar_cobertura <- downloadHandler(
    # Nombre del archivo
    filename = function()
      paste0(
        "cobertura_faltantes_",
        input$entidad,
        "_",
        input$corte,
        "_",
        input$moneda,
        ".csv"
      ),
    
    # Contenido del archivo a descargar
    content = function(file) {
      # Contenido del archivo a descargar
      datos <- filtrar_como_dt(bandas_cobertura_actuales(), input$tabla_cobertura_rows_all)
      
      # Se crea el csv correspondiente
      exportar_csv(datos, file)
    }
  )
  
  # Descarga la tabla de encaje de reservas
  output$descargar_encaje <- downloadHandler(
    # Nombre del archivo
    filename = function()
      paste0(
        "encaje_reservas_",
        input$entidad,
        "_",
        input$corte,
        "_",
        input$moneda,
        ".csv"
      ),
    
    # Contenido del archivo a descargar
    content = function(file) {
      # Contenido del archivo a descargar
      datos <- filtrar_como_dt(encaje_reservs_actual(), input$tabla_encaje_reservs_rows_all)
      
      # Se crea el csv correspondiente
      exportar_csv(datos, file)
    }
  )
  
  # Descarga la tabla de bandas con tipo de cambio
  output$descargar_bandas_tc <- downloadHandler(
    # Nombre del archivo
    filename = function()
      paste0(
        "bandas_",
        input$entidad,
        "_",
        input$corte,
        "_",
        input$moneda,
        ".csv"
      ),
    
    # Contenido del archivo a descargar
    content = function(file) {
      # Contenido del archivo a descargar
      datos <- filtrar_como_dt(bandas_tc_actual(), input$tabla_bandas_tc_rows_all)
      
      # Se crea el csv correspondiente
      exportar_csv(datos, file)
    }
  )
  
  # Descarga la tabla de los activos con mayor coste por liquidar
  output$descargar_liquidar <- downloadHandler(
    # Nombre del archivo
    filename = function()
      paste0(
        "activos_liquidar_",
        input$entidad,
        "_",
        input$corte,
        "_",
        input$moneda,
        ".csv"
      ),
    
    # Contenido del archivo a descargar
    content = function(file) {
      # Contenido del archivo a descargar
      datos <- filtrar_como_dt(tabla_liquidez(), input$tabla_coste_liq_rows_all)
      
      # Se crea el csv correspondiente
      exportar_csv(datos, file)
    }
  )
}

# Corre la aplicación juntando los elementos visuales y su funcionamiento interno
shinyApp(ui = ui, server = server)
