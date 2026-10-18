#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Librerias necesarias
pacman::p_load(janitor,
               readxl,
               shiny,
               tidyverse)

# Datos necesarios
datos_contables <- read_excel("data/miniLiquidez_DATOS.xlsx", sheet = "Datos_Contables")
datos_contrato <- read_excel("data/miniLiquidez_DATOS.xlsx", sheet = "Pagos_Contractuales")
datos_observados <- read_excel("data/miniLiquidez_DATOS.xlsx", sheet = "Observaciones")
datos_tc <- read_excel("data/miniLiquidez_DATOS.xlsx", sheet = "Tipo_Cambio")
datos_catalogo <- read_excel("data/miniLiquidez_DATOS.xlsx", sheet = "Catalogo_Usado")

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

# Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )

# Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white',
#              xlab = 'Waiting time to next eruption (in mins)',
#              main = 'Histogram of waiting times')
#     })
# }

# Run the application 
shinyApp(ui = ui, server = server)
