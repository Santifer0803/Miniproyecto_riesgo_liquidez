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
               shiny)

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
