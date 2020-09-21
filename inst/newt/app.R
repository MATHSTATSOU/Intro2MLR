

library(shiny)
library(Intro2MLR)

# Define UI for application that draws a polynomial
ui <- fluidPage(

    # Application title
    titlePanel("Quadratic roots"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            numericInput("a", "Coefficient of x squared, a:", 1, min = 1, max = 100),
            numericInput("b", "Coefficient of x, b:", -5, min = 1, max = 100),
            numericInput("c", "constant term, c:", 6, min = 1, max = 100),
            numericInput("x0", "Initial value:", 6, min = 1, max = 100)


        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {



    output$distPlot <- renderPlot({
      # create plot
        f <- function(x) input$a*x^2 + input$b*x + input$c
        fd <- function(x) 2*input$a * x + input$b

        mynewt(input$x0, f = f, fdash = fd)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
