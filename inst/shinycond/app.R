
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),


    sidebarLayout(
        sidebarPanel(
            sliderInput("percData",
                        "Percentage of data:",
                        min = 1,
                        max = 100,
                        value = 50),

            sliderInput("percData2",
                        "Percentage of data remaining:",
                        min = 0,
                        max = 100,
                        value = 50)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("hist1")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$hist1 <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        l <- length(x)
        UP <- floor(input$percData/100*l)


        # draw the histogram
        hist(sort(x)[1:UP],
             col = 'darkgray',
             border = 'white',
             xlab = "Time",
             main = paste0("First ", input$percData,"% of Data"))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
