

library(shiny)
library(ggplot2)
library(Intro2MLR)
data(ddt)

# Define UI for application that draws a ggplot
ui <- fluidPage(

    # Application title
    titlePanel("Conditional inputs"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            checkboxInput("smooth", "Smooth"),
            conditionalPanel(
                condition = "input.smooth == true",
                selectInput("smoothMethod", "Method",
                            list("lm", "glm", "gam", "loess", "rlm"))
            ),
            checkboxInput("catvar", "Fill Variable"),
            conditionalPanel(
                condition = "input.catvar == true",
                selectInput("catVarFill", "Categorical Variable",
                            list("SPECIES","RIVER"))

            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)


server <- function(input, output) {

    output$distPlot <- renderPlot({

    ggplot(ddt, aes(x=LENGTH,y=WEIGHT))+ geom_smooth(method =input$smoothMethod, aes(fill = ddt[,input$catVarFill]))

    })
}

# Run the application
shinyApp(ui = ui, server = server)
