## Only run this example in interactive R sessions
#' How to control one widget from another
#'
#'
#' @return Interactive browser with two widgets
#'
#' @export
#'
#' @examples
#' \dontrun{rcwidgets()}
rcwidgets <- function(){
  library(shiny)
if (interactive()) {
  shinyApp(
    ui = fluidPage(
      sidebarLayout(
        sidebarPanel(
          p("The first knot xk1, controls the second xk2"),
          sliderInput("control", "Controller:", min=0, max=20, value=10,
                      step=1),
          sliderInput("receive", "Receiver:", min=0, max=20, value=10,
                      step=1)
        ),
        mainPanel()
      )
    ),
    server = function(input, output, session) {
      observe({
        val <- input$control
        # Control the value, min, max, and step.

        updateSliderInput(session, "receive", value = val,
                          min = val, max = 20, step = 1)
      })
    }
  )
}
}
