#
# This is a Shiny web application. 

library(shiny)
library(semantic.dashboard)

# Define UI
ui <- fluidPage(
  
  titlePanel(
    h1("Antibiotics in Syria mission")
  ),
  
  sidebarLayout(
    sidebarPanel(
      h3("controls")
    ),
    mainPanel(
      "plots",
      p("p creates a paragraph of text."),
      p("A new p() command starts a new paragraph. Supply a style attribute to 
        change the format of the entire paragraph.", 
        style = "font-family: 'times'; font-si16pt"),
      strong("strong() makes bold text."),
      em("em() creates italicized (i.e, emphasized) text."),
      br(),
      code("code displays your text similar to computer code"),
      div("div creates segments of text with a similar style. 
          This division of text is all blue because I passed the 
          argument 'style = color:blue' to div", 
          style = "color:blue"),
      br(),
      p("span does the same thing as div, but it works with",
        span("groups of words", style = "color:blue"),
        "that appear inside a paragraph."),
      img(src = "msf.png", height = 200, width = 400),
      img(src = "rstudio.png", height = 140, width = 400)
    )
  )
)

# Define server logic
server <- function(input, output) {
}

# Run the application 
shinyApp(ui = ui, server = server)
