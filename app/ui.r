# user interface
ui <- fluidPage(
  theme = shinythemes::shinytheme("cosmo"),
  titlePanel(title = "the egos of Trumpworld"),
  tags$head(tags$style(HTML(paste(
    ".selectize-input, .selectize-dropdown {font-size: 80%; line-height: 90%;}"
  )))),
  fluidRow(
    # abstract
    HTML(paste(readLines("app/abs.txt"), collapse = ""))
  ),
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      width = 3,
      tags$head(tags$style("#plot{height:100vh !important;}")),
      wellPanel(
        h4("Specifics"),
        selectInput(
          inputId = "nodes", label = "Nodes",
          choices = c("All", "Organizations", "Persons"),
          selected = "All"
        ),
        #checkboxInput(
        #  inputId = "implicit", label = "Implicit links",
        #  value = FALSE
        #),
        uiOutput(outputId = "egos"),
        sliderInput(
          inputId = "order", label = "Distance from ego",
          min = 1, max = 4, value = 2, step = 1
        )
      ),
      wellPanel(
        h4("Aesthetics"),
        checkboxInput(
          inputId = "code.links", label = "Color-code links",
          value = TRUE
        ),
        sliderInput(
          inputId = "node.size", label = "Node size",
          min = .5, max = 6, value = 3, step = .1
        ),
        #sliderInput(
        #  inputId = "conn.thres", label = "Frequent link types",
        #  min = 1, max = 50, value = 15, step = 1
        #),
        selectInput(
          inputId = "layout.alg", label = "Layout",
          choices = c("Tree", "Star", "Force-directed")
        ),
        selectInput(
          inputId = "label.scheme", label = "Label",
          choices = c("All", "High-degree", "High-betweenness", "None")
        )
      )
    ),
    mainPanel = mainPanel(
      width = 9,
      tabsetPanel(
        id = "ego",
        tabPanel(
          title = "Table",
          div(shiny::dataTableOutput("table"), style = "font-size:80%")
          #shiny::dataTableOutput("table")
        ),
        tabPanel(
          title = "Plot",
          plotOutput(
            "plot",
            hover = "plot.hover",
            brush = brushOpts(id = "plot.brush",
                              delayType = "debounce",
                              resetOnNew = TRUE)
          ),
          verbatimTextOutput("info")
        )
      )
    )
  )
)
