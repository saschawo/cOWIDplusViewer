library(shiny)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     div(style="z-index: 999; position: fixed; top: 40%; left: 50%",
                         img(src = "https://kdm-manager.com/media/loading_io.gif", width = "50px", height = "50px"))),
    # Application title
    titlePanel("cOWIDplus Viewer"),

    # Sidebar with a slider input for number of bins
    tabsetPanel(
        tabPanel("Main", fluid = T,
                 sidebarLayout(
                     sidebarPanel(
                         textInput("searchPatterns", "Search patterns", value = "coronavirus,corona-krise",
                                   placeholder = "Enter search patterns..."),
                         h6("Separate several patterns with a comma. Lower/upper case is being ignored."),
                         radioButtons("inOrAll", label = "Type of search",
                                      choices = list("Search pattern matches word form exactly" = "all",
                                                     "Search pattern appears somewhere in word form" = "in"), selected = "all"),
                         dateRangeInput("dateRange", "Date range", start = "2020-01-01", end = "2020-07-02", # this has to be adapted whenever new data is available
                                        language = "en", separator = "to", min = "2020-01-01", max = "2020-07-02", # this has to be adapted whenever new data is available
                                        format = "yyyy-mm-dd"),
                         numericInput("rollWindow", "Window for rolling mean (in days)", value = 6, min = 1, max = 14),
                         h6("Window size of 1 indicates that no smoothing is being done."),
                         tags$head(
                             tags$style(HTML('#button{background-color:#5eaee7;color:white}'))
                         ),
                         actionButton("button", "Calculate", icon = icon("cog")),
                         hr(),
                         div(style = "display: flex; justify-content: flex-start",
                             div(style = "margin: 2px", downloadButton("download", "Data")),
                             div(style = "margin: 2px", downloadButton("downloadPNG", "PNG")),
                             div(style = "margin: 2px", downloadButton("downloadPDF", "PDF"))
                         ),
                         hr(),
                         h6("Data as of July 2nd, 2020"), # this has to be adapted whenever new data is available
                         h6("Program area 'Lexik empirisch & digital', IDS Mannheim"),
                         h6(a("Deutsche Version", href="https://owid.shinyapps.io/cOWIDplusViewer_BigramSearch",
                              target = "_self"))
                     ),
                     mainPanel(
                         plotOutput("resultPlot"),
                         
                         conditionalPanel(condition = "input.inOrAll == 'in'",
                                          DT::dataTableOutput("hit.pat.insts", width = "100%"))
                     )
                 )),
        tabPanel("Bigram finder", fluid = T,
                 sidebarLayout(
                     sidebarPanel(
                         textInput("bigramSearch", label = "Bigram part",
                                   value = "corona", placeholder = "Enter search pattern..."),
                         h6("You can only enter one search pattern."),
                         radioButtons("front_end_both", label = "Type of search",
                                      choices = list("Pattern somewhere in bigrams" = "egal",
                                                     "Pattern as first word" = "vorne",
                                                     "Pattern as second word" = "hinten"), selected = "egal"),
                         tags$head(
                             tags$style(HTML('#button-search{background-color:#5eaee7;color:white}'))
                         ),
                         actionButton("button-search", "Search", icon = icon("cog"))
                         
                     ),
                     mainPanel(
                         DT::dataTableOutput("bigram.results", width = "100%")
                     )
                 )
        )
        )
))
