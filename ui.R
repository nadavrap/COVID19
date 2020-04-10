library(shiny)
library(shinythemes)
library(ggplot2)

source('./functions.R')

theme_set(theme_bw())

#d <- get_raw_data()
#df <- to_long(d)
covid <- get_covid_normalized()

fluidPage(
    theme = shinytheme("superhero"),
    titlePanel("COVID19"),
    
    sidebarPanel(
        width = 3,
        
        #selectInput('x', 'X', names(d)),
        selectInput('y', 'Variable to plot', unique(covid$Var), unique(covid$Var)[[1]]),
        #selectInput('color', 'Color', c('None', names(d))),
        
        sliderInput('ylim', 'Limit Y axis', min=0, max=max(covid$value[covid$Var=='deaths']),
                     value=max(covid$value[covid$Var=='deaths']), step=1, round=0),
        
        checkboxInput('logscale', 'Log 10 Scale', value = FALSE),
        selectInput("countries", "Select countries",
                    choices = levels(covid$Country),
                    selected = levels(covid$Country),
                    multiple = TRUE
        ),
        #checkboxInput('smooth', 'Smooth'),
        
        #selectInput('facet_row', 'Facet Row', c(None='.', names(d))),
        #selectInput('facet_col', 'Facet Column', c(None='.', names(d)))
    ),
    
    mainPanel(
        paste("countries are alingned so day 0 is the first day when the",
        "country passed 10 commulative death from COVID-19."),
        # Output: Tabset w/ plot, summary, and table ----
        tabsetPanel(type = "tabs",
                    tabPanel("Plot", plotOutput("plot"))#,
                    #tabPanel("Summary", verbatimTextOutput("summary")),
                    #tabPanel("Table", tableOutput("table"))
        )
        
        #plotOutput('plot')
    )
)
