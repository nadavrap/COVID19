library(shiny)
library(shinythemes)
library(ggplot2)

source('./functions.R')

theme_set(theme_bw())

#d <- get_raw_data()
#df <- to_long(d)
df <- get_covid_data()

fluidPage(
    theme = shinytheme("superhero"),
    titlePanel("COVID19"),
    
    sidebarPanel(
        width = 3,
        
        #selectInput('x', 'X', names(d)),
        selectInput('y', 'Variable to plot', unique(df$Var), unique(df$Var)[[1]]),
        #selectInput('color', 'Color', c('None', names(d))),
        
        sliderInput('ylim', 'Limit Y axis', min=0, max=max(df$value[df$Var=='deaths']),
                     value=max(df$value[df$Var=='deaths']), step=1, round=0),
        
        checkboxInput('logscale', 'Log 10 Scale', value = FALSE),
        selectInput("countries", "Select countries",
                    choices = levels(df$Country),
                    selected = levels(df$Country),
                    multiple = TRUE
        ),
        #checkboxInput('smooth', 'Smooth'),
        
        #selectInput('facet_row', 'Facet Row', c(None='.', names(d))),
        #selectInput('facet_col', 'Facet Column', c(None='.', names(d)))
    ),
    
    mainPanel(
        paste("Contries are alingned so day 0 is the first day when the",
        "country passed 10 commulative death from COVID-19."),
        plotOutput('plot')
    )
)
