library(shiny)
library(shinythemes)
library(ggplot2)

source('./functions.R')

theme_set(theme_bw())

#d <- get_raw_data()
#df <- to_long(d)
covid <- get_covid_normalized()
worldometer <- get_worldometers_data()
all_countries <- sort(unique(worldometer$Country))
worldometer <- align_by_var_and_val(worldometer, var='total_deaths_per_1M', DEFAULT_MIN_VAL)

shinyUI(navbarPage("COVID19",
                   theme = shinytheme("superhero"),           
       # define the tabs to be used in the app ---------------------------------
       selected="Worldometer Data",
       # introduction splash
       tabPanel("Intro",
                includeMarkdown("./md/intro.Rmd"),
                hr()),
       # CSSEGISand Data
       tabPanel("CSSEGISand Data",
                sidebarPanel(
                    width = 3,
                    
                    #selectInput('x', 'X', names(d)),
                    selectInput('y', 'Variable to plot', unique(covid$Var), 
                                unique(covid$Var)[[1]]),
                    #selectInput('color', 'Color', c('None', names(d))),
                    
                    sliderInput('ylim', 'Limit Y axis', min=0, 
                                max=max(covid$value[covid$Var=='deaths']),
                                value=max(covid$value[covid$Var=='deaths']), 
                                step=1, round=0),
                    
                    checkboxInput('logscale', 'Log 10 Scale', value = FALSE),
                    selectInput("countries", "Select countries",
                                choices = levels(covid$Country),
                                selected = levels(covid$Country),
                                multiple = TRUE
                    ),
                    sliderInput('maxDays', 'Outcome at day', min=1, 
                                max=max(covid$Days),
                                value=max(covid$Days), step=1, round=0),
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
            ),
       # Worldometer Data
       tabPanel("Worldometer Data",
                sidebarPanel(
                    #width = 3,
                    selectInput('alignby', 'Align countries by variable', 
                                names(worldometer)[2:(ncol(worldometer)-1)], 
                                'total_deaths_per_1M'),
                    sliderInput('alignvalue', 'Align countries by variable having at least:', 
                                min=0, max=20,
                                value=DEFAULT_MIN_VAL, step=.25, round=2),
                    #selectInput('x', 'X', names(d)),
                    selectInput('var2plot', 'Variable to plot', 
                                names(worldometer)[2:(ncol(worldometer)-1)],
                                "total_deaths_per_1M"),
                    #selectInput('color', 'Color', c('None', names(d))),
                    
                    sliderInput('ylim_plot', 'Limit Y axis', min=0, max=round(max(worldometer[,'total_deaths_per_1M'], na.rm = TRUE)),
                                value=max(worldometer[,'total_deaths_per_1M'], na.rm=TRUE), step=1, round=0),
                    
                    #checkboxInput('log10scale', 'Log 10 Scale', value = FALSE),
                    selectInput("country_list", "Select countries",
                                choices = all_countries,
                                selected = all_countries,
                                multiple = TRUE
                    ),
                    sliderInput('maxDaysOutcome', 'Outcome at day', min=1, 
                                max=max(worldometer$Days),
                                value=20, step=1, round=TRUE),
                    #checkboxInput('smooth', 'Smooth'),
                    
                    #selectInput('facet_row', 'Facet Row', c(None='.', names(d))),
                    #selectInput('facet_col', 'Facet Column', c(None='.', names(d)))
                ),
                
                mainPanel(
                    paste("countries are alingned so day 0 is the first day", 
                          "when the variable 'Align countries by variable'",
                          "as selected on the left panel passed the value", 
                          "assigned to 'Align countries by variable having at",
                          "least'."),
                    # Output: Tabset w/ plot, summary, and table ----
                    tabsetPanel(type = "tabs",
                                selected='Regression',
                                tabPanel("Plot", plotOutput("plotW1")),
                                tabPanel("Regression", 
                                         textOutput("regression_output"),
                                         plotOutput("plotRegression")),
                                tabPanel("Correlations", plotOutput("corPlot")),
                                tabPanel('Multivariable', plotOutput('multivarOut'))
                                #tabPanel("Summary", verbatimTextOutput("summary")),
                                #tabPanel("Table", tableOutput("table"))
                    )
                    
                    #plotOutput('plot')
                )
       )
       # close the UI definition
))

