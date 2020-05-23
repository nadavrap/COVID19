library(shiny)
library(shinythemes)
library(ggplot2)

source('./functions.R')

theme_set(theme_bw())

#d <- get_raw_data()
#df <- to_long(d)
covid <- get_covid_normalized()
worldometer <- get_worldometers_data(as.Date(DEFAULT_DATE))
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

                    sliderInput('ylim', 'Limit Y axis', min=0, 
                                max=max(covid$value[covid$Var=='deaths']),
                                value=max(covid$value[covid$Var=='deaths']), 
                                step=1, round=0),
                    
                    checkboxInput('logscale', 'Log 10 Scale', value = FALSE),
                    selectInput("countries", "Select countries",
                                choices = unique(covid$Country),
                                selected = unique(covid$Country),
                                multiple = TRUE
                    ),
                    sliderInput('maxDays', 'Outcome at day', min=1, 
                                max=max(covid$Days),
                                value=max(covid$Days), step=1, round=0),
                ),
                
                mainPanel(
                    paste("countries are alingned so day 0 is the first day when the",
                          "country passed 10 commulative death from COVID-19."),
                    # Output: Tabset w/ plot, summary, and table ----
                    tabsetPanel(type = "tabs",
                                tabPanel("Plot", plotOutput("plot"))
                    )
                )
            ),
       # Worldometer Data
       tabPanel("Worldometer Data",
                sidebarPanel(
                    #width = 3,
                    dateInput("end_date", "Latest Date:", value = DEFAULT_DATE, 
                              max = Sys.Date(),
                              min=as.Date('2020-01-29')),
                    selectInput('alignby', 'Align countries by variable', 
                                names(worldometer)[2:(ncol(worldometer)-1)], 
                                'total_deaths_per_1M'),
                    sliderInput('alignvalue', 'Align countries by variable having at least:', 
                                min=0, max=20,
                                value=DEFAULT_MIN_VAL, step=.25, round=2),
                    selectInput('var2plot', 'Variable to plot', 
                                names(worldometer)[2:(ncol(worldometer)-1)],
                                "total_deaths_per_1M"),

                    sliderInput('ylim_plot', 'Limit Y axis', min=0, max=round(max(worldometer[,'total_deaths_per_1M'], na.rm = TRUE)),
                                value=max(worldometer[,'total_deaths_per_1M'], na.rm=TRUE), step=1, round=0),

                    radioButtons('depended_var', 'BCG administrated years',
                                 choiceNames = c('Strict', 'Imputed'), 
                                 selected = 'BCG administration years',
                                 inline = TRUE, width = NULL, 
                                 choiceValues = c('BCG administration years',
                                                  "BCG admin years (Imp)")),
                    
                    #checkboxInput('removeBCG', 'Remove BCG from multivar', value = FALSE),
                    #checkboxInput('removePS', 'Remove PS vars from multivar', value = FALSE),
                    #checkboxInput('ps25only', 'Remove ps25 and ps65', value = FALSE),
                    sliderInput('maxDaysOutcome', 'Outcome at day', min=1, 
                                max=max(worldometer$Days),
                                value=20, step=1, round=TRUE),
                    
                    sliderInput("country_size", 
                                label = 'Limit countries by population (in millions)', 
                                min = 0, 
                                max = ceiling(max(worldometer$`population size (M)`, na.rm = TRUE)),
                                value = c(0, ceiling(max(worldometer$`population size (M)`, na.rm = TRUE))),
                                step=1, round=0),
                    
                    # sliderInput('country_size', 
                    #             'Limit countries by population (in millions)', 
                    #             min=0, 
                    #             max=ceiling(max(worldometer$`population size (M)`, na.rm = TRUE)),
                    #             value=ceiling(max(worldometer$`population size (M)`, na.rm = TRUE)),
                    #             step=1, round=0),
                    selectInput("selected_countries", "Selected countries",
                                choices = sort(unique(worldometer$Country)),
                                selected = sort(unique(worldometer$Country)),
                                multiple = TRUE,
                                selectize = FALSE # Disable user changes
                    ),
                ),
                
                mainPanel(
                    paste("countries are alingned so day 0 is the first day", 
                          "when the variable 'Align countries by variable'",
                          "as selected on the left panel passed the value", 
                          "assigned to 'Align countries by variable having at",
                          "least'."),
                    # Output: Tabset w/ plot, summary, and table ----
                    
                    tabsetPanel(type = "tabs",
                                selected='Plot',
                                tabPanel("Intro",
                                         includeMarkdown("./md/intro.Rmd"),
                                         hr()),
                                tabPanel("Plot", 
                                         textOutput("regression_output"),
                                         plotOutput("plotW1")),
                                tabPanel("Regressions",
                                         #textOutput("regression_output"),
                                         plotOutput("plotRegressions", height=1300),
                                         downloadButton('downloadPlot', 'Download Plot')),
                                tabPanel("Correlations", plotOutput("corPlot", height = 500),
                                         downloadButton('downloadCors', 'Download Plot'),
                                         downloadButton('downloadCorsData', 'Download Data'),),
                                tabPanel('Univariable', 
                                         #textOutput("regression_output"),
                                         plotOutput('univarOut'),
                                         height=500),#downloadButton('downloadMultivarDarta', 'Download Data'),),
                                tabPanel('Multivariable', 
                                         #textOutput("regression_output"),
                                         plotOutput('multivarOut', height = 500),
                                         downloadButton('downloadMultivarDarta', 'Download Data'),)
                                #tabPanel('Decision Tree', plotOutput('decisionTree'))
                                #tabPanel("Summary", verbatimTextOutput("summary")),
                    )
                )
       )
       # close the UI definition
))

