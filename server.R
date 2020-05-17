library(shiny)
library(ggplot2)
library(directlabels)
library(gridExtra) # for grid.arrange function

library(rpart) # for decision tree
library(rpart.plot)
library(rattle) # for fancyRpartPlot function

source('./functions.R')

function(input, output, session) {
    
    dataset <- reactive({
        #df <- to_long(get_raw_data())
        df <- get_covid_normalized()
        df <- df[df$Var==input$y,]
        df <- df[df$Country %in% input$countries, ]
        if (input$logscale) { # Need to remove zeros
            df <- df[df$value != 0,]
        }
        droplevels(df)
    })

    worldo <- reactive({
        covid <- get_worldometers_data(input$end_date)
        covid <- align_by_var_and_val(covid, var=input$alignby, value=input$alignvalue)
        covid <- covid[!is.na(covid[,input$var2plot]), ]
        covid <- covid[covid$`population size (M)` >= input$country_size[1] &
                           covid$`population size (M)` <= input$country_size[2], ]
        #covid <- covid[covid$Country %in% input$selected_countries,]
        droplevels(covid)
    })
    
    countriesBCG <- reactive({
        # covid <- get_worldometers_data(input$end_date)
        # covid <- align_by_var_and_val(covid, var=input$alignby, value=input$alignvalue)
        # covid <- covid[!is.na(covid[,input$var2plot]), ]
        # covid <- droplevels(covid[covid$Country %in% input$selected_countries,])
        # covid <- covid[covid$`population size (M)` >= input$country_size[1] &
        #                    covid$`population size (M)` <= input$country_size[2], ]
        # covid <- droplevels(covid)
        # 
        x <- aggregate_and_merge_countries(worldo(), input$var2plot, input$maxDaysOutcome)
        x
    })
    
    output$plot <- renderPlot({
        miny <- ifelse(input$logscale, 1, 0)
        p <- ggplot(dataset(), aes(x=Days,y=value, col=Country)) +
            coord_cartesian(xlim=c(0, max(dataset()[,'Days'])),
                            ylim=c(miny,min(input$ylim, max(dataset()[,'value'])))) +
            geom_line() +
            ylab(input$y) +
            guides(col = guide_legend(nrow = 38)) +
            scale_y_continuous(trans=ifelse(input$logscale, 'log10', 'identity')) +
            geom_dl(aes(label = Country), method = list(dl.combine("last.points"), cex = 0.8)) 

        print(p)
        
    }, height=700)
    
    output$plotW1 <- renderPlot({
        p <- ggplot(worldo(), aes_string(x='Days',y=input$var2plot, col='Country')) +
            coord_cartesian(xlim=c(0, max(worldo()[,'Days'])),
                            ylim=c(0,min(input$ylim_plot, max(worldo()[,input$var2plot], na.rm = TRUE)))) +
            xlab(paste('Days since', input$alignby, '>=', input$alignvalue)) +
            geom_line() +
            ylab(input$var2plot) +
            guides(col = guide_legend(nrow = 38)) +
            #scale_y_continuous(trans=ifelse(input$log10scale, 'log10', 'identity')) +
            geom_dl(aes(label = Country), method = list(dl.combine("last.points"), cex = 0.8)) 
        
        print(p)
        
    }, height=700)
    
    output$plotRegression <- renderPlot({
        #warning(input$maxDaysOutcome)
        #warning(paste('Rows:', nrow(countriesBCG())))
        cors <- regress(countriesBCG(), input$var2plot)
        #warning(nrow(cor))
        if(is.null(cors)) {
            g1 <- ggplot() + 
                annotate("text", x = 4, y = 25, size=8, 
                         label = paste('No correlations, probably too little',
                                       '\ncontries satisfied the filters')) + 
                theme_bw() +
                theme(panel.grid.major=element_blank(),
                      panel.grid.minor=element_blank())
        } else {
        #warning(dim(cors))
            cors$`-Log10Pval` <- -log10(cors$Pval)
            cors$`-Log10Pval`[cors$Pval > 0.05] <- NA
            cors$Pval <- ifelse(cors$Pval > 0.05, NA, formatC(cors$Pval, format = "e", digits = 2))
            #cors$Pval <- formatC(cors$Pval, format = "e", digits = 2)
            #cors$Pval[is.na(cors$Pval)] <- ''
            
            # Order bars according to Cor
            g1 <- ggplot(cors, aes(x = reorder(Var, Cor), Cor, fill=`-Log10Pval`)) + geom_bar(stat="identity") +
                ylim(-1,1) +
                xlab('') +
                ggtitle(paste('Correlation of different variables with',input$var2plot)) +
                # Rotate x labels
                theme(axis.text.x = element_text(face = "bold", color = "#993333", 
                                                   size = 12, angle = 45, hjust = 1)) +
                geom_text(aes(label=Pval), position=position_dodge(width=0.9), vjust=-1)
        }
        
        g2 <- outcome_plot(countriesBCG(), input$var2plot)
        #grid.arrange(g1, g2, ncol=1, rel_heights = c(1/10, 9/10))
        cowplot::plot_grid(g1, g2, ncol=1, rel_heights = c(1/4, 3/4))
        #cowplot::plot_grid(g1, g2, align = "v", nrow = 2, rel_heights = c(2/3, 1/3))
    })
    
    output$regression_output <- renderText({ 
        paste('Data is aligned according to first day the contry had at least',
              input$alignvalue, 'cases of', input$alignby, '.\n',
              'Outcome was defined as the difference between day',
              input$maxDaysOutcome, 'and day 0 of', input$var2plot, '.\n',
              'Found', 
              nrow(countriesBCG()),
              'countries satisfying the requirements.'
              )
    })
    
    output$corPlot <- renderPlot({
        get_stats_table(input$alignby, input$alignvalue, 
                        depended_var=input$depended_var, 
                        input$end_date, 
                        selected_countries=input$selected_countries,
                        get_data_only=FALSE,
                        country_size_range=input$country_size)
    })
    
    output$decisionTree <- renderPlot({
        decisionTree(countriesBCG(), input$var2plot)
    })
    
    output$multivarOut <- renderPlot({
        multi_var(countriesBCG(), input$var2plot, depended_var=input$depended_var,
                  remove_BCG=FALSE, remove_ps=TRUE, #remove_ps=input$removePS
                  )
    })
    
    # Save plots
    output$downloadPlot <- downloadHandler(
        filename = 
            paste0('COVID_19_BCG_', 
                   format(Sys.Date(), '%Y_%m_%d'), '.pdf'),
        content = function(file) {
            ggsave(file, plot = outcome_plot(countriesBCG(), input$var2plot), 
                   device = "pdf", width = 7, height = 20)
        }
    )
    
    output$downloadCors <- downloadHandler(
        filename = paste0('COVID_19_BCG_Correlation_Table_', 
                          format(Sys.Date(), '%Y_%m_%d'), '.pdf'),
        content = function(file) {
            ggsave(file, plot=get_stats_table(input$alignby, input$alignvalue, 
                                              depended_var=input$depended_var, 
                                              input$end_date, 
                                              selected_countries=input$selected_countries,
                                              get_data_only=FALSE,
                                              country_size_range=input$country_size),
                   device='pdf', width = 12, height = 7)
        }
    )
    
    
    output$downloadCorsData <- downloadHandler(
        filename = paste0('COVID_19_stats_table_data' ,
                          format(Sys.Date(), '%Y_%m_%d'), '.csv'),
        content = function(file) {
            write.csv(get_stats_table(input$alignby, input$alignvalue,
                                      depended_var=input$depended_var,
                                      input$end_date,
                                      selected_countries=input$selected_countries,
                                      get_data_only=TRUE,
                                      country_size_range=input$country_size),
                      file, row.names = TRUE)
        }
    )
    
    output$downloadMultivarDarta <- downloadHandler(
        filename = paste0('COVID_19_Multivariable_Results_' ,
                   format(Sys.Date(), '%Y_%m_%d'), '.csv'),
        content = function(file) {
            write.csv(multi_var(countriesBCG(), input$var2plot, 
                                depended_var=input$depended_var,
                                remove_BCG=FALSE, 
                                remove_ps=TRUE,#remove_ps=input$removePS,
                                get_data=TRUE),
                      file, row.names = TRUE)   
        }
    )
    
    observe({
        y <- input$y
        updateSliderInput(session, "ylim", value = max(dataset()[,'value']),
                          min = 0, max = max(dataset()[,'value']), step = 1)
    })
    observe({
        y <- input$var2plot
        updateSliderInput(session, "ylim_plot", value = round(max(worldo()[,input$var2plot], na.rm = TRUE)),
                          min = 0, max = round(max(worldo()[,input$var2plot], na.rm = TRUE)), step = 1)
    })
    
    observe({
        y <- input$alignby
        #yval <- input$alignvalue
        max_val <- round(max(worldo()[,y], na.rm = TRUE))
        if (max_val > 1000) {
            step <- 50
            #yval <- round(yval)
        } else {
            step <- .25
            max_val <- 20
        }
        updateSliderInput(session, "alignvalue", #value = yval,
                          min = 0, max = max_val, step = step)
    })
    
    observe({
        c_range <- input$country_size
        countries_list <- unique(worldo()[worldo()$`population size (M)` >= c_range[1] &
                                              worldo()$`population size (M)` <= c_range[2], 'Country'])
        updateSelectInput(session, "selected_countries",
                          choices = unique(get_Danielle_data()$Country),
                          selected = countries_list)
    })
    
    # observe({
    #     country_list <- input$alignby
    #     val <- input$alignvalue
    #     updateSelectInput(session, 'country_list',
    #                       choices = unique(worldo()[,'Country']),
    #                       selected = input$country_list)
    # })
}
