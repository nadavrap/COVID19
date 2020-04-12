library(shiny)
library(ggplot2)
library(directlabels)
library(gridExtra) # for grid.arrange function

function(input, output, session) {
    
    dataset <- reactive({
        #df <- to_long(get_raw_data())
        #df <- droplevels(df[df$Var==input$y,])
        df <- get_covid_normalized()
        df <- df[df$Var==input$y,]
        df <- df[df$Country %in% input$countries, ]
        droplevels(df)
    })
    
    worldo <- reactive({
        covid <- get_worldometers_data()
        covid <- align_by_var_and_val(covid, var=input$alignby, value=input$alignvalue)
        covid <- covid[!is.na(covid[,input$var2plot]), ]
        covid <- covid[covid$Country %in% input$countries,]
        droplevels(covid)
    })
    
    output$plot <- renderPlot({
        p <- ggplot(dataset(), aes(x=Days,y=value, col=Country)) +
            coord_cartesian(xlim=c(0, max(dataset()[,'Days'])),
                            ylim=c(0,min(input$ylim, max(dataset()[,'value'])))) +
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
        cors <- regress(worldo(), input$var2plot, input$maxDaysOutcome)
        cors$`-Log10Pval` <- -log10(cors$Pval)
        # Order bars according to Cor
        g1 <- ggplot(cors, aes(x = reorder(Var, Cor), Cor, fill=`-Log10Pval`)) + geom_bar(stat="identity") +
            ylim(-1,1) +
            xlab('') +
            ggtitle(paste('Correlation of different variables with',input$var2plot)) +
            # Rotate x labels
            theme(axis.text.x = element_text(face = "bold", color = "#993333", 
                                               size = 12, angle = 45, hjust = 1))
        g2 <- outcome_plot(aggregate_and_merge_countries(worldo(), input$var2plot, input$maxDaysOutcome), input$var2plot)
        grid.arrange(g1, g2, ncol=1)
        #cowplot::plot_grid(g1, g2, align = "v", nrow = 2, rel_heights = c(2/3, 1/3))
    }, height=700)
    
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
        #var2plot <- input$var2plot
        # updateSliderInput(session, "maxDaysOutcome",
        #                   value = max(worldo()[,'Days']),
        #                   min = 0, max = max(worldo()[,'Days']), step = 1)
    })

}
