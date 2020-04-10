library(shiny)
library(ggplot2)
library(directlabels)

function(input, output, session) {
    
    dataset <- reactive({
        #df <- to_long(get_raw_data())
        #df <- droplevels(df[df$Var==input$y,])
        df <- get_covid_normalized()
        df <- df[df$Var==input$y,]
        df <- df[df$Country %in% input$countries, ]
        droplevels(df)
    })
    
    output$plot <- renderPlot({
        p <- ggplot(dataset(), aes(x=Days,y=value, col=Country)) +
            ylim(0,min(input$ylim, max(dataset()[,'value']))) +
            coord_cartesian(xlim=c(0, max(dataset()[,'Days']))) +
            geom_line() +
            ylab(input$y) +
            guides(col = guide_legend(nrow = 38)) +
            scale_y_continuous(trans=ifelse(input$logscale, 'log10', 'identity')) +
            geom_dl(aes(label = Country), method = list(dl.combine("last.points"), cex = 0.8)) 

        print(p)
        
    }, height=700)
    
    
    
    observe({
        y <- input$y
        # Control the value, min, max, and step.
        # Step size is 2 when input value is even; 1 when value is odd.
        updateSliderInput(session, "ylim", value = max(dataset()[,'value']),
                          min = 0, max = max(dataset()[,'value']), step = 1)
        #ylim(0,min(input$ylim, max(dataset()[,'value']))) +
    })
    
}
