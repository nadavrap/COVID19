library(shiny)
library(ggplot2)
library(directlabels)

function(input, output, session) {
    
    dataset <- reactive({
        #df <- to_long(get_raw_data())
        #df <- droplevels(df[df$Var==input$y,])
        df <- get_covid_data()
        df <- df[df$Var==input$y,]
        df <- df[df$Country %in% input$countries, ]
        droplevels(df)
    })
    
    output$plot <- renderPlot({
        # df_subset <- reactive({
        #     #a <- droplevels(subset(dataset(), 'Var' == input$y))
        #     a <- dataset()[dataset()[,'Var']==input$y,]
        #     return(a)
        # })
        
        #p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_point()
        #p <- ggplot(dataset()[dataset()[,'Var']==input$y,], aes(x=Days,y=value, col=Country)) +
        p <- ggplot(dataset(), aes(x=Days,y=value, col=Country)) +
            ylim(0,min(input$ylim, max(dataset()[,'value']))) +
            coord_cartesian(xlim=c(0, max(dataset()[,'Days']))) +
        #p <- ggplot(df_subset(), aes(x=Days,y=value, col=Country)) + 
            geom_line() +
            ylab(input$y) +
            guides(col = guide_legend(nrow = 38)) +
            #scale_colour_discrete(guide = 'none') +
            #scale_x_discrete(expand=c(0, 1)) +
            scale_y_continuous(trans=ifelse(input$logscale, 'log10', 'identity')) +
            geom_dl(aes(label = Country), method = list(dl.combine("last.points"), cex = 0.8)) 

        # Adjust legend height
        # panel_height = unit(1,"npc") - sum(ggplotGrob(p)[["heights"]][-3]) - unit(1,"line")
        # p <- p + guides(fill= guide_colorbar(barheight=panel_height))

        # if (input$color != 'None')
        #     p <- p + aes_string(color=input$color)
        # 
        # facets <- paste(input$facet_row, '~', input$facet_col)
        # if (facets != '. ~ .')
        #     p <- p + facet_grid(facets)
        # 
        # if (input$jitter)
        #     p <- p + geom_jitter()
        # if (input$smooth)
        #     p <- p + geom_smooth()
        
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
