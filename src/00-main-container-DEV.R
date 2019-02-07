library(shiny)
library(ggplot2)
library(dplyr)

server <- shinyServer(
  function(input, output, session) {
    # define value jumps for the counters
    jump <- 10
    # define bins
    nbins <- 2
    # define and initialize the reactiveValues object depending on the number of bins
    counter <- reactiveValues(countervalue = 0, .bin1=0, .bin2=0)
    
     observeEvent(input$add1, {
       counter$countervalue <- counter$countervalue + jump
       counter$.bin1 <- counter$.bin1 + jump
       })
     observeEvent(input$add2, {
       counter$countervalue <- counter$countervalue + jump
       counter$.bin2 <- counter$.bin2 + jump
     })
     eval(expression(observeEvent(input$sub1, {
       counter$countervalue <- counter$countervalue - jump
       counter$.bin1 <- counter$.bin1 - jump
     }),
     observeEvent(input$sub2, {
       counter$countervalue <- counter$countervalue - jump
       counter$.bin2 <- counter$.bin2 - jump
     })))
    
    output$count <- renderText({
      paste('There are', 100-counter$countervalue, 'left.') # print the latest value stored in the reactiveValues object
    })
    output$plot <- renderPlot({
      data <- 
        tibble(
          bin=1:nbins, 
          y=c(counter$.bin1, counter$.bin2)
          )
      ggplot(data, aes(x=bin, y=y)) +
        geom_bar(stat='identity', width=0.5) +
        scale_y_continuous(limits=c(0,100), expand = c(0,0)) +
        scale_x_continuous(expand = c(0,0)) +
        theme_void()
    })
    
  }
  
)

ui <- shinyUI(
  fluidPage(
    tags$b('Goldberg & Rothschild Question'),
    br(),
    plotOutput("plot", width = "90%"),
    actionButton("add1", "+ 1"),
    actionButton("sub1", "- 1"),
    actionButton("add2", "+ 1"),
    actionButton("sub2", "- 1"),
    actionButton("reset", "Reset"),
    br(),
    textOutput("count")
  )
  
)

shinyApp(ui = ui, server = server)
