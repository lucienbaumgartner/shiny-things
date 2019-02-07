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
    counter <- 
      eval(
      parse(text=
              paste0(
                'reactiveValues(countervalue = 0,', 
                paste0(
                  paste0(
                    '.bin', 1:nbins), '=0', collapse = ','
                  ), 
                ')'
                )
            )
      )
    eval(
      parse(
        text = 
          lapply(1:nbins, function(x){
            paste0(
              paste0(
              'observeEvent(input$add', x, 
              ', {counter$countervalue <- counter$countervalue + jump'
              ),
              '\n',
              paste0(
              'counter$.bin', x, ' <- counter$.bin', x, ' + jump',
              '})'
              )
            )
          }) %>% 
          unlist
      )
    )
    eval(
      parse(
        text = 
          lapply(1:nbins, function(x){
            paste0(
              paste0(
                'observeEvent(input$sub', x, 
                ', {counter$countervalue <- counter$countervalue - jump'
              ),
              '\n',
              paste0(
                'counter$.bin', x, ' <- counter$.bin', x, ' - jump',
                '})'
              )
            )
          }) %>% 
          unlist
      )
    )
    observeEvent(input$reset, {
      eval(
        parse(
          text=
            paste0(
              c('counter$countervalue', paste0('counter$.bin', 1:nbins)), '<- 0'
            )
        )
      ) 
      
      # if the reset button is clicked, set the counter value to zero
    })
    output$count <- renderText({
      paste('There are', 100-counter$countervalue, 'left.') # print the latest value stored in the reactiveValues object
    })
    eval(
      parse(
        text=
          paste0(
            'output$count.bin', 1:nbins,'<- renderText({counter$.bin', 1:nbins,'})'
          )
      )
    )
    
    output$plot <- renderPlot({
      data <- 
        tibble(
          bin=1:nbins, 
          y=eval(
            parse(
              text=
                paste0(
                  'c(', paste0('counter$.bin', 1:nbins, collapse = ','),')'
                )
            )
          )
        )
      ggplot(data, aes(x=bin, y=y, fill=as.factor(bin))) +
        geom_bar(stat='identity', width=1) +
        scale_y_continuous(limits=c(0,100), expand = c(0,0)) +
        scale_x_continuous(expand = c(0,0)) +
        scale_fill_discrete()+
        theme_light()
    })
    
  }
  
)

ui <- shinyUI(
  fluidPage(
     title="Goldberg & Rothschild Question",
     br(),
     p('Some description'),
     fluidRow(column(1, div("Bins")),
              eval(
                parse(
                  text= 
                    sapply(1:nbins, function(x){
                      paste0('column(', x+1, ', div(textOutput("count.bin', x, '"), "Bin ', x, '", br(), actionButton("add', x, '", "+ ', jump, '"), actionButton("sub', x, '", "- ', jump, '")))')
                      })
                )
              ), 
              #column(2, div(textOutput("count.bin1"), 'Bin 1', br(), actionButton("add1", "+ 1"), actionButton("sub1", "- 1"))),
              #column(3, div(textOutput("count.bin2"), 'Bin 2', br(), actionButton("add2", "+ 1"), actionButton("sub2", "- 1"))),
              column(4, br(), div(actionButton("reset", "Reset")))
              ),
     plotOutput("plot", width = "50%"),
     textOutput("count")
  )
)

shinyApp(ui = ui, server = server)
