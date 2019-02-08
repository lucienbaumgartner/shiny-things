library(shiny)
library(shinyjs)
library(ggplot2)
library(dplyr)
rm(list=ls())
server <- shinyServer(
  function(input, output, session) {
    # define value jumps for the counters
    jump <- 10
    # define bins
    nbins <- 4
    # define and initialize the reactiveValues object depending on the number of bins
    l <- reactiveValues(lala=0, loli=1)
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
              ', {counter$.bin', x, ' <- counter$.bin', x, ' + ifelse(counter$.bin', x, '==100|counter$countervalue==100, 0, jump)'
              ),
              '\n',
              paste0(
              'counter$countervalue <- counter$countervalue + ifelse(counter$countervalue==100, 0, jump)',
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
                ', {counter$.bin', x, ' <- counter$.bin', x, ' - ifelse(counter$.bin',x,'<=0|counter$countervalue==100, 0, jump)'
              ),
              '\n',
              paste0(
                'counter$countervalue <- counter$countervalue - ifelse(counter$.bin',x,'<=0, 0, jump)',
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
    eval(
      parse(
        text=
            lapply(1:nbins, function(x){
              paste0(
              paste0('output$plot', x,' <- renderPlot({\n
                     data <- tibble(bin=1, y=counter$.bin', x,')'),
              '\n',
             'data %>% ggplot(., aes(x=1, y=y)) +
                      geom_bar(stat="identity", width=1) +
                      scale_y_continuous(limits=c(0,100), expand = c(0,0)) +
                      scale_x_continuous(expand = c(0,0)) +
                      scale_fill_discrete()+
                      theme_void()',
             '})'
             )
            })
      )
      )
    if(FALSE){
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
        lapply(1:nbins, function(x){
          data %>% 
            filter(bin==x) %>% 
            ggplot(., aes(x=1, y=y, fill=as.factor(bin))) +
            geom_bar(stat='identity', width=1) +
            scale_y_continuous(limits=c(0,100), expand = c(0,0)) +
            scale_x_continuous(expand = c(0,0)) +
            scale_fill_discrete() +
            theme_void()
        })
        
      })
    }
    
  }
  
)

ui <- shinyUI(
  fluidPage(
    useShinyjs(),
    inlineCSS(list(.red = "text-align: center; display: inline-block; width: 20%, position: relative; margin: 20px;",
                   .blue = "margin: 10px;",
                   .plot = "display: block; margin: auto;")),
     title="Goldberg & Rothschild Question",
     br(),
     p('Some description'),
     fluidRow(

         div("Bins"),
         lapply(1:4, function(x){
           eval(
             parse(
               text = 
                 paste0('tags$div(class="red", tags$div(class="blue", textOutput("count.bin', x, '"), "Bin ', x, '", br(), actionButton("add', x, '", "+ 10"), actionButton("sub',x, '", "- 10")), br(), plotOutput("plot', x, '", width = 200))')
             )
           )
         }),
         #tags$div(class='red', tags$div(class='blue', textOutput("count.bin1"), 'Bin 1', br(), actionButton("add1", "+ 10"), actionButton("sub1", "- 10")), br(), plotOutput('plot1', width = 200)),
         #tags$div(class='red', tags$div(class='blue', textOutput("count.bin2"), 'Bin 2', br(), actionButton("add2", "+ 10"), actionButton("sub2", "- 10")), br(), plotOutput('plot2', width =200)),
         div(actionButton("reset", "Reset"))
       ),
     # plotOutput("plot", width = "50%"),
     textOutput("count"),
    br(),
    textOutput("val"),
    br(),
    textOutput("vals")
  )
)

shinyApp(ui = ui, server = server)

