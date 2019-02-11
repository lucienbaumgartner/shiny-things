library(shiny)
library(shinyjs)
library(ggplot2)
library(dplyr)

# define value jumps for the counters
jump <- 2
# define bins
nbins <- 12
# element you wanna store value of
store <- paste0('.bin', 1:nbins)
# function to save data
outputDir <- "storage/"
saveData <- function(data) {
  data <- as.data.frame(t(data))
  data <- t(data)
  rownames(data) <- paste0(seq(20, 79, 5), '-', seq(24, 79, 5), '%')
  # Create a unique file name
  fileName <- sprintf("testdata_%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the file to the local system
  write.csv(
    x = data,
    file = file.path(outputDir, fileName), 
    quote = TRUE
  )
}


server <- shinyServer(
  function(input, output, session) {
    #global <- reactiveValues(done = FALSE)
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
               # 'counter$countervalue <- counter$countervalue - ifelse(counter$.bin',x,'<=0, 0, jump)',
                'counter$countervalue <- counter$countervalue - ifelse(counter$countervalue<=0, 0, jump)',
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
      100-counter$countervalue # print the latest value stoouter-container in the reactiveValues object
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
                      scale_fill_discrete() +
                      theme_void()+
                      theme(plot.background=element_rect(fill="lightblue"))',
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
    #observe({
    #  if(counter$countervalue==100){global$done <- TRUE}else{global$done <- FALSE}
    #})
    observe({
      toggleClass(id = 'submit', class = 'active', condition = counter$countervalue==100)
    })
    formData <- reactive({
      data <- sapply(store, function(x) counter[[x]])
      data
    })
    observeEvent(input$submit, {
      saveData(formData())
    })
  }
  
)

ui <- shinyUI(
  fluidPage(
    useShinyjs(),
    theme="style.css",
     title="Goldberg & Rothschild Question",
     br(),
     h1("Goldberg & Rothschild Question"), 
     p('Some description: Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.'),
     fluidRow(
        tags$div(id='panel',
                 lapply(1:12, function(x){
                   eval(
                     parse(
                       text = 
                         paste0('tags$div(class="outer-container", tags$div(class="inner-container", h4(textOutput("count.bin', x, '")), h3("', 
                                paste0(seq(20, 79, 5), '-', seq(24, 79, 5), '%')[x], 
                                '"), br(), actionButton("add', x, '", "+ ', 2,'"), actionButton("sub',x, '", "- ', 2,'")), br(), tags$div(class="plot", plotOutput("plot', x, '", width = 50)))')
                     )
                   )
                 })
                 )
         
    ),
    fluidRow(
      tags$div(
        id='aux',
        tags$div(id='reset-btn', actionButton("reset", "Reset")),
        tags$div(id='count-container', div('There are', textOutput("count"), 'left.')),
        tags$div(id='submit-btn', actionButton('submit', "Submit answer"))
      )
      
    )
  )
)

shinyApp(ui = ui, server = server)

