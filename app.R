#App is a simple card with some content and a little output below that represents the last swipes result.
# devtools::install_github("nstrayer/shinysense")
library(shiny)
library(shinysense)
library(ggplot2)
#library(DBI)
#library(RSQLite)
#library(grid)
#library(gridExtra)
xtraVar <- 8
nswipeReward = 40


sqlitePath <- "swiperespons.sqlite"
table <- "swipes"
source("app_data_updater.R")

saveData <- function(input, output) {
    # Connect to the database
    data2 = cbind(input$useRname,input$Polarity, output[1,,drop = FALSE])
    #colnames(data2) = c("user", "ind", "mz","rt","swipe")
    colnames(data2) = c("user", "polarity","ind", "mz","rt","swipe")
    
    db <- DBI::dbConnect(RSQLite::SQLite(), sqlitePath)
    
    DBI::dbWriteTable(db, name=table, value=data2, row.names=FALSE, append=TRUE)
    
    DBI::dbDisconnect(db)
}



ui <- fluidPage(
    headerPanel('This is the GOA tindeResting! app.'),
    sidebarLayout(
        sidebarPanel(
            fluidRow(
                column(6,
                       textInput("useRname", "Your name", "Louise")
                ),
                column(6, 
                       radioButtons("Gender", "Gender", choiceNames = list(
                           icon("transgender"),
                           icon("venus"),
                           icon("mars")
                       ),
                       choiceValues = list(
                           "trans" , "female",  "male"
                       ), 
                       inline = TRUE
                       )
                )
            ),
            selectInput("Polarity", "Ion mode", c("Positive" = "Pos", "Negative" = "Neg")),
            fluidRow(
                column(6,
                       numericInput("SvsNC", "max S vs NC val.", 0.2)
                ),
                column(6, 
                       numericInput("SvsMB", "max S vs MB val.", 0.2)
                       )
            ),
            fluidRow(
                column(6,
                       selectInput("onlyNew", "Only Unseen", c("Yes" = "yes", "No" = "no"))
                ),
                column(6, selectInput("swipeOrder", "Swipe Order", c("Significance" = "sig", "Random" = "rnd")))
            ),
            plotOutput("selectedRegion", height = 300)
        ),
        mainPanel(
            h4("Swipe Me! Or use the arrows"),
            p("Swipe the plot to the right if the time profile is interesting. Left if not."),
            hr(),
            fluidRow(
                column(2, actionButton(inputId = "buttonLeft", label = "boring", icon = icon("arrow-left") ), align = "center", offset = 3),
                column(2, actionButton(inputId = "buttonUp", label = "other", icon = icon("arrow-up") ), align = "center", offset = 0),
                column(2, actionButton(inputId = "buttonRight", label = "interesting", icon = icon("arrow-right"), align = "center" , offset = 0))
            ),
            br(),
            #actionButton(inputId = "left", label = "boring", icon = icon("arrow-left") ),
            #actionButton(inputId = "up", label = "other", icon = icon("arrow-up") ),
            #actionButton(inputId = "right", label = "interesting", icon = icon("arrow-right") ),
            shinyswiprUI( "quote_swiper",
                          plotOutput("profilePlot")
            ),
            hr(),
            h4("Swipe History"),
            tableOutput("resultsTable")
            
        )
        
    ),
    hr(),
    print("Made by Charlie Beirnaert. Having Problems? The solution is just an email away, hopefully." )
    
)

server <- function(input, output, session) {
    card_swipe <- callModule(shinyswipr, "quote_swiper")
    
    
    dataSet <- reactive({
        test <- read.table(file = paste("./data/shiny",input$Polarity,"Data.txt",sep = ""), sep = ",", header = TRUE)
        test
        })
    
    SwipeHistory <- reactive({
        swipehist <- read.table(file = paste("./data/SwipeHistory",input$Polarity,".txt",sep = ""), sep = ",", header = TRUE)
        swipehist
    })


    dataSubset <- reactive({
        subset.selection <- rep(FALSE, nrow(dataSet()))
        if(input$onlyNew == "yes"){
            subset.selection[dataSet()$qSvsMB <= input$SvsMB &  
                                 dataSet()$qSvsNC<= input$SvsNC & 
                                 ! dataSet()$index %in% SwipeHistory()$ind[SwipeHistory()$user == input$useRname] ] <- TRUE
        } else{
            subset.selection[dataSet()$qSvsMB <= input$SvsMB &  
                                 dataSet()$qSvsNC<= input$SvsNC ] <- TRUE
        }
        subset.selection
        })


    selection.vector <- reactive({
        datasubset <- dataSet()[dataSubset(),]
        
        if(input$swipeOrder == "sig"){
            showorder <- order(datasubset$qSvsMB+datasubset$qSvsNC)
        } else if(input$swipeOrder == "rnd"){
            showorder <- order(runif(nrow(datasubset)))
        } else{
            showorder <- seq(1,nrow(datasubset))
        }
        showorder
    })
    
    
    
    output$selectedRegion <- renderPlot({
        #ggplot(dataSet(), aes(x = qSvsMB, y = qSvsNC)) +geom_point()
        input$useRname
        ggplot(dataSet(), aes(x = qSvsMB, y = qSvsNC, colour = as.factor(dataSubset()))) +
            geom_point() +
            labs(colour="Selected",
                 x = "q value. S vs MB",
                 y= "q value. S vs NC") +
            ggtitle(paste(as.character(sum(dataSubset()))," Features Selected", sep ="")) +
            theme(plot.title = element_text(hjust = 0.5))

    })
    
   
    
    
    ### old shit
    output$profilePlot        <- renderPlot({
        datasubset <- dataSet()[dataSubset(),] #data.subset()
        start <- (xtraVar+1) 
        end <-  ncol(dataSet())
        time <- as.numeric(unlist(lapply(strsplit(colnames(dataSet()[,start:end]), "_"), `[[`, 2)))
        type <- as.factor(unlist(lapply(strsplit(colnames(dataSet()[,start:end]), "_"), `[[`, 1)))
        Group <- as.character(type)
        Group[Group %in% c("S1", "S2", "S3")] <- "S"
        Group <- as.factor(Group)
        kk <- selection.vector()[as.numeric(appVals$k)]
        compoundData <- data.frame(t = time, int = as.numeric(datasubset[kk,start:end]), type = type, group = Group)  
        RTplot = as.character((round(100*(datasubset$rtmed[kk]/60))/100))
        MZplot = as.character(round(1000*datasubset$mzmed[kk])/1000)
        plotname = paste("mz:", MZplot, "  RT:",  RTplot, "min" , sep = " ")
        gg1 <- ggplot(compoundData, aes(x=time,y=int,group = type, colour = Group)) +
               geom_line() +
               ggtitle(plotname) +
               theme_bw() +
               theme(plot.title = element_text(hjust = 0.5),
                     legend.position="bottom")
        
        gg2 <- ggplot(compoundData, aes(x=time,y=log10(int),group = type, colour = Group)) +
            geom_line() +
            ggtitle(" ") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5),
                  legend.position="bottom")
        
        gridExtra::grid.arrange(gg1,gg2, layout_matrix = rbind(c(1,1,2),c(1,1,2),c(1,1,2)))
        
        
    })
   ####
    output$index <- renderText({dataSet()[dataSubset(),]$index[selection.vector()[as.numeric(appVals$k)]]})
    output$mz <- renderText({ dataSet()[dataSubset(),]$mzmed[selection.vector()[as.numeric(appVals$k)]]})
    output$rt <- renderText({dataSet()[dataSubset(),]$rtmed[selection.vector()[as.numeric(appVals$k)]]})
    
    #quote               <- fortune()
    #output$quote        <- renderText({ quote$quote })
    #output$quote_author <- renderText({ paste0("-",quote$author) })
    output$resultsTable <- renderDataTable({appVals$swipes})
    
   
    appVals <- reactiveValues(
        k  =  1,
        swipes = data.frame(index = character(), mz = character(), rt = character(), swipe = character())
    )
    
    
    observeEvent( card_swipe(),{
        #Record our last swipe results.
        appVals$swipes <- rbind(
            data.frame(index  = as.character(dataSet()[dataSubset(),]$index[selection.vector()[as.numeric(appVals$k)]]),
                       mz = as.character(dataSet()[dataSubset(),]$mzmed[selection.vector()[as.numeric(appVals$k)]]),
                       rt = as.character(dataSet()[dataSubset(),]$rtmed[selection.vector()[as.numeric(appVals$k)]]),
                       swipe  = card_swipe()
            ),
            appVals$swipes
        )
        #send results to the output.
        output$resultsTable <- renderTable({appVals$swipes})
        
        #update the quote
        appVals$k <-  appVals$k + 1 
 
        
        if(appVals$k %% nswipeReward == 0){
            if(input$Gender == "female"){
                showModal(modalDialog(
                    modalButton(label = img(src="reynolds1.jpeg", height = 250), icon = NULL),
                    modalButton(label = img(src="reynolds2.jpg", height = 250), icon = NULL),
                    easyClose = TRUE
                ))
            } else if(input$Gender == "male"){
                showModal(modalDialog(
                    modalButton(label = img(src="vikander1.jpg", height = 300), icon = NULL),
                    modalButton(label = img(src="vikander2.jpg", height = 300), icon = NULL),
                    easyClose = TRUE
                ))
            } else{
                showModal(modalDialog(
                    modalButton(label = img(src="kat.gif", height = 300), icon = NULL),
                    easyClose = TRUE
                ))
            }
            
        }
        
        
        saveData(input, appVals$swipes )
        
        #send update to the ui.
        output$index <- renderText({dataSet()[dataSubset(),]$index[selection.vector()[as.numeric(appVals$k)]]})
        output$mz <- renderText({ dataSet()[dataSubset(),]$mzmed[selection.vector()[as.numeric(appVals$k)]]})
        output$rt <- renderText({dataSet()[dataSubset(),]$rtmed[selection.vector()[as.numeric(appVals$k)]]})
        
        
        
    }) #close event observe.
    observeEvent( input$buttonLeft,{
        #Record our last swipe results.
        appVals$swipes <- rbind(
            data.frame(index  = as.character(dataSet()[dataSubset(),]$index[selection.vector()[as.numeric(appVals$k)]]),
                       mz = as.character(dataSet()[dataSubset(),]$mzmed[selection.vector()[as.numeric(appVals$k)]]),
                       rt = as.character(dataSet()[dataSubset(),]$rtmed[selection.vector()[as.numeric(appVals$k)]]),
                       swipe  = "left"
            ),
            appVals$swipes
        )
        #send results to the output.
        output$resultsTable <- renderTable({appVals$swipes})
        
        #update the quote
        appVals$k <-  appVals$k + 1 
        
        
        if(appVals$k %% nswipeReward == 0){
            if(input$Gender == "female"){
                showModal(modalDialog(
                    modalButton(label = img(src="reynolds1.jpeg", height = 250), icon = NULL),
                    modalButton(label = img(src="reynolds2.jpg", height = 250), icon = NULL),
                    easyClose = TRUE
                ))
            } else if(input$Gender == "male"){
                showModal(modalDialog(
                    modalButton(label = img(src="vikander1.jpg", height = 300), icon = NULL),
                    modalButton(label = img(src="vikander2.jpg", height = 300), icon = NULL),
                    easyClose = TRUE
                ))
            } else{
                showModal(modalDialog(
                    modalButton(label = img(src="kat.gif", height = 300), icon = NULL),
                    easyClose = TRUE
                ))
            }
            
        }
        
        
        saveData(input, appVals$swipes )
        
        #send update to the ui.
        output$index <- renderText({dataSet()[dataSubset(),]$index[selection.vector()[as.numeric(appVals$k)]]})
        output$mz <- renderText({ dataSet()[dataSubset(),]$mzmed[selection.vector()[as.numeric(appVals$k)]]})
        output$rt <- renderText({dataSet()[dataSubset(),]$rtmed[selection.vector()[as.numeric(appVals$k)]]})
        
        
        
    }) #close event observe.
    observeEvent( input$buttonUp,{
        #Record our last swipe results.
        appVals$swipes <- rbind(
            data.frame(index  = as.character(dataSet()[dataSubset(),]$index[selection.vector()[as.numeric(appVals$k)]]),
                       mz = as.character(dataSet()[dataSubset(),]$mzmed[selection.vector()[as.numeric(appVals$k)]]),
                       rt = as.character(dataSet()[dataSubset(),]$rtmed[selection.vector()[as.numeric(appVals$k)]]),
                       swipe  = "Up"
            ),
            appVals$swipes
        )
        #send results to the output.
        output$resultsTable <- renderTable({appVals$swipes})
        
        #update the quote
        appVals$k <-  appVals$k + 1 

        if(appVals$k %% nswipeReward == 0){
            if(input$Gender == "female"){
                showModal(modalDialog(
                    modalButton(label = img(src="reynolds1.jpeg", height = 250), icon = NULL),
                    modalButton(label = img(src="reynolds2.jpg", height = 250), icon = NULL),
                    easyClose = TRUE
                ))
            } else if(input$Gender == "male"){
                showModal(modalDialog(
                    modalButton(label = img(src="vikander1.jpg", height = 300), icon = NULL),
                    modalButton(label = img(src="vikander2.jpg", height = 300), icon = NULL),
                    easyClose = TRUE
                ))
            } else{
                showModal(modalDialog(
                    modalButton(label = img(src="kat.gif", height = 300), icon = NULL),
                    easyClose = TRUE
                ))
            }
            
        }
        
        saveData(input, appVals$swipes )
        
        #send update to the ui.
        output$index <- renderText({dataSet()[dataSubset(),]$index[selection.vector()[as.numeric(appVals$k)]]})
        output$mz <- renderText({ dataSet()[dataSubset(),]$mzmed[selection.vector()[as.numeric(appVals$k)]]})
        output$rt <- renderText({dataSet()[dataSubset(),]$rtmed[selection.vector()[as.numeric(appVals$k)]]})
        
        
        
    }) #close event observe.
    observeEvent( input$buttonRight,{
        #Record our last swipe results.
        appVals$swipes <- rbind(
            data.frame(index  = as.character(dataSet()[dataSubset(),]$index[selection.vector()[as.numeric(appVals$k)]]),
                       mz = as.character(dataSet()[dataSubset(),]$mzmed[selection.vector()[as.numeric(appVals$k)]]),
                       rt = as.character(dataSet()[dataSubset(),]$rtmed[selection.vector()[as.numeric(appVals$k)]]),
                       swipe  = "Right"
            ),
            appVals$swipes
        )
        #send results to the output.
        output$resultsTable <- renderTable({appVals$swipes})
        
        #update the quote
        appVals$k <-  appVals$k + 1 

        
        if(appVals$k %% nswipeReward == 0){
            if(input$Gender == "female"){
                showModal(modalDialog(
                    modalButton(label = img(src="reynolds1.jpeg", height = 250), icon = NULL),
                    modalButton(label = img(src="reynolds2.jpg", height = 250), icon = NULL),
                    easyClose = TRUE
                ))
            } else if(input$Gender == "male"){
                showModal(modalDialog(
                    modalButton(label = img(src="vikander1.jpg", height = 300), icon = NULL),
                    modalButton(label = img(src="vikander2.jpg", height = 300), icon = NULL),
                    easyClose = TRUE
                ))
            } else{
                showModal(modalDialog(
                    modalButton(label = img(src="kat.gif", height = 300), icon = NULL),
                    easyClose = TRUE
                ))
            }
            
        }
        
        
        saveData(input, appVals$swipes )
        
        #send update to the ui.
        output$index <- renderText({dataSet()[dataSubset(),]$index[selection.vector()[as.numeric(appVals$k)]]})
        output$mz <- renderText({ dataSet()[dataSubset(),]$mzmed[selection.vector()[as.numeric(appVals$k)]]})
        output$rt <- renderText({dataSet()[dataSubset(),]$rtmed[selection.vector()[as.numeric(appVals$k)]]})
        
        
        
    }) #close event observe.
}

shinyApp(ui, server)
