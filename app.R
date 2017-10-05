#App is a simple card with some content and a little output below that represents the last swipes result.
# devtools::install_github("nstrayer/shinysense")
library(shiny)
library(shinysense)
library(ggplot2)
library(DBI)
library(RSQLite)
xtraVar <- 8

outputDir = "responses/new"
sqlitePath <- "swiperespons.sqlite"
table <- "swipes"


saveData <- function(input, output) {
    # Connect to the database
    data2 = cbind(input$useRname,input$Polarity, output[1,,drop = FALSE])
    #colnames(data2) = c("user", "ind", "mz","rt","swipe")
    colnames(data2) = c("user", "polarity","ind", "mz","rt","swipe")
    
    db <- dbConnect(RSQLite::SQLite(), sqlitePath)
    
    dbWriteTable(db, name=table, value=data2, row.names=FALSE, append=TRUE)
    
    dbDisconnect(db)
}

# # save to normal file
# saveData2File <- function(input, output) {
#     data2 = cbind(input$useRname, output[1,,drop = FALSE])
#     #data2 = data2[1,,drop=FALSE]
#     # Create a unique file name
#     fileName <- sprintf("%s_%s_%s.csv", input$useRname, data2$index, round(100000*runif(1)))
#     #fileName <- sprintf("%s.csv", round(100000*runif(1)))
#     # Write the file to the local system
#     write.csv(
#         x = data2,
#         file = file.path(outputDir, fileName), 
#         row.names = FALSE, 
#         quote = TRUE
#     )
# }

ui <- fluidPage(
    headerPanel('This is the GOA tindeResting! app.'),
    sidebarLayout(
        sidebarPanel(
            textInput("useRname", "Your name", "Louise"),
            selectInput("Polarity", "Ion mode", c("Positive" = "Pos", "Negative" = "Neg")),
            numericInput("SvsNC", "max S vs. NC value:", 0.2),
            numericInput("SvsMB", "max S vs. MB value:", 0.2),
            selectInput("onlyNew", "Only Unseen", c("Yes" = "yes", "No" = "no")),
            selectInput("swipeOrder", "Swipe Order", c("Significance" = "sig", "Random" = "rnd")),
            plotOutput("selectedRegion", height = 200)
        ),
        mainPanel(
            h4("Swipe Me! Or use the arrows"),
            p("Swipe the plot to the right if the time profile is interesting. Left if not."),
            hr(),
            fluidRow(
                column(5, actionButton(inputId = "buttonLeft", label = "boring", icon = icon("arrow-left") ), align = "right"),
                column(2, actionButton(inputId = "buttonUp", label = "other", icon = icon("arrow-up") ), align = "center"),
                column(5, actionButton(inputId = "buttonRight", label = "interesting", icon = icon("arrow-right"), align = "left" ))
            ),
            br(),
            #actionButton(inputId = "left", label = "boring", icon = icon("arrow-left") ),
            #actionButton(inputId = "up", label = "other", icon = icon("arrow-up") ),
            #actionButton(inputId = "right", label = "interesting", icon = icon("arrow-right") ),
            shinyswiprUI( "quote_swiper",
                          hr(),
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
        test <- read.table(file = paste("shiny",input$Polarity,"Data.txt",sep = ""), sep = ",", header = TRUE)
        test
        })
    
    SwipeHistory <- reactive({
        swipehist <- read.table(file = paste("SwipeHistory",input$Polarity,".txt",sep = ""), sep = ",", header = TRUE)
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
        ggplot(compoundData, aes(x=time,y=int,group = type, colour = Group)) +
            geom_line() +
            #ggtitle(plotname) +
            theme_bw() 
            #theme(plot.title = element_text(hjust = 0.5))
        
        
    })
   ####
    output$index <- renderText({dataSet()[dataSubset(),]$index[selection.vector()[as.numeric(appVals$k)]]})
    output$mz <- renderText({ dataSet()[dataSubset(),]$mzmed[selection.vector()[as.numeric(appVals$k)]]})
    output$rt <- renderText({dataSet()[dataSubset(),]$rtmed[selection.vector()[as.numeric(appVals$k)]]})
    
    #quote               <- fortune()
    #output$quote        <- renderText({ quote$quote })
    #output$quote_author <- renderText({ paste0("-",quote$author) })
    output$resultsTable <- renderDataTable({appVals$swipes})
    
    # appVals <- reactiveValues(
    #     quote  = quote,
    #     swipes = data.frame(quote = character(), author = character(), swipe = character())
    # )
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
        k <- k + 1
        #appVals$k <- k + 1
        appVals$k <-  appVals$k + 1 
        #iterK()
        
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
        k <- k + 1
        #appVals$k <- k + 1
        appVals$k <-  appVals$k + 1 
        #iterK()
        
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
        k <- k + 1
        #appVals$k <- k + 1
        appVals$k <-  appVals$k + 1 
        #iterK()
        
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
        k <- k + 1
        #appVals$k <- k + 1
        appVals$k <-  appVals$k + 1 
        #iterK()
        
        if(appVals$k %% 20 == 0){
            showModal(modalDialog(
                modalButton(label = img(src="george1.jpg", height = 300), icon = NULL),
                modalButton(label = img(src="george2.jpg", height = 300), icon = NULL),
                easyClose = TRUE
            ))
        }
        
        
        saveData(input, appVals$swipes )
        
        #send update to the ui.
        output$index <- renderText({dataSet()[dataSubset(),]$index[selection.vector()[as.numeric(appVals$k)]]})
        output$mz <- renderText({ dataSet()[dataSubset(),]$mzmed[selection.vector()[as.numeric(appVals$k)]]})
        output$rt <- renderText({dataSet()[dataSubset(),]$rtmed[selection.vector()[as.numeric(appVals$k)]]})
        
        
        
    }) #close event observe.
}

shinyApp(ui, server)
