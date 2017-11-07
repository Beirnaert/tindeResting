#App is a simple card with some content and a little output below that represents the last swipes result.
# devtools::install_github("nstrayer/shinysense")
library(shiny)
library(shinysense)
library(ggplot2)

#library(DBI)
#library(RSQLite)
#library(grid)

xtraVar <- 6
nswipeReward = 50

sqlitePath <- "swiperespons.sqlite"
source("arrangeGrobLocal.R")
source("grid.arrangeLocal.R")


saveData <- function(input, output, iter) {
    # Connect to the database
    NatuRA_members = c("Ken", "Kenne", "Laura", "Sebastiaan", "Seb", "Deborah", "Anastasia", "Luc")
    
    NatuRA <- tolower(input$useRname) %in% tolower(NatuRA_members)
    
    data2 = cbind(iter, input$useRname, NatuRA, input$Polarity, output[1,,drop = FALSE])
    colnames(data2) = c("iter", "user", "NatuRA","polarity","ind", "mz","rt","swipe")
    
    db <- DBI::dbConnect(RSQLite::SQLite(), sqlitePath)
    
    DBI::dbWriteTable(db, name="swipes", value=data2, row.names=FALSE, append=TRUE)
    
    DBI::dbDisconnect(db)
}

getHistory <- function(input) {
    historyfile = "./data/SwipeHistory.sqlite"
    if(file.exists(historyfile)){
        if(input$onlyNew == "yes"){
            # new to NatuRA
            
            Db <- DBI::dbConnect(RSQLite::SQLite(), historyfile)
            
            history_indexes <- as.integer(DBI::dbGetQuery(Db, paste("SELECT ind FROM history WHERE polarity='",input$Polarity,"' AND NatuRA=1", sep = "") )[[1]] )
            
            DBI::dbDisconnect(Db)
        } else{
            # new to me
            historyfile = "./data/SwipeHistory.sqlite"
            Db <- DBI::dbConnect(RSQLite::SQLite(), historyfile)
            
            history_indexes <- as.integer(DBI::dbGetQuery(Db, paste("SELECT ind FROM history WHERE polarity='",input$Polarity,"' AND user='", as.character(input$useRname),"'", sep = "") )[[1]] )
            
            DBI::dbDisconnect(Db)
        }
    } else {
        history_indexes = NULL
    }
    return(history_indexes)
}

OopsieDaisyRemoveDbEntry <- function(input, output, iter){
    Db <- DBI::dbConnect(RSQLite::SQLite(), sqlitePath)
    
    if("swipes" %in% DBI::dbListTables(Db) ){
        DBI::dbExecute(Db, paste("DELETE FROM swipes WHERE user='", as.character(input$useRname),"' AND iter=", as.character(iter), sep = "") )
    }
    DBI::dbDisconnect(Db)
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
                           "LGBTQI" , "female",  "male"
                       ), 
                       inline = TRUE
                       )
                )
            ),
            fluidRow(
                column(6,
                       selectInput("Polarity", "Ion mode", c("Positive" = "Pos", "Negative" = "Neg"))
                ),
                column(6, 
                       numericInput("minRT", "minimal RT (min)", 0)
                )
            ),
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
                       selectInput("onlyNew", "Time profiles", c("New to NatuRA" = "yes", "New to me" = "no"))
                ),
                column(6, selectInput("swipeOrder", "Swipe Order", c("Significance" = "sig", "Random" = "rnd")))
            ),
            plotOutput("selectedRegion", height = 300),
            br(),
            fluidRow(
                column(12, actionButton("undo", 
                                       "Oopsie daisy",  
                                       style="color:#fff; background-color:Crimson"),
                       align = "center", 
                       offset = 0 ))
        ),
        mainPanel(
            h4("Swipe Me! Or use the arrows"),
            p("Swipe the plot to the right if the time profile is interesting. Left if not."),
            hr(),
            fluidRow(
                column(2, actionButton(inputId = "buttonLeft", label = "boring", icon = icon("arrow-left") ), align = "left", offset = 3),
                column(2, actionButton(inputId = "buttonUp", label = "other", icon = icon("arrow-up") ), align = "center", offset = 0),
                column(2, actionButton(inputId = "buttonRight", label = "interesting", icon = icon("arrow-right")), align = "right" , offset = 0)
            ),
            br(),
            shinyswiprUI( "quote_swiper",
                          plotOutput("profilePlot")
            ),
            hr(),
            h4("Swipe History"),
            tableOutput("resultsTable")
            
        )
        
    ),
    hr(),
    print("Made by Charlie Beirnaert. Having Problems? The solution is just an email away... hopefully." )
    
)

server <- function(input, output, session) {
    
    # update data in the beginning, also do this when session ends to speed it up
     source("app_data_updater.R")
    
    card_swipe <- callModule(shinyswipr, "quote_swiper")
    
    
    dataSet <- reactive({
        #test <- read.table(file = paste("./data/shiny",input$Polarity,"Data.txt",sep = ""), sep = ",", header = TRUE)
        test <- read.table(file = paste("./data/shiny",input$Polarity,"Data.txt",sep = ""), sep = ",", header = TRUE)
        test
    })
    
    dataSubset <- reactive({
        # the getHistory function knows which selection is to be made (because we give it input): user or natura based
        subset.selection <- rep(FALSE, nrow(dataSet()))
        subset.selection[dataSet()$qSvsMB <= input$SvsMB &
                             dataSet()$qSvsNC<= input$SvsNC &
                             dataSet()$rtmed >= (60*input$minRT) &
                             ! dataSet()$index %in% getHistory(input) ] <- TRUE
        
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
        
        input$useRname# to get the shit started
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
        
        input$useRname # to get the shit started
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
        
        gg2 <- ggplot(compoundData, aes(x=time,y=log10(int+1),group = type, colour = Group)) +
            geom_line() +
            ggtitle(" ") +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5),
                  legend.position="bottom")
        
        grid.arrangeLocal(gg1,gg2, layout_matrix = rbind(c(1,1,2),c(1,1,2),c(1,1,2)))
        
        
    })
    ####
    output$index <- renderText({dataSet()[dataSubset(),]$index[selection.vector()[as.numeric(appVals$k)]]})
    output$mz <- renderText({ dataSet()[dataSubset(),]$mzmed[selection.vector()[as.numeric(appVals$k)]]})
    output$rt <- renderText({dataSet()[dataSubset(),]$rtmed[selection.vector()[as.numeric(appVals$k)]]})
    
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
            nmales = length(list.files("www/male_celebs"))
            nfemales = length(list.files("www/female_celebs"))
            if(input$Gender == "female"){
                showModal(modalDialog(
                    modalButton(label = img(src=paste("male_celebs/male",sample(nmales,1),".jpg", sep = ""), height = 300), icon = NULL),
                    easyClose = TRUE
                ))
            } else if(input$Gender == "LGBTQI"){
                showModal(modalDialog(
                    modalButton(label = img(src="kat.gif", height = 300), icon = NULL),
                    easyClose = TRUE
                ))
            } else if(input$Gender == "male"){
                showModal(modalDialog(
                    modalButton(label = img(src=paste("female_celebs/fem",sample(nfemales,1),".jpg", sep = ""), height = 300), icon = NULL),
                    easyClose = TRUE
                ))
            }
        }
        
        
        saveData(input, appVals$swipes, appVals$k)
        
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
                       swipe  = "Left"
            ),
            appVals$swipes
        )
        #send results to the output.
        output$resultsTable <- renderTable({appVals$swipes})
        
        #update the quote
        appVals$k <-  appVals$k + 1 
        
        
        if(appVals$k %% nswipeReward == 0){
            nmales = length(list.files("www/male_celebs"))
            nfemales = length(list.files("www/female_celebs"))
            if(input$Gender == "female"){
                showModal(modalDialog(
                    modalButton(label = img(src=paste("male_celebs/male",sample(nmales,1),".jpg", sep = ""), height = 300), icon = NULL),
                    easyClose = TRUE
                ))
            } else if(input$Gender == "LGBTQI"){
                showModal(modalDialog(
                    modalButton(label = img(src="kat.gif", height = 300), icon = NULL),
                    easyClose = TRUE
                ))
            } else if(input$Gender == "male"){
                showModal(modalDialog(
                    modalButton(label = img(src=paste("female_celebs/fem",sample(nfemales,1),".jpg", sep = ""), height = 300), icon = NULL),
                    easyClose = TRUE
                ))
            }
        }
        
        
        saveData(input, appVals$swipes, appVals$k)
        
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
            nmales = length(list.files("www/male_celebs"))
            nfemales = length(list.files("www/female_celebs"))
            if(input$Gender == "female"){
                showModal(modalDialog(
                    modalButton(label = img(src=paste("male_celebs/male",sample(nmales,1),".jpg", sep = ""), height = 300), icon = NULL),
                    easyClose = TRUE
                ))
            } else if(input$Gender == "LGBTQI"){
                showModal(modalDialog(
                    modalButton(label = img(src="kat.gif", height = 300), icon = NULL),
                    easyClose = TRUE
                ))
            } else if(input$Gender == "male"){
                showModal(modalDialog(
                    modalButton(label = img(src=paste("female_celebs/fem",sample(nfemales,1),".jpg", sep = ""), height = 300), icon = NULL),
                    easyClose = TRUE
                ))
            }
        }
        
        saveData(input, appVals$swipes, appVals$k)
        
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
            nmales = length(list.files("www/male_celebs"))
            nfemales = length(list.files("www/female_celebs"))
            if(input$Gender == "female"){
                showModal(modalDialog(
                    modalButton(label = img(src=paste("male_celebs/male",sample(nmales,1),".jpg", sep = ""), height = 300), icon = NULL),
                    easyClose = TRUE
                ))
            } else if(input$Gender == "LGBTQI"){
                showModal(modalDialog(
                    modalButton(label = img(src="kat.gif", height = 300), icon = NULL),
                    easyClose = TRUE
                ))
            } else if(input$Gender == "male"){
                showModal(modalDialog(
                    modalButton(label = img(src=paste("female_celebs/fem",sample(nfemales,1),".jpg", sep = ""), height = 300), icon = NULL),
                    easyClose = TRUE
                ))
            }
        }
        
        
        
        saveData(input, appVals$swipes, appVals$k)
        
        #send update to the ui.
        output$index <- renderText({dataSet()[dataSubset(),]$index[selection.vector()[as.numeric(appVals$k)]]})
        output$mz <- renderText({ dataSet()[dataSubset(),]$mzmed[selection.vector()[as.numeric(appVals$k)]]})
        output$rt <- renderText({dataSet()[dataSubset(),]$rtmed[selection.vector()[as.numeric(appVals$k)]]})
        
        
        
    }) #close event observe.
    observeEvent( input$undo,{
        
        OopsieDaisyRemoveDbEntry(input, appVals$swipes, appVals$k)
        
        showModal(modalDialog(
            title = "Reaction deleted",
            "The last reaction you submitted to the database has been deleted. The time profile will be reviewed by someone else.",
            easyClose = TRUE
        ))
        
    }) #close event observe.
    
    session$onSessionEnded(function() {
        source("app_data_updater.R")
    })
}

shinyApp(ui, server)
