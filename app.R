#App is a simple card with some content and a little output below that represents the last swipes result.
# devtools::install_github("nstrayer/shinysense")
library(shiny)
library(shinysense)
library(ggplot2)
library(randomForest)
library(stats)

#library(DBI)
#library(RSQLite)
#library(grid)

xtraVar <- 8
nswipeReward = 25

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

UpdateModelPredictions <- function(input, xtraVar){
    
    #step 1: get reviewed indexes and their swipe response 
    historyfile = "./data/SwipeHistory.sqlite"
    Db <- DBI::dbConnect(RSQLite::SQLite(), historyfile)
    history_swipe_pos <- DBI::dbGetQuery(Db, "SELECT ind, swipe FROM history WHERE polarity='Pos' AND NatuRA=1" ) 
    history_swipe_neg <- DBI::dbGetQuery(Db, "SELECT ind, swipe FROM history WHERE polarity='Neg' AND NatuRA=1" ) 
    history_swipe_current <- DBI::dbGetQuery(Db, paste("SELECT ind FROM history WHERE polarity='",input$Polarity,"' AND NatuRA=1", sep = "") )
    DBI::dbDisconnect(Db)
    
    history_swipe_pos <- history_swipe_pos[!duplicated(history_swipe_pos$ind) & history_swipe_pos$swipe != "Up", ]
    history_swipe_neg <- history_swipe_neg[!duplicated(history_swipe_neg$ind) & history_swipe_neg$swipe != "Up", ]
    
    if (nrow(history_swipe_pos) > 0) rownames(history_swipe_pos) <- paste("Pos_", history_swipe_pos$ind, sep = "")  
    if (nrow(history_swipe_neg) > 0) rownames(history_swipe_neg) <- paste("Neg_", history_swipe_neg$ind, sep = "") 
    
    #step 2: get the time profiles matching the indices of history_swipe to train the model
    dat_pos <- read.table(file = "./data/shinyPosData.txt", sep = ",", header = TRUE)
    dat_neg <- read.table(file = "./data/shinyNegData.txt", sep = ",", header = TRUE)
    dat_current <- read.table(file = paste("./data/shiny",input$Polarity,"Data.txt",sep = ""), sep = ",", header = TRUE)
    
    trainingData_pos <- dat_pos[dat_pos$index %in% history_swipe_pos$ind ,]
    if (nrow(trainingData_pos) > 0) rownames(trainingData_pos) <- paste("Pos_", trainingData_pos$index, sep = "") 
    trainingData_neg <- dat_neg[dat_neg$index %in% history_swipe_neg$ind ,]
    if (nrow(trainingData_neg) > 0) rownames(trainingData_neg) <- paste("Neg_", trainingData_neg$index, sep = "") 
    # reorder to match with history_swipe
    trainingData_pos <- trainingData_pos[match(history_swipe_pos$ind, trainingData_pos$index), (xtraVar+1):ncol(dat_pos)]
    trainingData_neg <- trainingData_neg[match(history_swipe_neg$ind, trainingData_neg$index), (xtraVar+1):ncol(dat_neg)]
    trainingData = rbind(trainingData_pos,trainingData_neg)
    
    trainingLabels_pos <- history_swipe_pos$swipe == "Right"
    trainingLabels_neg <- history_swipe_neg$swipe == "Right"
    trainingLabels = as.factor(c(trainingLabels_pos,trainingLabels_neg))
    
    RF.model <- randomForest::randomForest(x = trainingData,
                                           y = trainingLabels, 
                                           ntree = 500, 
                                           importance = TRUE)
    
    # step 3: use model to predict unseen data
    PredictData <- dat_current[dat_current$qSvsMB <= input$qValue & 
                                   dat_current$qSvsNC <= input$qValue & 
                                   !dat_current$index %in% history_swipe_current$ind, (xtraVar+1):ncol(dat_current)]
    
    if(any(!colnames(PredictData) == colnames(trainingData))){
        stop("Something is wrong with the training data and testing data columns. They do not match properly.")
    }
    
    predicted.probs <- stats::predict(object = RF.model, 
                                      newdata = PredictData, 
                                      type = "prob")[,2]
    
    # set the 'modelPredicted' variable of the Predicted Data to TRUE
    dat_current$modelPredicted[dat_current$qSvsMB <= input$qValue & 
                                   dat_current$qSvsNC <= input$qValue & 
                               !dat_current$index %in% history_swipe_current$ind] <- TRUE
    
    # change the 'predictVal' variable of the Predicted Data to the corresponding probability
    dat_current$predictVal[dat_current$qSvsMB <= input$qValue & 
                               dat_current$qSvsNC <= input$qValue & 
                           !dat_current$index %in% history_swipe_current$ind] <- as.numeric(predicted.probs)
    # write the new data
    write.table(dat_current, file = paste("./data/shiny",input$Polarity,"Data.txt",sep = ""), sep = ",")
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
                       radioButtons("Preference", "Preference", choiceNames = list(
                           icon("github-alt"),
                           icon("venus"),
                           icon("mars")
                       ),
                       choiceValues = list(
                           "kitten" , "female",  "male"
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
                       radioButtons("modelFilter", "Model based filter", choiceNames = list(
                           "no",
                           "yes"
                       ),
                       choiceValues = list(
                           "FALSE" , "TRUE"
                       ), 
                       inline = TRUE
                       )
                )
            ),
            fluidRow(
                column(6,
                       numericInput("qValue", "max q value", 0.2, min = 0.0, max = 1.0, step = 0.1)
                ),
                column(6, 
                       numericInput("minRT", "minimal RT (min)", 0, min = 0, step = 1)
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
                column(6, actionButton("undo", 
                                       "Oopsie daisy",  
                                       style="color:#fff; background-color:Crimson"),
                       align = "center", 
                       offset = 0 ),
                column(6, actionButton("ModelPredict", 
                                        "Update Predictions",  
                                        style="color:#fff; background-color:DodgerBlue"),
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
        useless <- input$modelFilter
        dat <- read.table(file = paste("./data/shiny",input$Polarity,"Data.txt",sep = ""), sep = ",", header = TRUE)
        dat
    })
    
    dataSubset <- reactive({
        # the getHistory function knows which selection is to be made (because we give it input): user or natura based
        subset.selection <- rep(FALSE, nrow(dataSet()))
        if(input$modelFilter){
            subset.selection[dataSet()$qSvsMB <= input$qValue &
                                 dataSet()$qSvsNC<= input$qValue &
                                 dataSet()$rtmed >= (60*input$minRT) &
                                 ! dataSet()$index %in% getHistory(input) &
                                 dataSet()$predictVal > 0.5] <- TRUE
        }else{
            subset.selection[dataSet()$qSvsMB <= input$qValue &
                                 dataSet()$qSvsNC<= input$qValue &
                                 dataSet()$rtmed >= (60*input$minRT) &
                                 ! dataSet()$index %in% getHistory(input) ] <- TRUE   
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
            if(input$Preference == "male"){
                showModal(modalDialog(
                    modalButton(label = img(src=paste("male_celebs/male",sample(nmales,1),".jpg", sep = ""), height = 300), icon = NULL),
                    easyClose = TRUE
                ))
            } else if(input$Preference == "kitten"){
                showModal(modalDialog(
                    modalButton(label = img(src="kat.gif", height = 300), icon = NULL),
                    easyClose = TRUE
                ))
            } else if(input$Preference == "female"){
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
            if(input$Preference == "male"){
                showModal(modalDialog(
                    modalButton(label = img(src=paste("male_celebs/male",sample(nmales,1),".jpg", sep = ""), height = 300), icon = NULL),
                    easyClose = TRUE
                ))
            } else if(input$Preference == "kitten"){
                showModal(modalDialog(
                    modalButton(label = img(src="kat.gif", height = 300), icon = NULL),
                    easyClose = TRUE
                ))
            } else if(input$Preference == "female"){
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
            if(input$Preference == "male"){
                showModal(modalDialog(
                    modalButton(label = img(src=paste("male_celebs/male",sample(nmales,1),".jpg", sep = ""), height = 300), icon = NULL),
                    easyClose = TRUE
                ))
            } else if(input$Preference == "kitten"){
                showModal(modalDialog(
                    modalButton(label = img(src="kat.gif", height = 300), icon = NULL),
                    easyClose = TRUE
                ))
            } else if(input$Preference == "female"){
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
            if(input$Preference == "male"){
                showModal(modalDialog(
                    modalButton(label = img(src=paste("male_celebs/male",sample(nmales,1),".jpg", sep = ""), height = 300), icon = NULL),
                    easyClose = TRUE
                ))
            } else if(input$Preference == "kitten"){
                showModal(modalDialog(
                    modalButton(label = img(src="kat.gif", height = 300), icon = NULL),
                    easyClose = TRUE
                ))
            } else if(input$Preference == "female"){
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
    observeEvent( input$ModelPredict,{
        
        showModal(modalDialog(
            title = "Updating model predictions",
            "The model predictions have been updated. You will no longer see time profiles that the model flagged as uninteresting.",
            easyClose = TRUE
        ))
        
        UpdateModelPredictions(input, xtraVar)
    }) #close event observe.
    
    session$onSessionEnded(function() {
        source("app_data_updater.R")
        
    })
}

shinyApp(ui, server)
