# get the database
sqlitePath <- "swiperespons.sqlite"
mydb <- DBI::dbConnect(RSQLite::SQLite(), sqlitePath)
if("swipes" %in% DBI::dbListTables(mydb) ){
    swipedata <- DBI::dbReadTable(mydb, "swipes")
    data.exists = TRUE
    DBI::dbRemoveTable(mydb, "swipes")
} else{
    data.exists = FALSE
}
DBI::dbDisconnect(mydb)

if(data.exists){
    
    swipedata <- subset(swipedata, select = - iter )
    
    # update profile data files
    swipedata_Pos <- swipedata[swipedata$polarity == "Pos", ,drop = FALSE]
    swipedata_Neg <- swipedata[swipedata$polarity == "Neg", ,drop = FALSE]
    
    NatuRA_members = c("Ken", "Kenne", "Laura", "Sebastiaan", "Seb", "Deborah", "Anastasia", "Luc")
    
    swipedata_Pos_NatuRA <- swipedata[swipedata$polarity == "Pos" & tolower(swipedata$user) %in% tolower(NatuRA_members), ,drop = FALSE]
    swipedata_Neg_NatuRA <- swipedata[swipedata$polarity == "Neg" & tolower(swipedata$user) %in% tolower(NatuRA_members), ,drop = FALSE]
    
    # Positive polarity data
    if(nrow(swipedata_Pos_NatuRA) > 0){
        shinyPosData <- read.table( file = "./data/shinyPosData.txt", sep = ",", header = T)
        
        for(k in 1:nrow(swipedata_Pos_NatuRA)){
            current_swipe = swipedata_Pos_NatuRA[k,,drop=F]
            shinyPosData$swipesTotal[shinyPosData$index == current_swipe$ind] <- shinyPosData$swipesTotal[shinyPosData$index == current_swipe$ind] + 1
            if(current_swipe$swipe == "Right"){
                shinyPosData$swipesRight[shinyPosData$index == current_swipe$ind] <- shinyPosData$swipesRight[shinyPosData$index == current_swipe$ind] + 1
            }
        }
        #file.remove("./data/shinyPosData.txt")
        write.table(x = shinyPosData, file = "./data/shinyPosData.txt", sep = ",", col.names = TRUE, row.names = FALSE,append = FALSE)
        
    }
    
    if(nrow(swipedata_Pos) > 0){
       
        swipedata_Pos$date = as.character(Sys.time())
        #file.remove("./data/SwipeHistoryPos.txt")
        write.table(x = swipedata_Pos, file = "./data/SwipeHistoryPos.txt", sep = ",", col.names = !file.exists("./data/SwipeHistoryPos.txt"), row.names = FALSE,append = TRUE)
    }
   
    # Negative polarity data
    if(nrow(swipedata_Neg_NatuRA) > 0){
        shinyNegData <- read.table( file = "./data/shinyNegData.txt", sep = ",", header = T)
        
        for(k in 1:nrow(swipedata_Neg_NatuRA)){
            current_swipe = swipedata_Neg_NatuRA[k,,drop=F]
            shinyNegData$swipesTotal[shinyNegData$index == current_swipe$ind] <- shinyNegData$swipesTotal[shinyNegData$index == current_swipe$ind] + 1
            if(current_swipe$swipe == "Right"){
                shinyNegData$swipesRight[shinyNegData$index == current_swipe$ind] <- shinyNegData$swipesRight[shinyNegData$index == current_swipe$ind] + 1
            }
        }
        #file.remove("./data/shinyNegData.txt")
        write.table(x = shinyNegData, file = "./data/shinyNegData.txt", sep = ",", col.names = TRUE, row.names = FALSE,append = FALSE)
    }
    
    if(nrow(swipedata_Neg) > 0){
        swipedata_Neg$date = as.character(Sys.time())
        #file.remove("./data/SwipeHistoryNeg.txt")
        write.table(x = swipedata_Neg, file = "./data/SwipeHistoryNeg.txt", sep = ",", col.names = !file.exists("./data/SwipeHistoryNeg.txt"), row.names = FALSE,append = TRUE)
    }
    
   
}
