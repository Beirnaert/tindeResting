# get the database
mydb <- dbConnect(RSQLite::SQLite(), sqlitePath)
dbListTables(mydb) 
swipedata <- dbReadTable(mydb, "swipes")
dbDisconnect(mydb)
unlink(sqlitePath)

# update profile data files
swipedata_Pos <- swipedata[swipedata$polarity == "Pos", ,drop = FALSE]
swipedata_Neg <- swipedata[swipedata$polarity == "Neg", ,drop = FALSE]

# Positive polarity data
if(nrow(swipedata_Pos) > 0){
    shinyPosData <- read.table( file = "shinyPosData.txt", sep = ",", header = T)
    
    for(k in 1:nrow(swipedata_Pos)){
        current_swipe = swipedata_Pos[k,,drop=F]
        shinyPosData$swipesTotal[shinyPosData$index == current_swipe$ind] <- shinyPosData$swipesTotal[shinyPosData$index == current_swipe$ind] + 1
        if(current_swipe$swipe == "Right"){
            shinyPosData$swipesRight[shinyPosData$index == current_swipe$ind] <- shinyPosData$swipesRight[shinyPosData$index == current_swipe$ind] + 1
        }
    }
    
    write.table(x = shinyPosData, file = "shinyPosData.txt", sep = ",", col.names = TRUE, row.names = FALSE,append = FALSE)
    
    swipedata_Pos$date = as.character(Sys.time())
    write.table(x = swipedata_Pos, file = "SwipeHistoryPos.txt", sep = ",", col.names = !file.exists("SwipeHistoryPos.txt"), row.names = FALSE,append = TRUE)
    
}

# Negative polarity data
if(nrow(swipedata_Neg) > 0){
    shinyNegData <- read.table( file = "shinyNegData.txt", sep = ",", header = T)
    
    for(k in 1:nrow(swipedata_Neg)){
        current_swipe = swipedata_Neg[k,,drop=F]
        shinyNegData$swipesTotal[shinyNegData$index == current_swipe$ind] <- shinyNegData$swipesTotal[shinyNegData$index == current_swipe$ind] + 1
        if(current_swipe$swipe == "Right"){
            shinyNegData$swipesRight[shinyNegData$index == current_swipe$ind] <- shinyNegData$swipesRight[shinyNegData$index == current_swipe$ind] + 1
        }
    }
    
    write.table(x = shinyNegData, file = "shinyNegData.txt", sep = ",", col.names = TRUE, row.names = FALSE,append = FALSE)
    
    swipedata_Neg$date = as.character(Sys.time())
    
    write.table(x = swipedata_Neg, file = "SwipeHistoryNeg.txt", sep = ",", col.names = !file.exists("SwipeHistoryNeg.txt"), row.names = FALSE,append = TRUE)
    
    
}
