
lost = read.table("LostHistory.txt", sep = "", header = F)
colnames(lost) = c("user", "polarity", "ind" , "mz" , "rt" ,"swipe" , "date", "time")
lost$user = "Laura"
lost$swipe = as.character(lost$swipe)
lost$swipe[lost$swipe == "left"] = "Left"


lost$NatuRA = 1
lost= lost[,c("user", "NatuRA", "polarity", "ind", "mz", "rt", "swipe")] 




# mis = rep(0, nrow(lost))
# for(jj in 1:nrow(lost)){
#     mzdiff = lost$mz[jj] - shinyPosData$mzmed[shinyPosData$index == lost$ind[jj]]
#     rtdiff = lost$rt[jj] - shinyPosData$rtmed[shinyPosData$index == lost$ind[jj]]
#     if( rtdiff > 1){
#         mis[jj] = mis[jj] + 1
#     }
#     if(mzdiff > 1 ){
#         mis[jj] = mis[jj] + 10
#     }
#     
# }


lost$date = as.character(Sys.time())

historyfile = paste("./data/SwipeHistory.sqlite",sep = "")

Db <- DBI::dbConnect(RSQLite::SQLite(), historyfile)
DBI::dbWriteTable(Db, name="history", value=lost, row.names=FALSE, append=TRUE)
DBI::dbDisconnect(Db)

