# update profile database with new column for model predictions
dat <- read.table(file = paste("./data/shinyPosData_old.txt",sep = ""), sep = ",", header = TRUE)
View(dat)
dat = cbind(dat[,1:6], modelPredicted = FALSE, predictVal = 1, dat[,7:41])
write.table(dat, file = paste("./data/shinyPosData.txt",sep = ""), sep = ",")


# get the database

sqlitePath <- "data/SwipeHistory.sqlite"
mydb <- DBI::dbConnect(RSQLite::SQLite(), sqlitePath)
swipedata <- DBI::dbReadTable(mydb, "swipes")
DBI::dbDisconnect(mydb)



# data
inputData <- read.table(file = paste("./data/shinyPosData.txt",sep = ""), sep = ",", header = TRUE)
inputDataNeg <- read.table(file = paste("./data/shinyNegData.txt",sep = ""), sep = ",", header = TRUE)

LauraSwipes = swipedata[swipedata$user == "Laura", c("ind", "swipe")]
LauraSwipes = LauraSwipes[!duplicated(LauraSwipes),]
LauraSwipes$ind = as.integer(LauraSwipes$ind)
LauraData = inputData[inputData$index %in% LauraSwipes$ind,]


if(!all(LauraData$index == LauraSwipes$ind)){
    LauraData = LauraData[order(LauraData$index),]
    LauraSwipes = LauraSwipes[order(LauraSwipes$ind),]
}

if(all(LauraData$index == LauraSwipes$ind)){
    LauraData$swipe = LauraSwipes$swipe
} else{
    stop("Check this shit Charlie!")
}

MetaboMeeseeks::Meeseeks.RF(LauraData[LauraData$swipe != "Up",7:41], as.factor(LauraData$swipe[LauraData$swipe != "Up"]) )
RFtest = Meeseeks.RF.test(LauraData[LauraData$swipe != "Up",7:41], LauraData$swipe[LauraData$swipe != "Up"] == "Left" )


UpData <- LauraData[LauraData$swipe == "Up",7:41]
UnreviewedData <- inputData[!inputData$index %in% LauraSwipes$ind & inputData$qSvsMB <= 0.4 & inputData$qSvsNC <= 0.4, 7:41]
NegData <- inputDataNeg[inputDataNeg$qSvsMB <= 0.2 & inputDataNeg$qSvsNC <0.2 , 7:41]
trainData <- LauraData[LauraData$swipe != "Up",7:41]
trainLabels <- as.factor(LauraData$swipe[LauraData$swipe != "Up"] == "Left")

RF.model <- randomForest::randomForest(x = trainData,
                                       y = trainLabels, 
                                       ntree = 500, 
                                       importance = TRUE)

predlabel.probs <- stats::predict(object = RF.model, 
                                  newdata = UpData, 
                                  type = "prob")


predlabel.probs <- stats::predict(object = RF.model, 
                                  newdata = UnreviewedData, 
                                  type = "prob")
predlabel.vote <- stats::predict(object = RF.model, 
                                  newdata = UnreviewedData)

predlabel.probs <- stats::predict(object = RF.model, 
                                  newdata = NegData, 
                                  type = "prob")


which(predlabel.probs[,2]<0.05)




#kk_all <- as.numeric(which(predlabel.probs[,2]<0.5)) # this one works
kk_all <- as.numeric(which(predlabel.probs[,2]>0.9))
#plotdataset = NegData
plotdataset = UnreviewedData
interestingness = "interesting" 
#interestingness = "uninteresting"
Extraplotname = "Point2toPoint4_"

for(iter in seq_along(kk_all)){
    kk <- kk_all[iter]
    time <- as.numeric(unlist(lapply(strsplit(colnames(plotdataset), "_"), `[[`, 2)))
    type <- as.factor(unlist(lapply(strsplit(colnames(plotdataset), "_"), `[[`, 1)))
    Group <- as.character(type)
    Group[Group %in% c("S1", "S2", "S3")] <- "S"
    Group <- as.factor(Group)
    compoundData <- data.frame(t = time, int = as.numeric(plotdataset[kk,]), type = type, group = Group)  
    plotname = paste("Left:", predlabel.probs[kk,1],"  Right:", predlabel.probs[kk,2], sep = "")
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
    
    gg <- grid.arrangeLocal(gg1,gg2, layout_matrix = rbind(c(1,1,2),c(1,1,2),c(1,1,2)))
    
    #ggsave(filename = paste("/Users/Charlie/Downloads/plots_uninteresting/plot",kk,".pdf",sep = ""), plot = gg)
    ggsave(filename = paste("/Users/Charlie/Downloads/plots_",interestingness,"/plot",Extraplotname,kk,".pdf",sep = ""), plot = gg)
    
}