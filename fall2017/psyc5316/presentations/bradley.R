library('ROCR') 
library('ggplot2')

classification <- sample(c(0,1),100,replace=TRUE)
method1 <- rnorm(length(classification),classification,1) 
method2 <- rnorm(length(classification),classification,2)

# bind all classifications to a data frame
rocData <- as.data.frame(cbind(classification,method1,method2))

predictions <- sapply(rocData[-classification], function(x) prediction(x,rocData$classification))

# AUC for ROC
performances <- sapply(predictions,function(x) performance(x,measure="auc")) 
for(i in 1:length(performances)){
  print(paste("AUC",names(performances[i]),slot(performances[i][[1]],"y.values")[[1]])) 
}

## Choose one of ROC or Precision-Recall
chooseROC <- TRUE # set to TRUE for ROC or FALSE for Precision-Recall

if(chooseROC){ 
  # ROC
  performances <- sapply(predictions, function(x) performance(x, measure = "tpr", x.measure = "fpr")) 
  } else{
    # Precision-Recall
    performances <- sapply(predictions, function(x) performance(x, measure="prec", x.measure="rec")) 
  }

metaData <- c()
for(i in 1:length(performances)) 
{
  metaData <- rbind(metaData,cbind(rep(names(performances[i]),length(performances[i][[1]]@alpha.values[[1]])))) 
}
rocCurve <- do.call(rbind,lapply(performances, function(x) cbind(unlist(slot(x,"x.values") [[1]]),unlist(slot(x,"y.values")[[1]]))))
rocCurve <- as.data.frame(cbind(metaData,rocCurve))
colnames(rocCurve) <- c("StatisticID","X","Y")
rocCurve$X <- as.numeric(levels(rocCurve$X))[rocCurve$X] rocCurve$Y <- as.numeric(levels(rocCurve$Y))[rocCurve$Y]

# write data
# write.table(rocCurve,"roc_data.Rdat")

# to read cached data
# rocCurve <- read.table("roc_data.Rdat",header=TRUE)

g <- ggplot(data=rocCurve,aes(x=X,y=Y,color=factor(StatisticID)))
# for (potentially ragged) line
g <- g + geom_line()
# for (smoothed) line uncomment following line and comment above line # g <- g + geom_smooth(se=FALSE,method="loess")
# draws y=x line for 'random' classification
g <- g + geom_abline(intercept = 0, slope = 1, linetype="dotted") 
if(chooseROC){
  g <- g + xlab("false positive rate") + ylab("true positive rate") + ggtitle("Receiver Operating Characteristic") 
} else{
    g <- g + xlab("recall") + ylab("precision") + ggtitle("Precision-Recall")
}
g <- g + theme_bw() # white background
g <- g + theme(text = element_text(size=18), axis.text.x = element_text(size=18), axis.text.y = element_text(size=18), # text size
              
print(g)