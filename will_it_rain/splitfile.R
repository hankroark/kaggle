library(reshape2)
library(splitstackshape)
library(data.table)

organizeFile <- function(raw) {
  #split all the columns in the middle
  colsToSplit = colnames(raw)[2:19]
  split = cSplit(raw,splitCols=colsToSplit,sep=" ",direction="long", 
                 fixed=FALSE, makeEqual = FALSE)
  
  #make a data table
  dt = data.table(split)
  setkey(dt,Id)
  # find everytime new radar set starts, using time inversion or radar distance change
  # as indicator of radar changes
  dt[,TimeToEndInversion:=c(1,(sign(diff(TimeToEnd))+1)/2),by=Id]
  dt[,NewDistanceToRadar:=c(1,abs(sign(diff(DistanceToRadar)))),by=Id]
  dt[,NewRadarIndicator:=TimeToEndInversion | NewDistanceToRadar]
  # Now just cummulative sum of previous rows in group to incrementally number the radars
  dt[,RadarSeries := cumsum(NewRadarIndicator),by=Id]
  dt[,c("TimeToEndInversion", "NewDistanceToRadar", "NewRadarIndicator")] <- NULL
  return(dt)
}

tr_raw <- fread('train_2013.csv', sep=',', header=T, stringsAsFactors=F,
                colClasses=c(rep('integer',1), rep('character',18), 'numeric'), showProgress=T, data.table=T)
train <- organizeFile(tr_raw)
save("train",file="train2013.RData")
rm(tr_raw,train)

te_raw <- fread('test_2014.csv', sep=',', header=T, stringsAsFactors=F,
                colClasses=c(rep('integer',1), rep('character',18)), showProgress=T, data.table=T)
test <- organizeFile(te_raw)
save("test",file="test2014.RData")
rm(te_raw,test)
