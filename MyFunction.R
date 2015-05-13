Momentum <- function(Data, Days){
  Data <- as.vector(Data)
  Momentum <- vector()
  for(i in c(1:length(Data))){
    if(i < Days) Momentum[i] <- NA else Momentum[i] <- exp(sum(log1p(Data[(i-Days+1):i]))) - 1
  }
  return(Momentum)
}

Volatility <- function(Data, Days){
  Data <- as.vector(Data)
  Volatility <- vector()
  for(i in c(1:length(Data))){
    if(i < Days) Volatility[i] <- NA else Volatility[i] <- sd(Data[(i-Days+1):i])
  }
  return(Volatility)
}

Average <- function(Data, Days){
  Data <- as.vector(Data)
  Average <- vector()
  for(i in c(1:length(Data))){
    if(i < Days) Average[i] <- NA else Average[i] <- mean(Data[(i-Days+1):i])
  }
  return(Average)
}

Fill <- function(Data, Days){
  Data <- as.vector(Data)
  Fill <- Data
  for(i in c(1:length(Data))){
    if(is.na(Fill[i])) Fill[i] <- mean(Data[(i-Days):(i-1)])
  }
  return(Fill)
}

AdjustDate <- function(TradingDate, InfoPublicDate){
  InfoPublicDate <- as.vector(InfoPublicDate)
  NewDate <- vector()
  for(i in c(1:length(InfoPublicDate))){
    temp <- TradingDate[TradingDate<=InfoPublicDate[i]]
    NewDate[i] <- temp[length(temp)]
  }
  return(NewDate)
}

AdjustUSA <- function(China, USA){
  NewDate <- vector()
  for(i in c(1:length(China))){
    temp <- USA[USA[, 1] <= China[i], 2]
    NewDate[i] <- temp[length(temp)]
  }
  return(NewDate)
}


Score <- function(Data, ScoreNumber, IsMinusMean = 1){
  Data <- as.vector(Data)
  Score <- vector()
  for(i in c(ScoreNumber:length(Data))){
    if(i >= ScoreNumber){
      if(sum(is.na(Data[(i-ScoreNumber+1):i])) == 0){
        if(IsMinusMean == 1){
          Score[i] <- (Data[i] - mean(Data[(i-ScoreNumber+1):i]))/sd(Data[(i-ScoreNumber+1):i])
        }else{
          Score[i] <- Data[i]/sd(Data[(i-ScoreNumber+1):i])
        }
      }
    }
  }
  return(Score)
}


Count <- function(Data, Index, SumNumber = 8){
  count <- ifelse(Data*Index > 0, 1, 0)
  sumcount <- vector()
  for(i in c(SumNumber:length(count))){
      sumcount[i] <- sum(count[(i-SumNumber+1):i])
  }
  return(sumcount)
}

PlotCumlateReturn <- function(Data){
  a <- melt(Data, id = c("Date")) %>%
    group_by(variable) %>%
    arrange(Date) %>%
    mutate(CumulateReturn = exp(cumsum(log1p(value))) - 1,
           Date = as.numeric(as.POSIXct(Date))*1000) %>%
    ungroup()
  
  p <- hPlot(x = "Date", y = "CumulateReturn", data = a , type = "line", group = "variable")
  p$xAxis(type = 'datetime', labels = list(format = '{value:%Y-%M-%d}' ))
  print(p)
}

TotalPerformance <- function(Data){
  a <- melt(Data, id = c("Date")) %>%
    group_by(variable) %>%
    summarise(Return = exp(mean(log1p(value))*52) - 1,
              SD = sd(value)*sqrt(52),
              IR = Return/SD)
  return(a)
}
