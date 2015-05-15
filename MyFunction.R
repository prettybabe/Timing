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
    temp <- USA[USA[, 1] <= China[i], ]
    NewDate[i] <- temp[nrow(temp), 2]
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
  return(p)
}


YearlyReturn <- function(Data, kNumber){
  Return = exp(mean(log1p(Data))*kNumber) - 1
  return(Return)
}
YearlySD <- function(Data, kNumber){
  SD = sd(Data)*sqrt(kNumber)
  return(SD)
}
YearlyIR <- function(Data, kNumber){
  Return = exp(mean(log1p(Data))*kNumber) - 1
  SD = sd(Data)*sqrt(kNumber)
  IR = Return/SD
  return(IR)
}

TotalPerformance <- function(Data, kNumber){
   output <- data.frame(Return = exp(mean(log1p(Data))*kNumber) - 1,
              SD = sd(Data)*sqrt(kNumber)) %>%
     mutate(IR = Return/SD, MaxDrowdown = MaxDropDownRatio(Data))
  return(output)
}

YearPerformance <- function(Data, kNumber){
  temp <- lapply(Data, TotalPerformance, kNumber)
  output <- as.data.frame(lapply(temp, unlist))
  return(output)
}

Probably <- function(Data){
  temp <- ifelse(Data > 0, 1, 0) 
  count <- data.frame(AllNumber = length(temp), RightNumber = sum(temp)) %>%
    mutate(WinPro = RightNumber/AllNumber)
  return(count)
}

Performance <- function(Data, kNumber){
  temp <- melt(Data, id = "Date") %>% mutate(Year = format(Date, "%Y")) %>%
    group_by(Year, variable) %>%
    summarise(Return = exp(mean(log1p(value))*kNumber) - 1,
              SD = sd(value)*sqrt(kNumber),
              IR = Return/SD, 
              MaxDrowdown = MaxDropDownRatio(value)) %>%
    ungroup()
  
  p1 <- hPlot(x = "Year", y = "Return", group = "variable", data = temp, type = "column")
  p2 <- hPlot(x = "Year", y = "SD", group = "variable", data = temp, type = "column")
  p3 <- hPlot(x = "Year", y = "IR", group = "variable", data = temp, type = "column")
  p4 <- hPlot(x = "Year", y = "MaxDrowdown", group = "variable", data = temp, type = "column")

  print(p1)
  print(p2)
  print(p3)
  print(p4)
}


MaxDropDownRatio <-  function(returns){
  maxdropdown <- 0
  cum.return <- cumsum(log1p(returns))
  kNumber <- length(cum.return)
  if(maxdropdown > cum.return[1]) maxdropdown <- cum.return[1]
  
  for(i in c(2:kNumber)){
    temp <- cum.return[i] - max(cum.return[1:i])
    if(maxdropdown >  temp) maxdropdown <- temp
  }
  maxdropdown <- exp(maxdropdown) - 1
  return(maxdropdown)
}

Show <- function(forecast, index, date, knumber = 52){
  names(forecast) <- c("Date", "Score")
  names(index) <- c("Date", "Return")
  names(date) <- c("Date", "ForecastDate")
  
  returns <- forecast %>%
    filter(!is.na(Score)) %>%
    left_join(date, by = "Date") %>%
    left_join(index, by = c("ForecastDate" = "Date")) %>%
    arrange(Date) %>%
    mutate(LongOnly = ifelse(Score > 0, Return, 0),
           LongShort = ifelse(Score > 0, Return, -Return)) %>%
    filter(!is.na(Return))
  
  
  p <- PlotCumlateReturn(returns %>% select(Date = ForecastDate, Index = Return, LongOnly, LongShort))
  print(p)
  a <- YearPerformance(returns %>% select(Index = Return, LongOnly, LongShort), 52)
  cat("Summary\n\n")
  print(a)
  cat("\n\n")
  
  temp <- returns %>% 
    mutate(Forecast = Score * Return) %>%
    select(Forecast)
  
  cat("胜率\n\n")
  print(Probably(temp))
  cat("\n\n")
  
  
  Year <- returns %>%
    select(Date = ForecastDate, Return, LongOnly, LongShort) %>%
    mutate(Year = format(Date, "%Y")) %>%
    select(-Date)
  
  Year <- melt(Year, id = "Year")
  YearReturn <- Year %>% 
    group_by(Year, variable) %>%
    summarise(Return = YearlyReturn(value, 52)) %>%
    ungroup()
  YearReturn <- dcast(YearReturn, Year ~ variable) %>%
    rename(Index = Return)

  YearIR <- Year %>% 
    group_by(Year, variable) %>%
    summarise(IR = YearlyIR(value, 52)) %>%
    ungroup()
  YearIR <- dcast(YearIR, Year ~ variable) %>%
    rename(Index = Return)
  
  cat("Return\n\n")
  print(YearReturn)
  cat("\n\n")
  cat("IR\n\n")
  print(YearIR)
  cat("\n\n")
 
  Performance(returns %>%  select(Date = ForecastDate, Return, LongOnly, LongShort), knumber)
}


PerformanceCompare <- function(forecast, index, date, knumber = 52){
  names(index) <- c("Date", "Return")
  names(date) <- c("Date", "ForecastDate")
  
  LongOnly <- function(forecast, Return){
    temp <- data.frame(Forecast = forecast, Return = Return)
    returns <- temp %>%
      mutate(LongOnly = ifelse(Forecast > 0, Return, 0))
    return(returns$LongOnly)
  }
  
  LongShort <- function(forecast, Return){
    temp <- data.frame(Forecast = forecast, Return = Return)
    returns <- temp %>%
      mutate(LongShort = ifelse(Forecast > 0, Return, -Return))
    return(returns$LongShort)
  }
  
  Win <- function(forecast, index){
    temp <- ifelse(forecast * index > 0, 1, 0)
    winpro <- sum(temp)/length(temp)
    return(winpro)
  }
  

  temp <- forecast %>%
    left_join(date, by = "Date") %>%
    left_join(index, by = c("ForecastDate" = "Date")) 
  temp <- na.omit(temp)
  
  
  temp_forecast <- temp %>% select(-Date, -ForecastDate, -Return) 
  temp_index <- temp$Return
  longonly <- data.frame(Date = temp$ForecastDate,
                         as.data.frame(lapply(temp_forecast, LongOnly, temp_index)))
  
  longshort <- data.frame(Date = temp$ForecastDate,
                          as.data.frame(lapply(temp_forecast, LongShort, temp_index)))
  
  winpro <- as.data.frame(lapply(temp_forecast, Win, temp_index))
  
  year_return_longonly <- as.data.frame(lapply(longonly %>% select(-Date), YearlyReturn, knumber))
  year_sd_longonly <- as.data.frame(lapply(longonly %>% select(-Date), YearlySD, knumber))
  year_ir_longonly <- as.data.frame(lapply(longonly %>% select(-Date), YearlyIR, knumber))
  
  a <- rbind(year_return_longonly, year_sd_longonly, year_ir_longonly)
  row.names(a) <- c("Return", "SD", "IR")
  
  
  year_return_longshort <- as.data.frame(lapply(longshort %>% select(-Date), YearlyReturn, knumber))
  year_sd_longshort <- as.data.frame(lapply(longshort %>% select(-Date), YearlySD, knumber))
  year_ir_longshort <- as.data.frame(lapply(longshort %>% select(-Date), YearlyIR, knumber))
  b <- rbind(year_return_longshort, year_sd_longshort, year_ir_longshort)
  row.names(b) <- c("Return", "SD", "IR")
  
  p1 <- PlotCumlateReturn(longonly)
  p1$title(text = "LongOnly")
  print(p1)
  p2 <- PlotCumlateReturn(longshort)
  p2$title(text = "LongShort")
  print(p2)
  
  cat("胜率\n")
  print(winpro)
  
  cat("\n")
  cat("LongOnly\n")
  print(a)
  
  cat("\n")
  cat("LongShort\n")
  print(b)
}






