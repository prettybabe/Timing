setwd("D:\\working\\Timing")
options(java.parameters="-Xmx4g")
library(RSQLServer)
library(dplyr)
library(lubridate)
library(ggplot2)  
library(reshape2)
library(TTR)
library(rCharts)
library(xlsx)

source('D:/working/Timing/MyFunction.R')
channel <- src_sqlserver(server="SQL",database="XY",user="libo.jin",password="123456")
tradingDay <- tbl(channel, "QT_TradingDayNew") %>%
  filter(SecuMarket == 83L, IfTradingDay==1L) %>%
  select(TradingDate, IfWeekEnd, IfMonthEnd, IfQuarterEnd, IfYearEnd) %>%
  collect %>%
  mutate(TradingDate = as.Date(TradingDate))
#############################################################################################################
#  原始数据 
data <- list()
data$Index <- read.xlsx("D:\\working\\Timing\\数据.xlsx", sheetIndex = 1, startRow = 3,
               header = TRUE, encoding = "UTF-8") %>%
  rename(Date = 日期, PreClosePrice =  前收盘价, OpenClose = 开盘价, HighPrice = 最高价,
         LowPrice = 最低价, ClosePrice = 收盘价, TurnoverVolume = 成交量)

data$IF00 <- read.xlsx("D:\\working\\Timing\\数据.xlsx", sheetIndex = 2, startRow = 3,
                                     header = TRUE, encoding = "UTF-8") %>%
  rename(Date = 日期, ClosePrice = 收盘价, SettlementPrice =  结算价)

data$Nasdaq <- read.xlsx("D:\\working\\Timing\\数据.xlsx", sheetIndex = 3, startRow = 3,
                       header = TRUE, encoding = "UTF-8") %>%
  rename(Date = 日期, ClosePrice = 收盘价)

data$SP <- read.xlsx("D:\\working\\Timing\\数据.xlsx", sheetIndex = 4, startRow = 3,
                       header = TRUE, encoding = "UTF-8") %>%
  rename(Date = 日期, ClosePrice = 收盘价)

data$Yield <- read.xlsx("D:\\working\\Timing\\数据.xlsx", sheetIndex = 5, startRow = 2,
                       header = TRUE, encoding = "UTF-8") %>%
  rename(Date = 指标名称, OneYear =  中国固定利率国债到期收益率.1年, TwoYear = 中国固定利率国债到期收益率.2年,
         FiveYear = 中国固定利率国债到期收益率.5年, TenYear = 中国固定利率国债到期收益率.10年,
         ThirtyYear = 中国固定利率国债到期收益率.30年)

data$PMI <- read.xlsx("D:\\working\\Timing\\数据.xlsx", sheetIndex = 6, startRow = 2,
                       header = TRUE, encoding = "UTF-8") %>%
  rename(Date = 指标名称, PMI =  汇丰PMI.制造业, InfoPublicDate = 发布时间)

data$CPI <- read.xlsx("D:\\working\\Timing\\数据.xlsx", sheetIndex = 7, startRow = 2,
                      header = TRUE, encoding = "UTF-8") %>%
  rename(Date = 指标名称, CPI =  CPI.当月同比, InfoPublicDate = 发布时间)

data$PPI <- read.xlsx("D:\\working\\Timing\\数据.xlsx", sheetIndex = 8, startRow = 2,
                      header = TRUE, encoding = "UTF-8") %>%
  rename(Date = 指标名称, PPI =  PPI.当月同比, InfoPublicDate = 发布时间)

data$M2 <- read.xlsx("D:\\working\\Timing\\数据.xlsx", sheetIndex = 9, startRow = 2,
                      header = TRUE, encoding = "UTF-8") %>%
  rename(Date = 指标名称, M2 =  M2.货币和准货币..期末值, InfoPublicDate = 发布时间)

data$IP <- read.xlsx("D:\\working\\Timing\\数据.xlsx", sheetIndex = 10, startRow = 2,
                     header = TRUE, encoding = "UTF-8") %>%
  rename(Date = 指标名称, IP =  工业增加值.当月同比, InfoPublicDate = 发布时间)

data$Value <- read.xlsx("D:\\working\\Timing\\数据.xlsx", sheetIndex = 11,
                     header = TRUE, encoding = "UTF-8") 

data$Rsquare <- read.xlsx("D:\\working\\Timing\\数据.xlsx", sheetIndex = 12,
                     header = TRUE, encoding = "UTF-8")

#############################################################################################################
# 处理数据
# 日度数据
data_day <- list()
data_day$Momentum <- data$Index %>%
  mutate(Return =  ClosePrice/PreClosePrice - 1) %>%
  mutate(Momentum15D = Momentum(Return, 15)) %>%
  select(Date, Momentum15D)

data_day$SP <- data$SP %>%
  mutate(Return = ClosePrice/lag(ClosePrice, 5) - 1) %>%
  select(Date, Return)
data_day$SP <- data.frame(Date = data$Index$Date, SP = AdjustUSA(data$Index$Date, data_day$SP))
 

data_day$Nasdaq <- data$Nasdaq %>%
  mutate(Return = ClosePrice/lag(ClosePrice, 5) - 1) %>%
  select(Date, Return)
data_day$Nasdaq <- data.frame(Date = data$Index$Date, Nasdaq = AdjustUSA(data$Index$Date, data_day$Nasdaq))

  
data_day$Volatility <- data$Index %>%
  mutate(Return =  ClosePrice/PreClosePrice - 1) %>%
  mutate(Volatility10D = Volatility(Return, 10), Volatility5D = Volatility(Return, 5)) %>%
  select(Date, Volatility5D, Volatility10D)

data_day$Volume <- data$Index %>%
  mutate(Volume3d = Average(TurnoverVolume, 3), Volume5d = Average(TurnoverVolume, 5),
         Volume10d = Average(TurnoverVolume, 10)) %>%
  select(Date, Volume3d, Volume5d, Volume10d)

data_day$Yield <- data$Yield %>% 
  mutate(TenYearMinusTwoYear = TenYear - TwoYear, FiveYearMinusTwoYear = FiveYear - TwoYear,
         TwoYearChange = TwoYear - lag(TwoYear, 5), FiveYearChange = FiveYear - lag(FiveYear, 5),
         TenYearChange = TenYear - lag(TenYear, 5), TenYearMinusTwoYearChange = TenYearMinusTwoYear - lag(TenYearMinusTwoYear, 5),
         FiveYearMinusTwoYearChange = FiveYearMinusTwoYear - lag(FiveYearMinusTwoYear, 5)) 
data_day$Yield <- data.frame(Date = data$Index$Date, 
                             TwoYear = AdjustUSA(data$Index$Date, select(data_day$Yield, Date, TwoYear)),
                             TwoYearChange = AdjustUSA(data$Index$Date, select(data_day$Yield, Date, TwoYearChange)),
                             FiveYear = AdjustUSA(data$Index$Date, select(data_day$Yield, Date, FiveYear)),
                             FiveYearChange = AdjustUSA(data$Index$Date, select(data_day$Yield, Date, FiveYearChange)),
                             TenYear = AdjustUSA(data$Index$Date, select(data_day$Yield, Date, TenYear)),
                             TenYearChange = AdjustUSA(data$Index$Date, select(data_day$Yield, Date, TenYearChange)),
                             FiveYearMinusTwoYear = AdjustUSA(data$Index$Date, select(data_day$Yield, Date, FiveYearMinusTwoYear)),
                             FiveYearMinusTwoYearChange = AdjustUSA(data$Index$Date, select(data_day$Yield, Date, FiveYearMinusTwoYearChange)),
                             TenYearMinusTwoYear = AdjustUSA(data$Index$Date, select(data_day$Yield, Date, TenYearMinusTwoYear)),
                             TenYearMinusTwoYearChange = AdjustUSA(data$Index$Date, select(data_day$Yield, Date, TenYearMinusTwoYearChange)))
                             




data_day$CloseMinusSettle <- data$IF00 %>%
  mutate(CloseMinusSettle = ClosePrice - SettlementPrice) %>%
  select(Date, CloseMinusSettle)

data_day$Value <- data$Value

data_day$Rsquare <- data$Rsquare

#################################################################################################################
# 月度数据 
data_month <- list()
data_month$PMI <- data$PMI %>%
  mutate(PMIChange = PMI - lag(PMI, 1)) %>%
  mutate(PMIScore = Score(PMI, 6), PMIChangeScore = Score(PMIChange, 6, IsMinusMean = 0))
data_month$PMI$AdjustInfoPublicDate <- as.Date(AdjustDate(data$Index$Date, data_month$PMI$InfoPublicDate))

data_month$CPI <- data$CPI %>%
  mutate(CPIChange = CPI - lag(CPI, 1)) %>%
  mutate(CPIScore = Score(CPI, 6), CPIChangeScore = Score(CPIChange, 6, IsMinusMean = 0))
data_month$CPI$AdjustInfoPublicDate <- as.Date(AdjustDate(data$Index$Date, data_month$CPI$InfoPublicDate))

data_month$PPI <- data$PPI %>%
  mutate(PPIChange = PPI - lag(PPI, 1)) %>%
  mutate(PPIScore = Score(PPI, 6), PPIChangeScore = Score(PPIChange, 6, IsMinusMean = 0))
data_month$PPI$AdjustInfoPublicDate <- as.Date(AdjustDate(data$Index$Date, data_month$PPI$InfoPublicDate))

data_month$M2 <- data$M2 %>%
  mutate(M2Ratio = log(M2) - log(lag(M2, 12))) %>%
  mutate(M2Score = Score(M2Ratio, 6))
data_month$M2$AdjustInfoPublicDate <- as.Date(AdjustDate(data$Index$Date, data_month$M2$InfoPublicDate))

data_month$IP <- data$IP %>%
  mutate(IPFill = Fill(IP, 11)) %>%
  mutate(IPScore = Score(IPFill, 6))
data_month$IP$AdjustInfoPublicDate <- as.Date(AdjustDate(data$Index$Date, data_month$IP$InfoPublicDate))

###########################################################################################################
# 日期序列
date_weekly <- tradingDay %>% 
  filter(IfWeekEnd == 1) %>%
  arrange(TradingDate) %>%
  mutate(Lag = lag(TradingDate, 1)) %>%
  select(TradingDay = Lag, ForecastDay = TradingDate) %>% 
  filter(!is.na(TradingDay))

##########################################################################################################
# 周度数据
data_week <- list()

data_week$UnMinusMean <- data_day$Momentum %>%
  semi_join(date_weekly, by = c("Date" = "TradingDay")) %>%
  left_join(data_day$SP, by = "Date") %>%
  left_join(data_day$Nasdaq, by = "Date")

data_week$MinusMean <- data_day$Volatility %>%
  semi_join(date_weekly, by = c("Date" = "TradingDay")) %>%
  left_join(data_day$Value, by = "Date") %>%
  left_join(data_day$Rsquare, by = "Date") %>%
  left_join(data_day$CloseMinusSettle,  by = "Date") %>%
  left_join(data_day$Yield, by = "Date")

#########################################################################################################
# 

score <-
