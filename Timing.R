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

#############################################################################################################
#  原始数据 
data <- list()
data$Index <- read.xlsx("D:\\working\\Timing\\数据.xlsx", sheetIndex = 1, startRow = 3,
               header = TRUE, encoding = "UTF-8") %>%
  rename(Date = 日期, PreClosePrice =  前收盘价, OpenClose = 开盘价, HighPrice = 最高价,
         LowPrice = 最低价, ClosePrice = 收盘价, TurnoverVolume = 成交量)

data$IF00 <- read.xlsx("D:\\working\\Timing\\数据.xlsx", sheetIndex = 2, startRow = 3,
                                     header = TRUE, encoding = "UTF-8") %>%
  rename(Date = 日期, PreClosePrice =  前收盘价, ClosePrice = 收盘价)

data$Nasdaq <- read.xlsx("D:\\working\\Timing\\数据.xlsx", sheetIndex = 3, startRow = 3,
                       header = TRUE, encoding = "UTF-8") %>%
  rename(Date = 日期, PreClosePrice =  前收盘价, ClosePrice = 收盘价)

data$SP <- read.xlsx("D:\\working\\Timing\\数据.xlsx", sheetIndex = 4, startRow = 3,
                       header = TRUE, encoding = "UTF-8") %>%
  rename(Date = 日期, PreClosePrice =  前收盘价, ClosePrice = 收盘价)

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

#############################################################################################################
# 







