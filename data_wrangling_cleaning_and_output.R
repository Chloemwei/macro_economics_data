# libraries import

library(tidyverse)
install.packages("plyr")
library(plyr)
library(stringr)
library(ggplot2)

# data import
original <- read.csv("second_stage_concat_data/各国月度hs6位出口_20210429.csv")

tm <- readxl::read_excel("second_stage_concat/TM.xlsx")
head(tm)

#cmdCode needs to be 6-digit, sometimes it is wrote as 23456, but more accurately should be 023456
#Otherwise, will have issues while merge with dictionary
tm$cmdCode = str_pad(tm$cmdCode, 6, side = "left", "0")
tm1 <- tm %>%
  mutate(cmdCode = as.character(cmdCode))

# another set of data which have been classified
EU <- readxl::read_excel("second_stage_concat_data/EU完成版.xlsx")
EU$cmdCode = str_pad(EU$cmdCode, 6, side = "left", "0")

#dictionary of classification
dic <- readxl::read_excel("dic_fin.xlsx") %>%
  select(2:3)
#medical goods should be excluded, since COVID pandemic has caused abnormal values of this class
med <- readxl::read_excel("医疗物资.xls")
med_code <- med$hs6

#usa has unique data source ... 
usa <- readxl::read_excel("data/usa.xlsx") %>%
  select(-c(group))

#china data
china <- readxl::read_excel("中国分四大类进出口&消费品细分20210714.xlsx", sheet = "去除医疗物资") %>%
  select(1:6) %>%
  drop_na() 



# did this to filter data that is wrongly calculated (needed to *1000), and remove aggregated data

df <- original %>%
  filter(cmdCode == "TOTAL")

'%ni%' <- Negate('%in%')

incorrect_period <- unique(df$period)
incorrent_rtCode <- unique(df$rtCode)
#data5 %>% filter((period %in% incorrect_period) & (rtCode %ni% incorrent_rtCode))
incorrent_rtCode <- c(32,36,76,152,398,410,458,566,608,643,702,710,757,764,792)

df1 <- original %>%
  filter((period %in% incorrect_period) & (rtCode %in% incorrent_rtCode)) 
df2 <- df1 %>%
  mutate(TradeValue = TradeValue*1000) 
df3 <- original %>%
  filter((period %ni% incorrect_period) | (rtCode %ni% incorrent_rtCode)) 
full_df <- df2 %>%
  full_join(df3) %>%
  filter(cmdCode != "TOTAL")
full_df <- full_df %>%
  select(2:6)
head(full_df)

tm_full <- full_df %>%
  full_join(tm1,by = c("period", "rtCode", "cmdCode")) 

tm_full$TradeValue.x[is.na(tm_full$TradeValue.x)] <- 0
tm_full$TradeValue.y[is.na(tm_full$TradeValue.y)] <- 0

tm_full <- tm_full %>%
  mutate(Tradevalue = ifelse(tm_full$TradeValue.x >= tm_full$TradeValue.y,tm_full$TradeValue.x,tm_full$TradeValue.y))
tm_full <- tm_full %>%
  select(-c(TradeValue.y,TradeValue.x)) 
tm_full <- tm_full %>% select(-c(rtTitle.y))
names(tm_full)[3] <- "rtTitle"
head(tm_full)

tm_full_1 <- tm_full %>%
  full_join(usa) 

# TM_original only includes export data of some of the countries, so we are not finished..
# But we write a copy of it anyway

write.csv(tm_full_1, file = "TM_original.csv")

# if your R did not break by now 
tm_final <- tm_full_1
# if it broke, just import the dataframe again since you had it saved
tm_final <- read.csv("TM_original.csv")

#again, make sure cmdcode is in 6-digit form
tm_final$cmdCode = str_pad(tm_final$cmdCode, 6, side = "left", "0")


EU_final <- EU %>%
  left_join(dic, on = cmdCode)
EU_final$group[is.na(EU_final$group)] <- "Unclassified"
names(EU_final)[5] <- "Tradevalue"

# create classification column
tm_full_final <- tm_final %>%
  left_join(dic, on = cmdCode)

tm_full_final$group[is.na(tm_full_final$group)] <- "Unclassified"

final_final <- EU_final %>%
  full_join(tm_full_final)

final_final$Tradevalue[is.na(final_final$Tradevalue)] <- 0

final_final <- final_final %>%
  select(-c(X))

final_final <- final_final %>%
  mutate(period = period.strip("-"))

str_sub(final_final$period, 5, 4) <- "-"
final_final$period <- str_replace(final_final$period, "-","")


# to see again if the data is credible, by creating plot for each report country
final_final %>%
  select(-c(X)) %>%
  mutate(rtCode = as.character(rtCode),
         period = as.factor(period))%>%
  group_by(period,rtCode) %>%
  mutate(sum = sum(Tradevalue)) %>%
  ggplot(aes(x = period,y = sum, color=rtCode)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ rtCode)

write.csv(final_final, file = "export_2021_721.csv")

final_final <- read.csv("export_2021_721.csv")

# we do not want countries that export small volumes 
# we want 2017-2021 data only
final_final_final <- final_final %>%
  filter(as.numeric(period) >= 201701) %>%
  filter(as.numeric(period) <= 202103) %>%
  filter(rtCode != 156) %>%
  filter(rtCode != 376)%>%
  filter(rtCode != 566)

final_final_final <- final_final_final %>%
  select(-c(X))

final_rounf_final <- final_final_final %>% 
  filter(cmdCode %ni% med_code )

write.csv(final_rounf_final,"output_world_export.csv")


### check if export data are normal using aggregates, because I bet you cannot open the excel
table(tm_full_1$rtCode[tm_full_1$period == "202012"])
sum(tm_full_1$Tradevalue[tm_full_1$period == "202012"])
test <- ddply(final_rounf_final,.(period,rtCode),summarize,s = sum(Tradevalue))
write.csv(test,"test1.csv")

# import world data 
world <- read.csv("output_world_export.csv")

world_group <- world %>%
  mutate(rtCode = as.character(rtCode),
         period = as.factor(period))%>%
  select(-c(1,3,4,5))%>%
  group_by(period,group) %>%
  mutate(sum = sum(Tradevalue)) %>%
  select(-c(Tradevalue)) %>%
  unique() %>%
  pivot_wider(names_from = group,values_from = sum)

world_group <- world_group %>%
  select(-2,2) %>%
  arrange(by_group = period)

china$行标签 <- str_replace(china$行标签, "-","")
china$行标签 <- substring(china$行标签,1,6)
colnames(china) <- colnames(world_group)
china <- china %>% 
  filter(as.numeric(period) <= 202103)

global <- china+world_group 
global$period <- china$period

china$period <- as.numeric(china$period)
global$period <- as.numeric(global$period)
china_share <- china / global

china_share$period <- china$period

china_con <- readxl::read_excel("中国分四大类进出口&消费品细分20210714.xlsx", sheet = "消费品细分") %>%
  select(1:5) %>%
  drop_na() 

china_con <- china_con %>%
  select(-5)

china_con <- china_con %>%
  filter(as.numeric(行标签) <= 202103)

# we gave subtags to consumer goods -- inhouse consumer goods, ourdoor consumer goods, others
global_con <- world %>% 
  filter(group == "Consumer goods") %>%
  select(-c(1,3,7)) %>%
  mutate(cmdCode = substring(cmdCode,1,2))

global_con <- global_con %>%
  mutate(class = ifelse(cmdCode %in% c(85,94,95,96,96),"宅经济相关",ifelse(cmdCode %in% c(64,65,66,87),"出行相关","其他")))

global_con <- global_con %>%
  select(-c(2,3)) %>%
  unique() %>%
  group_by(class,period) %>%
  mutate(sum = sum(Tradevalue)) %>%
  select(-c(Tradevalue)) %>%
  unique() %>%
  pivot_wider(names_from = class,values_from = sum) 
  
global_con<-global_con %>% arrange(by_group = period) %>%
  select(-2,2)

global_con_inc <- global_con + china_con
china_con_share <- china_con / global_con_inc  

global_con_inc$period <-global_con$period
china_con_share$行标签 <-global_con_inc$period
names(china_con_share)[1] <- "period"

library(writexl)
sheets <- list("去除医疗物资" = global, "消费品细分" = global_con_inc, "去除医疗物资占比" = china_share,"消费品细分占比"= china_con_share) #载入数据为data frame
write_xlsx(sheets, "output_中国占比世界出口.xlsx")
