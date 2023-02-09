library(rvest)
library(jsonlite)
library(stringr)
library(magrittr)

page <- read_html('https://www.macrotrends.net/stocks/charts/TSLA/tesla/income-statement?freq=Q')  
#page <- read_html('https://www.macrotrends.net/stocks/charts/TSLA/tesla/income-statement')  
df = data.frame(jsonlite::fromJSON(str_match_all(page%>%html_text(),'var originalData = (.*);')[[1]][,2]))
df$field_name <-lapply(df$field_name, function(x) { read_html(x) %>% html_node('a,p') %>% html_text()})
df <- subset(df, select = -c(popup_icon))
colnames(df) <- lapply(colnames(df), function(x){gsub('X','',x)})
df <- df[!is.na(df$field_name),]
df <- apply(df,2,as.character)


write.csv(df,"data.csv", row.names = FALSE)
library(readr)
stock_names <- read_csv("https://raw.githubusercontent.com/datasets/s-and-p-500-companies/master/data/constituents.csv") %>%
  filter(!grepl("[.]", Name) )
library(dplyr)

df2 <- NULL
for (i in 1:nrow(stock_names)) {
  tryCatch({
pull <- paste0('https://www.macrotrends.net/stocks/charts/', stock_names$Symbol[i], "/", tolower(stock_names$Name[i]), "/",
               "income-statement?freq=Q")
pull <- gsub(" ", "-", pull)
print(pull)
print(i)
page <- read_html(pull)  
#page <- read_html('https://www.macrotrends.net/stocks/charts/TSLA/tesla/income-statement')  
df = data.frame(jsonlite::fromJSON(str_match_all(page%>%html_text(),'var originalData = (.*);')[[1]][,2]))
df$field_name <-lapply(df$field_name, function(x) { read_html(x) %>% html_node('a,p') %>% html_text()})
df <- subset(df, select = -c(popup_icon))
colnames(df) <- lapply(colnames(df), function(x){gsub('X','',x)})
df <- df[!is.na(df$field_name),]
df <- apply(df,2,as.character)
df <- t(df)
colnames(df) <- df[1,]
df <- df[-1,]
df <- as.data.frame(df)


df$date <- rownames(df)
df$symbol <- stock_names$Symbol[i]
df$stock_name <- stock_names$Name[i]
df$industry <- stock_names$Sector[i]
df2 <- bind_rows(df2, df)
  }, error=function(e){})
}

length(unique(df2$symbol))

df2$date <- as.Date(gsub("[.]", "-", df2$date))


write_csv(df2, "Stock_Qtrly_Reports.csv")


df2 <- read_csv("Stock_Qtrly_Reports.csv")

merge <- unique(df2$symbol)



df4 <- NULL
for (i in 1:length(merge)) {
  tryCatch({
  
  stocks <- merge[i]
  
  
  df3 <- getSymbols(stocks, from = '2008-01-01',warnings = FALSE,
                    auto.assign = F)
  
  df3 <- as.data.frame(df3)
  df3$date <- as.Date(rownames(df3))
  df3$symbol <- stocks
  df3 <- df3 %>% select(symbol, date, 4)
  colnames(df3)[3] <- "close"
  
  #df2 <- left_join(df2, df3, by = c("date", "symbol"))
  df4 <- bind_rows(df4, df3)
  print(i)
 
  
   }, error=function(e){})
  
  
}





df2 <- left_join(df2, df4, by = c("date", "symbol"))

df22 <- df2 %>% filter(is.na(close)) %>% select(-close)

df42 <- df4 %>% mutate(date = date+days(1))

df22 <- left_join(df22, df42, by = c("date", "symbol"))

####

df23 <- df22 %>% filter(is.na(close)) %>% select(-close)

df43 <- df4 %>% mutate(date = date+days(2))

df23 <- left_join(df23, df43, by = c("date", "symbol"))

####

df24 <- df23 %>% filter(is.na(close)) %>% select(-close)

df44 <- df4 %>% mutate(date = date+days(3))

df24 <- left_join(df24, df44, by = c("date", "symbol"))

df25 <- bind_rows(df2, df22, df23, df24) %>% filter(!is.na(close)) %>% distinct(date, symbol,.keep_all = T)


df33 <- df25 %>% group_by(date) %>% summarise(count = n())

plot(df33$count ~ df33$date)


#write_csv(df25, "Stock_Qtrly_Reports_w_Price.csv") 

df2 <- read_csv("Stock_Qtrly_Reports_w_Price.csv")

df2 <- df2 %>% group_by(symbol) %>% mutate(lag_close_1 = lead(close, 1),
                                                 lag_close_2 = lead(close, 2),
                                                 lag_close_3 = lead(close, 3),
                                                 lag_close_4 = lead(close, 4),
                                                pct_1 = (lag_close_1-close)/close,
                                           pct_2 = (lag_close_2-close )/close,
                                           pct_3 = (lag_close_3-close )/close,
                                           pct_4 = (lag_close_4-close )/close,
                                           pe = close/`Basic EPS`
                                           
                                                 
                                                 )

df3 <- df2 %>% ungroup() %>% filter(!is.na(lag_close_1),
                                    !is.na(lag_close_2),
                                    !is.na(lag_close_3),
                                    !is.na(lag_close_4),
                                    !is.infinite(pe),
                                    !is.infinite(pct_1),
                                    is.finite(pe)
                                    ) %>% mutate(pe = close/`Basic EPS`) %>% filter(pe > 0, pe < 300)

plot(df3$pct_2 ~ df3$pe)

summary(df3$pct_1)

df33 <- df3 %>% group_by(date) %>% summarise(count = n())

plot(df33$count ~ df33$date)


mod <- lm(df3$pct_4 ~ df3$pe*df3$`Gross Profit`)
summary(mod)
summary(df3$pe)

library(rpart)
library(rpart.plot)

df3 <- df3 %>% filter(abs(pct_1) < 2,
                      abs(pct_2) < 2,
                      abs(pct_3) < 2,
                      abs(pct_4) < 2
                      ) %>% mutate(profmarg = `Net Income`/Revenue,
                                   fl = `Net Income`/`Basic EPS`) 

df4 <- df3 %>% 
  ungroup() %>% 
  mutate(year = year(date),
         month = month(date),
         quarter = quarter(date)
         ) %>% 
  select(-pct_2, -pct_3, -pct_4, -date, -stock_name, -contains("lag"))%>% 
  group_by( year, month) %>% 
  mutate(rank = rank(pe)) %>% 
  ungroup() %>% 
  filter(year == 2021) %>% 
  select(-year, -month, -symbol)
  
  df5 <- df3 %>% 
    ungroup() %>% 
    mutate(year = year(date),
           month = month(date),
           quarter = quarter(date)
    ) %>% 
    select(-pct_2, -pct_3, -pct_4, -date, -stock_name, -contains("lag"))%>% 
    group_by( year, month) %>% 
    mutate(rank = rank(pe)) %>% 
    ungroup() %>% 
    filter(year >= 2022) %>% 
  select(-year, -month, -symbol)


tree <- rpart(pct_1 ~., df4)
tree <- prune(tree, cp = 0.016)
rpart.plot(tree)
tree

plot(predict(tree, df4) ~ df4$pct_1)
plot(predict(tree, df5) ~ df5$pct_1)

cor(predict(tree, df5) , df5$pct_1)
cor(predict(tree, df4) , df4$pct_1)


cor(df3$pct_1,df3$pe, use = "complete.obs")

plot(df3$pct_4 ~ df3$pe)

summary(df3$pe)

cor(df3$pct_4,df3$pe, use = "complete.obs")




sort(unique(df2$date))
library(tidyquant)
tq_get("AAPL")
library(tidyverse)
t3 <- NULL


for (i in 1:nrow(stock_names)) {
  tryCatch({
  t2 <- NULL
  df4 <- df2 %>% filter(symbol == stock_names$Symbol[i]) %>% 
    arrange(date)
  df3 <- getSymbols(stock_names$Symbol[i], from = '2009-01-01',warnings = FALSE,
                    auto.assign = F)
  df3 <- as.data.frame(df3)
  df3$date <- as.Date(rownames(df3))
  for (j in 1:(nrow(df4)-1)) {
  df5 <- df3 %>% filter(date > df4$date[j], date < df4$date[j+1]) 
  percs <- NULL

  for(k in 2:nrow(df5)) {

    perc <-  ((df5[k,6] - df5[1,6])/df5[1,6])*100
    percs <- rbind(percs, perc)

  }
  t <- summary(percs)
  t <- as.data.frame(t)
  t <- t(gsub(" ", "",str_sub(t$Freq, start = 9, end = -3)))

  t <- cbind(t, stock_names$Symbol[i],stock_names$Sector[i], as.Date(df4$date[j]))
  t2 <- rbind(t2, t)
    
  }
  t2 <- as.data.frame(t2)
  t3 <- rbind(t3, t2)
  }, error=function(e){})
}

individual_stocks_change <- t3 %>% 
  rename(min = V1,
         first_quartile = V2, 
         median = V3,
         average = V4,
         third_quartile = V5,
         max = V6,
         symbol = V7,
         sector = V8,
         date = V9) %>% 
  mutate(date = as.Date(as.numeric(date)))

for (i in 1:6) {
  individual_stocks_change[,i] = as.numeric(individual_stocks_change[,i])
}

sector_stocks_change <- as.data.frame(individual_stocks_change) %>% 
  group_by(sector, date) %>% 
  summarise(across(where(is.numeric), mean, na.rm = T))

all_stocks_change <- as.data.frame(individual_stocks_change) %>% 
  group_by(date) %>% 
  summarise(across(where(is.numeric), mean, na.rm = T))


plot(all_stocks_change$max, type = "l", ylim = c(-30, 30))
lines(all_stocks_change$min, type = "l")
lines(all_stocks_change$first_quartile, type = "l")
lines(all_stocks_change$median, type = "l")
lines(all_stocks_change$mean, type = "l")
lines(all_stocks_change$third_quartile, type = "l")
summary(all_stocks_change$median)


all_stocks_change_lag <- all_stocks_change %>% 
  ungroup() %>% 
  group_by() %>%
  mutate_all(funs(all_lag = lag(.), 
                  all_lag2 = lag(.,2),
             all_lag3 = lag(.,3),
             all_lag4 = lag(.,4))
             ) %>%
  ungroup()%>% 
  select(contains("lag"), date)

sector_stocks_change_lag <- sector_stocks_change %>% 
  ungroup() %>% 
  group_by(sector) %>%
  mutate_all(funs(sector_lag = lag(.), 
                  sector_lag2 = lag(.,2),
                  sector_lag3 = lag(.,3),
                  sector_lag4 = lag(.,4))
  ) %>%
           
  ungroup() %>% 
  select(contains("lag"), date, sector)

#lag2 <- function(x) { z <- lag(x, 2)

individual_stocks_change_lag <- individual_stocks_change %>% 
  ungroup() %>% 
  group_by(symbol, sector) %>%
  mutate_all(funs(ind_lag = lag(.), 
                  ind_lag2 = lag(.,2),
                  ind_lag3 = lag(.,3),
                  ind_lag4 = lag(.,4))
  ) %>%
  ungroup() %>% 
  dplyr::select(contains("lag"), date, symbol, sector, third_quartile) %>% 
  rename(median = third_quartile)

library(lubridate)


check <- left_join(sector_stocks_change_lag, all_stocks_change_lag) %>% 
  select(-contains("date_"))

# check <- check %>% mutate(month = month(date)) %>% 
#   filter(month ==3 | month == 6 | month == 9 | month == 12) #%>% 
#   #select(-date)

check2 <- left_join(individual_stocks_change_lag, check) %>% 
  select(-symbol, -contains("date_"))

check2 <- check2 %>% 
  mutate(month = month(date)) %>%
  filter(month ==3 | month == 6 | month == 9 | month == 12) %>%
  select(-date)

# check2 <- check2 %>% 
#   group_by(sector) %>% 
  
  

check2 <- as.data.frame(check2)
check2 <- na.omit(check2)

mod <- lm(median ~., check2)
summary(mod)
plot(predict(mod, check2),check2$median)

tree <- rpart(median ~., check2)
rpart.plot(tree)
plot(predict(tree, check2),check2$median)
boxplot(as.factor(as.character(predict(tree, check2))),check2$median)
(cor(predict(tree, check2),check2$median))^2

(cor((predict(tree, check2)+predict(mod, check2))/2,check2$median))^2

preds <- (predict(tree, check2)+predict(mod, check2))/2
plot(preds,check2$median)



######################

## Check report info

#####################
library(dplyr)
check <- left_join(sector_stocks_change_lag, all_stocks_change_lag) %>% 
  dplyr::select(-contains("date_"))


check2 <- left_join(individual_stocks_change_lag, check) %>% 
  dplyr::select(-contains("date_"))



# check2 <- check2 %>% 
#   mutate(month = month(date)) %>%
#   filter(month ==3 | month == 6 | month == 9 | month == 12)

for (i in 1:21) {
  df2[,i] <- as.numeric(df2[,i])
}
df3 <- df2 %>% 
  ungroup() %>% 
  select(date, symbol, where(is.numeric))

df3$`Other Income` <- NULL
df3$`Other Operating Income Or Expenses` <- NULL
df3_date <- df3$date
df3_lag <- df3 %>% 
  arrange(date) %>% 
  ungroup() %>% 
  select(-date) %>% 
  group_by(symbol) %>%
  mutate_all(funs(ind_lag = lag(.), 
                  ind_lag2 = lag(.,2),
                  ind_lag3 = lag(.,3),
                  ind_lag4 = lag(.,4),
                  ind_lag_diff = .-lag(.), 
                  ind_lag2_diff = .-lag(.,2),
                  ind_lag3_diff = .-lag(.,3),
                  ind_lag4_diff = .-lag(.,4)#,
                  # ind_lag_pdiff = .-lag(.)/., 
                  # ind_lag2_pdiff = .-lag(.,2)/.,
                  # ind_lag3_pdiff = .-lag(.,3)/.,
                  # ind_lag4_pdiff = .-lag(.,4)/.
                  )
  ) %>%
  ungroup() %>% 
  select(contains("lag"),  symbol)
df3_lag$date <- df3$date


df3_lag = as.data.frame(df3_lag)

for (i in 221:ncol(df3_lag)) {
  for (j in 1:nrow(df3_lag)) {
  df3_lag[j,i] = ifelse(is.nan(df3_lag[j,i]), 0, df3_lag[j,i]) 
  df3_lag[j,i] = ifelse(is.infinite(df3_lag[j,i]), 0, df3_lag[j,i]) 
  }
}

df3_lag[is.na(df3_lag)] <- 0
#df3[is.infinite(df3)] <- 0

check3 <- left_join(check2, df3_lag)

checkz <- left_join(check2 %>% dplyr::select(date,symbol, median), df3_lag %>% dplyr::select(date,  symbol,contains("diff")))
check3 <- as.data.frame(check3)
check3 <- check3 %>% dplyr::select(where(is.numeric))
check3$`Other Income` <- NULL
check3$`Other Operating Income Or Expenses` <- NULL
check3[is.na(check3)] <- 0 

mod <- lm(median ~min_ind_lag + first_quartile_ind_lag + 
            median_ind_lag + average_ind_lag + max_ind_lag + third_quartile_ind_lag2 + 
            max_ind_lag2 + min_ind_lag3 + first_quartile_ind_lag3 + median_ind_lag3 + 
            average_ind_lag3 + third_quartile_ind_lag3 + min_ind_lag4 + 
            first_quartile_ind_lag4 + median_ind_lag4 + average_ind_lag4 + 
            max_ind_lag4 + min_sector_lag + first_quartile_sector_lag + 
            median_sector_lag + third_quartile_sector_lag + min_sector_lag2 + 
            average_sector_lag2 + max_sector_lag2 + min_sector_lag3 + 
            first_quartile_sector_lag3 + median_sector_lag3 + third_quartile_sector_lag3 + 
            max_sector_lag3 + min_sector_lag4 + first_quartile_sector_lag4 + 
            median_sector_lag4 + average_sector_lag4 + max_sector_lag4 + 
            min_all_lag + median_all_lag + average_all_lag + max_all_lag + 
            min_all_lag2 + median_all_lag2 + average_all_lag2 + third_quartile_all_lag2 + 
            max_all_lag2 + min_all_lag3 + first_quartile_all_lag3 + median_all_lag3 + 
            third_quartile_all_lag3 + max_all_lag3 + min_all_lag4 + first_quartile_all_lag4 + 
            median_all_lag4 + average_all_lag4 + third_quartile_all_lag4 + 
            max_all_lag4, check3)
library(MASS)
step.model2 <- stepAIC(mod, direction = "both", 
                      trace = T)
summary(step.model)
summary(step.model2)

summary(mod)
plot(predict(mod, check3),check3$median)

tree <- rpart(median ~min_ind_lag + first_quartile_ind_lag + 
                median_ind_lag + average_ind_lag + max_ind_lag + third_quartile_ind_lag2 + 
                max_ind_lag2 + min_ind_lag3 + first_quartile_ind_lag3 + median_ind_lag3 + 
                average_ind_lag3 + third_quartile_ind_lag3 + min_ind_lag4 + 
                first_quartile_ind_lag4 + median_ind_lag4 + average_ind_lag4 + 
                max_ind_lag4 + min_sector_lag + first_quartile_sector_lag + 
                median_sector_lag + third_quartile_sector_lag + min_sector_lag2 + 
                average_sector_lag2 + max_sector_lag2 + min_sector_lag3 + 
                first_quartile_sector_lag3 + median_sector_lag3 + third_quartile_sector_lag3 + 
                max_sector_lag3 + min_sector_lag4 + first_quartile_sector_lag4 + 
                median_sector_lag4 + average_sector_lag4 + max_sector_lag4 + 
                min_all_lag + median_all_lag + average_all_lag + max_all_lag + 
                min_all_lag2 + median_all_lag2 + average_all_lag2 + third_quartile_all_lag2 + 
                max_all_lag2 + min_all_lag3 + first_quartile_all_lag3 + median_all_lag3 + 
                third_quartile_all_lag3 + max_all_lag3 + min_all_lag4 + first_quartile_all_lag4 + 
                median_all_lag4 + average_all_lag4 + third_quartile_all_lag4 + 
                max_all_lag4, check3)
rpart.plot(tree)
plot(predict(tree, check3),check3$median)
boxplot(as.factor(as.character(predict(tree, check3))),check3$median)
(cor(predict(tree, check3),check3$median))^2

(cor((predict(tree, check3)+predict(mod, check3))/2,check3$median))^2

preds <- (predict(tree, check3)+predict(mod, check3))/2
plot(preds,check3$median)
cor(preds,check3$median)
check2$pred <- predict(mod, check2)
check22 <- check2 %>% dplyr::select(date, symbol, median, pred)

check22 <- na.omit(check22)

check22 <- check22 %>% 
  filter(date > "2013-03-01") %>% 
  group_by(symbol) %>%
  mutate(cor = cor(pred, median, method = "spearman")) %>%
  ungroup() %>%
  group_by(date) %>%
  mutate(cor2 = cor(pred, median)) %>%
  ungroup()

hist(check22$cor2)
hist(check22$cor)
warnings()
check222 <- check22 %>% 
  ungroup() %>% 
  group_by(date) %>% 
  mutate(rank = quantile(pred, 0.99)) %>% 
  filter(pred > rank,
         pred > 0)
hist(check222$median)
plot(check222$median ~ check222$pred)
summary(check222$median)
summary(check22$median)
unique(check222$symbol)
unique(check22$cor2)
cor(check22$pred, check22$median, method = "spearman")
check22$

#############

tree

for (i in 1:6){
  t2[,i] <- as.numeric(t2[,i])
}
t2$lag <- lag(t2$V4)
t2$lag2 <- lag(t2$V3)


mod <- lm(V3 ~lag2+lag, t2)
summary(mod)

library(rpart)
library(rpart.plot)
tree <- rpart(V3 ~lag2+lag, t2)
rpart.plot(tree)
plot(predict(tree, t2),t2$V3)

c("AAPL", "GOOG", "FB") %>%
  tq_get(get = "stock.prices", from = "2016-01-01", to = "2017-01-01")
library(quantmod)
sdate <- as.Date("2018-07-01")
edate <- as.Date("2019-12-31")

# Samsung Electronics (005930), Naver (035420)
ss_stock=getSymbols("005930.KS",from="2018-07-01",to="2019-12-31",auto.assign = F)
install.packages('quantmod')

df3 <- getSymbols("AAPL", from = '2010-01-01',warnings = FALSE,
           auto.assign = F)
unique(df2$date)
chart_Series()

tickers = c("AAPL", "NFLX", "AMZN", "K", "O")

df3 <- getSymbols(tickers,
           from = "2017-01-01",
           to = "2017-01-15", auto.assign = F)
