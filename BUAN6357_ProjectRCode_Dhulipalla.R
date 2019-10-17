library('ggplot2')
library('scales')
library('grid')
library('gridExtra')
library('RColorBrewer')
library('corrplot')
library('dplyr')
library('readr')
library('data.table')
library('tibble')
library('tidyr')
library('stringr')
library('forcats')
library('ggrepel')
library('ggridges')
library('ggExtra')
library('ggforce')
library('viridis')
library('lazyeval')
library('broom')
library('purrr')
library('lubridate')
library('timeDate')
library('tseries')
library('forecast')
library('prophet')
library('timetk')
library('fpp2')


# Read Data Files
air_visits <- as.tibble(fread(str_c('air_visit_data.csv')))
air_reserve <- as.tibble(fread(str_c('air_reserve.csv')))
hpg_reserve <- as.tibble(fread(str_c('hpg_reserve.csv')))
air_store <- as.tibble(fread(str_c('air_store_info.csv')))
hpg_store <- as.tibble(fread(str_c('hpg_store_info.csv')))
holidays <- as.tibble(fread(str_c('date_info.csv')))
store_ids <- as.tibble(fread(str_c('store_id_relation.csv')))
test <- as.tibble(fread(str_c('sample_submission.csv')))




# Changing Dates to YMD and converting few other to Factors
air_visits <- air_visits %>%
  mutate(visit_date = ymd(visit_date))

air_reserve <- air_reserve %>%
  mutate(visit_datetime = ymd_hms(visit_datetime),
         reserve_datetime = ymd_hms(reserve_datetime))

hpg_reserve <- hpg_reserve %>%
  mutate(visit_datetime = ymd_hms(visit_datetime),
         reserve_datetime = ymd_hms(reserve_datetime))

air_store <- air_store %>%
  mutate(air_genre_name = as.factor(air_genre_name),
         air_area_name = as.factor(air_area_name))

hpg_store <- hpg_store %>%
  mutate(hpg_genre_name = as.factor(hpg_genre_name),
         hpg_area_name = as.factor(hpg_area_name))

holidays <- holidays %>%
  mutate(holiday_flg = as.logical(holiday_flg),
         date = ymd(calendar_date))



# Joining Air_visits Data and Air_Store data
data <- air_visits %>%
  left_join(air_store, by = "air_store_id")

data  %>% separate(air_area_name, c("prefecture"), sep = " ", remove = FALSE) -> data
data %>% distinct(air_genre_name) %>% nrow()
data %>% distinct(air_area_name) %>% nrow()
data %>% distinct(prefecture) %>% nrow()
data %>% distinct(prefecture,air_genre_name) %>% nrow()
data %>% distinct(prefecture,air_genre_name) -> distinct_area_gener

data <- data %>%
  mutate(calendar_date = as.character(visit_date)) %>%
  left_join(holidays, by = "calendar_date")

data$holiday_flg = as.numeric(data$holiday_flg)

data = subset(data, select = -c(date,calendar_date))
data = subset(data, select = -c(latitude,longitude))


# Exploratory Data Analysis

# EDA of Air_Visits:
air_visits %>%
  group_by(visit_date) %>%
  summarise(all_visitors = sum(visitors)) %>%
  ggplot(aes(visit_date,all_visitors)) +
  geom_line(col = 'green') +
  labs(y = "All visitors", x = "Date")


air_visits %>%
  group_by(visit_date) %>%
  summarise(all_visitors = median(visitors)) %>%
  ggplot(aes(visit_date,all_visitors)) +
  geom_line(col = 'orange') +
  labs(y = "Median visitors", x = "Date")


air_visits %>%
  ggplot(aes(visitors)) +
  geom_histogram(fill = "blue", bins = 30) +
  scale_x_log10()



air_visits %>%
  mutate(wday = wday(visit_date, label = TRUE)) %>%
  group_by(wday) %>%
  summarise(visits = median(visitors)) %>%
  ggplot(aes(wday, visits, fill = wday )) +
  geom_col() +
  theme(legend.position = "none", axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9)) +
  labs(x = "Day of the week", y = "Median visitors")


air_visits %>%
  mutate(month = month(visit_date, label = TRUE)) %>%
  group_by(month) %>%
  summarise(visits = median(visitors)) %>%
  ggplot(aes(month, visits, fill = month)) +
  geom_col() +
  theme(legend.position = "none") +
  labs(x = "Month", y = "Median visitors")

# Air Reserve

foo <- air_reserve %>%
  mutate(reserve_date = date(reserve_datetime),
         reserve_hour = hour(reserve_datetime),
         reserve_wday = wday(reserve_datetime, label = TRUE),
         visit_date = date(visit_datetime),
         visit_hour = hour(visit_datetime),
         visit_wday = wday(visit_datetime, label = TRUE),
         diff_hour = time_length(visit_datetime - reserve_datetime, unit = "hour"),
         diff_day = time_length(visit_datetime - reserve_datetime, unit = "day")
  )



foo %>%
  group_by(visit_date) %>%
  summarise(all_reserve_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(visit_date, all_reserve_visitors)) +
  geom_line(col='blue') +
  labs(x = "'air' visit date")


foo %>%
  group_by(visit_hour) %>%
  summarise(all_reserve_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(visit_hour, all_reserve_visitors, fill = visit_hour)) +
  geom_col()


foo %>%
  group_by(visit_wday) %>%
  summarise(all_reserve_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(visit_wday, all_reserve_visitors, fill = visit_wday)) +
  geom_col()


# HPG Reserve

foo <- hpg_reserve %>%
  mutate(reserve_date = date(reserve_datetime),
         reserve_hour = hour(reserve_datetime),
         visit_date = date(visit_datetime),
         visit_hour = hour(visit_datetime),
         visit_wday = wday(visit_datetime, label = TRUE),
         diff_hour = time_length(visit_datetime - reserve_datetime, unit = "hour"),
         diff_day = time_length(visit_datetime - reserve_datetime, unit = "day")
  )


foo %>%
  group_by(visit_date) %>%
  summarise(all_reserve_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(visit_date, all_reserve_visitors)) +
  geom_line(col='blue') +
  labs(x = "'hpg' visit date")


foo %>%
  group_by(visit_hour) %>%
  summarise(all_reserve_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(visit_hour, all_reserve_visitors, fill = visit_hour)) +
  geom_col()


foo %>%
  group_by(visit_wday) %>%
  summarise(all_reserve_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(visit_wday, all_reserve_visitors, fill = visit_wday)) +
  geom_col()

# Air Store
air_store %>%
  group_by(air_genre_name) %>%
  count() %>%
  ggplot(aes(reorder(air_genre_name, n, FUN = min), n, fill = air_genre_name)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = "Type of cuisine (air_genre_name)", y = "Number of air restaurants")


air_store %>%
  group_by(air_area_name) %>%
  count() %>%
  ungroup() %>%
  top_n(15,n) %>%
  ggplot(aes(reorder(air_area_name, n, FUN = min) ,n, fill = air_area_name)) +
  geom_col() +
  theme(legend.position = "none") +
  coord_flip() +
  labs(x = "Top 15 areas (air_area_name)", y = "Number of air restaurants")

# hpg Store


hpg_store %>%
  group_by(hpg_genre_name) %>%
  count() %>%
  ggplot(aes(reorder(hpg_genre_name, n, FUN = min), n, fill = hpg_genre_name)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = "Type of cuisine (hpg_genre_name)", y = "Number of hpg restaurants")


hpg_store %>%
  mutate(area = str_sub(hpg_area_name, 1, 20)) %>%
  group_by(area) %>%
  count() %>%
  ungroup() %>%
  top_n(15,n) %>%
  ggplot(aes(reorder(area, n, FUN = min) ,n, fill = area)) +
  geom_col() +
  theme(legend.position = "none") +
  coord_flip() +
  labs(x = "Top 15 areas (hpg_area_name)", y = "Number of hpg restaurants")

# Date Info
foo <- holidays %>%
  mutate(wday = wday(date))

foo %>%
  ggplot(aes(holiday_flg)) +
  geom_bar() +
  theme(legend.position = "none")


foo <- air_visits %>%
  mutate(calendar_date = as.character(visit_date)) %>%
  left_join(holidays, by = "calendar_date")

foo %>%
  ggplot(aes(holiday_flg, visitors)) +
  geom_boxplot() +
  scale_y_log10() +
  theme(legend.position = "none")

# Visitors per Genre

foo <- air_visits %>%
  left_join(air_store, by = "air_store_id")

foo %>%
  group_by(visit_date, air_genre_name) %>%
  summarise(mean_visitors = mean(visitors)) %>%
  ungroup() %>%
  ggplot(aes(visit_date, mean_visitors, color = air_genre_name)) +
  geom_line() +
  labs(y = "Average number of visitors to 'air' restaurants", x = "Date") +
  theme(legend.position = "none") +
  scale_y_log10() +
  facet_wrap(~ air_genre_name)


#Creating Time Series Data with Feature Engineering for ARIMA and ETS

all_visits <- tibble(visit_date = seq(min(air_visits$visit_date), max(air_visits$visit_date), 1))

holidays_2 = holidays %>% mutate(visit_date = date) 

all_visits %>%
  
  left_join(holidays_2, by = "visit_date") -> all_visits

all_visits = all_visits[,c('visit_date', 'day_of_week')]

dist_air_store_id = air_visits %>% distinct(air_store_id)

for (i in 1:829){
  
  air_id = as.character(dist_air_store_id[i,])
  
  foo <- air_visits %>%
    filter(air_store_id == air_id)
  
  visits <- foo %>%
    right_join(all_visits, by = "visit_date") %>% mutate(air_store_id = air_id)
  
  visits_left <- foo %>%
    left_join(all_visits, by = "visit_date") %>% mutate(air_store_id = air_id)
  
  
  Friday = visits %>%
    filter(day_of_week == 'Friday') %>% 
    replace_na(list(visitors = median(unlist(c((visits_left %>% filter(day_of_week == 'Friday'))[,'visitors'])))))
  
  
  Saturday = visits %>%
    
    filter(day_of_week == 'Saturday') %>% 
    replace_na(list(visitors = median(unlist(c((visits_left %>% filter(day_of_week == 'Saturday'))[,'visitors'])))))
  
  Sunday = visits %>%
    
    filter(day_of_week == 'Sunday') %>% 
    replace_na(list(visitors = median(unlist(c((visits_left %>% filter(day_of_week == 'Sunday'))[,'visitors'])))))
  
  
  
  Monday = visits %>%
    
    filter(day_of_week == 'Monday') %>% 
    replace_na(list(visitors = median(unlist(c((visits_left %>% filter(day_of_week == 'Monday'))[,'visitors'])))))
  
  Tuesday = visits %>%
    
    filter(day_of_week == 'Tuesday') %>% 
    replace_na(list(visitors = median(unlist(c((visits_left %>% filter(day_of_week == 'Tuesday'))[,'visitors'])))))
  
  
  Wednesday = visits %>%
    
    filter(day_of_week == 'Wednesday') %>% 
    replace_na(list(visitors = median(unlist(c((visits_left %>% filter(day_of_week == 'Wednesday'))[,'visitors'])))))
  
  
  Thursday = visits %>%
    
    filter(day_of_week == 'Thursday') %>% 
    replace_na(list(visitors = median(unlist(c((visits_left %>% filter(day_of_week == 'Thursday'))[,'visitors'])))))
  
  
  visits = rbind(Friday, Saturday,Sunday,Monday,Tuesday,Wednesday,Thursday)
  visits = visits %>% arrange(visit_date)
  visits = visits %>% 
    replace_na(list(visitors = median(foo$visitors)))
  
  if (i==1){
    cum_visits = visits
  }else {
    cum_visits = rbind(cum_visits, visits)
  }
}





# ARIMA Analysis req variables
pred_len <- test %>%
  
  separate(id, c("air", "store_id", "date"), sep = "_") %>%
  
  distinct(date) %>%
  
  nrow()

max_date <- max(air_visits$visit_date)

split_date <- max_date - pred_len


# ARIMA on few Restaurants
a = c('air_04341b588bde96cd', 'air_a88ac559064dec08', 'air_a17f0778617c76e2')
p = list()
for (i in 1:3){

air_id = a[i]
visits <- cum_visits %>%
  
  filter(air_store_id == air_id) %>%
  
  rownames_to_column()


visits_train <- visits %>% filter(visit_date <= split_date)

visits_valid <- visits %>% filter(visit_date > split_date)


arima.fit <- auto.arima(tsclean(ts(visits_train$visitors, frequency = 7)),
                        
                        stepwise = FALSE, approximation = FALSE)
arima_visits <- arima.fit %>% forecast(h = pred_len, level = c(80,95))
# s = unlist(arima_visits[4]) - visits_valid$visitors
# arima_forecats_MSE = c(arima_forecats_MSE, unlist(sum(s*s)/pred_len))
p[[i]] = arima_visits %>%

  autoplot +

  geom_line(aes(as.integer(rowname)/7, visitors), data = visits_valid, color = "red") +

  labs(x = "Time [weeks]", y = "visitors")

}

p[[1]]
p[[2]]
p[[3]]


#ARIMA FUnction
arima_forecats_MSE = list()
a = Sys.time()
for (i in 1:829){
  
  air_id = as.character(dist_air_store_id[i,])
  visits <- cum_visits %>%
    
    filter(air_store_id == air_id) %>%
    
    rownames_to_column()
  
  
  visits_train <- visits %>% filter(visit_date <= split_date)
  
  visits_valid <- visits %>% filter(visit_date > split_date)
  
  
  arima.fit <- auto.arima(tsclean(ts(visits_train$visitors, frequency = 7)),
                          
                          stepwise = FALSE, approximation = FALSE)
  arima_visits <- arima.fit %>% forecast(h = pred_len, level = c(80,95))
  s = unlist(arima_visits[4]) - visits_valid$visitors
  arima_forecats_MSE = c(arima_forecats_MSE, unlist(sum(s*s)/pred_len))
  # arima_visits %>%
  #   
  #   autoplot +
  #   
  #   geom_line(aes(as.integer(rowname)/7, visitors), data = visits_valid, color = "red") +
  #   
  #   labs(x = "Time [weeks]", y = "visitors")
  if(i%%10 == 0){
    print(i)
    print(Sys.time()-a)
  }
}
arima_forecats_MSE = unlist(arima_forecats_MSE)
RMSE = (sum(arima_forecats_MSE)/i)^0.5
RMSE

# ETS req variables
pred_len <- test %>%
  
  separate(id, c("air", "store_id", "date"), sep = "_") %>%
  
  distinct(date) %>%
  
  nrow()

max_date <- max(air_visits$visit_date)

split_date <- max_date - pred_len

#ETS for few restaurnts
a = c('air_04341b588bde96cd', 'air_a88ac559064dec08', 'air_a17f0778617c76e2')
p_ETS = list()
d_ETS = list()
for (i in 1:3){
  
  air_id = a[i]
  visits <- cum_visits %>%
    
    filter(air_store_id == air_id) %>%
    
    rownames_to_column()
  
  
  visits_train <- visits %>% filter(visit_date <= split_date)
  
  visits_valid <- visits %>% filter(visit_date > split_date)
  
  
  ETS.fit <- ets(tsclean(ts(visits_train$visitors, frequency = 7)))
  ETS_visits <- ETS.fit %>% forecast(h = pred_len, level = c(80,95))
  # s = unlist(ETS_visits[2]) - visits_valid$visitors
  # ETS_forecats_MSE = c(ETS_forecats_MSE, unlist(sum(s*s)/pred_len))
  p_ETS[[i]] = ETS_visits %>%

    autoplot +

    geom_line(aes(as.integer(rowname)/7, visitors), data = visits_valid, color = "red") +

    labs(x = "Time [weeks]", y = "visitors")

  d_ETS[[i]] = autoplot(ETS.fit)
  
}

p_ETS[[1]]
d_ETS[[1]]
p_ETS[[2]]
d_ETS[[2]]
p_ETS[[3]]
d_ETS[[3]]
  

#ETS FUnction
ETS_forecats_MSE = list()
a = Sys.time()
for (i in 1:829){
  air_id = as.character(dist_air_store_id[i,])
  visits <- cum_visits %>%
    
    filter(air_store_id == air_id) %>%
    
    rownames_to_column()
  
  
  visits_train <- visits %>% filter(visit_date <= split_date)
  
  visits_valid <- visits %>% filter(visit_date > split_date)
  
  
  ETS.fit <- ets(tsclean(ts(visits_train$visitors, frequency = 7)))
  ETS_visits <- ETS.fit %>% forecast(h = pred_len, level = c(80,95))
  s = unlist(ETS_visits[2]) - visits_valid$visitors
  ETS_forecats_MSE = c(ETS_forecats_MSE, unlist(sum(s*s)/pred_len))
  # ETS_visits %>%
  # 
  #   autoplot +
  # 
  #   geom_line(aes(as.integer(rowname)/7, visitors), data = visits_valid, color = "red") +
  # 
  #   labs(x = "Time [weeks]", y = "visitors")
  # if(i%%2 == 0){
  #   print(i)
  #   print(Sys.time()-a) }
}
ETS_forecats_MSE = unlist(ETS_forecats_MSE)
RMSE_ETS = (sum(ETS_forecats_MSE)/i)^0.5
RMSE_ETS


# Prophet for Few Restaurants

plot_prophet_air_id_holiday_Graph <- function(air_id, use_hday){
  
  
  
  hday <- holidays %>%
    
    filter(holiday_flg == TRUE) %>%
    
    mutate(holiday = "holiday") %>%
    
    select(ds = date, holiday)
  
  
  
  pred_len <- test %>%
    
    separate(id, c("air", "store_id", "date"), sep = "_") %>%
    
    distinct(date) %>%
    
    nrow()
  
  
  
  max_date <- max(air_visits$visit_date)
  
  split_date <- max_date - pred_len
  
  all_visits <- tibble(visit_date = seq(min(air_visits$visit_date), max(air_visits$visit_date), 1))
  
  
  
  foo <- air_visits %>%
    
    filter(air_store_id == air_id)
  
  
  
  visits <- foo %>%
    
    right_join(all_visits, by = "visit_date") %>%
    
    mutate(visitors = visitors) %>%
    
    rownames_to_column() %>%
    
    select(y = visitors,
           
           ds = visit_date)
  
  
  
  visits_train <- visits %>% filter(ds <= split_date)
  
  visits_valid <- visits %>% filter(ds > split_date)
  
  
  
  if (use_hday == TRUE){
    
    proph <- prophet(visits_train,
                     
                     changepoint.prior.scale=0.5,
                     
                     yearly.seasonality=FALSE,
                     
                     holidays = hday)
    
    ptitle = "Prophet (w/ holidays) for "
    
  } else {
    
    proph <- prophet(visits_train,
                     
                     changepoint.prior.scale=0.5,
                     
                     yearly.seasonality=FALSE)
    
    ptitle = "Prophet for "
    
  }
  
  
  
  future <- make_future_dataframe(proph, periods = pred_len)
  
  fcast <- predict(proph, future)
  
  
  
  p <- fcast %>%
    
    as.tibble() %>%
    
    mutate(ds = date(ds)) %>%
    
    ggplot(aes(ds, yhat)) +
    
    geom_ribbon(aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), fill = "light blue") +
    
    geom_line(colour = "blue") +
    
    geom_line(data = visits_train, aes(ds, y), colour = "black") +
    
    geom_line(data = visits_valid, aes(ds, y), colour = "grey50") +
    
    labs(title = str_c(ptitle, air_id))
  
  
  d = prophet_plot_components(proph, fcast)
  s = list(p,d)
  return(s)
  
}  

w = plot_prophet_air_id_holiday_Graph(a[1], TRUE)
x = plot_prophet_air_id_holiday_Graph(a[2], TRUE)
y = plot_prophet_air_id_holiday_Graph(a[3], TRUE)


# Prophet Loop
dist_air_store_id = air_visits %>% distinct(air_store_id)
prophet_forecats_MSE = list()
a = Sys.time()
for (i in 1:829){
  air_id = as.character(dist_air_store_id[i,])
  use_hday = TRUE
  hday <- holidays %>%
    
    filter(holiday_flg == TRUE) %>%
    
    mutate(holiday = "holiday") %>%
    
    select(ds = date, holiday)
  
  
  
  pred_len <- test %>%
    
    separate(id, c("air", "store_id", "date"), sep = "_") %>%
    
    distinct(date) %>%
    
    nrow()
  
  
  
  max_date <- max(air_visits$visit_date)
  
  split_date <- max_date - pred_len
  
  all_visits <- tibble(visit_date = seq(min(air_visits$visit_date), max(air_visits$visit_date), 1))
  
  
  
  foo <- air_visits %>%
    
    filter(air_store_id == air_id)
  
  
  
  visits <- foo %>%
    
    right_join(all_visits, by = "visit_date") %>%
    
    mutate(visitors = visitors) %>%
    
    rownames_to_column() %>%
    
    select(y = visitors,
           
           ds = visit_date)
  
  
  
  visits_train <- visits %>% filter(ds <= split_date)
  
  visits_valid <- visits %>% filter(ds > split_date)# %>% replace_na(list(y = median(foo$visitors)))
  
  visits_train$y -> na_check
  na_check <- na_check[!is.na(na_check)]
  
  if (length(na_check)>=2){
    
    if (use_hday == TRUE){
      
      proph <- prophet(visits_train,
                       
                       changepoint.prior.scale=0.5,
                       
                       yearly.seasonality=FALSE,
                       
                       holidays = hday)
      
      ptitle = "Prophet (w/ holidays) for "
      
    } else {
      
      proph <- prophet(visits_train,
                       
                       changepoint.prior.scale=0.5,
                       
                       yearly.seasonality=FALSE)
      
      ptitle = "Prophet for "
      
    }
  } else{
    next
  }
  
  
  future <- make_future_dataframe(proph, periods = pred_len)
  
  fcast <- predict(proph, future)
  fcast_test = fcast %>% filter(ds > split_date)
  s = fcast_test$yhat - visits_valid$y
  s <- s[!is.na(s)]
  prophet_forecats_MSE = c(prophet_forecats_MSE, sum(s*s)/length(s))
  
  if(i%%10 == 0){
    print(i)
    print(Sys.time()-a) }
}


prophet_forecats_MSE = unlist(prophet_forecats_MSE)
prophet_forecats_MSE <- prophet_forecats_MSE[!is.nan(prophet_forecats_MSE)]
RMSE_prophet = (sum(prophet_forecats_MSE)/length(prophet_forecats_MSE))^0.5
RMSE_prophet



# Comparing ARIMA Vs ETS Vs prophet
ALOGORITHM = c("ARIMA_RMSE", "ETS_RMSE", "PROPHET_RMSE")
RMSE = c(RMSE,RMSE_ETS,RMSE_prophet)
df = data.frame(ALOGORITHM,RMSE)
p1 <- df %>%
  ggplot(aes(x = ALOGORITHM, y = RMSE, fill=ALOGORITHM)) +
  geom_col() +
  theme(legend.position = "none")
p1
