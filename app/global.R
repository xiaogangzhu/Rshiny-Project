library(dplyr)
library(ggplot2)
library(shinydashboard)
library(shiny)
library(dygraphs)
library(xts)
library(plotly)
library(rbokeh)
library(tidyr)
library(googleVis)

df_tot <- readRDS( '../data_clean/df_tot.RDS' )
df_tot_user <- readRDS('../data_clean/df_tot_user.RDS')
#get metrics for date
dates <- seq(as.Date("2019-12-01"), length = 91, by = "days")
df_tot1_date = df_tot %>% 
  group_by(Date) %>% 
  summarise(UV = n_distinct(user_id),
            PV = sum(event_type =='view'),
            UV.PV = UV/PV,
            sales = sum(event_type== 'purchase'))
df_tot2_date = df_tot %>%
  filter(event_type=='purchase') %>%
  group_by(Date) %>%
  summarise(revenue = sum(price))

df_tot3_date = df_tot %>% 
  group_by(Date,user_id) %>% 
  summarise(purchase = sum(event_type=='purchase')
  ) %>% 
  summarise(purchase_user = sum(purchase>0),
            purchase_more_than_once = sum(purchase>1),
            all_user = n(),
            conver_rate = purchase_user/all_user,
            repurchase_rate = purchase_more_than_once/purchase_user) %>% 
  select(-purchase_user,-purchase_more_than_once,-all_user)

df_sum_date = cbind(df_tot1_date,df_tot2_date[,-1],df_tot3_date[,-1])  
  
tidy_xts_date = xts(df_sum_date[, -1], dates)


df_tot_cr = df_tot_user %>% 
  summarise(purchase_user = sum(purchase>0),
            purchase_more_than_once = sum(purchase>1),
            all_user = n(),
            conver_rate = purchase_user/all_user,
            repurchase_rate = purchase_more_than_once/purchase_user)



