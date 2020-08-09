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
library(ggthemes)
library(DT)

df_tot <- readRDS( '../data_clean/df_tot.RDS' )
df_tot_user <- readRDS('../data_clean/df_tot_user.RDS')
df_sum_date <- readRDS('../data_clean/df_sum_date.RDS')

dates <- seq(as.Date("2019-12-01"), length = 91, by = "days")  
tidy_xts_date = xts(df_sum_date[, -1], dates)

names = colnames(df_sum_date)[-1]
id = as.character(names(df_sum_date[,c(2:8)]))
choice_var = setNames(id,names)

df_tot_cr = df_tot_user %>% 
  summarise(purchase_user = sum(purchase>0),
            purchase_more_than_once = sum(purchase>1),
            all_user = n(),
            conver_rate = purchase_user/all_user,
            repurchase_rate = purchase_more_than_once/purchase_user)



