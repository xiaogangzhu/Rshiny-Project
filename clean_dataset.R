library(dplyr)
# read all files into a list all_dfs
csv_filenames = list.files(path = './data_origin',pattern = '*.csv')
csv_filepaths = paste0('./data_origin/',csv_filenames[c(1,5,4)])
all_dfs <- lapply( csv_filepaths, FUN = function( fp ) read.csv( fp, stringsAsFactors = F ,na.strings="") )



# transfer the event_time to datetiem type and extract month,day,wday and hour
clean_data = function(df){
  require(lubridate)
  require(dplyr)
  return(df %>% 
           filter(df$price>0) %>% 
           select(-category_code,-user_session,-brand) %>% 
           mutate(event_time = parse_date_time(event_time,order = c("Ymd HMS")),
                  Date = ymd(format(event_time,"%Y-%m-%d")),
                  month = format(event_time,"%Y-%m"),
                  wday = wday(Date,label = TRUE),
                  hour = hour(event_time)) %>% 
           select(-event_time)
         )
}
#apply to all_dfs_clean and save
all_dfs_clean = lapply(all_dfs,clean_data)
#all_dfs_clean = lapply(all_dfs,unique)
saveRDS(all_dfs_clean, file = './data_clean/all_dfs.RDS')
#Combine all dfs into one file
create_df_tot <- function( df_lst ){
  require( dplyr )
  df_lst %>% bind_rows()
}
df_tot = create_df_tot(all_dfs_clean)
saveRDS(df_tot, './data_clean/df_tot.RDS')

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
saveRDS(df_sum_date, './data_clean/df_sum_date.RDS')


# create user dataset
df_tot_user <- df_tot %>% 
  group_by(user_id) %>% 
  summarise(view = sum(event_type == 'view'),
            cart = sum(event_type == 'cart'),
            remove_from_cart = sum(event_type == 'remove_from_cart'),
            purchase = sum(event_type=='purchase'),
            total = n())
saveRDS(df_tot_user, './data_clean/df_tot_user.RDS')
