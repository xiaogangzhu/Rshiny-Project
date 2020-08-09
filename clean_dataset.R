# read all files into a list all_dfs
csv_filenames = list.files(path = './data_origin',pattern = '*.csv')
csv_filepaths = paste0('./data_origin/',csv_filenames[c(1,5,4)])
all_dfs <- lapply( csv_filepaths, FUN = function( fp ) read.csv( fp, stringsAsFactors = F ,na.strings="") )


# sapply(df, function(x) sum(is.na(x)))
# df= all_dfs[[2]]
# df$Time = as_hms(df$Time)
# df$Date = parse_date_time(df$Date,order = c("Ymd"))
# df = df %>% 
#   separate(event_time,c("Date","Time"),sep = "[\\s]+")
# str(df)
# head(df)
# 
# df %>% 
#   filter(Date == as.Date("2019-12-01")) %>% 
#   group_by(hour) %>% 
#   summarise(n=n()) %>% 
#   ggplot(aes(x=hour,y=n)) + geom_col()
#   
# df = df %>% 
#   filter(df$price>0) %>% 
#   select(-category_code,-user_session,-brand) %>% 
#   mutate(event_time = parse_date_time(event_time,order = c("Ymd HMS")),
#          Date = ymd(format(event_time,"%Y-%m-%d")),
#          month = format(event_time,"%Y-%m"),
#          week = strftime(event_time,format="%W"),
#          wday = wday(Date,label = TRUE),
#          hour = hour(event_time)) %>% 
#   select(-event_time)
# 
# time = head(df$event_time,14)
# strftime(time,format="%W")
# unique(df$week)
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
# create user dataset
df_tot_user <- df_tot %>% 
  group_by(user_id) %>% 
  summarise(view = sum(event_type == 'view'),
            cart = sum(event_type == 'cart'),
            remove_from_cart = sum(event_type == 'remove_from_cart'),
            purchase = sum(event_type=='purchase'),
            total = n())
saveRDS(df_tot_user, './data_clean/df_tot_user.RDS')
