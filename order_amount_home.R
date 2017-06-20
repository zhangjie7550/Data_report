library(dplyr)
library(tidyr)
library(RPostgreSQL)
library(readxl)
library(stringr)
library(reshape2)
library(ggplot2)
library(gridExtra)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, user = user.name, password = user.password, 
                 dbname = dbname, host = host)

orders <- dbGetQuery(con, "select * from ods.order_success")

home <- orders %>% 
  filter(order_type == "民宿") %>% 
  select(order_id, order_time, country, city, total_amt, persons = check_in_person, days = living_day,
         check_in_date, check_out_date, home_id, rooms) %>% 
  mutate(year = format(order_time, "%Y"), month = format(order_time, "%m"),
         avg_night = round(total_amt / days , 1),
         avg_person_night = round(total_amt / days / persons , 1))

df <- home %>% 
  group_by(year, month) %>% 
  summarise(total_amt = sum(total_amt), total_amt_w = round(sum(total_amt)/10000, 2), n = n())

df800 <- home %>% 
  filter(avg_person_night < 800) %>% 
  group_by(year, month) %>% 
  summarise(total_amt = sum(total_amt), total_amt_w = round(sum(total_amt)/10000, 2), n = n())

df %>% 
  ggplot(aes(x = factor(month), y = total_amt, group = 1)) +
  geom_line() + 
  geom_point(aes(size = n, alpha = n), colour = "darkorange2") +
  facet_wrap(~ year)

df800 %>% 
  ggplot(aes(x = factor(month), y = total_amt, group = 1)) +
  geom_line() + 
  geom_point(aes(size = n, alpha = n), colour = "darkorange2") +
  facet_wrap(~ year)

df1 <- df %>% filter(year == 2016 & month < '06')
df2 <- df %>% filter(year == 2017)

df_r <- bind_cols(df2, df1) %>% 
  mutate(amt_tb_ratio = round((total_amt/total_amt1 -1) * 100, 1),
         cnt_tb_ratio = round((n/n1 - 1) * 100, 1)) %>% 
  select(year, month, amt_tb_ratio, cnt_tb_ratio)

#df_r %>% ggplot(aes(x = factor(month), y = ratio, group = type, colour = type)) + geom_line()

df_g <- bind_cols(df2, df_r) %>% bind_rows(df1) %>% select(-year1, -month1) %>% as.data.frame()

p1 <- df_g %>% ggplot(aes(month, weight = total_amt_w, group = year)) +
  geom_bar(aes(fill = year), position = "dodge") +
  geom_text(data = df_g, aes(month, total_amt_w, group = year,
                             label = amt_tb_ratio), 
            position = position_dodge(0.5), na.rm = TRUE) +
            #position = position_stack(vjust = 0.5)) +
  labs(title = "订单金额同期对比", subtitle = "数字为同期差别百分比(%)",
       x = "月份", y = "订单金额(¥万)") 

##人均间夜价格800以下
df1_800 <- df800 %>% filter(year == 2016 & month < '06')
df2_800 <- df800 %>% filter(year == 2017)

df_r_800 <- bind_cols(df2_800, df1_800) %>% 
  mutate(amt_tb_ratio = round((total_amt/total_amt1 -1) * 100, 1),
         cnt_tb_ratio = round((n/n1 - 1) * 100, 1)) %>% 
  select(year, month, amt_tb_ratio, cnt_tb_ratio)

#df_r_800 %>% ggplot(aes(x = factor(month1), y = ratio, group = type, colour = type)) + geom_line()

df_g_800 <- bind_cols(df2_800, df_r_800) %>% bind_rows(df1_800) %>% select(-year1, -month1) %>% as.data.frame()

p2 <- df_g_800 %>% ggplot(aes(month, weight = total_amt_w, group = year)) +
  geom_bar(aes(fill = year), position = "dodge") +
  geom_text(data = df_g_800, aes(month, total_amt_w, group = year,
                             label = amt_tb_ratio), 
            position = position_dodge(0.5), na.rm = TRUE) +
  #position = position_stack(vjust = 0.5)) +
  labs(title = "订单金额同期对比(人均间夜价格800以下)", subtitle = "数字为同期差别百分比(%)",
       x = "月份", y = "订单金额(¥万)") 

grid.arrange(p1, p2, ncol = 2, nrow = 1)
