library(dplyr)
library(ggplot2)
gender <- read.csv("gender.csv")
profile <- read.csv("profile.csv")



# Exercise Part 1 ---------------------------------------------------------


fertility <- profile %>% 
  inner_join(gender, by = "id") %>% 
  arrange(id)



# Exercise Part 2 ---------------------------------------------------------

# 1
table(gender$gender1, gender$gender2)

# 2
profile %>% 
  mutate(type = ifelse(afam == 'yes', "afam", NA),
         type = ifelse(hispanic == 'yes', "hispanic", type),
         type = ifelse(other == 'yes', "other", type),
         type = ifelse(afam == "no" & hispanic == "no" & other == "no", "white", type)) %>% 
  group_by(type) %>% 
  summarise(pct = mean(work <= 4))

# 3
fertility %>% 
  filter(age >= 22 & age <=24) %>% 
  summarise(pct = mean(gender1 == 'male'))


# Exercise Part 3 ---------------------------------------------------------


fertility_long <- read.csv("fertility_long.csv", stringsAsFactors = TRUE)

# 4
fertility_long %>% 
  filter(race_code != '0') %>% 
  filter(child == 2) %>% 
  ggplot(aes(x = age, group = as.factor(race_code), color = as.factor(race_code))) +
  geom_density()


dat <- profile %>% 
  inner_join(gender, by = "id") %>% 
  mutate(type = ifelse(afam == 'yes', "afam", NA),
         type = ifelse(hispanic == 'yes', "hispanic", type),
         type = ifelse(other == 'yes', "other", type),
         type = ifelse(afam == "no" & 
                         hispanic == "no" &
                         other == "no", "white", type)) %>% 
  select(id, age, work, gender1, gender2, type)

dat %>% 
  filter(type != "white") %>% 
  ggplot(aes(x = age, group = type, fill = type)) +
  geom_histogram(position = "dodge") +
  theme(legend.position = "bottom")

# 5

# dat %>% 
#   select(gender1, type) %>% 
#   group_by(type) %>%
#   mutate(n = n()) %>% 
#   mutate(p = sum(gender1 == "male")/sum(gender1 == "female")) %>% 
#   select(type, n, p) %>% 
#   unique()
  
fertility_long %>% 
  filter(child == 1) %>% 
  group_by(race_code, gender) %>% 
  summarise(n = n()) %>% 
  mutate(pct = n/sum(n)) %>% 
  ggplot(aes(x = as.factor(race_code), y = pct, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge")
