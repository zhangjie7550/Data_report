library(tidyverse)
library(ggthemes)
library(lubridate)

library(RColorBrewer)
library(gridExtra)
library(showtext)

set.seed(666)

# set theme
old <- theme_get()
new <- theme_set(theme_tufte() + theme(text = element_text(family = "Noto Sans CJK SC"),
                                       legend.position = "bottom"))
# read data
profile <- read_csv("exam/profile.csv") %>% as_tibble()


profile %>% 
  group_by(age) %>% 
  summarise(n = n())

# distribution of age
profile %>% 
  ggplot(aes(x = age)) +
  geom_histogram(binwidth = 1)
