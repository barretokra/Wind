if (!require("tidyverse")) install.packages("tidyverse") ; library(tidyverse)
if (!require("readr")) install.packages("readr") ; library(readr)
if (!require("lubridate")) install.packages("lubridate") ; library(lubridate)
if (!require("reshape2")) install.packages("reshape2") ; library(reshape2)

require(ggplot2)
require(dplyr)

### Loading the data ----

wind1 <- read_delim("Eta_HadGEM2-ES_Hist_1960_2005_W100_3h_P1.txt", 
                    ";", escape_double = FALSE, col_names = FALSE, 
                    trim_ws = TRUE)
wind1 <- wind1 %>% 
  mutate(date = ymd(substr(X1,1,8))) %>% 
  mutate(hour = substr(X1,9,10)) %>% 
  mutate(year = year(date)) %>% 
  na.omit() %>%  #o modelo retorna valores para dia 30 e 31 de fevereiro
  filter(year < 2016)


wind2 <- read_delim("Eta_HadGEM2-ES_RCP8.5_2006_2040_W100_3h_P1.txt", 
                    ";", escape_double = FALSE, col_names = FALSE, 
                    trim_ws = TRUE)
wind2 <- wind2 %>% 
  mutate(date = ymd(substr(X1,1,8))) %>% 
  mutate(hour = substr(X1,9,10)) %>% 
  mutate(year = year(date)) %>% 
  na.omit() %>%  #o modelo retorna valores para dia 30 e 31 de fevereiro
  filter(year < 2040)


wind3 <- read_delim("Eta_HadGEM2-ES_RCP8.5_2040_2070_W100_3h_P1.txt", 
                    ";", escape_double = FALSE, col_names = FALSE, 
                    trim_ws = TRUE)
wind3 <- wind3 %>% 
  mutate(date = ymd(substr(X1,1,8))) %>% 
  mutate(hour = substr(X1,9,10)) %>% 
  mutate(year = year(date)) %>% 
  na.omit() %>%  #o modelo retorna valores para dia 30 e 31 de fevereiro
  filter(year < 2070)


wind4 <- read_delim("Eta_HadGEM2-ES_RCP8.5_2070_2099_W100_3h_P1.txt", 
                    ";", escape_double = FALSE, col_names = FALSE, 
                    trim_ws = TRUE)
wind4 <- wind4 %>% 
  mutate(date = ymd(substr(X1,1,8))) %>% 
  mutate(hour = substr(X1,9,10)) %>% 
  mutate(year = year(date)) %>% 
  na.omit()   #o modelo retorna valores para dia 30 e 31 de fevereiro


wind <- rbind(wind1,wind2,wind3,wind4)


cols <- c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462',
          '#b3de69','#fccde5','#d9d9d9')

ggplot(wind, aes(x = X2, y = ..density..)) +
  geom_histogram(data = subset(wind, year < min(year)+1), fill = cols[1], alpha = 0.7, binwidth = 0.4) +
  geom_histogram(data = subset(wind, year < min(year)+10), fill = cols[2], alpha = 0.7,binwidth = 0.4) +
  geom_histogram(data = subset(wind, year < min(year)+20), fill = cols[3], alpha = 0.7,binwidth = 0.4) +
  geom_histogram(data = subset(wind, year < min(year)+40), fill = cols[4], alpha = 0.7,binwidth = 0.4) +
  geom_histogram(data = subset(wind, year < min(year)+60), fill = cols[5], alpha = 0.7,binwidth = 0.4) +
  geom_histogram(data = subset(wind, year < min(year)+80),fill = cols[6], alpha = 0.7,binwidth = 0.4) +
  geom_histogram(data = subset(wind, year < min(year)+100),fill = cols[7], alpha = 0.7,binwidth = 0.4) +
  geom_histogram(data = subset(wind, year < min(year)+ 120), fill = cols[8], alpha = 0.7,binwidth = 0.4) +
  geom_histogram(data = subset(wind, year < max(year), fill = cols[9], alpha = 0.7,binwidth = 0.4)) +
  labs(x = "Wind Speed")

save(wind, file = "wind.RData")


### Fiting the Weibull


require(broom)
require(purrr)
require(tidyr)
require(MASS)

# loading data

load(file = "wind.RData")

wind_weibull <- wind %>% 
  filter(year < 2018 & year > 1990) %>% 
  dplyr::select(X2, hour) %>% 
  group_by(hour) %>% 
  summarise(vec = X2 %>% list) %>% 
  mutate(mod = map(vec, ~MASS::fitdistr(.x, "Weibull"))) %>% 
  mutate(mod_est_shape = map(mod, ("estimate")) %>% map_dbl("shape")) %>% 
  mutate(mod_est_scale = map(mod, ("estimate")) %>% map_dbl("scale")) %>% 
  mutate(mod_glance = map(mod, glance)) %>% 
  dplyr::select(-vec, -mod) %>% 
  unnest()

# 3 Horas

hour_3 <- data.frame(x = rweibull(wind_weibull$n[2], wind_weibull$mod_est_shape[2], 
                                  scale = wind_weibull$mod_est_scale[2]))


wind %>% 
  filter(year < 2018 & year > 1990) %>% 
  filter(hour == "03") %>% 
  ggplot(aes(x = X2, y = ..density..)) +
  geom_histogram(binwidth = 0.45, fill = "red", alpha = 0.7)+
  geom_histogram(data = hour_3, aes(x = x, y = ..density..), fill = "blue",
                 binwidth = 0.45, alpha = 0.7)


  

