if (!require("tidyverse")) install.packages("tidyverse") ; library(tidyverse)
if (!require("readr")) install.packages("readr") ; library(readr)
if (!require("lubridate")) install.packages("lubridate") ; library(lubridate)
if (!require("reshape2")) install.packages("reshape2") ; library(reshape2)



### Loading the data

wind <- read_delim("Eta_HadGEM2-ES_RCP8.5_2006_2040_W100_3h_P1.txt", 
                   ";", escape_double = FALSE, col_names = FALSE, 
                   trim_ws = TRUE)

wind <- wind %>% 
  mutate(date = ymd(substr(X1,1,8))) %>% 
  mutate(hour = substr(X1,9,10)) %>% 
  mutate(year = year(date)) %>% 
  na.omit()  #o modelo retorna valores para dia 30 e 31 de fevereiro

cols <- c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462',
          '#b3de69','#fccde5','#d9d9d9')

ggplot(wind, aes(x = X2, y = ..density..)) +
  geom_histogram(data = subset(wind, year < min(year)+1), fill = cols[1], alpha = 0.7) +
  geom_histogram(data = subset(wind, year < min(year)+2), fill = cols[2], alpha = 0.7) +
  geom_histogram(data = subset(wind, year < min(year)+3), fill = cols[3], alpha = 0.7) +
  geom_histogram(data = subset(wind, year < min(year)+4), fill = cols[4], alpha = 0.7) +
  geom_histogram(data = subset(wind, year < min(year)+5), fill = cols[5], alpha = 0.7) +
  geom_histogram(data = subset(wind, year < min(year)+10),fill = cols[6], alpha = 0.7) +
  geom_histogram(data = subset(wind, year < min(year)+20),fill = cols[7], alpha = 0.7) +
  geom_histogram(data = subset(wind, year < min(year)+30), fill = cols[8], alpha = 0.7) +
  geom_histogram(data = subset(wind, year < max(year), fill = cols[9], alpha = 0.7)) +
  labs(x = "Wind Speed")

# E ai? Converge?


