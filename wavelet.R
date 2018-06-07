if (!require("tidyverse")) install.packages("tidyverse") ; library(tidyverse)
if (!require("readr")) install.packages("readr") ; library(readr)
if (!require("lubridate")) install.packages("lubridate") ; library(lubridate)
if (!require("reshape2")) install.packages("reshape2") ; library(reshape2)
if (!require("WaveletComp")) install.packages("WaveletComp") ; library(WaveletComp)
if (!require("hms")) install.packages("hms") ; library(hms)


# loading data

load(file = "wind.RData")


# Transform Data


wind <- wind %>% 
  mutate(time = as.hms(as.numeric(hour)*60*60)) %>% 
  mutate(time = date + time)


my.data <- data.frame(date = wind$time, x = wind$X2)

my.w <- analyze.wavelet(my.data, "x", loess.span = 0, dt = 1/8, dj = 1/10,
                        make.pval = FALSE)

wt.image(my.w, color.key = "q",
         legend.params = list(lab = "wavelet power levels"))

