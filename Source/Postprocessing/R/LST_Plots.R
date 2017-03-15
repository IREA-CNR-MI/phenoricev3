library(ggplot2)
library(magrittr)
library(dplyr)


infile_ita <- "/home/lb/ITALST.txt"
infile_phl <- "/home/lb/Google_Drive/IREA/Conference&paper/Pheno-Rice paper/2nd_submission/New_analysis/LST_PHL.txt"
infile_ind <- "/home/lb/Google_Drive/IREA/Conference&paper/Pheno-Rice paper/2nd_submission/New_analysis/LST_IND.txt"

indata_ita <- read.csv(infile_ita, header = F, skip = 1, sep = " ", strip.white = T) ; names(indata_ita) = c("DOY","LST")
indata_phl <- read.csv(infile_phl, header = F, skip = 1, sep = " ", strip.white = T) ; names(indata_phl) = c("DOY","LST")
indata_ind <- read.csv(infile_ind,header = F, skip = 1, sep = " ", strip.white = T) ; names(indata_ind) = c("DOY","LST")

indata_ita [indata_ita == -1] = NA
indata_ita [indata_ita == 32767] = NA
indata_phl [indata_phl == -1] = NA
indata_phl [indata_phl == 32767] = NA
indata_ind [indata_ind == -1] = NA
indata_ind [indata_ind == 32767] = NA


indata_ita <- indata_ita %>% 
  mutate(country = "ITA", LST = 0.02*LST-273.15, date = doytodate(DOY, year = 2013))

indata_phl <- indata_phl %>% 
  mutate(country = "PHL", LST = 0.02*LST-273.15, date = doytodate(DOY, year = 2013) )

indata_ind <- indata_ind %>% 
  mutate(country = "IND", LST = 0.02*LST-273.15, date = doytodate(DOY, year = 2014) )

indata <- rbind(indata_ita,indata_ind, indata_phl)
indata <- subset(indata, is.na(LST) == FALSE)

p <- ggplot(subset(indata, country =="ITA"), aes(x = date, y = LST))
p <-  p + geom_line() + facet_wrap(~country)+ scale_x_date() + 
  scale_x_date (limits = c(as.Date("2013-01-01"),as.Date("2014-01-01")))+
  ylim(0,45)
p

p <- ggplot(subset(indata, country =="IND"), aes(x = date, y = LST))
p <-  p + geom_line() + facet_wrap(~country)+ scale_x_date() + 
  scale_x_date (limits = c(as.Date("2014-01-01"),as.Date("2014-12-01")))+
  ylim(0,45)
p

p <- ggplot(subset(indata, country =="PHL"), aes(x = date, y = LST))
p <-  p + geom_line() + facet_wrap(~country)+ scale_x_date() + 
  scale_x_date (limits = c(as.Date("2013-01-01"),as.Date("2014-01-01")))+
  ylim(0,45)
p


