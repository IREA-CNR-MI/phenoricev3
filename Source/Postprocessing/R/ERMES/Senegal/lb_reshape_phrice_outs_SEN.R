# Funcrion to extract Sowing dates data from full phenorice results
#

library(raster)
library(dplyr)
library(ireaRscripts)

in_folder = "/media/nr_working/shared/PhenoRice/Processing/Senegal/January2/Outputs/"
out_folder = "/media/nr_working/shared/PhenoRice/Processing/Senegal/January2/Outputs/sowrasters_2"
years = paste0(seq(2003,2016,1),"/")
in_folders = file.path(in_folder, years)

yy = 1
years = seq(2003,2016,1)

for(yy in seq(along = years)) {

  print(yy)
  in_file = list.files(in_folders[[yy]], pattern = paste0('Phenorice_out_',years[yy]-1,'_209?'), full.names = TRUE)[1]
  out_file_dry = file.path(out_folder,'sos', 'dry',paste0("sow_dry_", years[yy],'.tif'))
  out_file_wet = file.path(out_folder,'sos','wet',paste0("sow_wet_", years[yy],'.tif'))
  out_file_dry_eos = file.path(out_folder,'eos','dry',paste0("eos_dry_", years[yy],'.tif'))
  out_file_wet_eos = file.path(out_folder,'eos','wet',paste0("eos_wet_", years[yy],'.tif'))
  out_file_dry_flow = file.path(out_folder,'flow','dry',paste0("flow_dry_", years[yy],'.tif'))
  out_file_wet_flow = file.path(out_folder,'flow','wet',paste0("flow_wet_", years[yy],'.tif'))
  out_file_dry_lgt = file.path(out_folder,'lgt','dry',paste0("lgt_dry_", years[yy],'.tif'))
  out_file_wet_lgt = file.path(out_folder,'lgt','wet',paste0("lgt_wet_", years[yy],'.tif'))
  dir.create(dirname(out_file_dry),recursive = TRUE)
  dir.create(dirname(out_file_wet),recursive = TRUE)
  dir.create(dirname(out_file_dry_eos),recursive = TRUE)
  dir.create(dirname(out_file_wet_eos),recursive = TRUE)
  dir.create(dirname(out_file_dry_flow),recursive = TRUE)
  dir.create(dirname(out_file_wet_flow),recursive = TRUE)
  dir.create(dirname(out_file_dry_lgt),recursive = TRUE)
  dir.create(dirname(out_file_wet_lgt),recursive = TRUE)
  # OPen the raster
  n_seas <- raster(in_file, band = 1) ; n_seas_data <- values(n_seas)
  sow_1 <- raster(in_file, band = 2) ; sow_1_data <- values(sow_1)
  sow_2 <- raster(in_file, band = 3) ; sow_2_data <- values(sow_2)
  sow_3 <- raster(in_file, band = 4) ; sow_3_data <- values(sow_3)

  eos_1 <- raster(in_file, band = 8) ; eos_1_data <- values(eos_1)
  eos_2 <- raster(in_file, band = 9) ; eos_2_data <- values(eos_2)
  eos_3 <- raster(in_file, band = 10) ; eos_3_data <- values(eos_3)

  flow_1 <- raster(in_file, band = 5) ; flow_1_data <- values(flow_1)
  flow_2 <- raster(in_file, band = 6) ; flow_2_data <- values(flow_2)
  flow_3 <- raster(in_file, band = 7) ; flow_3_data <- values(flow_3)
  
  lgt_1 <- raster(in_file, band = 17) ; lgt_1_data <- values(lgt_1)
  lgt_2 <- raster(in_file, band = 18) ; lgt_2_data <- values(lgt_2)
  lgt_3 <- raster(in_file, band = 19) ; lgt_3_data <- values(lgt_3)

  # remove NA results

  sow_1_data[which(sow_1_data >= 365)] <- NA
  sow_2_data[which(sow_2_data >= 365)] <- NA
  sow_3_data[which(sow_3_data >= 365)] <- NA

  sow_1_data[which(sow_1_data == 0)] <- NA
  sow_2_data[which(sow_2_data == 0)] <- NA
  sow_3_data[which(sow_3_data == 0)] <- NA
  
  sow_1_data[which(sow_1_data == -999)] <- NA
  sow_2_data[which(sow_2_data == -999)] <- NA
  sow_3_data[which(sow_3_data == -999)] <- NA

  flow_1_data[which(flow_1_data >= 400)] <- NA
  flow_2_data[which(flow_2_data >= 400)] <- NA
  flow_3_data[which(flow_3_data >= 400)] <- NA

  flow_1_data[which(flow_1_data == 0)] <- NA
  flow_2_data[which(flow_2_data == 0)] <- NA
  flow_3_data[which(flow_3_data == 0)] <- NA
  
  flow_1_data[which(flow_1_data == -999)] <- NA
  flow_2_data[which(flow_2_data == -999)] <- NA
  flow_3_data[which(flow_3_data == -999)] <- NA

  eos_1_data[which(eos_1_data >= 450)] <- NA
  eos_2_data[which(eos_2_data >= 450)] <- NA
  eos_3_data[which(eos_3_data >= 450)] <- NA

  eos_1_data[which(eos_1_data == 0)] <- NA
  eos_2_data[which(eos_2_data == 0)] <- NA
  eos_3_data[which(eos_3_data == 0)] <- NA
  
  eos_1_data[which(eos_1_data == -999)] <- NA
  eos_2_data[which(eos_2_data == -999)] <- NA
  eos_3_data[which(eos_3_data == -999)] <- NA
  
  lgt_1_data[which(lgt_1_data >= 450)] <- NA
  lgt_2_data[which(lgt_2_data >= 450)] <- NA
  lgt_3_data[which(lgt_3_data >= 450)] <- NA
  
  lgt_1_data[which(lgt_1_data == 0)] <- NA
  lgt_2_data[which(lgt_2_data == 0)] <- NA
  lgt_3_data[which(lgt_3_data == 0)] <- NA
  
  lgt_1_data[which(lgt_1_data == -999)] <- NA
  lgt_2_data[which(lgt_2_data == -999)] <- NA
  lgt_3_data[which(lgt_3_data == -999)] <- NA

  sow_1[] <- sow_1_data
  sow_2[] <- sow_2_data
  sow_3[] <- sow_3_data

  eos_1[] <- eos_1_data
  eos_2[] <- eos_2_data
  eos_3[] <- eos_3_data

  flow_1[] <- flow_1_data
  flow_2[] <- flow_2_data
  flow_3[] <- flow_3_data

  lgt_1[] <- lgt_1_data
  lgt_2[] <- lgt_2_data
  lgt_3[] <- lgt_3_data
  
  setValues(sow_1, sow_1_data)
  setValues(sow_2, sow_2_data)
  setValues(sow_3, sow_3_data)

  setValues(flow_1, flow_1_data)
  setValues(flow_2, flow_2_data)
  setValues(flow_3, flow_3_data)

  setValues(eos_1, eos_1_data)
  setValues(eos_2, eos_2_data)
  setValues(eos_3, eos_3_data)
  
  setValues(lgt_1, lgt_1_data)
  setValues(lgt_2, lgt_2_data)
  setValues(lgt_3, lgt_3_data)
  

  sow_1_dry = sow_1
  sow_1_dry[sow_1_dry >= 130] = NA
  sow_1_dry[sow_1_dry <= 0] = NA

  eos_1_dry = eos_1
  flow_1_dry = flow_1
  eos_1_dry[sow_1 >= 130] = NA
  eos_1_dry[sow_1 <= 0] = NA
  flow_1_dry[sow_1 >= 130] = NA
  flow_1_dry[sow_1 <= 0] = NA

  eos_2_dry = eos_2
  flow_2_dry = flow_2
  sow_2_dry = sow_2
  sow_2_dry[sow_2 >= 130] = NA
  sow_2_dry[sow_2 <= 0] = NA

  eos_2_dry[sow_2 >= 130] = NA
  eos_2_dry[sow_2 <= 0] = NA
  flow_2_dry[sow_2 >= 130] = NA
  flow_2_dry[sow_2 <= 0] = NA

  sow_3_dry = sow_3
  sow_3_dry[sow_3 >= 130] = NA
  sow_3_dry[sow_3 <= 0] = NA

  eos_3_dry = eos_3
  flow_3_dry = flow_3

  eos_3_dry[sow_3 >= 130] = NA
  eos_3_dry[sow_3 <= 0] = NA
  flow_3_dry[sow_3 >= 130] = NA
  flow_3_dry[sow_3 <= 0] = NA

  sow_1_wet = sow_1
  sow_1_wet[sow_1 <= 150] = NA

  sow_2_wet = sow_2
  sow_2_wet[sow_2 <= 150] = NA

  sow_3_wet = sow_3
  sow_3_wet[sow_3 <= 150] = NA

  eos_1_wet = eos_1
  flow_1_wet = flow_1

  eos_1_wet[sow_1 <= 170] = NA
  eos_1_wet[sow_1 <= 0] = NA
  flow_1_wet[sow_1 <= 170] = NA
  flow_1_wet[sow_1 <= 0] = NA


  eos_2_wet = eos_2
  flow_2_wet = flow_2
  eos_2_wet[sow_2 <= 170] = NA
  eos_2_wet[sow_2 <= 0] = NA
  flow_2_wet[sow_2 <= 170] = NA
  flow_2_wet[sow_2 <= 0] = NA

  eos_3_wet = eos_3
  flow_3_wet = flow_3
  eos_3_wet[sow_3 <= 170] = NA
  eos_3_wet[sow_3 <= 0] = NA
  flow_3_wet[sow_3 <= 170] = NA
  flow_3_wet[sow_3 <= 0] = NA

  sow_dry = sum(stack(sow_1_dry, sow_2_dry, sow_3_dry), na.rm = T)
  sow_wet = sum(stack(sow_1_wet, sow_2_wet, sow_3_wet), na.rm = T)
  sow_dry[sow_dry == 0] = NA
  sow_wet[sow_wet == 0] = NA
  sow_dry[sow_dry >= 365] = NA
  sow_wet[sow_wet >= 365] = NA

  eos_dry = sum(stack(eos_1_dry, eos_2_dry, eos_3_dry), na.rm = T)
  eos_wet = sum(stack(eos_1_wet, eos_2_wet, eos_3_wet), na.rm = T)
  eos_dry[eos_dry == 0] = NA
  eos_wet[eos_wet == 0] = NA
  eos_dry[eos_dry >= 450] = NA
  eos_wet[eos_wet >= 450] = NA

  flow_dry = sum(stack(flow_1_dry, flow_2_dry, flow_3_dry), na.rm = T)
  flow_wet = sum(stack(flow_1_wet, flow_2_wet, flow_3_wet), na.rm = T)
  flow_dry[flow_dry == 0] = NA
  flow_wet[flow_wet == 0] = NA
  flow_dry[flow_dry >= 400] = NA
  flow_wet[flow_wet >= 400] = NA
  
  lgt_dry = eos_dry - sow_dry
  lgt_wet = eos_wet - sow_wet

  writeRaster(sow_dry, filename = out_file_dry, overwrite = T)
  writeRaster(sow_wet, filename = out_file_wet, overwrite = T)

  writeRaster(flow_dry, filename = out_file_dry_flow, overwrite = T)
  writeRaster(flow_wet, filename = out_file_wet_flow, overwrite = T)

  writeRaster(eos_dry, filename = out_file_dry_eos, overwrite = T)
  writeRaster(eos_wet, filename = out_file_wet_eos, overwrite = T)
  
  writeRaster(lgt_dry, filename = out_file_dry_lgt, overwrite = T)
  writeRaster(lgt_wet, filename = out_file_wet_lgt, overwrite = T)


}

dry_stack = stack(list.files(file.path(out_folder, 'sos','dry'), '.tif$', full.names = TRUE))
wet_stack = stack(list.files(file.path(out_folder, 'sos', 'wet'), '.tif$', full.names = TRUE))
dry_stack_eos = stack(list.files(file.path(out_folder,'eos','dry'), '.tif$', full.names = TRUE))
wet_stack_eos = stack(list.files(file.path(out_folder,'eos', 'wet'), '.tif$', full.names = TRUE))
dry_stack_flow = stack(list.files(file.path(out_folder,'flow','dry'), '.tif$', full.names = TRUE))
wet_stack_flow = stack(list.files(file.path(out_folder,'flow', 'wet'), '.tif$', full.names = TRUE))
dry_stack_lgt = stack(list.files(file.path(out_folder,'lgt','dry'), '.tif$', full.names = TRUE))
wet_stack_lgt = stack(list.files(file.path(out_folder,'lgt', 'wet'), '.tif$', full.names = TRUE))


in_dates = as.Date('2004-01-01')+365*seq(0,length(years)-1,1)
in_dates[2:11] = in_dates[2:11] + 1
in_dates[6:11] = in_dates[6:11] + 1
in_dates[8:11] = in_dates[8:11] + 1

# lb_writeenvits(dry_stack, in_dates = in_dates, out_file = "/media/nr_working/shared/PhenoRice/Processing/Senegal/January2/Outputs/sow_rasters/sos/dry/sow_dry_2004_2014.dat")
# lb_writeenvits(wet_stack, in_dates = in_dates, out_file = "/media/nr_working/shared/PhenoRice/Processing/Senegal/January2/Outputs/sow_rasters/sos/wet/sow_wet_2004_2014.dat")
# 
# lb_writeenvits(dry_stack_eos, in_dates = in_dates, out_file = "/media/nr_working/shared/PhenoRice/Processing/Senegal/January2/Outputs/eos/dry/eos_dry_2004_2014.dat")
# lb_writeenvits(wet_stack_eos, in_dates = in_dates, out_file = "/media/nr_working/shared/PhenoRice/Processing/Senegal/January2/Outputs/wet/eos_wet_2004_2014.dat")
# 
# lb_writeenvits(dry_stack_flow, in_dates = in_dates, out_file = "/media/nr_working/shared/PhenoRice/Processing/Senegal/January2/Outputs/flow/dry/flow_dry_2004_2014.dat")
# lb_writeenvits(wet_stack_flow, in_dates = in_dates, out_file = "/media/nr_working/shared/PhenoRice/Processing/Senegal/January2/Outputs/flow/wet/flow_wet_2004_2014.dat")

sow_data_dry = data.frame(year = NULL, DOY = NULL)
eos_data_dry =  data.frame(year = NULL, DOY = NULL)
flow_data_dry =  data.frame(year = NULL, DOY = NULL)
lgt_data_dry =  data.frame(year = NULL, DOY = NULL)
sow_data_wet =  data.frame(year = NULL, DOY = NULL)
eos_data_wet =  data.frame(year = NULL, DOY = NULL)
flow_data_wet =  data.frame(year = NULL, DOY = NULL)
lgt_data_wet =  data.frame(year = NULL, DOY = NULL)


sow_dry = getValues(dry_stack)
sow_wet = getValues(wet_stack)
eos_dry = getValues(dry_stack_eos)
eos_wet = getValues(wet_stack_eos)
flow_dry = getValues(dry_stack_flow)
flow_wet = getValues(wet_stack_flow)
lgt_dry = getValues(dry_stack_lgt)
lgt_wet = getValues(wet_stack_lgt)

for (yy in 1:length(years)){

  sow_data_dry = rbind(sow_data_dry, data.frame(year = years[yy], DOY = sow_dry[,yy][!is.na(sow_dry[,yy])]))
  sow_data_wet = rbind(sow_data_wet, data.frame(year = years[yy], DOY = sow_wet[,yy][!is.na(sow_wet[,yy])]))

  eos_data_dry = rbind(eos_data_dry, data.frame(year = years[yy], DOY = eos_dry[,yy][!is.na(eos_dry[,yy])]))
  eos_data_wet = rbind(eos_data_wet, data.frame(year = years[yy], DOY = eos_wet[,yy][!is.na(eos_wet[,yy])]))

  flow_data_dry = rbind(flow_data_dry, data.frame(year = years[yy], DOY = flow_dry[,yy][!is.na(flow_dry[,yy])]))
  flow_data_wet = rbind(flow_data_wet, data.frame(year = years[yy], DOY = flow_wet[,yy][!is.na(flow_wet[,yy])]))
  
  lgt_data_dry = rbind(lgt_data_dry, data.frame(year = years[yy], DOY =  lgt_dry[,yy][!is.na(lgt_dry[,yy])]))
  lgt_data_wet = rbind(lgt_data_wet, data.frame(year = years[yy], DOY =  lgt_wet[,yy][!is.na(lgt_wet[,yy])]))
  

}



sowdry_stats = summarise(group_by(sow_data_dry, year), mean = mean(DOY), sd = sd(DOY), min = min(DOY), max = max(DOY),N = length (DOY))
sowwet_stats = summarise(group_by(sow_data_wet, year), mean = mean(DOY), sd = sd(DOY), min = min(DOY), max = max(DOY),N = length (DOY))

eosdry_stats = summarise(group_by(eos_data_dry, year), mean = mean(DOY), sd = sd(DOY), min = min(DOY), max = max(DOY), N = length (DOY))
eoswet_stats = summarise(group_by(eos_data_wet, year), mean = mean(DOY), sd = sd(DOY), min = min(DOY), max = max(DOY), N = length (DOY))

flowdry_stats = summarise(group_by(flow_data_dry, year), mean = mean(DOY), sd = sd(DOY), N = length (DOY), min = min(DOY), max = max(DOY),N = length (DOY))
flowwet_stats = summarise(group_by(flow_data_wet, year), mean = mean(DOY), sd =sd(DOY), N = length (DOY), min = min(DOY), max = max(DOY),N = length (DOY))

lgtdry_stats = summarise(group_by(lgt_data_dry, year), mean = mean(DOY), sd = sd(DOY), N = length (DOY), min = min(DOY), max = max(DOY),N = length (DOY))
lgtwet_stats = summarise(group_by(lgt_data_wet, year), mean = mean(DOY), sd =sd(DOY), N = length (DOY), min = min(DOY), max = max(DOY),N = length (DOY))


save(sowdry_stats,eosdry_stats,flowdry_stats, sowwet_stats, eoswet_stats, flowwet_stats,
     lgtdry_stats,lgtwet_stats,
     file = '/media/nr_working/shared/PhenoRice/Processing/Senegal/January2/Outputs/sowrasters_2/Statistics.RData')
load('/media/nr_working/shared/PhenoRice/Processing/Senegal/January2/Outputs/sowrasters_2/Statistics.RData')
#
 sow_data_all = rbind(sow_data_dry, sow_data_wet)
 sow_data_all$Date = doytodate(sow_data_all$DOY, 2000)
 p = ggplot(sow_data_all) + theme_bw()
 p = p + geom_histogram(aes (x = doytodate(DOY, 2014), y = ..count.. * 231.656*231.656/10000), binwidth = 8,
                        fill = 'grey75', color = 'black') + facet_wrap(~year)
 p = p + ylab("Area [ha]")+ xlab ('Date') + scale_x_date(date_breaks = "1 month",  date_labels = "%b")
 p = p + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1 ))
 p
 #
#
p = ggplot(sow_data_all, aes(Date))+theme_bw()
# p = p + geom_histogram(aes(y=100*..count../sum(..count..)),alpha=.5, position = 'identity', breaks =as.numeric(seq(min(sow_data_all$Date),max(sow_data_all$Date),'8 day')), color = 'black')
p = p + geom_histogram(aes(y =100*(..count.. / (sapply(PANEL, FUN=function(x) sum(count[PANEL == x]))))),alpha=.5, position = 'identity', breaks =as.numeric(seq(min(sow_data_all$Date),max(sow_data_all$Date),'8 day')), color = 'black')
# p = p + geom_density(aes(y = 100*(..count../(sapply(PANEL, FUN=function(x) sum(count[PANEL == x]))))), position = 'identity', fill = 'transparent', adjust = 1.2)
p = p + theme(plot.title = element_text(vjust = 1, hjust = 0)) + facet_wrap(~year)
p2_mins = p + theme(axis.title = element_text(size = 20)) + ylab('Frequency [%]') +
  scale_x_date(breaks = date_breaks('2 month'), labels = date_format("%b")) +
  theme(axis.text.x = element_text(angle = 45, hjust  = 1 , vjust = 1))
p2_mins + theme(axis.text = element_text(size = 18)) + theme(strip.text.x= element_text(size = 18))

#
 in_areafile = "D:/Documents/Personal/Articles/abstracts/ESA_LP_2016/Poster/Areas.csv"
 areadata = read_delim(in_areafile, delim = ';')
 # areamelt = melt(areadata, id.vars = c("Year", "MOD_Winter","MOD_total","MOD_Summer"))
 # names (areamelt)[6] = 'x'
 # areamelt = melt(areamelt, id.vars = c("Year", "variable","x"))
 # names (areamelt)[5] = 'y'
 areadata$yy = substr(areadata$Year,3,4)
 
 sowdry_stats$var  <- 'Sow'   ; sowdry_stats$seas  <- 'Dry'
 sowwet_stats$var  <- 'Sow'   ; sowwet_stats$seas  <- 'Wet'
 flowdry_stats$var <- 'Flow'  ; flowdry_stats$seas  <- 'Dry'
 flowwet_stats$var <- 'Flow'  ; flowwet_stats$seas  <- 'Wet'
 eosdry_stats$var  <- 'Eos'   ; eosdry_stats$seas  <- 'Dry'
 eoswet_stats$var  <- 'Eos'   ; eoswet_stats$seas  <- 'Wet'
 lgtdry_stats$var  <- 'Lgt'   ; lgtdry_stats$seas  <- 'Dry'
 lgtwet_stats$var  <- 'Lgt'   ; lgtwet_stats$seas  <- 'Wet'
 
 data_tot <- as_tibble(rbind(sowdry_stats,eosdry_stats,flowdry_stats, sowwet_stats, eoswet_stats, flowwet_stats,
                   lgtdry_stats,lgtwet_stats))
 data_tot$var <- factor(data_tot$var)
 data_tot$seas <- factor(data_tot$seas)
 data_tot$wrappe = factor(paste0(data_tot$var, data_tot$seas), 
                          levels = c('SowDry','SowWet', "FlowDry", "FlowWet","EosDry","EosWet","LgtDry",  "LgtWet"),
                          labels = c('Sowing date - Dry Season','Sowing date - Wet Season', 'Flowering date - Dry Season','Flowering date - Wet Season',
                                     'Harvesting date - Dry Season','Harvesting date - Wet Season', 'Season Length - Dry Season','Season Length - Wet Season'))
data_tot
  stats <- data_tot %>% 
   group_by(wrappe) %>% 
   do(glance(lm(mean ~ year, .)))
 
 p <- ggplot(data_tot, aes (x = year, y = doytodate(mean,2001)))  + 
        geom_point() + 
        geom_smooth(method = 'lm') + 
        theme_bw() + 
        facet_wrap(~wrappe, scales = 'free_y', ncol = 2)
p
 #
# ggplot(data = areamelt, aes(x = x, y = y, label=y))+
#   stat_smooth(geom="text",method="lm",hjust=0,parse=TRUE) +
#   geom_smooth(method="lm",se=FALSE) +
#   geom_point() + facet_wrap(~variable)
#
#
# p = ggplot(areadata)
# p = p + geom_point(aes(y = MOD_Winter, x = Stat_winter), size = 5)
# p = p + geom_text(aes(y = MOD_Winter, x = Stat_winter, label = yy), hjust=0, vjust=1.1, size = 7)
# p = p + geom_smooth(aes(y = MOD_Winter, x = Stat_winter),method = "lm", se=FALSE, color="orange", formula = y~x, lty = 'dashed')
# p = p + theme_bw() + xlim(-50,100) + ylim (-50,100)
# # p = p + geom_hline(yintercept = 0) + geom_vline(xintercept = 0 )
# p = p + ylab("Phenorice Rice Area\n % difference with 2003 values")
# p = p + xlab("Official Rice Area\n % difference with 2003 values")
# p = p + geom_text(aes(x = -65, y = 130, label = lm_eqn(lm(MOD_Winter ~ Stat_winter,areadata))), parse = TRUE, size = 7)
# p_wet= p + theme(axis.title = element_text(size = 20),
#     axis.text = element_text(size = 20),
#     plot.title = element_text(size = 20))
#
# p = ggplot(areadata)
# p = p + geom_point(aes(y = MOD_Summer, x = Stat_Summer), color = 'red', size = 5)
# p = p + geom_text(aes(y = MOD_Summer, x = Stat_Summer, label = yy), hjust=0, vjust=1.1, size = 7)
# p = p + geom_smooth(aes(y = MOD_Summer, x = Stat_Summer),method = "lm", se=FALSE, color="orange", formula = y~x, lty = 'dashed')
# p = p + theme_bw() + xlim(-50,900) + ylim (-50,900)
# # p = p + geom_hline(yintercept = 0) + geom_vline(xintercept = 0 )
# p = p + ylab("Phenorice Rice Area\n % difference with 2003 values")
# p = p + xlab("Official Rice Area\n % difference with 2003 values")
# p = p + geom_text(aes(x = -65, y = 130, label = lm_eqn(lm(MOD_Summer ~ Stat_Summer,areadata))), parse = TRUE, size = 7)
# p_dry= p + theme(axis.title = element_text(size = 20),
#                  axis.text = element_text(size = 20),
#                  plot.title = element_text(size = 20))
#
# grid.arrange(p_wet, p_dry, ncol = 2)
#
# p = p + geom_point(aes(y = MOD_total, x = Stat_Total), color = 'orange')
# p = p + geom_text(aes(y = MOD_total, x = Stat_Total, label = Year), hjust=0, vjust=1.1)
#
#
#
# lm_eqn = function(m) {
#
#   l <- list(a = format(coef(m)[1], digits = 2),
#             b = format(abs(coef(m)[2]), digits = 2),
#             r2 = format(summary(m)$r.squared, digits = 3));
#
#   if (coef(m)[2] >= 0)  {
#     eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
#   } else {
#     eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
#   }
#
#   as.character(as.expression(eq));
# }
#
