it_shape_file_16 = 'Z:/Validation/2016/IT/Accessory/Static_info_2016_sinu.shp'
it_shape_file_15 = 'Z:/Validation/2016/IT/Accessory/Static_info_2015_sinu.shp'
it_shape_file_14 = 'Z:/Validation/2016/IT/Accessory/Static_info_2014_sinu.shp'

it = openshape(it_shape_file_14)
names(it)[9] = "Sowing DOY"
it$`Sowing DOY` = as.numeric(as.character(it$`Sowing DOY`))
mapview(it,zcol = "Sowing DOY", legend = TRUE , 
        color = brewer.pal(8,"RdYlGn"), fill = TRUE ,
        alpha.regions = 0.7, layer.name = "Italy-2014")

es_shape_file_16 = 'Z:/Validation/2016/ES/Accessory/Static_info_2016_sinu.shp'
es_shape_file_15 = 'Z:/Validation/2016/ES/Accessory/Static_info_2015_sinu.shp'
es_shape_file_14 = 'Z:/Validation/2016/ES/Accessory/Static_info_2014_sinu.shp'

es_shape_file_16 = 'Z:/Validation/2016/ES/Accessory/Static_info_2016_sinu.shp'
es_shape_file_15 = 'Z:/Validation/2016/ES/Accessory/Static_info_2015_sinu.shp'
es_shape_file_14 = 'Z:/Validation/2016/ES/Accessory/Static_info_2014_sinu.shp'

es = openshape(gr_shape_file_14)
names(es)[9] = "Sowing DOY"
es$`Sowing DOY` = as.numeric(as.character(es$`Sowing DOY`))
mapview(es,zcol = "Sowing DOY", legend = TRUE , 
        color = brewer.pal(8,"RdYlGn"), fill = TRUE ,
        alpha.regions = 0.7, layer.name = "Spain-2014")

gr_shape_file_16 = 'Z:/Validation/2016/GR/Accessory/Static_info_2016_sinu.shp'
gr_shape_file_15 = 'Z:/Validation/2016/GR/Accessory/Static_info_2015_sinu.shp'
gr_shape_file_14 = 'Z:/Validation/2016/GR/Accessory/Static_info_2014_sinu.shp'

gr = openshape(gr_shape_file_16)
names(gr)[10] = "Sowing DOY"
gr$`Sowing DOY` = as.numeric(as.character(gr$`Sowing DOY`))
mapview(gr,zcol = "Sowing DOY", legend = TRUE , 
        color = brewer.pal(8,"RdYlGn"), fill = TRUE ,
        alpha.regions = 0.7, layer.name = "Greece-2014")
