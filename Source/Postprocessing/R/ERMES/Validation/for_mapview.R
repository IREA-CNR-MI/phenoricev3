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

gr_shape_file_16 = '/home/lb/projects/ermes/datasets/rs_products/Phenology/Validation/2016/GR/Accessory/Static_info_2016_sinu.shp'
gr_shape_file_15 = '/home/lb/projects/ermes/datasets/rs_products/Phenology/Validation/2016/GR/Accessory/Static_info_2015_sinu.shp'
gr_shape_file_14 = '/home/lb/projects/ermes/datasets/rs_products/Phenology/Validation/2016/GR/Accessory/Static_info_2014_sinu.shp'

gr = openshape(gr_shape_file_14)
names(gr)[9] = "Sowing DOY"
gr$`Sowing DOY` = as.numeric(as.character(gr$`Sowing DOY`))
mapview(gr,zcol = "Sowing DOY", legend = TRUE , 
        color = brewer.pal(8,"RdYlGn"), fill = TRUE ,
        alpha.regions = 0.7, layer.name = "Greece-2016")

# 2k maps

it_rast_sow = '/home/lb/projects/ermes/datasets/rs_products/Phenology/IT/2016/v1.0/Outputs/ERMES_Grid/TIFFS/2016/MinDoys/IT_Phenology_MinDoys_2016_203.tif'
it_rast_sos = '/home/lb/projects/ermes/datasets/rs_products/Phenology/IT/2016/v1.0/Outputs/ERMES_Grid/TIFFS/2016/SoSDoys/IT_Phenology_SoSDoys_2016_203.tif'
it_rast_flw = '/home/lb/projects/ermes/datasets/rs_products/Phenology/IT/2016/v1.0/Outputs/ERMES_Grid/TIFFS/2016/MaxDoys/IT_Phenology_MaxDoys_2016_203.tif'

it_sow = raster(it_rast_sow)
mapview(it_sow, legend = FALSE, at = c(110,118,126,134,142,150,158,166),
        col.regions= brewer.pal(8,"RdYlGn"), alpha.regions = 0.7, layer.name = "Italy - Sowing Dates")

it_sos = raster(it_rast_sos)
mapview(it_sos, legend = FALSE, at = c(110,118,126,134,142,150,158,166)+15,
        col.regions = brewer.pal(8,"RdYlGn"), alpha.regions = 0.7, layer.name = "Italy - Emergence Dates")

it_flw = raster(it_rast_flw)
mapview(it_flw, legend = FALSE, at = ,
        col.regions= brewer.pal(8,"RdYlGn"), alpha.regions = 0.7, layer.name = "Italy - Flowering Dates")

es_rast_sow = '/home/lb/projects/ermes/datasets/rs_products/Phenology/ES/2016/v1.0/Outputs/ERMES_Grid/TIFFS/2016/MinDoys/ES_Phenology_MinDoys_2016_203.tif'
es_rast_sos = '/home/lb/projects/ermes/datasets/rs_products/Phenology/ES/2016/v1.0/Outputs/ERMES_Grid/TIFFS/2016/SoSDoys/ES_Phenology_SoSDoys_2016_203.tif'
es_rast_flw = '/home/lb/projects/ermes/datasets/rs_products/Phenology/ES/2016/v1.0/Outputs/ERMES_Grid/TIFFS/2016/MaxDoys/ES_Phenology_MaxDoys_2016_203.tif'

es_sow = raster(es_rast_sow)
mapview(es_sow, legend = FALSE, at = c(110,118,126,134,142,150,158,166),
        col.regions= brewer.pal(8,"RdYlGn"), alpha.regions = 0.7, layer.name = "Spain - Sowing Dates")

es_sos = raster(es_rast_sos)
mapview(es_sos, legend = FALSE, at = c(110,118,126,134,142,150,158,166)+15,
        col.regions = brewer.pal(8,"RdYlGn"), alpha.regions = 0.7, layer.name = "Spain - Emergence Dates")

es_flw = raster(es_rast_flw)
mapview(es_flw, legend = FALSE, at = c(110,118,126,134,142,150,158,166)+72,
        col.regions= brewer.pal(8,"RdYlGn"), alpha.regions = 0.7, layer.name = "Spain - Flowering Dates")

gr_rast_sow = '/home/lb/projects/ermes/datasets/rs_products/Phenology/GR/2016/v1.0/Outputs/ERMES_Grid/TIFFS/2016/MinDoys/GR_Phenology_MinDoys_2016_203.tif'
gr_rast_sos = '/home/lb/projects/ermes/datasets/rs_products/Phenology/GR/2016/v1.0/Outputs/ERMES_Grid/TIFFS/2016/SoSDoys/GR_Phenology_SoSDoys_2016_203.tif'
gr_rast_flw = '/home/lb/projects/ermes/datasets/rs_products/Phenology/GR/2016/v1.0/Outputs/ERMES_Grid/TIFFS/2016/MaxDoys/GR_Phenology_MaxDoys_2016_203.tif'

gr_sow = raster(gr_rast_sow)
mapview(gr_sow, legend = FALSE, at = c(110,118,126,134,142,150,158,166),
        col.regions= brewer.pal(8,"RdYlGn"), alpha.regions = 0.7, layer.name = "Greece - Sowing Dates")

gr_sos = raster(gr_rast_sos)
mapview(gr_sos, legend = FALSE, at = c(110,118,126,134,142,150,158,166)+15,
        col.regions = brewer.pal(8,"RdYlGn"), alpha.regions = 0.7, layer.name = "Greece - Emergence Dates")

gr_flw = raster(gr_rast_flw)
mapview(gr_flw, legend = FALSE, at = c(110,118,126,134,142,150,158,166)+72,
        col.regions= brewer.pal(8,"RdYlGn"), alpha.regions = 0.7, layer.name = "Greece - Flowering Dates")
