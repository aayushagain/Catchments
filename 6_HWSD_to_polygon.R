require(sf)
require(terra)

#before soil data is obtained a soil classification triangle for hwsd2 is created
##getting soil texture for each layer 
require(soiltexture)
TT.plot( class.sys = "USDA.TT" )
                                                                                #plot usda as well as de.bk classification triangles
usda <- TT.get('USDA.TT')
for( i in 1:length(usda) )                                                      #this line imitates the structure of usda
{ #
  print(
    paste(
      names( usda )[i],
      class( usda[[i]] ),
      sep = ": "
    ) #
  ) #
} #

#this library doesn't have heavy clay, so add that first
usda.here <-usda$tt.points
usda.clay_sep <- c(0.600, 0.000, 0.400)
usda$tt.points <- rbind(usda.here, usda.clay_sep) 
usda$tt.polygons
usda$tt.polygons$Cl$points <- c(2,6,5,1,27)                                     #changes coordinantes of clay acc to hwsd2
usda$tt.polygons[['ClH']] <- list(name = 'heavy clay', points = c(24,2,27))     #adds new coordinates for clay heavy

usda.hwsd.class <- c(3,2,8,5,4,10,9,7,11,6,12,13,1)                             #the numbering classification based on hwsd2 documentation
for (i in seq_along(usda$tt.polygons)) {
  usda$tt.polygons[[i]]$HWSD_USDA_CLASS <- usda.hwsd.class[i]
}
TT.add('HWSD2_USDA.TT' = usda)                                                  #adding for when setting for first time
TT.set('HWSD2_USDA.TT' = usda)                                                  #setting for updating after adding once
TT.plot( class.sys = "HWSD2_USDA.TT", main = 'HWSD2_USDA')

                                                                                #soil dict to use in the hwsd functions
soil_dict <- c(                                                                 #this dict created using chatgpt inputing the 
  Cl = 3, SiCl = 2, SaCl = 8, ClLo = 5, SiClLo = 4, SaClLo = 10,
  Lo = 9, SiLo = 7, SaLo = 11, Si = 6, LoSa = 12, Sa = 13, ClH = 1
)
  ##inputs to ChatGPT to get the soil dict
  #soil_class <- names(usda$tt.polygons)
  #hwsd2_id <- c(3, 2, 8, 5, 4, 10, 9, 7, 11, 6, 12, 13, 1)
















## converting hwsd database into polygons with all required attributes
hwsd <- rast('C:/Users/Adhikari/Desktop/soil_data/HWSD2.bil')                   ####change this file path
print(hwsd)
cat_etrs <- st_read('C:/Users/Adhikari/Desktop/cat/catchments_merged.shp')      ####change this file path

#this  projects the catchments into wgs, counterintuitive because it makes the projection worse, but 
#less computationally intensive and uncertainty from reprojecting the raster
cat <- st_transform(cat_etrs, crs = 4326)
cat.bound <- st_as_sf(cat, ID = "catchments",crs = "+proj=utm +zone=32 +datum=WGS84")
ext(cat.bound)
cat.poly <- as.polygons(vect(cat.bound))

#crop the raster
hwsd.cat.bbox <- crop(hwsd,ext(cat.poly))

#mask the raster with catchment polygon
hwsd.cat <- mask(hwsd.cat.bbox, cat.poly)
(cat.id <- sort(unique(values(hwsd.cat))))
#project the polygon into ETRG utm 32
epsg <- 25832
proj <- "EPSG:25832"
proj <- "+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs"
hwsd.cat.poly.utm <- project(cat.poly, "EPSG:25832")
hwsd.cat.sf.utm <- st_as_sf(hwsd.cat.poly.utm)

#sqlite database downloaded from: https://data.isric.org/geonetwork/srv/eng/catalog.search#/metadata/54aebf11-ec73-4ff8-bf6c-ecff4b0725ea
require('RSQLite')

#creating connection with the sqlite dataframe
m <- dbDriver('SQLite')

#creates connection with the sqlite dataset
con <- dbConnect(m, dbname = 'C:/Users/Adhikari/Desktop/soil_data/HWSD2.sqlite') ####change this file path

dbListTables(con)
print(data.frame(
  name = dbGetQuery(con, "pragma table_info(HWSD2_SMU)")$name,
  type = dbGetQuery(con, "pragma table_info(HWSD2_SMU)")$type)) #displays name and type from hwsd2_smu table?

#display metadata
print(dbGetQuery(con, "select * from HWSD2_SMU_METADATA"), width=100) #connects and prints from hwsd2_smu_metadata table

#creating connection with the domain 
dbGetQuery(con, "select * from D_TEXTURE_USDA") #creates connection with the d_texture_usda table 
dbGetQuery(con, "select count(*) as grid_total from HWSD2_SMU") #displays total number of rows in hwsd2_smu
#this reads the table D_WRB4: WRB4 as string into memory 


#every map unit doesn't have only one component
dbExecute(con, "drop table if exists WINDOW_NL")
#writes a column window_nl containing all smu ids of the catchment
dbWriteTable(con, name="WINDOW_NL",
             value=data.frame(smu_id=unique(cat.id)),
             overwrite=TRUE)
str(d.wrb4 <- dbGetQuery(con, "select * from D_WRB4")) #small table 191 rows, 3 fields
#joins smu properties from hwsd2_smu table for the catchment window
records.nl <-dbGetQuery(con, "select T.* from HWSD2_SMU as T 
                        join WINDOW_NL as U 
                        on T.HWSD2_SMU_ID=U.SMU_ID 
                        order by HWSD2_SMU_ID")

print(dim(records.nl))
print(unique(records.nl$HWSD2_SMU_ID))
print(unique(records.nl$SHARE))
length(ix <- which(records.nl$SHARE != 100))

#shows dataset from dominant soil groups only
#hwsd2_smu table has only the dominant soil 
(records.nl[ix,c("ID","HWSD2_SMU_ID", "SHARE")])


#hwsd2_layers has all soil proportions
#gets different sequences 
comp.9355 <- dbGetQuery(con,
                        "select T.HWSD2_SMU_ID, U.SEQUENCE, U.SHARE, U.WRB_PHASES from HWSD2_SMU as T
                        join HWSD2_LAYERS as U on T.HWSD2_SMU_ID = U.HWSD2_SMU_ID
                        where ((T.HWSD2_SMU_ID)=10216 and (U.LAYER='D1'))
                        order by T.HWSD2_SMU_ID, U.SEQUENCE")

print(cbind(comp.9355, WRB_NAME=d.wrb4[match(comp.9355$WRB_PHASES,
                                             d.wrb4$CODE), "VALUE"]))

#gets properties of all sequences at different depths
layers.comp.9355 <- dbGetQuery(con, "select T.HWSD2_SMU_ID,U.SEQUENCE, U.SHARE, U.WRB_PHASES, U.TEXTURE_USDA ,U.TOPDEP,U.BOTDEP,U.COARSE, U.SAND, U.SILT, U.CLAY,
                               U.BULK, U.ORG_CARBON, U.PH_WATER 
                               from HWSD2_SMU as T
                               join HWSD2_LAYERS as U 
                               on T.HWSD2_SMU_ID = U.HWSD2_SMU_ID
                               where ((T.HWSD2_SMU_ID)=10216)
                               order by T.HWSD2_SMU_ID, U.SEQUENCE, U.LAYER")

print(layers.comp.9355, width = 100)

#######
#Top soil
#gets all mapunits as string
#all operations in the smu table; only dominant soil value taken
idString <- toString(sprintf("'%s'", cat.id))
sql.stmt <- "select * from HWSD2_SMU  where HWSD2_SMU.HWSD2_SMU_ID in (%s) order by HWSD2_SMU_ID"
sql.stmt <- sprintf(sql.stmt, idString)
nl.smu <- dbGetQuery(con, sql.stmt)
nl.smu$HWSD2_SMU_ID <- as.factor(nl.smu$HWSD2_SMU_ID)
print(dim(nl.smu))
print(names(nl.smu))

#operations in the layers table; all soil values taken
sql.stmt <- "select * from HWSD2_LAYERS where HWSD2_LAYERS.HWSD2_SMU_ID in (%s) order by HWSD2_SMU_ID"
sql.stmt <- "select * from HWSD2_LAYERS where HWSD2_LAYERS.HWSD2_SMU_ID in (%s) order by HWSD2_SMU_ID"
sql.stmt <- sprintf(sql.stmt, idString)
nl.layers <- dbGetQuery(con, sql.stmt)
nl.layers$HWSD2_SMU_ID <- as.factor(nl.layers$HWSD2_SMU_ID) #format conversion of the HWSD2_SMU_ID
print(dim(nl.layers))
print(names(nl.layers))




#replacing missing values with NA
#for columns with no texture_sda, sand silt clay % is negative
length(v <- which(is.na(nl.layers$TEXTURE_USDA))) 
nl.layers[v,c("BULK", "SAND", "SILT", "CLAY", "COARSE")] <- NA #where texture = NA no data for clay, sand, and silt exist; if one exists other should too; this logic used for averaging


##same for all depth layer, don't copy this, copy one beneath, without comments
#this returns the average soil type % for every depth segment for all map units, the averaging is done later from shapefile
nl.layers.topsoil <- nl.layers[nl.layers$TOPDEP == 0, #make change to this parameter to get values for different depth
                               c('ID','HWSD2_SMU_ID','SEQUENCE', 'SHARE', 'LAYER', 'TOPDEP', 'BOTDEP','TEXTURE_USDA','COARSE', "BULK", "SAND", "SILT", "CLAY", "COARSE")]
nl.layers.topsoil.s <- split(nl.layers.topsoil, nl.layers.topsoil$HWSD2_SMU_ID)
  #get clay, sand, and silt for the topsoil
soil_1 <- lapply(nl.layers.topsoil.s, function(x) { #sums the area with values, if na present in sequence, ignores that specific sequence only
  non_zero <- !is.na(x$TEXTURE_USDA)
  if (all(is.na(x$TEXTURE_USDA))) { #if not np.all(np_array) #if none of the sequence of the smu have any data, this returns NA; this is to differentiate 0 from na
    clay_value <- NA
    sand_value <- NA
    silt_value <- NA
    area_share <- NA
    usda_class <- NA
  }
  else{
    area_share <- sum(x$SHARE[non_zero]) #, na.rm = TRUE)  #, na.rm = TRUE) with above non_zero list these should be redundant
    clay_value <- sum(x$CLAY[non_zero] * x$SHARE[non_zero]/area_share) 
    sand_value <- sum(x$SAND[non_zero] * x$SHARE[non_zero]/area_share)
    silt_value <- sum(x$SILT[non_zero] * x$SHARE[non_zero]/area_share)
    x <- data.frame(clay_value,silt_value,sand_value)
    colnames(x) <- c('CLAY', 'SILT', 'SAND')
    sl_code <- TT.points.in.classes(tri.data = x,class.sys = "HWSD2_USDA.TT", PiC.type = "t")
    usda_class <- soil_dict[[sl_code]] 
    print(sl_code) 
    print(usda_class)
  }
  return_list <- (list('clay_perc' = clay_value,'sand_perc' = sand_value, 'silt_perc' = silt_value, 'area_perc' = area_share, 'USDA_class' = usda_class))
  return(return_list)
}
)
nl.smu$clay_0_20 <- lapply(soil_1, `[[`, "clay_perc")
nl.smu$sand_0_20 <- lapply(soil_1, `[[`, "sand_perc")
nl.smu$silt_0_20 <- lapply(soil_1, `[[`, "silt_perc")
nl.smu$a_0_20 <- lapply(soil_1, `[[`, "area_perc")
nl.smu$usda_0_20 <- lapply(soil_1, `[[`, "USDA_class")


##copy from here for each soil depth layer
nl.layers.topsoil <- nl.layers[nl.layers$TOPDEP == 20, #change this value based on layer depth
                               c('ID','HWSD2_SMU_ID','SEQUENCE', 'SHARE', 'LAYER', 'TOPDEP', 'BOTDEP','TEXTURE_USDA','COARSE', "BULK", "SAND", "SILT", "CLAY", "COARSE")]
nl.layers.topsoil.s <- split(nl.layers.topsoil, nl.layers.topsoil$HWSD2_SMU_ID)
soil_1 <- lapply(nl.layers.topsoil.s, function(x) {
  non_zero <- !is.na(x$TEXTURE_USDA)
  if (all(is.na(x$TEXTURE_USDA))) {
    clay_value <- NA
    sand_value <- NA
    silt_value <- NA
    area_share <- NA
    usda_class <- NA
  }
  else{
    area_share <- sum(x$SHARE[non_zero])
    clay_value <- sum(x$CLAY[non_zero] * x$SHARE[non_zero]/area_share) 
    sand_value <- sum(x$SAND[non_zero] * x$SHARE[non_zero]/area_share)
    silt_value <- sum(x$SILT[non_zero] * x$SHARE[non_zero]/area_share)
    x <- data.frame(clay_value,silt_value,sand_value)
    colnames(x) <- c('CLAY', 'SILT', 'SAND')
    sl_code <- TT.points.in.classes(tri.data = x,class.sys = "HWSD2_USDA.TT", PiC.type = "t")
    usda_class <- soil_dict[[sl_code]]
    print(sl_code) 
    print(usda_class)
  }
  return_list <- (list('clay_perc' = clay_value,'sand_perc' = sand_value, 'silt_perc' = silt_value, 'area_perc' = area_share, 'USDA_class' = usda_class))
  return(return_list)
}
)

nl.smu$clay_20_40 <- lapply(soil_1, `[[`, "clay_perc") #change variable name in nl.smu$clay_new_depth
nl.smu$sand_20_40 <- lapply(soil_1, `[[`, "sand_perc")
nl.smu$silt_20_40 <- lapply(soil_1, `[[`, "silt_perc")
nl.smu$a_20_40 <- lapply(soil_1, `[[`, "area_perc")
nl.smu$usda_20_40 <- lapply(soil_1, `[[`, "USDA_class")
##copy end here


##copy from here for each soil depth layer
nl.layers.topsoil <- nl.layers[nl.layers$TOPDEP == 40, #change this value based on layer depth
                               c('ID','HWSD2_SMU_ID','SEQUENCE', 'SHARE', 'LAYER', 'TOPDEP', 'BOTDEP','TEXTURE_USDA','COARSE', "BULK", "SAND", "SILT", "CLAY", "COARSE")]
nl.layers.topsoil.s <- split(nl.layers.topsoil, nl.layers.topsoil$HWSD2_SMU_ID)
soil_1 <- lapply(nl.layers.topsoil.s, function(x) {
  non_zero <- !is.na(x$TEXTURE_USDA)
  if (all(is.na(x$TEXTURE_USDA))) {
    clay_value <- NA
    sand_value <- NA
    silt_value <- NA
    area_share <- NA
    usda_class <- NA
  }
  else{
    print(x$SHARE[non_zero])
    area_share <- sum(x$SHARE[non_zero])
    clay_value <- sum(x$CLAY[non_zero] * x$SHARE[non_zero]/area_share) 
    sand_value <- sum(x$SAND[non_zero] * x$SHARE[non_zero]/area_share)
    silt_value <- sum(x$SILT[non_zero] * x$SHARE[non_zero]/area_share)
    x <- data.frame(clay_value,silt_value,sand_value)
    colnames(x) <- c('CLAY', 'SILT', 'SAND')
    sl_code <- TT.points.in.classes(tri.data = x,class.sys = "HWSD2_USDA.TT", PiC.type = "t")
    usda_class <- soil_dict[[sl_code]]
  }
  return_list <- (list('clay_perc' = clay_value,'sand_perc' = sand_value, 'silt_perc' = silt_value, 'area_perc' = area_share, 'USDA_class' = usda_class))
  return(return_list)
}
)

nl.smu$clay_40_60 <- lapply(soil_1, `[[`, "clay_perc") #change variable name in nl.smu$clay_new_depth
nl.smu$sand_40_60 <- lapply(soil_1, `[[`, "sand_perc")
nl.smu$silt_40_60 <- lapply(soil_1, `[[`, "silt_perc")
nl.smu$a_40_60 <- lapply(soil_1, `[[`, "area_perc")
nl.smu$usda_40_60 <- lapply(soil_1, `[[`, "USDA_class")
##copy end here


##copy from here for each soil depth layer
nl.layers.topsoil <- nl.layers[nl.layers$TOPDEP == 60, #change this value based on layer depth
                               c('ID','HWSD2_SMU_ID','SEQUENCE', 'SHARE', 'LAYER', 'TOPDEP', 'BOTDEP','TEXTURE_USDA','COARSE', "BULK", "SAND", "SILT", "CLAY", "COARSE")]
nl.layers.topsoil.s <- split(nl.layers.topsoil, nl.layers.topsoil$HWSD2_SMU_ID)
soil_1 <- lapply(nl.layers.topsoil.s, function(x) {
  non_zero <- !is.na(x$TEXTURE_USDA)
  if (all(is.na(x$TEXTURE_USDA))) {
    clay_value <- NA
    sand_value <- NA
    silt_value <- NA
    area_share <- NA
    usda_class <- NA
  }
  else{
    print(x$SHARE[non_zero])
    area_share <- sum(x$SHARE[non_zero])
    clay_value <- sum(x$CLAY[non_zero] * x$SHARE[non_zero]/area_share) 
    sand_value <- sum(x$SAND[non_zero] * x$SHARE[non_zero]/area_share)
    silt_value <- sum(x$SILT[non_zero] * x$SHARE[non_zero]/area_share)
    x <- data.frame(clay_value,silt_value,sand_value)
    colnames(x) <- c('CLAY', 'SILT', 'SAND')
    sl_code <- TT.points.in.classes(tri.data = x,class.sys = "HWSD2_USDA.TT", PiC.type = "t")
    usda_class <- soil_dict[[sl_code]]
  }
  return_list <- (list('clay_perc' = clay_value,'sand_perc' = sand_value, 'silt_perc' = silt_value, 'area_perc' = area_share, 'USDA_class' = usda_class))
  return(return_list)
}
)

nl.smu$clay_60_80 <- lapply(soil_1, `[[`, "clay_perc") #change variable name in nl.smu$clay_new_depth
nl.smu$sand_60_80 <- lapply(soil_1, `[[`, "sand_perc")
nl.smu$silt_60_80 <- lapply(soil_1, `[[`, "silt_perc")
nl.smu$a_60_80 <- lapply(soil_1, `[[`, "area_perc")
nl.smu$usda_60_80 <- lapply(soil_1, `[[`, "USDA_class")
##copy end here


##copy from here for each soil depth layer
nl.layers.topsoil <- nl.layers[nl.layers$TOPDEP == 80, #change this value based on layer depth
                               c('ID','HWSD2_SMU_ID','SEQUENCE', 'SHARE', 'LAYER', 'TOPDEP', 'BOTDEP','TEXTURE_USDA','COARSE', "BULK", "SAND", "SILT", "CLAY", "COARSE")]
nl.layers.topsoil.s <- split(nl.layers.topsoil, nl.layers.topsoil$HWSD2_SMU_ID)
soil_1 <- lapply(nl.layers.topsoil.s, function(x) {
  non_zero <- !is.na(x$TEXTURE_USDA)
  if (all(is.na(x$TEXTURE_USDA))) {
    clay_value <- NA
    sand_value <- NA
    silt_value <- NA
    area_share <- NA
    usda_class <- NA
  }
  else{
    print(x$SHARE[non_zero])
    area_share <- sum(x$SHARE[non_zero])
    clay_value <- sum(x$CLAY[non_zero] * x$SHARE[non_zero]/area_share) 
    sand_value <- sum(x$SAND[non_zero] * x$SHARE[non_zero]/area_share)
    silt_value <- sum(x$SILT[non_zero] * x$SHARE[non_zero]/area_share)
    x <- data.frame(clay_value,silt_value,sand_value)
    colnames(x) <- c('CLAY', 'SILT', 'SAND')
    sl_code <- TT.points.in.classes(tri.data = x,class.sys = "HWSD2_USDA.TT", PiC.type = "t")
    usda_class <- soil_dict[[sl_code]]
    
  }
  return_list <- (list('clay_perc' = clay_value,'sand_perc' = sand_value, 'silt_perc' = silt_value, 'area_perc' = area_share, 'USDA_class' = usda_class))
  return(return_list)
}
)

nl.smu$clay_80_100 <- lapply(soil_1, `[[`, "clay_perc") #change variable name in nl.smu$clay_new_depth
nl.smu$sand_80_100 <- lapply(soil_1, `[[`, "sand_perc")
nl.smu$silt_80_100 <- lapply(soil_1, `[[`, "silt_perc")
nl.smu$a_80_100 <- lapply(soil_1, `[[`, "area_perc")
nl.smu$usda_80_100 <- lapply(soil_1, `[[`, "USDA_class")
##copy end here



##ggplot
require(ggplot2)
hwsd.nl.poly <- as.polygons(hwsd.cat, dissolve = TRUE, values = TRUE)
values(hwsd.nl.poly) <- nl.smu
plot(hwsd.nl.poly)
g.drivers <- gdal(drivers = TRUE)
head(v.drivers <- g.drivers[g.drivers$type == "vector", ], 12)



####change file paths here 

if (dir.exists("C:/Users/Adhikari/Desktop/soil_data/HWSD_cat")) unlink("C:/Users/Adhikari/Desktop/soil_data", 
                                                                       recursive = TRUE)
#this writes the shp in wgs84, project this before using it
terra::writeVector(hwsd.nl.poly, filename = "C:/Users/Adhikari/Desktop/soil_data/HWSD_Merged_Smoothened_Catchment_all_smu.shp",
                   filetype = 'ESRI Shapefile', overwrite = TRUE)

dbDisconnect(con)
