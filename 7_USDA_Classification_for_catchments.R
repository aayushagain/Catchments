require(sf)
require(terra)


#1. CREATE USDA SOIL TRIANGLE ACCORDING TO HWSD2 

#before soil data is obtained a soil classification triangle for hwsd2 is created
##getting soil texture for each layer 
require(soiltexture)
TT.plot( class.sys = "USDA.TT" )
#plot usda as well as de.bk classification triangles
usda <- TT.get('USDA.TT')
for( i in 1:length(usda) ) #this line imitates the structure of usda
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
usda$tt.polygons$Cl$points <- c(2,6,5,1,27) #changes coordinantes of clay acc to hwsd2
usda$tt.polygons[['ClH']] <- list(name = 'heavy clay', points = c(24,2,27)) #adds new coordinates for clay heavy
#the numbering classification based on hwsd2 documentation
usda.hwsd.class <- c(3,2,8,5,4,10,9,7,11,6,12,13,1)
for (i in seq_along(usda$tt.polygons)) {
  usda$tt.polygons[[i]]$HWSD_USDA_CLASS <- usda.hwsd.class[i]
}
TT.add('HWSD2_USDA.TT' = usda) #adding for when setting for first time
TT.set('HWSD2_USDA.TT' = usda) #setting for updating after adding once
TT.plot( class.sys = "HWSD2_USDA.TT", main = 'HWSD2_USDA')

#soil dict to use in the hwsd functions
soil_dict <- c( #this dict created using chatgpt inputing the 
  Cl = 3, SiCl = 2, SaCl = 8, ClLo = 5, SiClLo = 4, SaClLo = 10,
  Lo = 9, SiLo = 7, SaLo = 11, Si = 6, LoSa = 12, Sa = 13, ClH = 1
)
##inputs to ChatGPT to get the soil dict
#soil_class <- names(usda$tt.polygons)
#hwsd2_id <- c(3, 2, 8, 5, 4, 10, 9, 7, 11, 6, 12, 13, 1)



#2. READ THE CATCHMENT FEATURE CLASS 
  #classification validation (visual) [https://www.arcgis.com/apps/mapviewer/index.html?layers=aa9a3a2dc6924f46adc5a999787f7961]
catchments <- st_read("C:/Users/Adhikari/Desktop/Catchment_SoilMap/lsbav_all.shp") ####change this path 
View(catchments)

catchments$top_usda <- NA
catchments$top_usda_code <- NA
catchments$bot_usda <- NA
catchments$bot_usda_code <- NA
for (i in 1:nrow(catchments)) {
  # Create texture data for current polygon
  texture_data_top <- data.frame(
    CLAY = catchments$clay_top[i],
    SILT = catchments$silt_top[i],
    SAND = catchments$sand_top[i]
  )
  # Classify soil texture
  classification_top <- TT.points.in.classes(
    tri.data = texture_data_top,
    class.sys = "HWSD2_USDA.TT",
    PiC.type = "t"
  )
  print(classification_top)
  # Store results in the catchment table
  catchments$top_usda[i] <- classification_top
  catchments$top_usda_code[i] <- soil_dict[[classification_top]]
  
  # Create texture data for current polygon
  texture_data_bot <- data.frame(
    CLAY = catchments$clay_bot[i],
    SILT = catchments$silt_bot[i],
    SAND = catchments$sand_bot[i]
  )
  # Classify soil texture
  classification_bot <- TT.points.in.classes(
    tri.data = texture_data_bot,
    class.sys = "HWSD2_USDA.TT",
    PiC.type = "t"
  )
  print(classification_bot)
  # Store results in the catchment table
  catchments$bot_usda[i] <- classification_bot
  catchments$bot_usda_code[i] <- soil_dict[[classification_bot]]
  
}

#3. Write the catchments. 
st_write(catchments, "C:/Users/Adhikari/Desktop/bavaria/cats_ls/lsbav.shp") ####change this path. field names will be drastically changed. rename fields.

#4. join field input = catchments, join table = catchments_with_HWSD_class.shp



