# Catchments
Scripts used to obtain catchment attributes from different dataset.  

**1_Get_all_catchments**
This script is used as a postprocessor of catchments obtained by using New-England Method. All catchments obtained separately are compiled into one database. 
  New England method was used as the DEM resolution and local water body data of higher resolution were available.  
  Compared to the CAMELS dataset, the catchment area obtained here were much closer to the catchment area reported by local authorities.

**2_Catchment_LULC_Soil_composition**
This script gets % of different classes of landuse land composition and soil components for each catchment from CORINE and HWSD database respectively.

**4_lfp_tc_merit_hydro**
This script gets longest flow path and time of concentration for catchments from merit hydro. 
   Longest flow path = Polyline(MAX(Upstream Raster + Downstream Raster))
   The above method creates polyline with branches i.e. longest flow path with branches which is not hydrologically plausible. 
     Such artefacts are removed by using a depth first search.  

**5_lfp_tc_new_eng**
This script gets longest flow path and time of concentration for catchments where local DEM manipulation was done. 
  A separate script is created for this, as the MERIT Hydro provided flow direction raster as well.

**6_HWSD_to_polygon.R**
R script used to convert HWSD data (raster + SQL database) into polygons. 
  https://files.isric.org/public/documents/R_HWSD2.pdf was taken as reference.

**7_USDA_Classification_for_catchments.R**
After 6 and 2, the catchment soil composition for topsoil and bottom soil are obtained,
  these were converted into USDA classes.
