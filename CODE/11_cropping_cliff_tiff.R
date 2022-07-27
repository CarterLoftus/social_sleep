





library(raster)    # For extent(), xmin(), ymax(), et al.
library(gdalUtils) # For gdal_translate()

inFile <- 'DATA/cliff_tiff/ebee_highdam_transparent_mosaic_group1.tif'
outFile <-  'DATA/cliff_tiff/cropped_cliff.tif'


xmin <- 264840.1668309913
xmax <- 264873.4557198801
ymin <- 34020.59151187160
ymax <- 34083.16928964938

ex <- extent( xmin, xmax, ymin, ymax )


gdal_translate(inFile, outFile,
               projwin=c(xmin(ex), ymax(ex), xmax(ex), ymin(ex)))




inFile <- 'DATA/cliff_tiff/ebee_highdam_transparent_mosaic_group1.tif'
outFile <-  'DATA/cliff_tiff/less_cropped_cliff.tif'


xmin <- 264828.1668309913
xmax <- 264873.4557198801
ymin <- 34010.59151187160
ymax <- 34083.16928964938


ex <- extent( xmin, xmax, ymin, ymax )


gdal_translate(inFile, outFile,
               projwin=c(xmin(ex), ymax(ex), xmax(ex), ymin(ex)))






# ### the following method would also work
# 
# 
# library( raster )
# library( sp )
# 
# cliff_raster <- stack( 'DATA/drone_imagery/baboon_cliff/ebee_highdam_transparent_mosaic_group1.tif' )
# 
# proj4string( cliff_raster )
# 
# plotRGB( cliff_raster )
# 
# #cliff_raster <- cliff_raster[[ - nlayers( cliff_raster ) ]]
# 
# #plotRGB( cliff_raster )
# 
# xmin <- 264828.1668309913
# xmax <- 264873.4557198801
# ymin <- 34010.59151187160
# ymax <- 34083.16928964938
# 
# 
# 
# extent_obj <- extent( xmin, xmax, ymin, ymax )
# 
# 
# cropped_cliff <- crop( cliff_raster, extent_obj )
# 
# #plot( cropped_cliff )
# 
# plotRGB( cropped_cliff )
#
# writeRaster( cropped_cliff, 'DATA/drone_imagery/baboon_cliff/cropped_cliff1.tif', format="GTiff", datatype='INT1U', overwrite=TRUE )
#
#
#
#
#
#
