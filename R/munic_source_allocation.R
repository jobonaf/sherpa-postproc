# module load r-rgdal geos netcdf-fortran/4.5.2/gcc

# read potencies (%)
files <- Sys.glob("/lustre/arpa/bonafeg/data/sherpa/source-allocation/*/*/potencies_overview_per_sector.nc")
library(raster)
library(dplyr)
Dat <- NULL
for (i in 1:length(files)) {
	as.data.frame(rasterToPoints(raster(files[i], var="DC_C_alpha_all"))) %>%
	mutate(Origin=strsplit(files[i],"/")[[1]][9],
	       Pollutant=strsplit(files[i],"/")[[1]][8])%>%
	bind_rows(Dat) -> Dat
}

# prepare 
Dat %>% filter(x>=12.03,y>=45.38,x<=14.16,y<=46.87) %>%
rename(Lon=x, Lat=y, Potency=DC_C_alpha_all) -> pDat
pDat$Origin <- factor(pDat$Origin, 
		      levels=c("FVG","Veneto","rest_of_Italy","Austria","Slovenia"),
		      labels=c("Friuli - Venezia Giulia","Veneto","resto d'Italia",
			       "Austria","Slovenia"), 
		      ordered=T)
ll <- pDat %>% distinct(Lon,Lat)
pp <- ll
coordinates(pp) <- ~Lon+Lat
crs(pp) <- "+init=epsg:4326"
pp <- spTransform(pp, CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"))

# read municipalities
library(rgdal)
mm <- readOGR("/lustre/arpa/bonafeg/data/geo/LimitiAmministrativi/Limiti_2018_WGS84_UTM32N/Com01012018_g_WGS84.shp")
mm <- mm[mm@data$COD_REG==6,]
# centroids
library(rgeos)
cc <- gCentroid(mm,byid = T)
cc <- spTransform(cc, CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs"))

# find closest cell for each municipality
d <- gDistance(cc, pp, byid = T)
colnames(d) <- NULL
whichmin.d <- apply(d, 2, which.min)
mDat <- data.frame(mm@data[,c("PRO_COM","COMUNE")], ll[whichmin.d,]) %>%
  rename(Municipality=COMUNE, IstatCode=PRO_COM) %>%
  left_join(pDat) %>%
  droplevels() %>%
  mutate(Potency=round(Potency,1)) %>%
  arrange(Municipality)
library(readr)
write_csv(mDat, "out/SpatialSourceApp_MunicFVG.csv")
