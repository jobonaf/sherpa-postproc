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

# prepare for maps
Dat %>% filter(x>=12.03,y>=45.38,x<=14.16,y<=46.87) %>%
rename(Lon=x, Lat=y, Pot=DC_C_alpha_all) -> pDat
pDat$Origin <- factor(pDat$Origin, 
		      levels=c("FVG","Veneto","rest_of_Italy","Austria","Slovenia"),
		      labels=c("Friuli - Venezia Giulia","Veneto","resto d'Italia",
			       "Austria","Slovenia"), 
		      ordered=T)

# read zones
library(rgdal)
library(rgeos)
zz <- readOGR("/lustre/arpa/bonafeg/data/geo/ZoneAria/FriuliVeneziaGiulia_Zone_20120118.shp")
zz <- spTransform(zz, CRS("+init=epsg:4326"))

# maps
source("/u/arpa/bonafeg/src/rutilante/R/gg_themes.R")
library(ggplot2)
library(RColorBrewer)
bb <- c(0,5,10,20,50,75,100)
ggplot()+
geom_tile(data=pDat, aes(x=Lon, y=Lat, fill=Pot))+
#scale_fill_viridis_c(trans="sqrt",breaks=bb,labels=bb)+
scale_fill_stepsn(trans="sqrt",breaks=bb,
		  colors=rev(brewer.pal(length(bb)-1,"Spectral")))+
labs(fill="efficienza*"~"(%)",
     caption=expression("*)"~eta[c]==Delta(I[c])/Delta(E)))+
#geom_contour(data=pDat, aes(x=Lon, y=Lat, z=Pot),
#	     col="white", breaks=bb, size=0.1)+
geom_path(data=fortify(zz), aes(x=long, y=lat, group=group),
	  col="grey30")+
facet_grid(Pollutant~Origin)+
theme_void() +
theme(
      text = element_text(family = "Decima WE", color = "grey20"),
      strip.text.x = element_text(hjust = 0.5, size=11),
      strip.text.y = element_text(hjust = 0., vjust=1, size=11),
      legend.position="bottom",
      plot.caption=element_text(hjust=1,size=9,colour="grey30"),
      plot.subtitle=element_text(face="italic",size=12,colour="grey40"),
      plot.title=element_text(size=18,face="bold"))-> p
ggsave_fvg(p,filename="map_SA_spaziale.pdf",height=5.5, width=8)
