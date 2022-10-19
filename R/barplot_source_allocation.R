# module load r-rgdal geos netcdf-fortran/4.5.2/gcc

# read municipalities
library(rgdal)
library(rgeos)
readOGR("/lustre/arpa/bonafeg/data/geo/LimitiAmministrativi/Limiti_2015_WGS84_UTM33N/Com2015_WGS84.shp")->pp
pp <- pp[pp$COD_REG==6|pp$COMUNE=="Sappada",]
proj4string(pp) <- CRS("+init=epsg:32633")
pp <- spTransform(pp,CRSobj=CRS("+init=epsg:4326"))
ppCentroids <- gCentroid(pp,byid=TRUE)

# read potencies (%)
files <- Sys.glob("/lustre/arpa/bonafeg/data/sherpa/source-allocation/*/*/potencies_overview_per_sector.nc")
library(raster)
library(dplyr)
Dat <- NULL
for (i in 1:length(files)) {
        data.frame(Municipality=droplevels(pp@data$COMUNE),
		   Pot=extract(raster(files[i],varname="DC_C_alpha_all"), ppCentroids)) %>%
        mutate(Origin=strsplit(files[i],"/")[[1]][9],
               Pollutant=strsplit(files[i],"/")[[1]][8])%>%
        bind_rows(Dat) -> Dat
}
Dat$Origin <- factor(Dat$Origin,
		      levels=c("FVG","Veneto","rest_of_Italy","Austria","Slovenia"),
		      labels=c("Friuli Venezia Giulia","Veneto","resto d'Italia",
			       "Austria","Slovenia"),
		      ordered=T)

# prepare for plot
library(forcats)
Dat %>%
filter(Municipality %in% c("Brugnera",   "Gorizia",    "Monfalcone", "Pordenone",  "Tarvisio",
			   "Tolmezzo",   "Trieste",    "Udine")) %>%
mutate(Municipality=fct_rev(Municipality),
       Origin=fct_reorder(Origin,Pot,sum),
       label=if_else(Origin=="Friuli Venezia Giulia"|Pot>7,
		     paste0(round(Pot),"%"),
		     ""))-> pDat

# plot
source("~/src/rutilante/R/gg_themes.R")
library(ggplot2)
ggplot(pDat, aes(x=Pot, y=Municipality, fill=Origin, label=label))+
geom_col(col="grey30",alpha=0.7)+
geom_text(position = position_stack(vjust = 0.5), family="Decima WE")+
scale_fill_brewer(type="qual", name="origine:")+
facet_wrap("Pollutant",ncol=1)+
guides(fill = guide_legend(reverse=T))+
xlab("efficienza* (%)")+
ylab(NULL)+
labs(caption=expression("*)"~eta[c]==Delta(I[c])/Delta(E)))+
theme_fvg()+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      legend.position="right",
      strip.text.x = element_text(size=12))->p
ggsave_fvg(p,filename="barplot_SA_spaziale.pdf",height=7,width=7)



