	## EVENTUALMENTE RIADATTARE PER SHERPA

# module load r-rgdal geos netcdf-fortran/4.5.2/gcc

sa_sankey <- function(poll) {

	# legge popolazione su griglia
	library(raster)
	pop <- readRDS("/lustre/arpa/bonafeg/data/geo/Popolazione/Pop_FARMFVG_2km.rds")
	Dat <- as.data.frame(rasterToPoints(pop))

	# legge zone aria
	library(rgeos)
	library(rgdal)
	library(dplyr)
	zz <- readOGR("/lustre/arpa/bonafeg/data/geo/ZoneAria/FriuliVeneziaGiulia_Zone_20120118.shp")
	zn <- levels(zz$ZONE_NAME)
	zz <- spTransform(zz, crs(pop))
	zz <- rasterize(zz,pop,"ZONE_NAME")
	Dat %>% 
	left_join(as.data.frame(rasterToPoints(zz)) %>% 
		  mutate(Zone=zn[layer], layer=NULL)) -> Dat

	# legge delta degli scenari
	nsc <- c( "Agrcltr","CEnergy","FsFuels","Industr","Livstck","Maritim","NIBioms",
		 "NIOther","OthMobl","RTrFrgt","RTrPsng","Solvent" )
	dsc <- paste0("./farm/",nsc,"/diffmap.g1.",poll,".avg.nc")
	dat <- NULL
	for(i in 1:length(dsc)) {
		as.data.frame(rasterToPoints(raster(dsc[i], var=paste0("c_",poll)))) %>% 
		mutate(Sector=nsc[i]) -> dd
		colnames(dd) <- c("x","y","Delta","Sector")
		dat <- bind_rows(dat, dd)
	}
	Dat %>% left_join(dat) -> Dat

#	# legge media del caso base
#	raster(paste0("./farm/base/avgmap.conc.g1.",poll,".avg.nc"), 
#	       var=paste0("c_",poll)) %>%
	# legge media (kriging 2016)
	ff <- Sys.glob(paste0("/lustre/arpa/operative/data/ariareg/databases/airq_reg/1001F0B0D0_2016/kriging_CivilYear/20160101/data/output/",poll,"_2016_CivilYear_Avg_DLGS155_02_2011_ukriging*.asc"))[1]
	raster(ff) %>%
	rasterToPoints(.) %>% as.data.frame() -> avg
	colnames(avg) <- c("x","y","Conc")
	avg$x <- avg$x/1000
	avg$y <- avg$y/1000

	# calcola tre fasce in base ai quantili di concentrazione
	qq <- c(0,0.66,0.9,1)
	avg %>% mutate(Concentration=cut(Conc,quantile(Conc,qq,na.rm=T),
					 labels=c("bassa","media","alta"),
					 ordered_result=T),
		       Conc=NULL) %>%
	right_join(Dat) -> Dat

	# calcolo e sommatoria su zone e fasce dei delta * popolazione
	Dat %>% 
	mutate(Pollutant=ifelse(poll=="PM25","PM2.5",poll)) %>%
	filter(!is.na(Zone)) %>%
	group_by(Concentration,Zone,Sector,Pollutant) %>%
	summarize(Contrib=sum(population*Delta)) %>%
	ungroup() %>%
	mutate(Perc=100*Contrib/sum(Contrib)) -> pDat

	# aggrega settori meno rilevanti
	minp <- 5
	pDat %>%
	mutate(Sector=recode(Sector,
			     Agrcltr="agricoltura",
			     CEnergy="energia",
			     FsFuels="carburanti",
			     Industr="industria",
			     Livstck="allevamenti",
			     Maritim="porti",
			     NIBioms="riscald. legna",
			     NIOther="risc. no legna",
			     OthMobl="altre mobili",
			     RTrFrgt="tr. merci",
			     RTrPsng="tr. passeggeri",
			     Solvent="solventi")) %>%
	group_by(Sector) %>%
	mutate(Sector=ifelse(sum(Perc)>minp,Sector,"[altro]")) %>%
	ungroup() %>%
	group_by(Concentration,Zone,Sector,Pollutant) %>%
	summarize(Perc=sum(Perc)) %>%
	ungroup() -> pDat

	# ordina settori
	pDat %>% group_by(Sector) %>% summarize(Perc=sum(Perc)) %>% arrange(Perc) -> pp
	pDat$Sector <- ordered(pDat$Sector,c("[altro]",setdiff(pp$Sector,"[altro]")))

	# Sankey plot
	library(ggplot2,lib.loc="/u/arpa/bonafeg/R/x86_64-pc-linux-gnu-library/3.5")
	library(ggalluvial)
	library(ggrepel)
	library(forcats)
	library(scales)
	source("/u/arpa/bonafeg/src/rutilante/R/gg_themes.R")
	ggplot(pDat,
	       aes(y = Perc, 
		   axis3 = Concentration,
		   axis2 = fct_reorder(Zone,Perc,sum), 
		   axis1 = Sector)) +
	geom_alluvium(width = 0.4, reverse=F, aes(fill=Concentration)) +
	geom_stratum(width = 0.4, reverse=F, col="grey50") +
	geom_text(stat = "stratum", family="Decima WE", 
		  aes(label = stat(stratum)), reverse=F, check_overlap = F, col="grey20") +
	scale_x_discrete(limits = c("settore\ndi attività","zona","concentrazione"), 
			 expand = c(.05, .05), position="top") +
	scale_fill_manual(values=c(bassa=muted("blue"),media="olivedrab",alta=muted("red"))) +
	labs(title=paste0("attribuzione alle sorgenti - media annua di ",
			  unique(pDat$Pollutant)),
	     subtitle="contributi relativi settoriali, pesati sul FVG con la popolazione",
	     caption=paste0("concentrazione 'bassa': inferiore al ",
			    qq[2]*100,"esimo percentile,\n",
			    "concentrazione 'alta': superiore al ",
			    qq[3]*100,"esimo percentile;\n",
			    "'[altro]': settori che danno un contributo < ",
			    minp,"%, aggregati")) +
	guides(fill = "none") +
	ylab("contributo (%)") +
	theme_fvg() +
	theme(axis.ticks.x=element_blank(),
	      panel.border=element_blank(),
	      panel.grid.major.x = element_blank(), 
	      panel.grid.minor.x = element_blank())-> p
	ggsave_fvg(p,filename=paste0("sankey_",poll,".pdf"),width=8,height=5)

}
sa_sankey("PM10")
sa_sankey("PM25")
sa_sankey("NO2")



