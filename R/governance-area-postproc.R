dat_dir <- "/lustre/arpa/bonafeg/data/sherpa/governance-area"
polls <- c("PM10","PM2.5")
library(dplyr)
Dat <- NULL
for (poll in polls) {
	points <- dir (paste0(dat_dir,"/",poll))
	for (pp in points) {
		ff <- paste0(dat_dir,"/",poll,"/",pp,"/","radius_result.txt")
	       print(ff)
	       dat <- read.table(ff,header=T, stringsAsFactors=F)
	       Dat <- bind_rows(Dat, data.frame(dat,Point=pp, Pollutant=poll))
	}
}

colnames(Dat)[2] <- "Perc"

# aggregazione
library(forcats)
Dat %>%
mutate(nuts2=nuts_code,
       nuts1=substr(nuts_code,1,3),
       nuts0=substr(nuts_code,1,2)) %>%
group_by(Point,Pollutant,
	 origine=case_when(nuts2=="ITH4" ~ "Friuli Venezia Giulia",
			  nuts2=="ITH3" ~ "Veneto",
			  nuts0=="SI"   ~ "Slovenia",
			  nuts0=="AT"   ~ "Austria",
			  nuts0=="IT"   ~ "resto d'Italia",
			  TRUE          ~ NA_character_)
	 ) %>%
summarize(Perc=sum(Perc)) %>% ungroup() %>%
filter(!is.na(origine)) %>%
mutate(Point=fct_reorder(Point,Perc*as.numeric(origine=="Friuli Venezia Giulia"),sum),
       origine=fct_reorder(origine,Perc,sum),
       label=if_else(origine=="Friuli Venezia Giulia"|Perc>7,
		     paste0(round(Perc),"%"),
		     ""))-> pDat

# plot
source("~/src/rutilante/R/gg_themes.R")
library(ggplot2)
ggplot(pDat, aes(x=Perc, y=Point, fill=origine, label=label))+
geom_col(col="grey30",alpha=0.7)+
geom_text(position = position_stack(vjust = 0.5), family="Decima WE")+
scale_fill_brewer(type="qual", name="origine:")+
facet_grid(.~Pollutant)+
guides(fill = guide_legend(reverse=TRUE))+
xlab("contributo (%)")+
ylab(NULL)+
theme_fvg()+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())->p
ggsave_fvg(p,filename="barplot_SA_spaziale.pdf",height=4.,width=6.5)
