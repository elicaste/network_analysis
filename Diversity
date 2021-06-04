require(bigreadr)
require(tidyverse)
require(gridExtra)
require(countrycode)
library(data.table)
library(sf)
library(rnaturalearth)
library(raster)
library(RStoolbox)
library(ade4)
library(adiv)
library(rasterVis)
library(viridis)
library(scico)
library(phyloregion)
library(vegan)
library(scales)
library(CoordinateCleaner)


#------------------ Amphibia --------------------------------
#-------------------- 10deg ---------------------------------
amph<-read.csv('gbif_november_amphibia_clean.csv')
# barplot(prop.table(table(amph$basisOfRecord)))
# amph$countryCode<-  countrycode(amph$countryCode, origin =  'iso2c', destination = 'iso3c')

amph %>%
  filter(!is.na(decimalLongitude), !is.na(decimalLatitude)) %>%
  filter(basisOfRecord!='UNKNOWN'& basisOfRecord!='LIVING_SPECIMEN')->amph2

saveRDS(amph2,'AmphDef.rds')

amph<-readRDS('AmphDef.rds')
amph %>%
    dplyr::select(genus,family,order) %>%
    drop_na() %>%
    group_by(genus) %>%
    distinct()%>%
    filter(!is.na(genus)) %>%
    dplyr::select(genus,family,order)->taxAmphGen

write.csv(taxAmpGen,'taxAmphGen.csv'
  
taxAmph<-read.csv('Amphibia/taxAmphGen.csv',sep=';')

taxAmph %>% 
    filter(!is.na(genus))->taxAmph2
 
 
#10degree
world <- ne_countries(scale = "medium", returnclass = "sf")
r<-raster::extent(c(xmin = -180, xmax = 180, ymin = -90,ymax = 90))
p <- as(r, "SpatialPolygons")
m <- sp::SpatialPolygonsDataFrame(p, data.frame(sp = "x"))
m10 <- fishnet(mask = m, res = 10)
plot(m10)
crs(m10) <- crs(world)

amph->amph2
coordinates(amph2)= ~decimalLongitude+decimalLatitude
crs(amph2)=crs(m10)
ov10 <- over(amph2[,3], m10) #genus level
y10 <- as.data.frame(cbind(amph2[,3], ov10))
y10 <- y10[complete.cases(y10),]
spp10 <- data.frame(as.matrix(long2sparse(y10,species = 'genus')))[,-1]

dim(spp10)
dim(taxAmph2)
Gen2drop<-which(!(taxAmph2$genus %in% colnames(spp10)))

dtaxo <- taxa2dist(taxAmph2,varstep = F,labels = taxAmph2[,1])
#raoTax10<-data.frame('Rao'=QE(comm=spp10,dis=dtaxo,scale = F),grids=row.names(raoTax10))
#mpdTax10<-data.frame('MPD'=melodic_mpd(samp=spp10,dis=dtaxo,abundance.weighted = FALSE),grids=row.names(raoTax10))
divTax10<-data.frame('Rao'=QE(comm=spp10,dis=dtaxo,scale = F)$diversity,
                     'MPD'=melodic_mpd(samp=spp10,dis=dtaxo,abundance.weighted = FALSE),
                     'Richness'=specnumber(spp10),
                     grids=row.names(spp10))


mydf10 <- sp::merge(m10, divTax10, by="grids", all=F)


mydf10 %>% 
  st_as_sf->mydf10sf


myTheme<-theme(panel.background= element_rect(color="black", fill="gray90"),
               panel.grid.major = element_blank(),
               plot.title = element_text(size=40,face = 'bold'),
               legend.title=element_text(size=40),
               legend.text = element_text(size=40),
               axis.title.x = element_text(size = 40),
               axis.text.x = element_text(size = 40),
               axis.title.y = element_text(size = 40),
               axis.text.y = element_text(size = 40))

ggplot()+
  geom_sf(data=world[world$continent!='Antarctica',])+
  geom_sf(data=mydf10sf[mydf10sf$Richness>1,],aes(fill=Rao),color=NA) +
  coord_sf(label_axes ='--EN',expand = FALSE,xlim=c(-180,180))+
  scale_fill_viridis(option='magma',direction = 1,alpha = 0.7)+
  labs(title = "Amphibia 10?")+myTheme->rao10

ggplot()+
  geom_sf(data=world[world$continent!='Antarctica',])+
  geom_sf(data=mydf10sf[mydf10sf$Richness>1,],aes(fill=MPD),color=NA) +
  coord_sf(expand = F,xlim=c(-180,180))+
  scale_fill_viridis(option='magma',direction = 1,alpha = 0.7)+
  labs(title = "Amphibia 10?")+myTheme->mpd10;mpd10


ggplot()+
  geom_sf(data=world[world$continent!='Antarctica',])+
  geom_sf(data=mydf10sf[mydf10sf$Richness>1,],aes(fill=log(Richness)),color=NA) +
  coord_sf(expand = F,xlim=c(-180,180))+
  scale_fill_viridis(option='magma',direction = 1,alpha = 0.7)+
  labs(title = "Amphibia 10?",fill='GR (log)')+myTheme->Rich10;Rich10

##################-------------------- 1 degree -----------------------##################
m1 <- fishnet(mask = m, res = 1)

crs(m1) <- crs(world)

ov1 <- over(amph2[,3], m1) #genus level
y1 <- as.data.frame(cbind(amph2[,3], ov1))
y1 <- y1[complete.cases(y1),]
spp1 <- data.frame(as.matrix(long2sparse(y1,species = 'genus')))[,-1]

divTax1<-data.frame('Rao'=QE(comm=spp1,dis=dtaxo,scale = F)$diversity,
                    'MPD'=melodic_mpd(samp=spp1,dis=dtaxo,abundance.weighted = FALSE),
                    'Richness'=specnumber(spp1),
                    grids=row.names(spp1))


mydf1 <- sp::merge(m1, divTax1, by="grids", all=F)
mydf1 %>% 
  st_as_sf->mydf1sf


ggplot()+
  geom_sf(data=world[world$continent!='Antarctica',])+
  geom_sf(data=mydf1sf[mydf1sf$Richness>1,],aes(fill=MPD),color=NA) +
  coord_sf(expand = F,xlim=c(-180,180))+
  scale_fill_viridis(option='magma',direction = 1,alpha = 0.7)+
  labs(title = "Amphibia 1?")+myTheme->mpd1;mpd1

ggplot()+
  geom_sf(data=world[world$continent!='Antarctica',])+
  geom_sf(data=mydf1sf[mydf1sf$Richness>1,],aes(fill=log(Richness)),color=NA) +
  coord_sf(expand = F,xlim=c(-180,180))+
  scale_fill_viridis(option='magma',direction = 1,alpha = 0.7)+
  labs(title = "Amphibia 1?",fill='GR (log)')+myTheme->Rich1;Rich1

ggplot()+
  geom_sf(data=world[world$continent!='Antarctica',])+
  geom_sf(data=mydf1sf[mydf1sf$Richness>1,],aes(fill=Rao),color=NA) +
  coord_sf(expand = F,xlim=c(-180,180))+
  scale_fill_viridis(option='magma',direction = 1,alpha = 0.7)+
  labs(title = "Amphibia 1?")+myTheme->rao1;rao1
