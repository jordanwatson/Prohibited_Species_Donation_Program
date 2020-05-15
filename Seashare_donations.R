#  Make figures that summarize the donations of PSC from the SeaShare Program
#  Author: Jordan Watson jordan.watson@noaa.gov
#  Creation date: 04/30/2020

library(tidyverse)
library(readxl)
library(rgdal)

OceansBlue1='#0093D0'
WavesTeal1='#1ECAD3'
CrustaceanOrange1='#FF8300'
CoralRed1='#FF4438'
SeagrassGreen1='#93D500'
UrchinPurple1='#7F7FFF'


png("SeaShare_Annual_Donations_type.png",w=6.5,h=4.5,units="in",res=300)
read_excel("SeaShare_PSC_Donations.xlsx",sheet="donations_by_fish") %>% 
  gather(type,pounds,-year) %>% 
  mutate(tonnes=pounds/2205) %>% 
  ggplot(aes(year,tonnes,fill=type)) +
  geom_bar(stat="identity") + 
  theme_bw() +
  scale_fill_manual(values=c(OceansBlue1,CoralRed1)) + 
  theme(legend.position=c(0.9,0.8),
        legend.background = element_blank()) +
  guides(fill=guide_legend("")) +
  scale_x_discrete(labels=c("1994-\n2003","","","2006","","","","2010","","","","2014","","","","2018",""),
                   expand=c(0.05,0.05)) + 
  #scale_x_discrete(labels=c("","","","1996","","","","2000","","","","2004","","","","2008","","","","2012","","","","2016","","",""),
  #                 expand=c(0.025,0.025)) + 
  ylab("PSC Donations (mt)") +
  xlab("Year")
dev.off()




#  List of Alaska towns downloaded from:
#http://www.asgdc.state.ak.us/#14

library(sf)
library(here)
library(ggspatial)
library(rgeos)

options(digits = 3)
theme_set(theme_minimal())

usa <- here("alaska_shp", "cb_2013_us_state_20m.shp") %>%
  st_read() %>% 
  filter(NAME=="Alaska") %>% 
  st_crop(xmin=-170,xmax=-132,ymin=52,ymax=71)

seasharetowns <- read_excel("SeaShare_PSC_Donations.xlsx",sheet="Communities") %>% 
  mutate(banktype=ifelse(Community==NAME,"Hub","Remote")) %>% 
  filter(NAME!="Bethel")
  
townsmain <- here("towns_shapefile", "mv_town_pt.shp") %>%
  st_read() %>% 
  filter(NAME %in% seasharetowns$NAME) 

towns1 <- townsmain %>% 
  filter(NAME %in% seasharetowns$NAME[seasharetowns$Community%in%c("Nome","Kotzebue","Dillingham")]) %>% 
  left_join(seasharetowns)

towns2 <- townsmain %>% 
  filter(!NAME %in% seasharetowns$NAME[seasharetowns$Community%in%c("Nome","Kotzebue","Dillingham")]) %>% 
  left_join(seasharetowns)

#ak_proj <- st_crs("+proj=aea +lat_1=50 +lat_2=80 +lat_0=30 +lon_0=-155 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

newcoast <- st_crop(coastlines_sf,xmin=-170,xmax=-132,ymin=52,ymax=71)

#mylim <- st_sfc(st_point(c(-170,-130)), st_point(c(50, 71)),crs = 3338) %>% 
#  st_transform(3338)

mymap <- usa %>% 
  ggplot() + 
  geom_sf(color=NA,fill="grey10") +
  geom_sf(data=newcoast) +
  geom_sf(data=towns1,aes(color=Community,size=banktype)) +
  geom_sf(data=towns2,size=5,color=UrchinPurple1) +
  coord_sf(crs=st_crs("+proj=aea +lat_1=60 +lat_2=70 +lat_0=52 +lon_0=-145 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")) +

  scale_size_manual(breaks=c("Hub","Remote"),values=c(6,3)) + 
  scale_color_manual(values=c(WavesTeal1,CrustaceanOrange1,SeagrassGreen1)) + 
  theme(legend.position="none") + 
  annotation_scale(location = "bl", width_hint = 0.3)

png("SeaShare_map.png",width=5,height=5,units="in",res=300);mymap;dev.off()
pdf("SeaShare_map.pdf",width=5,height=5);mymap;dev.off()


pdf("SeaShare_inset.pdf",width=5,height=5)
here("alaska_shp", "cb_2013_us_state_20m.shp") %>%
  st_read() %>% 
  st_crop(xmin=-180,xmax=-45,ymin=15,ymax=75) %>% 
  ggplot() + 
  geom_sf(color=NA,fill="grey10",size=0.1) +
  geom_sf(data=st_crop(coastlines_sf[2],xmin=-180,xmax=-45,ymin=15,ymax=75),color="grey10",size=0.1)
dev.off()




#--------------------------------------------------------------------------------------------------
#  Not used
#--------------------------------------------------------------------------------------------------




png("SeaShare_Annual_Donations.png",w=6.5,h=4.5,units="in",res=300)
data <- read_excel("SeaShare_PSC_Donations.xlsx") %>% 
  gather(type,pounds,-Year) %>% 
  mutate(tonnes=pounds/2205) %>% 
  ggplot(aes(factor(Year),tonnes,fill=type)) +
  geom_bar(stat="identity") + 
  theme_bw() +
  scale_fill_manual(values=c(OceansBlue1,CoralRed1)) + 
  theme(legend.position=c(0.15,0.8),
        legend.background = element_blank()) +
  guides(fill=guide_legend("")) +
  scale_x_discrete(labels=c("1993","","","","1997","","","","2001","","","","2005","","","","2009","","","","2013","","","","2017","",""),
                   expand=c(0.025,0.025)) + 
  #scale_x_discrete(labels=c("","","","1996","","","","2000","","","","2004","","","","2008","","","","2012","","","","2016","","",""),
  #                 expand=c(0.025,0.025)) + 
  ylab("PSC Donations (mt)") +
  xlab("Year")
dev.off()


data <- 

png("SeaShare_Annual_Donations_product.png",w=6.5,h=4.5,units="in",res=300)
read_excel("SeaShare_PSC_Donations.xlsx",sheet="donations_by_product") %>% 
  gather(type,pounds,-year) %>% 
  mutate(tonnes=pounds/2205) %>% 
  ggplot(aes(year,tonnes,fill=type)) +
  geom_bar(stat="identity") + 
  theme_bw() +
  scale_fill_manual(values=c(OceansBlue1,WavesTeal1,CrustaceanOrange1,CoralRed1)) + 
  theme(legend.position=c(0.9,0.8),
        legend.background = element_blank()) +
  guides(fill=guide_legend("")) +
  scale_x_discrete(labels=c("1994-\n2003","","","2006","","","","2010","","","","2014","","","","2018",""),
                   expand=c(0.05,0.05)) + 
  #scale_x_discrete(labels=c("","","","1996","","","","2000","","","","2004","","","","2008","","","","2012","","","","2016","","",""),
  #                 expand=c(0.025,0.025)) + 
  ylab("PSC Donations (mt)") +
  xlab("Year")
dev.off()