rm(list=ls())

library(tidyverse)
library(curl)
library(xml2)
library(rvest)
library(sf)
library(readxl)
library(nngeo)
library(gtools)
library(scales)
library(lubridate)
library(extrafont)
library(ragg)
library(ggtext)
library(ggridges)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"),
          plot.subtitle=element_text(colour="Grey40", hjust=0, vjust=1),
          plot.caption=element_text(colour="Grey40", hjust=1, vjust=1, size=rel(0.8)),
          axis.text=element_text(colour="Grey40"),
          axis.title=element_text(colour="Grey20"),
          legend.text=element_text(colour="Grey40"),
          legend.title=element_text(colour="Grey20"))
}

#Council wards shapefile 2022 boundaries
temp <- tempfile()
temp2 <- tempfile()
source <- "https://t.co/p0jE1JJap8"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

shapefile_wards22 <- st_read(file.path(temp2,"Data/GB/district_borough_unitary_ward_region.shp")) %>% 
  filter(substr(CODE,1,1)=="S")

ggplot(shapefile_wards22, aes(geometry=geometry))+
  geom_sf()+
  theme_void()

#Council wards shapefile 2017 boundaries
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/api/v3/datasets/646b3b925ac047e792a16cdeaab41797_0/downloads/data?format=shp&spatialRefId=27700"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

#shapefile_wards <- st_read(file.path(temp2,"All_Scotland_wards_4th.shp")) 
shapefile_wards17 <- st_read(file.path(temp2,"Wards__December_2019__Boundaries_UK_BFC.shp")) %>% 
  filter(substr(WD19CD,1,1)=="S")

ggplot(shapefile_wards17, aes(geometry=geometry))+
  geom_sf()+
  theme_void()

#Download SIMD data (just use 2020)
temp <- tempfile()
source <- "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/01/scottish-index-of-multiple-deprivation-2020-ranks-and-domain-ranks/documents/scottish-index-of-multiple-deprivation-2020-ranks-and-domain-ranks/scottish-index-of-multiple-deprivation-2020-ranks-and-domain-ranks/govscot%3Adocument/SIMD%2B2020v2%2B-%2Branks.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

SIMDdata <- read_excel(temp, sheet="SIMD 2020v2 ranks") %>% 
  select(Data_Zone, Total_population, SIMD2020v2_Rank) %>% 
  set_names("DataZone", "Pop", "SIMDrank")

#Data zone shapefile
temp <- tempfile()
temp2 <- tempfile()
source <- "https://maps.gov.scot/ATOM/shapefiles/SG_DataZoneBdry_2011.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

shapefile_dz<- st_read(file.path(temp2,"SG_DataZone_Bdry_2011.shp")) 

#Merge in deprivation
shapefile_SIMD <- shapefile_dz %>% 
  left_join(SIMDdata)

#Get centroids of each datazone
DZpoints <- shapefile_SIMD %>% 
  st_centroid()

#Match to shapefiles using datazone centroids
SIMDmapping2017 <- st_join(DZpoints, shapefile_wards17, join = st_within) %>% 
  st_drop_geometry() %>% 
  as.data.frame() %>% 
  group_by(WD19CD) %>% 
  summarise(SIMDrank=weighted.mean(SIMDrank, Pop)) %>% 
  ungroup() %>% 
  mutate(SIMDdecile=quantcut(SIMDrank, q=10, labels=FALSE))

SIMDmapping2022 <- st_join(DZpoints, shapefile_wards22, join = st_within) %>% 
  st_drop_geometry() %>% 
  as.data.frame() %>% 
  group_by(CODE) %>% 
  summarise(SIMDrank=weighted.mean(SIMDrank, Pop)) %>% 
  ungroup() %>% 
  mutate(SIMDdecile=quantcut(SIMDrank, q=10, labels=FALSE))

#2017 results
#From https://www.macs.hw.ac.uk/~denis/stv_elections/SC2017/index.html#councils

#Aberdeen
url <- "https://www.macs.hw.ac.uk/~denis/stv_elections/SC2017/Aberdeen/index.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
Aberdeen17 <- as.data.frame(html_table(temp[1])) %>% 
  set_names(slice(., 1)) %>% 
  slice(-1) %>% 
  mutate(Council="Aberdeen City")

#Aberdeenshire
url <- "https://www.macs.hw.ac.uk/~denis/stv_elections/SC2017/Aberdeenshire/index.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
Aberdeenshire17 <- as.data.frame(html_table(temp[1])) %>% 
  set_names(slice(., 1)) %>% 
  slice(-1) %>% 
  mutate(Council="Aberdeenshire")

#Angus
url <- "https://www.macs.hw.ac.uk/~denis/stv_elections/SC2017/Angus/index.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
Angus17 <- as.data.frame(html_table(temp[1])) %>% 
  set_names(slice(., 1)) %>% 
  slice(-1) %>% 
  mutate(Council="Angus")

#Argyll and Bute
url <- "https://www.macs.hw.ac.uk/~denis/stv_elections/SC2017/Argyll_and_Bute/index.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
ArgyllBute17 <- as.data.frame(html_table(temp[1])) %>% 
  set_names(slice(., 1)) %>% 
  slice(-1) %>% 
  mutate(Council="Argyll and Bute")

#Clackmannanshire
url <- "https://www.macs.hw.ac.uk/~denis/stv_elections/SC2017/Clackmannanshire/index.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
Clackmannanshire17 <- as.data.frame(html_table(temp[1])) %>% 
  set_names(slice(., 1)) %>% 
  slice(-1) %>% 
  mutate(Council="Clackmannanshire")

#Dumfries and Galloway 
url <- "https://www.macs.hw.ac.uk/~denis/stv_elections/SC2017/Dumfries_and_Galloway/index.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
DumfriesandGalloway17 <- as.data.frame(html_table(temp[1])) %>% 
  set_names(slice(., 1)) %>% 
  slice(-1) %>% 
  mutate(Council="Dumfries and Galloway")

#Dundee
url <- "https://www.macs.hw.ac.uk/~denis/stv_elections/SC2017/Dundee/index.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
Dundee17 <- as.data.frame(html_table(temp[1])) %>% 
  set_names(slice(., 1)) %>% 
  slice(-1) %>% 
  mutate(Council="Dundee City")

#East Ayrshire
url <- "https://www.macs.hw.ac.uk/~denis/stv_elections/SC2017/East_Ayrshire/index.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
EastAyrshire17 <- as.data.frame(html_table(temp[1])) %>% 
  set_names(slice(., 1)) %>% 
  slice(-1) %>% 
  mutate(Council="East Ayrshire")

#East Dunbartonshire
url <- "https://www.macs.hw.ac.uk/~denis/stv_elections/SC2017/East_Dunbartonshire/index.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
EastDunbartonshire17 <- as.data.frame(html_table(temp[1])) %>% 
  set_names(slice(., 1)) %>% 
  slice(-1) %>% 
  mutate(Council="East Dunbartonshire")

#East Lothian
url <- "https://www.macs.hw.ac.uk/~denis/stv_elections/SC2017/East_Lothian/index.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
EastLothian17 <- as.data.frame(html_table(temp[1])) %>% 
  set_names(slice(., 1)) %>% 
  slice(-1) %>% 
  mutate(Council="East Lothian")

#East Renfrewshire
url <- "https://www.macs.hw.ac.uk/~denis/stv_elections/SC2017/East_Renfrewshire/index.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
EastRenfrewshire17 <- as.data.frame(html_table(temp[1])) %>% 
  set_names(slice(., 1)) %>% 
  slice(-1) %>% 
  mutate(Council="East Renfrewshire")

#Edinburgh
url <- "https://www.macs.hw.ac.uk/~denis/stv_elections/SC2017/Edinburgh/index.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
Edinburgh17 <- as.data.frame(html_table(temp[1])) %>% 
  set_names(slice(., 1)) %>% 
  slice(-1) %>% 
  mutate(Council="City of Edinburgh")

#Eilean Siar
url <- "https://www.macs.hw.ac.uk/~denis/stv_elections/SC2017/Eilean_Siar/index.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
EileanSiar17 <- as.data.frame(html_table(temp[1])) %>% 
  set_names(slice(., 1)) %>% 
  slice(-1) %>% 
  mutate(Council="Na h-Eileanan an Iar")

#Falkirk
url <- "https://www.macs.hw.ac.uk/~denis/stv_elections/SC2017/Falkirk/index.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
Falkirk17 <- as.data.frame(html_table(temp[1])) %>% 
  set_names(slice(., 1)) %>% 
  slice(-1) %>% 
  mutate(Council="Falkirk")

#Fife
url <- "https://www.macs.hw.ac.uk/~denis/stv_elections/SC2017/Fife/index.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
Fife17 <- as.data.frame(html_table(temp[1])) %>% 
  set_names(slice(., 1)) %>% 
  slice(-1) %>% 
  mutate(Council="Fife")

#Glasgow
url <- "https://www.macs.hw.ac.uk/~denis/stv_elections/SC2017/Glasgow/index.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
Glasgow17 <- as.data.frame(html_table(temp[1])) %>% 
  set_names(slice(., 1)) %>% 
  slice(-1) %>% 
  mutate(Council="Glasgow City")

#Highland
url <- "https://www.macs.hw.ac.uk/~denis/stv_elections/SC2017/Highland/index.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
Highland17 <- as.data.frame(html_table(temp[1])) %>% 
  set_names(slice(., 1)) %>% 
  slice(-1) %>% 
  mutate(Council="Highland")

#Inverclyde
url <- "https://www.macs.hw.ac.uk/~denis/stv_elections/SC2017/Inverclyde/index.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
Inverclyde17 <- as.data.frame(html_table(temp[1])) %>% 
  set_names(slice(., 1)) %>% 
  slice(-1) %>% 
  mutate(Council="Inverclyde")

#Midlothian
url <- "https://www.macs.hw.ac.uk/~denis/stv_elections/SC2017/Midlothian/index.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
Midlothian17 <- as.data.frame(html_table(temp[1])) %>% 
  set_names(slice(., 1)) %>% 
  slice(-1) %>% 
  mutate(Council="Midlothian")

#Moray
url <- "https://www.macs.hw.ac.uk/~denis/stv_elections/SC2017/Moray/index.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
Moray17 <- as.data.frame(html_table(temp[1])) %>% 
  set_names(slice(., 1)) %>% 
  slice(-1) %>% 
  mutate(Council="Moray")

#North Ayrshire
url <- "https://www.macs.hw.ac.uk/~denis/stv_elections/SC2017/North_Ayrshire/index.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
NorthAyrshire17 <- as.data.frame(html_table(temp[1])) %>% 
  set_names(slice(., 1)) %>% 
  slice(-1) %>% 
  mutate(Council="North Ayrshire")

#North Lanarkshire
url <- "https://www.macs.hw.ac.uk/~denis/stv_elections/SC2017/North_Lanarkshire/index.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
NorthLanarkshire17 <- as.data.frame(html_table(temp[1])) %>% 
  set_names(slice(., 1)) %>% 
  slice(-1) %>% 
  mutate(Council="North Lanarkshire")

#Orkney
url <- "https://www.macs.hw.ac.uk/~denis/stv_elections/SC2017/Orkney/index.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
Orkney17 <- as.data.frame(html_table(temp[1])) %>% 
  set_names(slice(., 1)) %>% 
  slice(-1) %>% 
  mutate(Council="Orkney Islands")

#Perth & Kinross
url <- "https://www.macs.hw.ac.uk/~denis/stv_elections/SC2017/Perth_and_Kinross/index.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
PerthandKinross17 <- as.data.frame(html_table(temp[1])) %>% 
  set_names(slice(., 1)) %>% 
  slice(-1) %>% 
  mutate(Council="Perth and Kinross")

#Renfrewshire
url <- "https://www.macs.hw.ac.uk/~denis/stv_elections/SC2017/Renfrewshire/index.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
Renfrewshire17 <- as.data.frame(html_table(temp[1])) %>% 
  set_names(slice(., 1)) %>% 
  slice(-1) %>% 
  mutate(Council="Renfrewshire")

#Borders
url <- "https://www.macs.hw.ac.uk/~denis/stv_elections/SC2017/Scottish_Borders/index.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
Borders17 <- as.data.frame(html_table(temp[1])) %>% 
  set_names(slice(., 1)) %>% 
  slice(-1) %>% 
  mutate(Council="Scottish Borders")

#Shetland
url <- "https://www.macs.hw.ac.uk/~denis/stv_elections/SC2017/Shetland/index.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
Shetland17 <- as.data.frame(html_table(temp[1])) %>% 
  set_names(slice(., 1)) %>% 
  slice(-1) %>% 
  mutate(Council="Shetland Islands")

#South Ayrshire
url <- "https://www.macs.hw.ac.uk/~denis/stv_elections/SC2017/South_Ayrshire/index.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
SouthAyrshire17 <- as.data.frame(html_table(temp[1])) %>% 
  set_names(slice(., 1)) %>% 
  slice(-1) %>% 
  mutate(Council="South Ayrshire")

#South Lanarkshire
url <- "https://www.macs.hw.ac.uk/~denis/stv_elections/SC2017/South_Lanarkshire/index.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
SouthLanarkshire17 <- as.data.frame(html_table(temp[1])) %>% 
  set_names(slice(., 1)) %>% 
  slice(-1) %>% 
  mutate(Council="South Lanarkshire")

#Stirling
url <- "https://www.macs.hw.ac.uk/~denis/stv_elections/SC2017/Stirling/index.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
Stirling17 <- as.data.frame(html_table(temp[1])) %>% 
  set_names(slice(., 1)) %>% 
  slice(-1) %>% 
  mutate(Council="Stirling")

#West Lothian
url <- "https://www.macs.hw.ac.uk/~denis/stv_elections/SC2017/West_Lothian/index.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
WestLothian17 <- as.data.frame(html_table(temp[1])) %>% 
  set_names(slice(., 1)) %>% 
  slice(-1) %>% 
  mutate(Council="West Lothian")

#West Dunbartonshire
url <- "https://www.macs.hw.ac.uk/~denis/stv_elections/SC2017/West_Dunbartonshire/index.html"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
WestDunbartonshire17 <- as.data.frame(html_table(temp[1])) %>% 
  set_names(slice(., 1)) %>% 
  slice(-1) %>% 
  mutate(Council="West Dunbartonshire")

data2017 <- bind_rows(Aberdeen17, Aberdeenshire17, Angus17, ArgyllBute17, Borders17, Clackmannanshire17,
                      DumfriesandGalloway17, Dundee17, EastAyrshire17, EastDunbartonshire17,
                      EastLothian17, EastRenfrewshire17, Edinburgh17, EileanSiar17, Falkirk17, Fife17,
                      Glasgow17, Highland17, Inverclyde17, Midlothian17, Moray17, NorthAyrshire17,
                      NorthLanarkshire17, Orkney17, PerthandKinross17, Renfrewshire17, Shetland17,
                      SouthAyrshire17, SouthLanarkshire17, Stirling17, WestDunbartonshire17,
                      WestLothian17) %>% 
  rename("Ward"="Ward (click on name for details)") %>% 
  mutate(Ward=gsub("-", " ", Ward),
         Ward=gsub("_", " ", Ward),
         Ward=gsub("&", "and", Ward),
         Ward=gsub(",", "", Ward),
         Con=as.numeric(Con), Lab=as.numeric(Lab), SLD=as.numeric(SLD), SNP=as.numeric(SNP), 
         Grn=as.numeric(Grn), Ind=as.numeric(Ind), Rub=as.numeric(Rub), OMG=as.numeric(OMG),
         WDCP=as.numeric(WDCP), 
         across(c(Con, Lab, SLD, SNP, Grn, Ind, Rub, OMG, WDCP), ~if_else(is.na(.), 0, .)),
         Oth=Ind+Rub+OMG+WDCP,
         Count=Con+Lab+SLD+SNP+Grn+Oth,
         Con_alpha=if_else(Con==0, 0, Con/Count),
         Lab_alpha=if_else(Lab==0, 0, Lab/Count),
         SLD_alpha=if_else(SLD==0, 0, SLD/Count),
         SNP_alpha=if_else(SNP==0, 0, SNP/Count),
         Grn_alpha=if_else(Grn==0, 0, Grn/Count),
         Oth_alpha=if_else(Oth==0, 0, Oth/Count),
         Ward=case_when(
           Ward=="Eilean a' Cheo" ~ "Eilean a Cheo",
           Ward=="Howe Of Fife and Tay Coast" ~ "Howe of Fife and Tay Coast",
           Ward=="St. Andrews" ~ "St Andrews",
           Ward=="Inverness Ness side" ~ "Inverness Ness-side",
           Ward=="Kinross Shire" ~ "Kinross-shire",
           Ward=="Sgir' Uige agus Ceann a Tuath nan Loch" ~ "Sgir'Uige agus Ceann a Tuath nan Loch",
           TRUE ~ Ward))


#Merge into shapefile
map2017 <- shapefile_wards17 %>% 
  rename("Name"="WD19NM") %>% 
  mutate(Ward=gsub("\\/", " ", Name), Ward=gsub(",", "", Ward),
         Ward=gsub("Monifeith", "Monifieth", Ward),
         Ward=gsub(" Ward", "", Ward)) %>% 
  left_join(data2017) %>% 
  left_join(SIMDmapping2017)

agg_png("Outputs/ScotlandWard2017.png", units="in", width=9, height=8, res=800)
ggplot(map2017, aes(geometry=geometry))+
  geom_sf(aes(alpha=Con_alpha), fill="#0087DC", colour=NA)+
  geom_sf(aes(alpha=Lab_alpha), fill="#E4003B", colour=NA)+
  geom_sf(aes(alpha=SNP_alpha), fill="#FDF38E", colour=NA)+
  geom_sf(aes(alpha=Grn_alpha), fill="#00B140", colour=NA)+
  geom_sf(aes(alpha=SLD_alpha), fill="#FAA61A", colour=NA)+
  #geom_sf(aes(alpha=Oth_alpha), fill="Black", colour=NA)+
  scale_alpha(limits=c(0,1))+
  theme_void()+
  theme(legend.position="none")
dev.off()

agg_png("Outputs/ScotlandWard2017Facets.png", units="in", width=12, height=9, res=800)
map2017 %>% gather(Party, Councillors, c("Lab", "Con", "SLD", "Grn", "SNP", "Oth")) %>% 
  filter(Councillors>0) %>% 
  ggplot(aes(geometry=geometry, fill=Party, alpha=Councillors/4))+
  geom_sf(colour=NA)+
  scale_fill_manual(values=c("#0087DC", "#00B140", "#E4003B", "Black", "#FAA61A", "#FDF38E"))+
  scale_alpha(limits=c(0,1))+
  facet_wrap(~Party)+
  theme_void()
dev.off()

waffledata2017 <- map2017 %>% 
  st_drop_geometry() %>% 
  as.data.frame() %>% 
  gather(Party, Councillors, c("Lab", "Con", "SLD", "Grn", "SNP", "Oth")) %>% 
  filter(Councillors>0) %>% 
  group_by(SIMDdecile) %>% 
  arrange(SIMDrank) %>% 
  mutate(position=c(1:n())) %>% 
  ungroup() 

agg_png("Outputs/CouncilWardsxSIMD.png", units="in", width=12, height=6, res=800)
ggplot(waffledata2017, aes(y=as.factor(SIMDdecile), x=position, fill=Party))+
  geom_tile()+
  scale_x_continuous(name="")+
  scale_y_discrete(name="Index of Multiple Deprivation", labels=c("Most deprived\ndecile", "","","","",
                                                                  "","","","","Least deprived\ndecile"))+
  scale_fill_manual(values=c("#0087DC", "#00B140", "#E4003B", "Black", "#FAA61A", "#FDF38E"))+
  theme_custom()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text.x=element_blank(),
        legend.position="top", plot.title=element_text(face="bold", size=rel(2.5), hjust=0,
                                                       margin=margin(0,0,5.5,0)),)+
  labs(title="The politics of inequality",
       subtitle="Party affiliation for current English local councillors, arranged by decile of the Index of Multiple Deprivation.\nWithin each decile, wards are sorted with the most deprived wards on the left and the least deprived on the right.",
       caption="Data from opencouncildata.co.uk, ONS and MHCLG\nPlot by @VictimOfMaths\nInspired by @undertheraedar")
dev.off()

agg_png("Outputs/CouncilWardsxSIMDRidges.png", units="in", width=9, height=6, res=800)
ggplot(waffledata2017, aes(x=SIMDrank, y=Party, fill=Party))+
  geom_density_ridges()+
  scale_x_continuous(name="Index of Multiple Deprivation", breaks=c(0,32500), 
                     labels=c("Most\ndeprived", "Least\ndeprived"))+
  scale_y_discrete(name="")+
  scale_fill_manual(values=c("#0087DC", "#00B140", "#E4003B", "Black", "#FAA61A", "#FDF38E"))+
  theme_custom()+
  theme(plot.title=element_text(face="bold", size=rel(2.5), hjust=0,
                                margin=margin(0,0,5.5,0)))+
  labs(title="The politics of inequality",
       subtitle="The distribution of deprivation (as measured by the Index of Multiple Deprivation) for English local council wards\nby political affiliation of current councillors",
       caption="Data from opencouncildata.co.uk, ONS and MHCLG\nPlot by @VictimOfMaths")
dev.off()

ggplot(waffledata2017, aes(x=as.factor(SIMDdecile), y=Councillors, fill=Party))+
  geom_col()+
  theme_custom()+
  scale_fill_manual(values=c("#0087DC", "#00B140", "#E4003B", "Black", "#FAA61A", "#FDF38E"))
  

#2022 results
temp <- tempfile()
#source <- "https://candidates.democracyclub.org.uk/media/csv-archives/results-2022-05-05.csv"
source <- "https://candidates.democracyclub.org.uk/media/candidates-2022-05-05.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

data2022 <- read.csv(temp) %>% 
  filter(elected=="True" & NUTS1=="Scotland") %>% 
  select(name, party_name, post_id, post_label, organisation_name) %>% 
  mutate(CODE=gsub("gss:", "", post_id),
         party_name=case_when(
           party_name=="Conservative and Unionist Party" ~ "Con",
           party_name %in% c("Labour and Co-operative Party", "Labour Party") ~ "Lab",
           party_name=="Liberal Democrats" ~ "SLD",
           party_name=="Scottish National Party (SNP)" ~ "SNP",
           party_name=="Scottish Green Party" ~ "Grn",
           TRUE ~ "Oth")) %>% 
  group_by(party_name, CODE, post_label, organisation_name) %>% 
  summarise(Count=n()) %>% 
  ungroup() %>% 
  spread(party_name, Count) %>% 
  mutate(across(c(Con, Lab, SLD, SNP, Grn, Oth), ~if_else(is.na(.), 0, as.double(.))),
         Count=Con+Lab+SLD+SNP+Grn+Oth,
         Con_alpha=if_else(Con==0, 0, Con/Count),
         Lab_alpha=if_else(Lab==0, 0, Lab/Count),
         SLD_alpha=if_else(SLD==0, 0, SLD/Count),
         SNP_alpha=if_else(SNP==0, 0, SNP/Count),
         Grn_alpha=if_else(Grn==0, 0, Grn/Count),
         Oth_alpha=if_else(Oth==0, 0, Oth/Count),
         CODE=case_when(
           post_label=="Arran" ~ "S13003146",
           post_label=="Ardrossan" ~ "S13003145",
           post_label=="Barraigh agus Bhatarsaigh" ~ "S13003135",
           post_label=="East Mainland, South Ronaldsay and Burray" ~ "S13003150",
           post_label=="Garnock Valley" ~ "S13003147",
           post_label=="Irvine East" ~ "S13003033",
           post_label=="Irvine South" ~ "S13003041",
           post_label=="Irvine West" ~ "S13003032",
           post_label=="Kilwinning" ~ "S13003034",
           post_label=="Kirkwall East" ~ "S13003151",
           post_label=="Kirkwall West and Orphir" ~ "S13003152",
           post_label=="Lerwick North and Bressay" ~ "S13002777",
           post_label=="Lerwick South" ~ "S13003153",
           post_label=="Loch a Tuath" ~ "S13003136",
           post_label=="Na Hearadh" ~ "S13003137",
           post_label=="North Coast" ~ "S13003148",
           post_label=="North Isles" & organisation_name=="Orkney Islands Council" ~ "S13002737",
           post_label=="North Isles" ~ "S13002772",
           post_label=="Saltcoats and Stevenston" ~ "S13003149",
           CODE=="ELS:sgir-uige-agus-carlabhagh" ~ "S13003138",
           post_label=="SgÃ¬re an Rubha" ~ "S13003139",
           post_label=="SgÃ¬re nan Loch" ~ "S13003140",
           post_label=="Shetland Central" ~ "S13003154",
           post_label=="Shetland North" ~ "S13002773",
           post_label=="Shetland South" ~ "S13003155",
           post_label=="Shetland West" ~ "S13003156",
           post_label=="SteÃ²rnabhagh a Deas" ~ "S13003141",
           post_label=="SteÃ²rnabhagh a Tuath" ~ "S13003142",
           post_label=="Stromness and South Isles" ~ "S13002734",
           post_label=="Uibhist a Deas, Ãˆirisgeigh agus Beinn na Faoghla" ~ "S13003143",
           post_label=="Uibhist a Tuath" ~ "S13003144",
           post_label=="West Mainland" ~ "S13002735",
           TRUE ~ CODE))
  
  #mutate(Council=gsub("local.", "", election_id),
  #       Council=gsub(".2022-05-05", "", Council),
  #       Ward=gsub("local.", "", ballot_paper_id),
  #       Ward=gsub(".2022-05-05", "", Ward)) %>% 
  #rowwise() %>% 
  #mutate(Ward=gsub(paste0(Council, "."), "", Ward)) %>% 
  #ungroup() %>% 
  #mutate(WardMatch=gsub("\\.", "", Ward),
  #       WardMatch=gsub("-", "", WardMatch))

map2022 <- shapefile_wards22 %>% 
  left_join(data2022) %>% 
  left_join(SIMDmapping2022)

test <- map2022 %>% 
  filter(is.na(Con)) %>% 
  st_drop_geometry() %>% 
  as.data.frame() %>% 
  select(NAME, CODE)

agg_png("Outputs/ScotlandWard2022.png", units="in", width=9, height=8, res=800)
ggplot(map2022, aes(geometry=geometry))+
  geom_sf(aes(alpha=Con_alpha), fill="#0087DC", colour=NA)+
  geom_sf(aes(alpha=Lab_alpha), fill="#E4003B", colour=NA)+
  geom_sf(aes(alpha=SNP_alpha), fill="#FDF38E", colour=NA)+
  geom_sf(aes(alpha=Grn_alpha), fill="#00B140", colour=NA)+
  geom_sf(aes(alpha=SLD_alpha), fill="#FAA61A", colour=NA)+
  #geom_sf(aes(alpha=Oth_alpha), fill="Black", colour=NA)+
  scale_alpha(limits=c(0,1))+
  theme_void()+
  theme(legend.position="none")
dev.off()

agg_png("Outputs/ScotlandWard2022Facets.png", units="in", width=12, height=9, res=800)
map2022 %>% gather(Party, Councillors, c("Lab", "Con", "SLD", "Grn", "SNP", "Oth")) %>% 
  filter(Councillors>0) %>% 
  ggplot(aes(geometry=geometry, fill=Party, alpha=Councillors/4))+
  geom_sf(colour=NA)+
  scale_fill_manual(values=c("#0087DC", "#00B140", "#E4003B", "Black", "#FAA61A", "#FDF38E"))+
  scale_alpha(limits=c(0,1), labels=c("0", "1", "2", "3", "4"), name="Councillors")+
  facet_wrap(~Party)+
  theme_void()
dev.off()

waffledata2022 <- map2022 %>% 
  st_drop_geometry() %>% 
  as.data.frame() %>% 
  gather(Party, Councillors, c("Lab", "Con", "SLD", "Grn", "SNP", "Oth")) %>% 
  filter(Councillors>0) %>% 
  group_by(SIMDdecile) %>% 
  arrange(SIMDrank) %>% 
  mutate(position=c(1:n())) %>% 
  ungroup() %>% 
  mutate(Party=factor(Party, levels=c("Con", "Grn", "Lab", "SLD", "SNP", "Oth")))

agg_png("Outputs/CouncilWardsxSIMD.png", units="in", width=12, height=6, res=800)
ggplot(waffledata2022, aes(y=as.factor(SIMDdecile), x=position, fill=Party))+
  geom_tile()+
  scale_x_continuous(name="")+
  scale_y_discrete(name="Index of Multiple Deprivation", labels=c("Most deprived\ndecile", "","","","",
                                                                  "","","","","Least deprived\ndecile"))+
  scale_fill_manual(values=c("#0087DC", "#00B140", "#E4003B", "Black", "#FAA61A", "#FDF38E"))+
  theme_custom()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text.x=element_blank(),
        legend.position="top", plot.title=element_text(face="bold", size=rel(2.5), hjust=0,
                                                       margin=margin(0,0,5.5,0)),)+
  labs(title="The politics of inequality",
       subtitle="Party affiliation for current English local councillors, arranged by decile of the Index of Multiple Deprivation.\nWithin each decile, wards are sorted with the most deprived wards on the left and the least deprived on the right.",
       caption="Data from opencouncildata.co.uk, ONS and MHCLG\nPlot by @VictimOfMaths\nInspired by @undertheraedar")
dev.off()

agg_png("Outputs/CouncilWardsxSIMDRidges.png", units="in", width=9, height=6, res=800)
ggplot(waffledata2022, aes(x=SIMDrank, y=Party, fill=Party))+
  geom_density_ridges()+
  scale_x_continuous(name="Index of Multiple Deprivation", breaks=c(0,32500), 
                     labels=c("Most\ndeprived", "Least\ndeprived"))+
  scale_y_discrete(name="")+
  scale_fill_manual(values=c("#0087DC", "#00B140", "#E4003B", "Black", "#FAA61A", "#FDF38E"))+
  theme_custom()+
  theme(plot.title=element_text(face="bold", size=rel(2.5), hjust=0,
                                margin=margin(0,0,5.5,0)))+
  labs(title="The politics of inequality",
       subtitle="The distribution of deprivation (as measured by the Index of Multiple Deprivation) for English local council wards\nby political affiliation of current councillors",
       caption="Data from opencouncildata.co.uk, ONS and MHCLG\nPlot by @VictimOfMaths")
dev.off()

agg_png("Outputs/CouncilWardsxSIMDBars.png", units="in", width=9, height=6, res=800)
ggplot(waffledata2022, aes(x=as.factor(SIMDdecile), y=Councillors, fill=Party))+
  geom_col()+
  scale_x_discrete(name="Index of Multiple Deprivation", labels=c("Most deprived\ndecile", "","","","",
                                                                  "","","","","Least deprived\ndecile"))+
  scale_fill_manual(values=c("#0087DC", "#00B140", "#E4003B", "#FAA61A", "#FDF38E", "Black"),
                    labels=c("Conservative", "Green", "Labour", "Lib Dem", "SNP", "Independent/Other"),
                    name="")+
  theme_custom()+
  labs(title="The deprivation profile of Scottish local politics",
       subtitle="Local Councillors elected in the 2022 elections in Scotland by average deprivation level within their ward",
       caption="Data from Democracy Club, Scottish Government and ONS\nPlot by @VictimOfMaths")
  
dev.off()





#2017/22 comparisons
waffledatacomb <- waffledata2017 %>% 
  group_by(Party, SIMDdecile) %>% 
  summarise(Councillors17=sum(Councillors)) %>% 
  ungroup() %>% 
  merge(waffledata2022 %>% 
          group_by(Party, SIMDdecile) %>% 
          summarise(Councillors22=sum(Councillors)) %>% 
          ungroup()) %>% 
  mutate(change=Councillors22-Councillors17,
         Party=factor(Party, levels=c("Con", "Grn", "Lab", "SLD", "SNP", "Oth")))

agg_png("Outputs/CouncilWardsChangesxSIMDBars.png", units="in", width=9, height=6, res=800)
ggplot(waffledatacomb, aes(x=as.factor(SIMDdecile), y=change, fill=Party))+
  geom_col()+
  geom_hline(yintercept=0, colour="White")+
  scale_x_discrete(name="Index of Multiple Deprivation", labels=c("Most deprived\ndecile", "","","","",
                                                                  "","","","","Least deprived\ndecile"))+
  scale_y_continuous(name="Change in number of councillors 2017-22")+
  scale_fill_manual(values=c("#0087DC", "#00B140", "#E4003B", "#FAA61A", "#FDF38E", "Black"),
                    labels=c("Conservative", "Green", "Labour", "Lib Dem", "SNP", "Independent/Other"),
                    name="")+
  theme_custom()+
  labs(title="Scottish Conservatives took a kicking *everywhere*",
       subtitle="Changes in local councillors elected in the 2022 elections in Scotland compared to 2017 by average deprivation level within their ward",
       caption="Data from Democracy Club, Scottish Government and ONS\nPlot by @VictimOfMaths")

dev.off()

agg_png("Outputs/ScotlandWard201722Facets.png", units="in", width=4, height=12, res=800)
map2017 %>% gather(Party, Councillors, c("Lab", "Con", "SLD", "Grn", "SNP", "Oth")) %>% 
  filter(Councillors>0) %>% mutate(Year=2017) %>% 
  bind_rows(map2022 %>% gather(Party, Councillors, c("Lab", "Con", "SLD", "Grn", "SNP", "Oth")) %>% 
              filter(Councillors>0) %>% mutate(Year=2022)) %>% 
  ggplot(aes(geometry=geometry, fill=Party, alpha=Councillors/4))+
  geom_sf(colour=NA)+
  scale_fill_manual(values=c("#0087DC", "#00B140", "#E4003B", "Black", "#FAA61A", "#FDF38E"))+
  scale_alpha(limits=c(0,1), labels=c("0", "1", "2", "3", "4"), name="Councillors")+
  facet_grid(Party~Year)+
  theme_void()#+
  #xlim(210000,370000)+
  #ylim(620000,730000)
dev.off()


totals <- data2017 %>% 
  gather(Party, Councillors, c("Lab", "Con", "SLD", "Grn", "SNP", "Oth")) %>% 
  filter(Councillors>0) %>% 
  group_by(Party) %>% 
  summarise(Total2017=sum(Councillors)) %>% 
  ungroup() %>% 
  merge(data2022 %>% 
          gather(Party, Councillors, c("Lab", "Con", "SLD", "Grn", "SNP", "Oth")) %>% 
          filter(Councillors>0) %>% 
          group_by(Party) %>% 
          summarise(Total2022=sum(Councillors)) %>% 
          ungroup())


