rm(list=ls())

library(tidyverse)
library(curl)
library(snakecase)
library(ggtern)
library(extrafont)
library(ragg)
library(tricolore)
library(sf)
library(cowplot)

#Read in historic election data from House of Commons library
temp <- tempfile()
source <- "https://researchbriefings.files.parliament.uk/documents/CBP-8647/1918_2019election_results.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

data <- read.csv(temp) %>% 
  #Keep only English data as other parties are too large a factor elsewhere
  filter(!country.region %in% c("Scotland", "Wales", "Northern Ireland")) %>% 
  #Tidy up the constituency names
  mutate(constituency=to_upper_camel_case(gsub("&", "and", constituency), sep_out=" "),
         lab_votes=if_else(is.na(lab_votes), 0, as.double(lab_votes)),
         lib_votes=if_else(is.na(lib_votes), 0, as.double(lib_votes)),
         con_votes=if_else(is.na(con_votes), 0, as.double(con_votes))) %>% 
  #Add in post-2019 by-election results manually
  bind_rows(data.frame(constituency=c("Hartlepool", "Chesham And Amersham", "Batley And Spen"),
            lab_votes=c(8589, 622, 13296),
            lib_votes=c(349, 21517, 1254),
            con_votes=c(15529, 13489, 12973),
            election=c("2021", "2021", "2021"))) %>% 
  rowwise() %>% 
  #Calculate % of votes going to main parties which went to each one
  mutate(Lab_prop=lab_votes/(lab_votes+con_votes+lib_votes),
         Lib_prop=lib_votes/(lab_votes+con_votes+lib_votes),
         Con_prop=con_votes/(lab_votes+con_votes+lib_votes),
         #Add in winner
         winner=case_when(
           max(lab_votes, lib_votes, con_votes)==lab_votes ~ "Labour",
           max(lab_votes, lib_votes, con_votes)==lib_votes ~ "Lib Dems",
           max(lab_votes, lib_votes, con_votes)==con_votes ~ "Conservative"),
         election=case_when(
           election=="1974F" ~ 1974,
           election=="1974O" ~ 1974.5,
           TRUE ~ as.numeric(election))) %>% 
  select(constituency, Lab_prop, Lib_prop, Con_prop, election, winner) %>% 
  ungroup() %>% 
  group_by(constituency) %>% 
  arrange(-election) %>% 
  ungroup()

#generate background
con <- data.frame(y=c(0,0.5,1/3, 0),
                  x=c(0,0,1/3,0.5),
                  z=c(1,0.5,1/3,0.5), Col="Blue")
lib <- data.frame(x=c(0,0.5,1/3, 0),
                  z=c(0,0,1/3,0.5),
                  y=c(1,0.5,1/3,0.5), Col="Yel")

lab <- data.frame(z=c(0,0.5,1/3, 0),
                  y=c(0,0,1/3,0.5),
                  x=c(1,0.5,1/3,0.5), Col="Red")

background <- rbind(con, lib, lab)

const <- "Batley And Spen"

plotdata <- data %>% filter(constituency==const & election>=1974)
plottitle <- paste("Electoral shifts in", const, "since", min(plotdata$election))


agg_tiff("Outputs/UKElectionsBatleySpen.tiff", units="in", width=6, height=6, res=800)
ggtern()+
  geom_polygon(data=background, aes(x, y, z,fill=Col), alpha=0.2)+
  theme_hidegrid()+
  theme_showarrows()+
  scale_fill_manual(values=c("#0087dc", "#d50000", "#fdbb30"), guide=FALSE)+
  geom_segment(aes(x=0.5, xend=1/3, y=0.5, yend=1/3, z=0, zend=1/3), colour="white")+
  geom_segment(aes(x=0.5, xend=1/3, y=0, yend=1/3, z=0.5, zend=1/3), colour="white")+
  geom_segment(aes(x=0, xend=1/3, y=0.5, yend=1/3, z=0.5, zend=1/3), colour="white")+
  geom_path(data=plotdata, aes(x=Lab_prop, y=Lib_prop, z=Con_prop),
            arrow = arrow(angle = 15, ends="first", type = "closed", length=unit(0.1,"inches")))+
  geom_point(data=plotdata, aes(x=Lab_prop, y=Lib_prop, z=Con_prop, colour=winner),
             show.legend=FALSE)+
  scale_colour_manual(values=c(Conservative="#0087dc", Labour="#d50000", `Lib Dems`="#fdbb30"), guide=FALSE)+
  labs(x="", xarrow="Labour vote %", y="", yarrow="Lib Dem vote %", 
       z="", zarrow="Conservative vote %", title=plottitle)+
  theme(text=element_text(family="Lato"), plot.title=element_text(face="bold", size=rel(1.5)))
dev.off()

###########################
#Map of the latest results

latest <- data %>% 
  filter(election>=2019) %>% 
  group_by(constituency) %>% 
  filter(election==max(election)) %>% 
  ungroup()

#Set up ternary colour scheme
tricolore2019 <- Tricolore(latest, "Lib_prop", "Con_prop", "Lab_prop", breaks=100)

#Tidy up the key
key <- tricolore2019$key+
  labs(L='Lib Dem', R='Labour', T='Conservative', x="", y="", z="")+
  scale_T_continuous(breaks=c(0.25,0.5,0.75, 1), labels=c("25%", "50%", "75%", "100%"))+
  scale_L_continuous(breaks=c(0.25,0.5,0.75, 1), labels=c("25%", "50%", "75%", "100%"))+
  scale_R_continuous(breaks=c(0.25,0.5,0.75, 1), labels=c("25%", "50%", "75%", "100%"))+
  theme(text=element_text(family="Lato"))

latest$rgb <- tricolore2019$rgb

#Download Carl Baker's lovely map
parl <- tempfile()
source <- ("https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous/raw/main/geopackages/Constituencies.gpkg")
parl <- curl_download(url=source, destfile=parl, quiet=FALSE, mode="wb")

Background <- st_read(parl, layer="5 Background") %>% 
  filter(Name=="England & Wales")

votes <- st_read(parl, layer="4 Constituencies") %>%
  mutate(constituency=to_upper_camel_case(pcon.name, sep_out=" ")) %>% 
  filter(!RegionNati %in% c("Wales", "Scotland", "Northern Ireland")) %>% 
  left_join(latest, by="constituency", all=TRUE)

Cities <- st_read(parl, layer="3 City outlines") %>% 
  filter(!RegionNati %in% c("Wales", "Scotland", "Northern Ireland")) 

Groups <- st_read(parl, layer="2 Group outlines") %>% 
  filter(!RegionNati %in% c("Wales", "Scotland", "Northern Ireland")) 

Group_labels <- st_read(parl, layer="1 Group names") %>% 
  mutate(just=if_else(LabelPosit=="Left", 0, 1)) %>%
  filter(!RegionNati %in% c("Wales", "Scotland", "Northern Ireland")) 


plot1 <- ggplot()+
  geom_sf(data=Background, aes(geometry=geom), fill="White")+
  geom_sf(data=votes, aes(geometry=geom, fill=rgb), colour="White", size=0.1)+
  geom_sf(data=Groups, aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf(data=Cities, aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labels, aes(geometry=geom, label=Group.labe,
                                      hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_identity()+
  coord_sf(clip="off")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(2)),
        text=element_text(family="Lato"))+
  labs(title="Vote share in England by constituency")+
  annotation_custom(
    ggplotGrob(key),
    xmin = 43, xmax = 63, ymin = 43, ymax = 60)+
  annotate("text", x=58, y=2, family="Lato", size=rel(3), hjust=1,
           label="Data and cartogram from the House of Commons Library\nPlot by @VictimOfMaths")+
  annotate("text", x=17, y=50, family="Lato", size=rel(3), hjust=0.5,
           label="By convention, the speaker, Lindsay Hoyle's\n Chorley constituency was not contested\nin the 2019 General Election")+
  geom_curve(aes(x=19, y=48, xend=27, yend=39), curvature=0.2)+
  annotate("text", x=11, y=58, family="Lato", size=rel(4), hjust=0,
           label="Every parliamentary constituency in England, coloured by their vote share\nbetween the three major parties in the most recent General or By-Election.")

agg_tiff("Outputs/UKElectionsTernaryCartogram.tiff", units="in", width=9, height=10, res=800)
plot1
dev.off()
