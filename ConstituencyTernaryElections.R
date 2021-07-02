rm(list=ls())

library(tidyverse)
library(curl)
library(snakecase)
library(ggtern)
library(extrafont)
library(ragg)
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
plottitle <- paste("Electoral shifts in", const, "since", min(plotdata$election))

plotdata <- data %>% filter(constituency==const)

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


