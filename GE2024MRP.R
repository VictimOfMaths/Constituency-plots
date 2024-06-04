rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(ggtern)
library(extrafont)
library(ragg)

#Download 2019 GE results from House of Commons library website
temp <- tempfile()
url <- "https://researchbriefings.files.parliament.uk/documents/CBP-8749/HoC-GE2019-results-by-constituency.csv"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

GE2019 <- read.csv(temp) %>% 
  select(ONS.ID, Constituency.name, Con, Lab, LD) %>% 
  #filter only English constituencies
  filter(substr(ONS.ID, 1, 1)=="E") 

#Reconfigure to new constituency boundaries

#Download boundary review data from House of Commons library website
#(big shout out to the HoC library for making excellent things like this)

temp2 <- tempfile()
url2 <- "https://commonslibrary.shinyapps.io/new_constituencies_insight/_w_bd745655/Boundary_changes_data_file.xlsx"
temp2 <- curl_download(url=url2, destfile=temp2, quiet=FALSE, mode="wb")

ConstMapping <- read_excel(temp2, sheet="All overlaps", range="A2:J1436") %>% 
  select(c(1:4, 9,10)) %>% 
  set_names("OldCode", "OldName", "NewCode", "NewName", "PopProp_Old", "PopProp_New") %>% 
  #filter only English constituencies
  filter(substr(`NewCode`, 1, 1)=="E") 

#Join together to make pseudo-2019 results based on new constituencies
GE2019_New <- ConstMapping %>% 
  merge(GE2019, by.x="OldCode", by.y="ONS.ID", all.x=TRUE) %>% 
  group_by(`NewCode`, `NewName`) %>% 
  summarise(Lab=sum(Lab*PopProp_Old),
            Con=sum(Con*PopProp_Old),
            LD=sum(LD*PopProp_Old), .groups="drop") %>% 
  #calculate vote shares between 3 main parties only
  mutate(ConProp_2019=Con/(Con+Lab+LD),
         LabProp_2019=Lab/(Con+Lab+LD),
         LDProp_2019=LD/(Con+Lab+LD))

#Bring in latest MRP figures from YouGov
temp3 <- tempfile()
url3 <- "https://ygo-assets-websites-editorial-emea.yougov.net/documents/Results-GE2024-Model-030624-website-final.xlsx"
temp3 <- curl_download(url=url3, destfile=temp3, quiet=FALSE, mode="wb")

MRP_2024 <- read_excel(temp3) %>% 
  #Normalise vote share between 3 main parties
  mutate(ConProp_2024=ConShare/(ConShare+LabShare+LibDemShare),
         LabProp_2024=LabShare/(ConShare+LabShare+LibDemShare),
         LDProp_2024=LibDemShare/(ConShare+LabShare+LibDemShare)) %>% 
  select(const, ConProp_2024, LabProp_2024, LDProp_2024)

#Bring together
data <- merge(GE2019_New, MRP_2024, by.x="NewCode", by.y="const", all.x=T) %>% 
  #Identify winning party (of the 3 main parties) in each year
  mutate(Winner_2019=case_when(
    LabProp_2019>ConProp_2019 & LabProp_2019>LDProp_2019 ~ "Lab",
    ConProp_2019>LabProp_2019 & ConProp_2019>LDProp_2019 ~ "Con",
    LDProp_2019>ConProp_2019 & LDProp_2019>LabProp_2019 ~ "LD"),
    Winner_2024=case_when(
      LabProp_2024>ConProp_2024 & LabProp_2024>LDProp_2024 ~ "Lab",
      ConProp_2024>LabProp_2024 & ConProp_2024>LDProp_2024 ~ "Con",
      LDProp_2024>ConProp_2024 & LDProp_2024>LabProp_2024 ~ "LD"))
  

#Set up background for plot
#generate background
con <- data.frame(y=c(0,0.5,1/3, 0),
                  x=c(0,0,1/3,0.5),
                  z=c(1,0.5,1/3,0.5), Col="Con")
lib <- data.frame(x=c(0,0.5,1/3, 0),
                  z=c(0,0,1/3,0.5),
                  y=c(1,0.5,1/3,0.5), Col="LD")

lab <- data.frame(z=c(0,0.5,1/3, 0),
                  y=c(0,0,1/3,0.5),
                  x=c(1,0.5,1/3,0.5), Col="Lab")

background <- rbind(con, lib, lab)

agg_png("Outputs/GE2024MRP.png", units="in", width=7.5, height=7.5, res=800)
ggtern()+
  geom_polygon(data=background, aes(x, y, z,fill=Col), alpha=0.2)+
  #theme_hidegrid()+
  theme_showarrows()+
  geom_segment(aes(x=0.5, xend=1/3, y=0.5, yend=1/3, z=0, zend=1/3), colour="white")+
  geom_segment(aes(x=0.5, xend=1/3, y=0, yend=1/3, z=0.5, zend=1/3), colour="white")+
  geom_segment(aes(x=0, xend=1/3, y=0.5, yend=1/3, z=0.5, zend=1/3), colour="white")+
  geom_segment(data=data, aes(x=LabProp_2019, xend=LabProp_2024,
                              y=LDProp_2019, yend=LDProp_2024,
                              z=ConProp_2019, zend=ConProp_2024))+
  geom_point(data=data, aes(x=LabProp_2024, y=LDProp_2024, z=ConProp_2024, fill=Winner_2024),
           show.legend=FALSE, shape=21)+
  scale_fill_manual(values=c("#0087dc", "#d50000", "#fdbb30"), guide="none")+
  labs(x="", xarrow="Labour vote %", y="", yarrow="Lib Dem vote %", 
       z="", zarrow="Conservative vote %", 
       title="Wipeout!",
       subtitle="Forecasted shifts in vote share between the 3 main parties in English constituences from 2019 to 2024\nDots represent the winning party in each constituency in 2024 according to YouGov's latest MRP projection\nLines represent how that constituency's vote has changed since the 2019 General Election")+
  theme(text=element_text(family="Lato"), plot.title=element_text(face="bold", size=rel(2.5)),
        plot.subtitle=element_text(colour="Grey40"))

dev.off()
