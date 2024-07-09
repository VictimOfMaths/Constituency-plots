rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(extrafont)
library(ggbeeswarm)
library(ragg)
library(paletteer)
library(stringi)

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

#Download data on pre-election MPs and their political values from mpsleftright.co.uk
temp <- tempfile()
url <- "https://github.com/chrishanretty/pairwise_mps/raw/a11e77efc875abe6d0c62a91d6da04e85d9557fd/outputs/mpsleftright_excel.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

PastMPs <- read_excel(temp, sheet="MP values") %>% 
  mutate(Name=tolower(Name),
         Name=stri_trans_general(str=Name, id="Latin-ASCII"))

ggplot(PastMPs, aes(x=Value, y=0, colour=Party))+
  geom_quasirandom(cex=1.5)+
  scale_colour_manual(values=c("#0087dc", "#008066", "#d50000", "#fdbb30", "#3f8428", "#fff95d"))+
  theme_custom()+
  theme(axis.line=element_blank(), axis.ticks.y=element_blank(),
        axis.text.y=element_blank(), axis.title.y=element_blank())
                      
#Download data on candidates who stood in the 2024 GE to distinguish MPs who stood for re-election vs. those who stood down
#From the excellent Democracy Club
temp <- tempfile()
url <- "https://candidates.democracyclub.org.uk/data/export_csv/?election_id=parl.2024-07-04"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

Candidates <- read.csv(temp) %>% 
  mutate(Name=tolower(person_name),
         Name=stri_trans_general(str=Name, id="Latin-ASCII"),
         #Faff about with some names to align datasets
         Name=case_when(
           Name=="andrew campbell bowie" ~ "andrew bowie",
           Name=="sir edward leigh" ~ "edward leigh",
           Name=="mary kelly foy" ~ "mary foy",
           Name=="richard john holden" ~ "richard holden",
           Name=="sarah joanne dyke" ~ "sarah dyke",
           TRUE ~ Name))                   
                      
#Download list of MPS who won from the HoC library
temp <- tempfile()
url <- "https://researchbriefings.files.parliament.uk/documents/CBP-10009/Winning-members.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

NewMPs <- read_excel(temp) %>% 
  mutate(Name=tolower(paste(firstname, surname)),
         Name=stri_trans_general(str=Name, id="Latin-ASCII"))  

#Stick it all together
FinalData <- PastMPs %>% 
  select(Constituency, Name, Party, Value) %>% 
  merge(NewMPs, by="Name", all.x=T) %>% 
  mutate(Result2024=if_else(is.na(ons_id), "Lost", "Won")) %>% 
  select(Constituency, Name, Party, Value, Result2024) %>% 
  merge(Candidates, by="Name", all.x=T) %>% 
  mutate(Stood2024=if_else(is.na(person_id), "Did not stand", "Stood")) %>% 
  select(Constituency, Name, Party, Value, Result2024, Stood2024) %>% 
  mutate(Change=case_when(
    Result2024=="Lost" & Stood2024=="Did not stand" ~ "Did not stand",
    Result2024=="Lost" & Stood2024=="Stood" ~ "Stood and lost",
    Result2024=="Won" & Stood2024=="Stood" ~ "Stood and won"))
  
#Check we haven't messed up and got candidates winning that didn't stand
#(This should be empty)
test <- FinalData %>% filter(Stood2024=="Did not stand" & Result2024=="Won")

FinalData %>% filter(Party=="Conservative") %>%  
  group_by(Change) %>% 
  summarise(Value=mean(Value))

ggplot(FinalData, aes(x=Value, y=0, colour=Change))+
  geom_quasirandom(cex=1.5)+
  #scale_colour_manual(values=c("#0087dc", "#008066", "#d50000", "#fdbb30", "#3f8428", "#fff95d"))+
  facet_wrap(~Party)+
  theme_custom()+
  theme(axis.line=element_blank(), axis.ticks.y=element_blank(),
        axis.text.y=element_blank(), axis.title.y=element_blank())

agg_png("Outputs/GEElectionOutcomes2024.png", units="in", width=9, height=6, res=600)
ggplot(FinalData %>% filter(Party=="Conservative"), aes(x=Value, y=0, colour=Change))+
  geom_quasirandom(cex=2)+
  scale_colour_manual(values=c("#a82203", "#f1af3a", "#208cc0"))+
  scale_x_continuous(name="Political left/right score\n(higher = more right wing)\n")+
  facet_grid(Change~., switch="y")+
  theme_custom()+
  theme(axis.line=element_blank(), axis.ticks.y=element_blank(),
        axis.text.y=element_blank(), axis.title.y=element_blank(),
        legend.position="none", strip.placement = "outside",
        strip.text.y.left =  element_text(angle = 0,vjust = 1,size=12))+
  labs(title="Political leaning had no association with election outcomes for the Conservatives",
       subtitle="Political left/right score for Tory MPs in 2023 by outcomes in the 2024 General Election\n",
       caption="Data from mpsleftright.co.uk, Democracy Club and House of Commons Library\nPlot by @VictimOfMaths")

dev.off()

