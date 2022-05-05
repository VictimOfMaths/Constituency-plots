rm(list=ls())

library(tidyverse)
library(curl)
library(sf)
library(readxl)
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

#Download council ward data for 2021 from opencouncildata.co.uk
#(and fix a bunch of typos)
temp <- tempfile()
wardurl <- "http://opencouncildata.co.uk/csv2.php?y=2021"
temp <- curl_download(url=wardurl, destfile=temp, quiet=FALSE, mode="wb")

warddata <- read.csv(temp) %>% 
  mutate(next.election.date=as.Date(next.election.date),
         wardName=gsub("&", "and", wardName),
         wardName=gsub("And", "and", wardName),
         wardName=gsub("With", "with", wardName),
         wardName=gsub("andover", "Andover", wardName),
         wardName=gsub("CAndovers", "Candovers", wardName),
         wardName=gsub("andrew", "Andrew", wardName),
         wardName=gsub("withington", "Withington", wardName),
         wardName=gsub("witherley", "Witherley", wardName),
         wardName=gsub(" The ", " the ", wardName),
         wardName=gsub(" On ", " on ", wardName),
         wardName=gsub("'", "", wardName),
         wardName=gsub("St. ", "St ", wardName),
         wardName=gsub(" without", " Without", wardName),
         wardName=gsub(" within", " Within", wardName),
         wardName=gsub("witham", "Witham", wardName),
         wardName=gsub(" division", "", wardName),
         wardName=gsub("Louth ", "", wardName),
         wardName=gsub("Skegness ", "", wardName),
         wardName=gsub("Droitwich Spa ", "Droitwich ", wardName),
         wardName=case_when(
           wardName=="Allhallows and Waverton" ~ "Allhallow and Waverton",
           wardName=="Newton and Morton North" ~ "Newtown and Morton North",
           wardName=="Martoneast" ~ "Marton East",
           wardName=="Settle and Ribble Banks" ~ "Settle and Ribblebanks",
           wardName=="Bilinge and Beardwood" ~ "Billinge and Beardwood",
           wardName=="Flyingdales and Ravenscar" ~ "Fylingdales and Ravenscar",
           wardName=="Weaponess and Ramshill" ~ "Weaponness and Ramshill",
           wardName=="Hunmansby" ~ "Hunmanby",
           wardName=="Famley and Wortley" ~ "Farnley and Wortley",
           wardName=="anderton" ~ "Adlington and Anderton",
           wardName=="Clayton East, Brindle and Houghton" ~ "Clayton East, Brindle and Hoghton",
           wardName=="Daresbury, Moore and Sandymoore" ~ "Daresbury, Moore and Sandymoor",
           wardName=="Ashton St Peters" ~ "St Peters",
           wardName=="Hotown" ~ "Howard Town",
           wardName=="Kingswalk" ~ "Kings Walk",
           wardName=="Beacon (Newark)" ~ "Beacon",
           wardName=="Bridge (Newark)" ~ "Bridge",
           wardName=="Castle (Newark)" ~ "Castle",
           wardName=="Devon (Newark)" ~ "Devon",
           wardName=="withern and Theddlethorpe" ~ "Withern and Theddlethorpe",
           wardName=="Spalding Monkshouse" ~ "Spalding Monks House",
           wardName=="Brinston" ~ "Briston",
           wardName=="Dereham withburga" ~ "Dereham Withburga",
           wardName=="Hedleigh North" ~ "Hadleigh North",
           wardName=="The Bentleys and Fratling" ~ "The Bentleys and Frating",
           wardName=="withersfield" ~ "Withersfield",
           wardName=="withyham" ~ "Withyham",
           wardName=="witheridge" ~ "Witheridge",
           wardName=="withdean" ~ "Withdean",
           wardName=="Saint Andrews" ~ "St Andrews",
           wardName=="Longfield, New Barm and Southfleet" ~ "Longfield, New Barn and Southfleet",
           wardName=="Wilmington Sutton-at-Hone and Hawley" ~ "Wilmington, Sutton-at-Hone and Hawley",
           wardName=="Darneth" ~ "Darenth",
           wardName=="Boothen and Oakhill" ~ "Boothen and Oak Hill",
           wardName=="Broughton Astley - Primethorpe and Sutton" ~ "Broughton Astley-Primethorpe and Sutton",
           wardName=="Broughton Atley South and Leire" ~ "Broughton Astley South and Leire",
           wardName=="Market Harborough - Little Bowden" ~ "Market Harborough-Little Bowden",
           wardName=="Market Harborough - Logan" ~ "Market Harborough-Logan",
           wardName=="Market Harborough - Welland" ~ "Market Harborough-Welland",
           wardName=="Market Harborough -Great Bowden and Arden" ~ "Market Harborough-Great Bowden and Arden",
           wardName=="Luberham" ~ "Lubenham",
           wardName=="Etching Hill and the Heath" ~ "Etching Hill and The Heath",
           wardName=="Weoley and Selley Oak" ~ "Weoley and Selly Oak",
           wardName=="Tanworthinarden" ~ "Tanworth-in-Arden",
           wardName=="warden Hill" ~ "Warden Hill",
           wardName=="Henleyinarden" ~ "Henley-in-Arden",
           wardName=="Wootton Wawen" ~ "Wotton Wawen",
           wardName=="Redhill" & council=="Herefordshires" ~ "Red Hill",
           wardName=="Mitcheldean, Ruarden and Drybrook" ~ "Mitcheldean, Ruardean and Drybrook",
           wardName=="Benhall and the Reddings" ~ "Benhall and The Reddings",
           wardName=="Farmhill and Paganhill" ~ "Stroud Farmhill and Paganhill",
           wardName=="Gorsehill and Pinehurst" ~ "Gorse Hill and Pinehurst",
           wardName=="Exmouth withycobe Raleigh" ~ "Exmouth Withycombe Raleigh",
           wardName=="Hartcliffe and withywood" ~ "Hartcliffe and Withywood",
           wardName=="Huntspill and Pawelett" ~ "Huntspill and Pawlett",
           wardName=="Crewkern" ~ "Crewkerne",
           wardName=="Chard Avishaves" ~ "Chard Avishayes",
           wardName=="Lunton and Lynmouth" ~ "Lynton and Lynmouth",
           wardName=="Chumleigh" ~ "Chulmleigh",
           wardName=="Goodington with Roselands" ~ "Goodrington with Roselands",
           wardName=="Altamum and Stoke Climsland" ~ "Altarnun and Stoke Climsland",
           wardName=="Threemilestones and Chacewater" ~ "Threemilestone and Chacewater",
           wardName=="Bishopston" ~ "Bishopton",
           wardName=="Cowes South and Norwood" ~ "Cowes South and Northwood",
           wardName=="Pulborough" ~ "Pulborough, Coldwaltham and Amberley",
           wardName=="Bewbush and North" ~ "Bewbush and North Broadfield",
           wardName=="Hertsmonceux and Pevensey Levels" ~ "Herstmonceux and Pevensey Levels",
           wardName=="Bexhill Sidney" ~ "Bexhill Sidley",
           wardName=="Alkham and Capel-le-Feme" ~ "Alkham and Capel-le-Ferne",
           wardName=="Barming" ~ "Barming and Teston",
           wardName=="Paddock Wood (West)" ~ "Paddock Wood West",
           wardName=="Paddock Wood (East)" ~ "Paddock Wood East",
           wardName=="Oakley and the Candovers" ~ "Oakley and The Candovers",
           wardName=="Town and Crawley Hill" ~ "Town",
           wardName=="Henleyonthames" ~ "Henley-on-Thames",
           wardName=="Hampetersham and Richmond Riverside" ~ "Ham, Petersham and Richmond Riverside",
           wardName=="Bovingdon Flaunden and Chipperfield" ~ "Bovingdon, Flaunden and Chipperfield",
           wardName=="Beltley Heath and the Royds" ~ "Bentley Heath and The Royds",
           wardName=="The Mundens and Cottered" ~ "Mundens and Cottered",
           wardName=="Stanstead Abbotts" ~ "Stanstead Abbots",
           wardName=="Hastingwood Matching and Sheering Village" ~ "Hastingwood, Matching and Sheering Village",
           wardName=="High Ongar, Willingale and the Rodings" ~ "High Ongar, Willingale and The Rodings",
           wardName=="Broomfield and the Walthams" ~ "Broomfield and The Walthams",
           wardName=="Boreham and the Leighs" ~ "Boreham and The Leighs",
           wardName=="Old Heath and the Hythe" ~ "Old Health and The Hythe",
           wardName=="Stadbroke and Laxfield" ~ "Stradbroke and Laxfield",
           wardName=="Aston and  Todwick" ~ "Aston and Todwick",
           TRUE ~ wardName),
         council=case_when(
           substr(wardName,1,9)=="Harrogate" & council=="North Yorkshire" ~ "Harrogate",
           wardName=="Sherburn in Elmet" & council=="North Yorkshire" ~ "Selby",
           wardName=="Bridge" & council=="Dartford" ~ "Bexley",
           wardName %in% c("Droitwith West", "Droitwich East") & council=="Worcestershire" ~ "Wychavon",
           wardName=="Chewton Mendip and Ston Easton" & council=="Mendip" ~ "Bath and North East Somerset",
           wardName=="Crewkerne" & council=="Somerset" ~ "South Somerset",
           wardName=="Pulborough, Coldwaltham and Amberley" & council=="West Sussex" ~ "Horsham",
           council=="Bristol, City of" ~ "Bristol",
           council=="County Durham" ~ "Durham",
           council=="Herefordshire, County of" ~ "Herefordshire",
           council=="Kingston upon Hull, City of" ~ "Kingston upon Hull",
           TRUE ~ council))

#Download ward boundary shapefile for 2020
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/api/v3/datasets/5c11da1763024bd59ef0b6beafa59ae6_0/downloads/data?format=shp&spatialRefId=27700"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

shapefile20 <- st_read(file.path(temp2,"WD_DEC_2020_UK_BFC_V2.shp")) %>% 
  filter(substr(WD20CD, 1, 1)=="E")%>% 
  mutate(WD20NM=gsub("'", "", WD20NM),
         WD20NM=gsub("&", "and", WD20NM),
         WD20NM=gsub("St. ", "St ", WD20NM))

#2021 shapefile
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/api/v3/datasets/72949ed55a424896934147d45f7771ea_0/downloads/data?format=shp&spatialRefId=27700"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

shapefile21 <- st_read(file.path(temp2,"WD_DEC_2021_GB_BFC.shp")) %>% 
  filter(substr(WD21CD, 1, 1)=="E") %>% 
  mutate(WD21NM=gsub("'", "", WD21NM),
         WD21NM=gsub("&", "and", WD21NM),
         WD21NM=gsub("St. ", "St ", WD21NM))

#Download LSOA to Ward lookip from ONS for 2020 ward boundaries
temp <- tempfile()
lsoalookupurl <- "https://opendata.arcgis.com/api/v3/datasets/7a9e4c5e7e8847b8b6a1ac93acd66358_0/downloads/data?format=csv&spatialRefId=4326"
temp <- curl_download(url=lsoalookupurl, destfile=temp, quiet=FALSE, mode="wb")

LSOAlookup20 <- read.csv(temp)

#2021 boundaries
temp <- tempfile()
lsoalookup21url <- "https://www.arcgis.com/sharing/rest/content/items/81bcefcd048e43acb948ad069c5e06c0/data"
temp <- curl_download(url=lsoalookup21url, destfile=temp, quiet=FALSE, mode="wb")

LSOAlookup21 <- read_excel(temp)

#Download IMD data from MHCLG
temp <- tempfile()
IMDurl <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833970/File_1_-_IMD2019_Index_of_Multiple_Deprivation.xlsx"
temp <- curl_download(url=IMDurl, destfile=temp, quiet=FALSE, mode="wb")

IMDdata <- read_excel(temp, sheet="IMD2019") %>% 
  set_names("LSOA11CD", "LSOA Name", "LAD19CD", "LAD19NM", "IMDrank", "IMDdecile")

#Bring in LSOA-level populations
temp <- tempfile()
popurl <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2flowersuperoutputareamidyearpopulationestimates%2fmid2020sape23dt2/sape23dt2mid2020lsoasyoaestimatesunformatted.xlsx"
temp <- curl_download(url=popurl, destfile=temp, quiet=FALSE, mode="wb")

pop <- read_excel(temp, sheet="Mid-2020 Persons", range="A6:G34758", col_names=FALSE) %>% 
  select(-c(2:6)) %>% 
  set_names("LSOA11CD", "pop")

#Combine into one big lookup
#2020
IMDlookup20 <- LSOAlookup20 %>% 
  merge(IMDdata, all=T) %>% 
  merge(pop, all=T) %>% 
  mutate(WD20NM=gsub("&", "and", WD20NM),
         WD20NM=gsub("'", "", WD20NM))

#2021
IMDlookup21 <- LSOAlookup21 %>% 
  merge(IMDdata, all=T) %>% 
  merge(pop, all=T)%>% 
  mutate(WD21NM=gsub("&", "and", WD21NM),
         WD21NM=gsub("'", "", WD21NM))

#Collapse to ward-level deprivation data
IMDwards20 <- IMDlookup20 %>% 
  group_by(WD20CD, WD20NM, LAD20NM) %>% 
  summarise(IMDrank=weighted.mean(IMDrank, pop)) %>% 
  ungroup()%>% 
  filter(substr(WD20CD,1,1)=="E")

IMDwards21 <- IMDlookup21 %>% 
  group_by(WD21CD, WD21NM, LAD21NM) %>% 
  summarise(IMDrank=weighted.mean(IMDrank, pop)) %>% 
  ungroup()%>% 
  filter(substr(WD21CD,1,1)=="E")

#Add in wards missing because they don't have any unique LSOAs
#(this is a big old, imperfectly executed faff for which I make no apologies because 
#English geography is stupid)
miss1 <- shapefile20 %>% 
  select("WD20CD", "WD20NM") %>% 
  st_drop_geometry() %>% 
  as.data.frame()%>% 
  mutate(WD20NM=gsub("&", "and", WD20NM))

miss2 <- IMDwards20 %>% 
  merge(miss1, all=T) %>% 
  filter(is.na(IMDrank)) %>% 
  filter(substr(WD20CD,1,1)=="E")

IMDwards20 <- IMDwards20 %>% 
  bind_rows(miss2) %>% 
  mutate(LAD20NM=case_when(
    WD20CD=="E05011721" ~ LAD20NM[WD20CD=="E05011742"],
    WD20CD=="E05003230" ~ LAD20NM[WD20CD=="E05003223"],
    WD20CD=="E05006760" ~ LAD20NM[WD20CD=="E05006781"],
    WD20CD=="E05006764" ~ LAD20NM[WD20CD=="E05012474"],
    WD20CD %in% c("E05009289", "E05009290", "E05009291", "E05009292", "E05009293", "E05009294", "E05009295", "E05009296",
                  "E05009297", "E05009298", "E05009299", "E05009300", "E05009301", "E05009303", "E05009305", "E05009306",
                  "E05009307", "E05009309", "E05009310", "E05009312") ~ LAD20NM[WD20CD=="E05009304"],
    WD20CD=="E05009880" ~ LAD20NM[WD20CD=="E05009874"],
    WD20CD=="E05010102" ~ LAD20NM[WD20CD=="E05010101"],
    WD20CD=="E05010187" ~ LAD20NM[WD20CD=="E05010209"],
    WD20CD %in% c("E05011090", "E05011091", "E05011092", "E05011094") ~ 
      LAD20NM[WD20CD=="E05011093"],
    WD20CD=="E05011760" ~ LAD20NM[WD20CD=="E05011746"],
    WD20CD=="E05011788" ~ LAD20NM[WD20CD=="E05011809"],
    WD20CD=="E05012048" ~ LAD20NM[WD20CD=="E05012051"],
    WD20CD=="E05012085" ~ LAD20NM[WD20CD=="E05012930"],
    WD20CD=="E05012326" ~ LAD20NM[WD20CD=="E05012355"],
    WD20CD=="E05012394" ~ LAD20NM[WD20CD=="E05011231"],
    WD20CD=="E05012397" ~ LAD20NM[WD20CD=="E05012407"],
    WD20CD=="E05012632" ~ LAD20NM[WD20CD=="E05012642"],
    WD20CD=="E05013830" ~ LAD20NM[WD20CD=="E05011401"],
    WD20CD=="E05013831" ~ LAD20NM[WD20CD=="E05011388"],
    LAD20NM=="Bristol, City of" ~ "Bristol",
    LAD20NM=="County Durham" ~ "Durham",
    LAD20NM=="Herefordshire, County of" ~ "Herefordshire",
    LAD20NM=="Kingston upon Hull, City of" ~ "Kingston upon Hull",
    TRUE~LAD20NM),
    IMDrank=case_when(
      WD20CD=="E05011721" ~ IMDrank[WD20CD=="E05011742"],
      WD20CD=="E05003230" ~ IMDrank[WD20CD=="E05003223"],
      WD20CD=="E05006760" ~ IMDrank[WD20CD=="E05006781"],
      WD20CD=="E05006764" ~ IMDrank[WD20CD=="E05012474"],
      WD20CD %in% c("E05009289", "E05009290", "E05009291", "E05009292", "E05009293", "E05009294", "E05009295", "E05009296",
                    "E05009297", "E05009298", "E05009299", "E05009300", "E05009301", "E05009303", "E05009305", "E05009306",
                    "E05009307", "E05009309", "E05009310", "E05009312") ~ IMDrank[WD20CD=="E05009304"],
      WD20CD=="E05009880" ~ IMDrank[WD20CD=="E05009874"],
      WD20CD=="E05010102" ~ IMDrank[WD20CD=="E05010101"],
      WD20CD=="E05010187" ~ IMDrank[WD20CD=="E05010209"],
      WD20CD %in% c("E05011090", "E05011091", "E05011092", "E05011094") ~ 
        IMDrank[WD20CD=="E05011093"],
      WD20CD=="E05011760" ~ IMDrank[WD20CD=="E05011746"],
      WD20CD=="E05011788" ~ IMDrank[WD20CD=="E05011809"],
      WD20CD=="E05012048" ~ IMDrank[WD20CD=="E05012051"],
      WD20CD=="E05012085" ~ IMDrank[WD20CD=="E05012930"],
      WD20CD=="E05012326" ~ IMDrank[WD20CD=="E05012355"],
      WD20CD=="E05012394" ~ IMDrank[WD20CD=="E05011231"],
      WD20CD=="E05012397" ~ IMDrank[WD20CD=="E05012407"],
      WD20CD=="E05012632" ~ IMDrank[WD20CD=="E05012642"],
      WD20CD=="E05013830" ~ IMDrank[WD20CD=="E05011401"],
      WD20CD=="E05013831" ~ IMDrank[WD20CD=="E05011388"],
      TRUE~IMDrank),
    WD20NM=gsub("'", "", WD20NM),
    WD20NM=gsub("St. ", "St ", WD20NM))

miss3 <- shapefile21 %>% 
  select("WD21CD", "WD21NM") %>% 
  st_drop_geometry() %>% 
  as.data.frame()%>% 
  mutate(WD21NM=gsub("&", "and", WD21NM))
  
miss4 <- IMDwards21 %>% 
  merge(miss3, all=T) %>% 
  filter(is.na(IMDrank)) %>% 
  filter(substr(WD21CD,1,1)=="E")

IMDwards21 <- IMDwards21 %>% 
  bind_rows(miss4) %>% 
  mutate(LAD21NM=case_when(
    WD21CD=="E05011721" ~ LAD21NM[WD21CD=="E05011742"],
    WD21CD=="E05003230" ~ LAD21NM[WD21CD=="E05003223"],
    WD21CD=="E05006760" ~ LAD21NM[WD21CD=="E05006781"],
    WD21CD=="E05006764" ~ LAD21NM[WD21CD=="E05012474"],
    WD21CD %in% c("E05009289", "E05009290", "E05009291", "E05009292", "E05009293", "E05009294", "E05009295", "E05009296",
                  "E05009297", "E05009298", "E05009299", "E05009300", "E05009301", "E05009303", "E05009305", "E05009306",
                  "E05009307", "E05009309", "E05009310", "E05009312") ~ LAD21NM[WD21CD=="E05009304"],
    WD21CD=="E05009880" ~ LAD21NM[WD21CD=="E05009874"],
    WD21CD=="E05010102" ~ LAD21NM[WD21CD=="E05010101"],
    WD21CD=="E05010187" ~ LAD21NM[WD21CD=="E05010209"],
    WD21CD %in% c("E05011090", "E05011091", "E05011092", "E05011094") ~ 
      LAD21NM[WD21CD=="E05011093"],
    WD21CD=="E05011760" ~ LAD21NM[WD21CD=="E05011746"],
    WD21CD=="E05011788" ~ LAD21NM[WD21CD=="E05011809"],
    WD21CD=="E05012048" ~ LAD21NM[WD21CD=="E05012051"],
    WD21CD=="E05012085" ~ LAD21NM[WD21CD=="E05012930"],
    WD21CD=="E05012326" ~ LAD21NM[WD21CD=="E05012355"],
    WD21CD=="E05012394" ~ LAD21NM[WD21CD=="E05011231"],
    WD21CD=="E05012397" ~ LAD21NM[WD21CD=="E05012407"],
    WD21CD=="E05012632" ~ LAD21NM[WD21CD=="E05012642"],
    WD21CD=="E05013001" ~ LAD21NM[WD21CD=="E05013008"],
    WD21CD=="E05013830" ~ LAD21NM[WD21CD=="E05011401"],
    WD21CD=="E05013831" ~ LAD21NM[WD21CD=="E05011388"],
    #LAD21NM=="Bristol, City of" ~ "Bristol",
    #LAD21NM=="County Durham" ~ "Durham",
    #LAD21NM=="Herefordshire, County of" ~ "Herefordshire",
    #LAD21NM=="Kingston upon Hull, City of" ~ "Kingston upon Hull",
    TRUE~LAD21NM),
    IMDrank=case_when(
      WD21CD=="E05011721" ~ IMDrank[WD21CD=="E05011742"],
      WD21CD=="E05003230" ~ IMDrank[WD21CD=="E05003223"],
      WD21CD=="E05006760" ~ IMDrank[WD21CD=="E05006781"],
      WD21CD=="E05006764" ~ IMDrank[WD21CD=="E05012474"],
      WD21CD %in% c("E05009289", "E05009290", "E05009291", "E05009292", "E05009293", "E05009294", "E05009295", "E05009296",
                    "E05009297", "E05009298", "E05009299", "E05009300", "E05009301", "E05009303", "E05009305", "E05009306",
                    "E05009307", "E05009309", "E05009310", "E05009312") ~ IMDrank[WD21CD=="E05009304"],
      WD21CD=="E05009880" ~ IMDrank[WD21CD=="E05009874"],
      WD21CD=="E05010102" ~ IMDrank[WD21CD=="E05010101"],
      WD21CD=="E05010187" ~ IMDrank[WD21CD=="E05010209"],
      WD21CD %in% c("E05011090", "E05011091", "E05011092", "E05011094") ~ 
        IMDrank[WD21CD=="E05011093"],
      WD21CD=="E05011760" ~ IMDrank[WD21CD=="E05011746"],
      WD21CD=="E05011788" ~ IMDrank[WD21CD=="E05011809"],
      WD21CD=="E05012048" ~ IMDrank[WD21CD=="E05012051"],
      WD21CD=="E05012085" ~ IMDrank[WD21CD=="E05012930"],
      WD21CD=="E05012326" ~ IMDrank[WD21CD=="E05012355"],
      WD21CD=="E05012394" ~ IMDrank[WD21CD=="E05011231"],
      WD21CD=="E05012397" ~ IMDrank[WD21CD=="E05012407"],
      WD21CD=="E05012632" ~ IMDrank[WD21CD=="E05012642"],
      WD21CD=="E05013001" ~ IMDrank[WD21CD=="E05013008"],
      WD21CD=="E05013830" ~ IMDrank[WD21CD=="E05011401"],
      WD21CD=="E05013831" ~ IMDrank[WD21CD=="E05011388"],
      TRUE~IMDrank),
    WD21NM=gsub("'", "", WD21NM),
    WD21NM=gsub("St. ", "St ", WD21NM))

#Check IMD data covers entire country
mapdata <- shapefile20 %>% 
  left_join(IMDwards20, by="WD20CD", all=T)

agg_tiff("Outputs/CouncilWardsxIMD.tiff", units="in", width=9, height=8, res=1400)
ggplot(mapdata, aes(geometry=geometry, fill=IMDrank))+
  geom_sf(colour=NA)+
  theme_void()
dev.off()

mapdata2 <- shapefile21 %>% 
  left_join(IMDwards21, by="WD21CD", all=T)

agg_tiff("Outputs/CouncilWardsxIMDv2.tiff", units="in", width=9, height=8, res=1400)
ggplot(mapdata2, aes(geometry=geometry, fill=IMDrank))+
  geom_sf(colour=NA)+
  theme_void()
dev.off()

warddata_full <- warddata %>% 
  select(c("council", "wardName", "next.election.date", "partyName")) %>% 
  merge(IMDwards20, by.x=c("wardName", "council"), by.y=c("WD20NM", "LAD20NM"), all=TRUE) %>% 
  merge(IMDwards21, by.x=c("wardName", "council"), by.y=c("WD21NM", "LAD21NM"), all=TRUE) %>% 
  mutate(IMDrank=if_else(is.na(IMDrank.x), IMDrank.y, IMDrank.x),
    PartyShort=case_when(
    partyName %in% c("Labour Party", "Conservative and Unionist", "Liberal Democrats", "Green Party") ~
      partyName,
    is.na(partyName) ~ NA_character_,
    TRUE ~ "Other"),
    decile=quantcut(IMDrank, q=10, labels=FALSE)) %>% 
  group_by(wardName, council) %>% 
  mutate(councillors=n()) %>% 
  ungroup()

test <- warddata_full %>% 
  filter(is.na(partyName))

waffledata <- warddata_full %>% 
  filter(!is.na(decile) & !is.na(partyName) & !WD21CD %in% c("E05009289", "E05009290", "E05009291", "E05009292", "E05009293", "E05009294", "E05009295", "E05009296",
                                                            "E05009297", "E05009298", "E05009299", "E05009300", "E05009301", "E05009303", "E05009305", "E05009306",
                                                            "E05009307", "E05009309", "E05009310", "E05009312")) %>% 
  mutate(decile=quantcut(IMDrank, q=10, labels=FALSE)) %>% 
  group_by(decile) %>% 
  arrange(IMDrank) %>% 
  mutate(position=c(1:n())) %>% 
  ungroup()

agg_png("Outputs/CouncilWardsxIMD.png", units="in", width=12, height=6, res=800)
ggplot(waffledata, aes(y=as.factor(decile), x=position, fill=PartyShort))+
  geom_tile()+
  scale_x_continuous(name="")+
  scale_y_discrete(name="Index of Multiple Deprivation", labels=c("Most deprived\ndecile", "","","","",
                                                                  "","","","","Least deprived\ndecile"))+
  scale_fill_manual(values=c("#0087DC", "#6AB023", "#E4003B", "#FAA61A", "Grey30"), 
                    na.value="White", name="")+
  theme_custom()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text.x=element_blank(),
        legend.position="top", plot.title=element_text(face="bold", size=rel(2.5), hjust=0,
                                                        margin=margin(0,0,5.5,0)),)+
  labs(title="The politics of inequality",
       subtitle="Party affiliation for current English local councillors, arranged by decile of the Index of Multiple Deprivation.\nWithin each decile, wards are sorted with the most deprived wards on the left and the least deprived on the right.",
       caption="Data from opencouncildata.co.uk, ONS and MHCLG\nPlot by @VictimOfMaths\nInspired by @undertheraedar")
dev.off()

agg_png("Outputs/CouncilWardsxIMDRidges.png", units="in", width=9, height=6, res=800)
ggplot(waffledata, aes(x=IMDrank, y=PartyShort, fill=PartyShort))+
  geom_density_ridges()+
  scale_x_continuous(name="Index of Multiple Deprivation", breaks=c(0,32500), 
                     labels=c("Most\ndeprived", "Least\ndeprived"))+
  scale_y_discrete(name="")+
  scale_fill_manual(values=c("#0087DC", "#6AB023", "#E4003B", "#FAA61A", "Grey30"), 
                    na.value="White", name="")+
  theme_custom()+
  theme(plot.title=element_text(face="bold", size=rel(2.5), hjust=0,
                                 margin=margin(0,0,5.5,0)))+
  labs(title="The politics of inequality",
       subtitle="The distribution of deprivation (as measured by the Index of Multiple Deprivation) for English local council wards\nby political affiliation of current councillors",
       caption="Data from opencouncildata.co.uk, ONS and MHCLG\nPlot by @VictimOfMaths")
dev.off()

mapdata3 <- shapefile20 %>% 
  left_join(warddata_full, by="WD20CD", all=T) %>% 
  filter(!is.na(PartyShort))

mapdata4 <- shapefile21 %>% 
  left_join(warddata_full, by="WD21CD", all=T) %>% 
  filter(!is.na(PartyShort))

agg_png("Outputs/CouncilWardsxParty.png", units="in", width=9, height=8, res=800)
ggplot()+
  geom_sf(data=mapdata4, aes(geometry=geometry, fill=PartyShort), colour=NA)+
  geom_sf(data=mapdata3, aes(geometry=geometry, fill=PartyShort), colour=NA)+
  scale_fill_manual(values=c("#0087DC", "#6AB023", "#E4003B", "#FAA61A", "Grey30"), 
                    na.value="transparent", name="")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(2.5), hjust=0,
                                margin=margin(0,0,5.5,0)),
        text=element_text(family="Lato"),
        plot.subtitle=element_text(colour="Grey40", hjust=0, vjust=1),
        plot.caption=element_text(colour="Grey40", hjust=1, vjust=1, size=rel(0.8)),)+
  labs(title="The lie of the land",
       subtitle="Party affiliation of current English councillors",
       caption="Data from opencouncildata.co.uk & ONS")
dev.off()

agg_png("Outputs/CouncilWardsxPartyMay22.png", units="in", width=9, height=8, res=800)
ggplot()+
  geom_sf(data=mapdata4 %>% filter(next.election.date==as.Date("2022-05-05")), aes(geometry=geometry, fill=PartyShort), colour=NA)+
  geom_sf(data=mapdata3 %>% filter(next.election.date==as.Date("2022-05-05")), aes(geometry=geometry, fill=PartyShort), colour=NA)+
  scale_fill_manual(values=c("#0087DC", "#6AB023", "#E4003B", "#FAA61A", "Grey30"), 
                    na.value="transparent", name="")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(2.5), hjust=0,
                                margin=margin(0,0,5.5,0)),
        text=element_text(family="Lato"),
        plot.subtitle=element_text(colour="Grey40", hjust=0, vjust=1),
        plot.caption=element_text(colour="Grey40", hjust=1, vjust=1, size=rel(0.8)),)+
  labs(title="On the line",
       subtitle="Party affiliation of English councillors whose seats are up for reelection today",
       caption="Data from opencouncildata.co.uk & ONS")
dev.off()


#Download council composition data for 2021 from opencouncildata.co.uk
temp <- tempfile()
councilurl <- "http://opencouncildata.co.uk/csv1.php"
temp <- curl_download(url=councilurl, destfile=temp, quiet=FALSE, mode="wb")

councildata <- read.csv(temp) %>% 
  mutate(next.election.date=as.Date(next.election.date))



#Map differences between 2020 & 2021 ward boundaries
agg_tiff("Outputs/CouncilWardsChanges.tiff", units="in", width=9, height=8, res=1400)
ggplot()+
  geom_sf(data=shapefile, aes(geometry=geometry), colour="red")+
  geom_sf(data=shapefile21, aes(geometry=geometry), colour="Grey30", fill=NA)+
  theme_void()
dev.off()

