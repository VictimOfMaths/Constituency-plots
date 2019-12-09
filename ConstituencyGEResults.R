rm(list=ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)
library(ggtern)

#Read in data
data <- fread("Data/ConstituencyResults.csv")

data$LibLabCon <- data$Con+data$LibDem+data$Lab
data$LibLabConProp <- data$LibLabCon/data$Votes

data$ConProp <- data$Con/(data$Con+data$Lab+data$LibDem)
data$LabProp <- data$Lab/(data$Con+data$Lab+data$LibDem)
data$LibProp <- data$LibDem/(data$Con+data$Lab+data$LibDem)

data$year <- as.numeric(data$year)

#Align constituency names (doesn't fully solve it as some names reordered in data in some years)
data$cons <- gsub("&", "AND", data$cons)
data$cons <- gsub(",", "", data$cons)

#Filter England only data
Engdata <- subset(data, country=="England")

Engdata$Winner <- case_when(
  Engdata$ConProp>Engdata$LabProp & Engdata$ConProp>Engdata$LibProp ~ "Con",
  Engdata$LabProp>Engdata$LibProp~ "Lab",
  TRUE ~ "Lib"
)

#Get data into appropriate wide shape
#Start with 2010 onwards as id variables change between 2005 and 2010
Engdata1017 <- spread(subset(Engdata, year>=2010)[,c("id", "year", "ConProp")], year, ConProp)
colnames(Engdata1017) <- c("id", "ConProp2010", "ConProp2015", "ConProp2017")

temp <- spread(subset(Engdata, year>=2010)[,c("id", "year", "LabProp")], year, LabProp)
colnames(temp) <- c("id", "LabProp2010", "LabProp2015", "LabProp2017")

temp2 <- spread(subset(Engdata, year>=2010)[,c("id", "year", "LibProp")], year, LibProp)
colnames(temp2) <- c("id", "LibProp2010", "LibProp2015", "LibProp2017")

Engdata1017 <- merge(Engdata1017, temp)
Engdata1017 <- merge(Engdata1017, temp2)

Engdata1017 <- merge(Engdata1017, subset(data, year==2017)[,c("id", "cons")], by="id")

Engdata1017$Winner2017 <- case_when(
  Engdata1017$ConProp2017>Engdata1017$LabProp2017 & Engdata1017$ConProp2017>Engdata1017$LibProp2017 ~ "Con",
  Engdata1017$LabProp2017>Engdata1017$LibProp2017 ~ "Lab",
  TRUE ~ "Lib"
)

Engdata1017$Winner2015 <- case_when(
  Engdata1017$ConProp2015>Engdata1017$LabProp2015 & Engdata1017$ConProp2015>Engdata1017$LibProp2015 ~ "Con",
  Engdata1017$LabProp2015>Engdata1017$LibProp2015 ~ "Lab",
  TRUE ~ "Lib"
)

Engdata1017$Winner2010 <- case_when(
  Engdata1017$ConProp2010>Engdata1017$LabProp2010 & Engdata1017$ConProp2010>Engdata1017$LibProp2010 ~ "Con",
  Engdata1017$LabProp2010>Engdata1017$LibProp2010 ~ "Lab",
  TRUE ~ "Lib"
)

#Generate flags for flipped consituencies
Engdata1017$Flip2017 <- ifelse(Engdata1017$Winner2017!=Engdata1017$Winner2015, 1, 0)
Engdata1017$Flip2015 <- ifelse(Engdata1017$Winner2015!=Engdata1017$Winner2010, 1, 0)

#Move onto older data 1983-2005
Engdata8305 <- spread(subset(Engdata, year<2010)[,c("id", "year", "ConProp")], year, ConProp)
colnames(Engdata8305) <- c("id", "ConProp1983", "ConProp1987", "ConProp1992", "ConProp1997",
                           "ConProp2001", "ConProp2005")

temp <- spread(subset(Engdata, year<2010)[,c("id", "year", "LabProp")], year, LabProp)
colnames(temp) <- c("id", "LabProp1983", "LabProp1987", "LabProp1992", "LabProp1997",
                    "LabProp2001", "LabProp2005")

temp2 <- spread(subset(Engdata, year<2010)[,c("id", "year", "LibProp")], year, LibProp)
colnames(temp2) <- c("id", "LibProp1983", "LibProp1987", "LibProp1992", "LibProp1997",
                     "LibProp2001", "LibProp2005")

Engdata8305 <- merge(Engdata8305, temp)
Engdata8305 <- merge(Engdata8305, temp2)

Engdata8305 <- merge(Engdata8305, subset(data, year==2005)[,c("id", "cons")], by="id")

Engdata8305$Winner2005 <- case_when(
  Engdata8305$ConProp2005>Engdata8305$LabProp2005 & Engdata8305$ConProp2005>Engdata8305$LibProp2005 ~ "Con",
  Engdata8305$LabProp2005>Engdata8305$LibProp2005 ~ "Lab",
  TRUE ~ "Lib"
)

Engdata8305$Winner2001 <- case_when(
  Engdata8305$ConProp2001>Engdata8305$LabProp2001 & Engdata8305$ConProp2001>Engdata8305$LibProp2001 ~ "Con",
  Engdata8305$LabProp2001>Engdata8305$LibProp2001 ~ "Lab",
  TRUE ~ "Lib"
)

Engdata8305$Winner1997 <- case_when(
  Engdata8305$ConProp1997>Engdata8305$LabProp1997 & Engdata8305$ConProp1997>Engdata8305$LibProp1997 ~ "Con",
  Engdata8305$LabProp1997>Engdata8305$LibProp1997 ~ "Lab",
  TRUE ~ "Lib"
)

Engdata8305$Winner1992 <- case_when(
  Engdata8305$ConProp1992>Engdata8305$LabProp1992 & Engdata8305$ConProp1992>Engdata8305$LibProp1992 ~ "Con",
  Engdata8305$LabProp1992>Engdata8305$LibProp1992 ~ "Lab",
  TRUE ~ "Lib"
)

Engdata8305$Winner1987 <- case_when(
  Engdata8305$ConProp1987>Engdata8305$LabProp1987 & Engdata8305$ConProp1987>Engdata8305$LibProp1987 ~ "Con",
  Engdata8305$LabProp1987>Engdata8305$LibProp1987 ~ "Lab",
  TRUE ~ "Lib"
)

Engdata8305$Winner1983 <- case_when(
  Engdata8305$ConProp1983>Engdata8305$LabProp1983 & Engdata8305$ConProp1983>Engdata8305$LibProp1983 ~ "Con",
  Engdata8305$LabProp1983>Engdata8305$LibProp1983 ~ "Lab",
  TRUE ~ "Lib"
)

#Generate flags for flipped consituencies
Engdata8305$Flip2005 <- ifelse(Engdata8305$Winner2005!=Engdata8305$Winner2001, 1, 0)
Engdata8305$Flip2001 <- ifelse(Engdata8305$Winner2001!=Engdata8305$Winner1997, 1, 0)
Engdata8305$Flip1997 <- ifelse(Engdata8305$Winner1997!=Engdata8305$Winner1992, 1, 0)
Engdata8305$Flip1992 <- ifelse(Engdata8305$Winner1992!=Engdata8305$Winner1987, 1, 0)
Engdata8305$Flip1987 <- ifelse(Engdata8305$Winner1987!=Engdata8305$Winner1983, 1, 0)

#Stick both together. This loses a few constituencies due to a combination of boundary changes
#and inconsistent names
Fulldata <- merge(Engdata1017, Engdata8305, by="cons", all.x=TRUE)

#Generate 2010 flag
Fulldata$Flip2010 <- ifelse(Fulldata$Winner2010!=Fulldata$Winner2005, 1, 0)

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

tiff("Outputs/201517Tern.tiff", units="in", width=8, height=8, res=500)
ggtern()+
  geom_polygon(data=background, aes(x, y, z,fill=Col), alpha=0.1)+
  theme_hidegrid()+
  theme_showarrows()+
  scale_fill_manual(values=c("#0087dc", "#fdbb30","#d50000"), guide=FALSE)+
  geom_segment(aes(x=0.5, xend=1/3, y=0.5, yend=1/3, z=0, zend=1/3), colour="white")+
  geom_segment(aes(x=0.5, xend=1/3, y=0, yend=1/3, z=0.5, zend=1/3), colour="white")+
  geom_segment(aes(x=0, xend=1/3, y=0.5, yend=1/3, z=0.5, zend=1/3), colour="white")+
  geom_point(data=Engdata1017, aes(z=ConProp2017, y=LibProp2017, x=LabProp2017, colour=Winner2017), 
             alpha=0.2)+
  geom_segment(data=Engdata1017, aes(x=LabProp2015, xend=LabProp2017,
                                     y=LibProp2015, yend=LibProp2017,
                                     z=ConProp2015, zend=ConProp2017, colour=Winner2017), 
               alpha=0.2)+
  geom_point(data=subset(Engdata1017, Flip2017==1), aes(z=ConProp2017, y=LibProp2017, 
                                                        x=LabProp2017, colour=Winner2017))+
  geom_segment(data=subset(Engdata1017, Flip2017==1), aes(x=LabProp2015, xend=LabProp2017,
                                                          y=LibProp2015, yend=LibProp2017,
                                                          z=ConProp2015, zend=ConProp2017, colour=Winner2017))+
  scale_colour_manual(values=c("#0087dc", "#d50000", "#fdbb30"), guide=FALSE)+
  labs(x="", xarrow="Labour vote %", y="", yarrow="Lib Dem vote %", 
       z="", zarrow="Conservative vote %", 
       title="Electoral shifts between 2015 and 2017 General Elections", 
       subtitle="English constituencies only", 
       caption="Data from House of Commons library | Plot by @VictimOfMaths")
dev.off()


tiff("Outputs/201015Tern.tiff", units="in", width=8, height=8, res=500)
ggtern()+
  geom_polygon(data=background, aes(x, y, z,fill=Col), alpha=0.1)+
  theme_hidegrid()+
  theme_showarrows()+
  scale_fill_manual(values=c("#0087dc", "#fdbb30","#d50000"), guide=FALSE)+
  geom_segment(aes(x=0.5, xend=1/3, y=0.5, yend=1/3, z=0, zend=1/3), colour="white")+
  geom_segment(aes(x=0.5, xend=1/3, y=0, yend=1/3, z=0.5, zend=1/3), colour="white")+
  geom_segment(aes(x=0, xend=1/3, y=0.5, yend=1/3, z=0.5, zend=1/3), colour="white")+
  geom_point(data=Engdata1017, aes(z=ConProp2015, y=LibProp2015, x=LabProp2015, colour=Winner2015), 
             alpha=0.2)+
  geom_segment(data=Engdata1017, aes(x=LabProp2010, xend=LabProp2015,
                                     y=LibProp2010, yend=LibProp2015,
                                     z=ConProp2010, zend=ConProp2015, colour=Winner2015), 
               alpha=0.2)+
  geom_point(data=subset(Engdata1017, Flip2017==1), aes(z=ConProp2015, y=LibProp2015, 
                                                        x=LabProp2015, colour=Winner2015))+
  geom_segment(data=subset(Engdata1017, Flip2017==1), aes(x=LabProp2010, xend=LabProp2015,
                                                          y=LibProp2010, yend=LibProp2015,
                                                          z=ConProp2010, zend=ConProp2015, 
                                                          colour=Winner2015))+
  scale_colour_manual(values=c("#0087dc", "#d50000", "#fdbb30"), guide=FALSE)+
  labs(x="", xarrow="Labour vote %", y="", yarrow="Lib Dem vote %", 
       z="", zarrow="Conservative vote %", 
       title="Electoral shifts between 2010 and 2015 General Elections", 
       subtitle="English constituencies only", 
       caption="Data from House of Commons library | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/200510Tern.tiff", units="in", width=8, height=8, res=500)
ggtern()+
  geom_polygon(data=background, aes(x, y, z,fill=Col), alpha=0.1)+
  theme_hidegrid()+
  theme_showarrows()+
  scale_fill_manual(values=c("#0087dc", "#fdbb30","#d50000"), guide=FALSE)+
  geom_segment(aes(x=0.5, xend=1/3, y=0.5, yend=1/3, z=0, zend=1/3), colour="white")+
  geom_segment(aes(x=0.5, xend=1/3, y=0, yend=1/3, z=0.5, zend=1/3), colour="white")+
  geom_segment(aes(x=0, xend=1/3, y=0.5, yend=1/3, z=0.5, zend=1/3), colour="white")+
  geom_point(data=Fulldata, aes(z=ConProp2010, y=LibProp2010, x=LabProp2010, colour=Winner2010), 
             alpha=0.2)+
  geom_segment(data=Fulldata, aes(x=LabProp2005, xend=LabProp2010,
                                  y=LibProp2005, yend=LibProp2010,
                                  z=ConProp2005, zend=ConProp2010, colour=Winner2010), 
               alpha=0.2)+
  geom_point(data=subset(Fulldata, Flip2010==1), aes(z=ConProp2010, y=LibProp2010, 
                                                     x=LabProp2010, colour=Winner2010))+
  geom_segment(data=subset(Fulldata, Flip2010==1), aes(x=LabProp2005, xend=LabProp2010,
                                                       y=LibProp2005, yend=LibProp2010,
                                                       z=ConProp2005, zend=ConProp2010, 
                                                       colour=Winner2010))+
  scale_colour_manual(values=c("#0087dc", "#d50000", "#fdbb30"), guide=FALSE)+
  labs(x="", xarrow="Labour vote %", y="", yarrow="Lib Dem vote %", 
       z="", zarrow="Conservative vote %", 
       title="Electoral shifts between 2005 and 2010 General Elections", 
       subtitle="English constituencies only", 
       caption="Data from House of Commons library | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/200105Tern.tiff", units="in", width=8, height=8, res=500)
ggtern()+
  geom_polygon(data=background, aes(x, y, z,fill=Col), alpha=0.1)+
  theme_hidegrid()+
  theme_showarrows()+
  scale_fill_manual(values=c("#0087dc", "#fdbb30","#d50000"), guide=FALSE)+
  geom_segment(aes(x=0.5, xend=1/3, y=0.5, yend=1/3, z=0, zend=1/3), colour="white")+
  geom_segment(aes(x=0.5, xend=1/3, y=0, yend=1/3, z=0.5, zend=1/3), colour="white")+
  geom_segment(aes(x=0, xend=1/3, y=0.5, yend=1/3, z=0.5, zend=1/3), colour="white")+
  geom_point(data=Engdata8305, aes(z=ConProp2005, y=LibProp2005, x=LabProp2005, colour=Winner2005), 
             alpha=0.2)+
  geom_segment(data=Engdata8305, aes(x=LabProp2001, xend=LabProp2005,
                                     y=LibProp2001, yend=LibProp2005,
                                     z=ConProp2001, zend=ConProp2005, colour=Winner2005), 
               alpha=0.2)+
  geom_point(data=subset(Engdata8305, Flip2005==1), aes(z=ConProp2005, y=LibProp2005, 
                                                        x=LabProp2005, colour=Winner2005))+
  geom_segment(data=subset(Engdata8305, Flip2005==1), aes(x=LabProp2001, xend=LabProp2005,
                                                          y=LibProp2001, yend=LibProp2005,
                                                          z=ConProp2001, zend=ConProp2005, 
                                                          colour=Winner2005))+
  scale_colour_manual(values=c("#0087dc", "#d50000", "#fdbb30"), guide=FALSE)+
  labs(x="", xarrow="Labour vote %", y="", yarrow="Lib Dem vote %", 
       z="", zarrow="Conservative vote %", 
       title="Electoral shifts between 2001 and 2005 General Elections", 
       subtitle="English constituencies only", 
       caption="Data from House of Commons library | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/19972001Tern.tiff", units="in", width=8, height=8, res=500)
ggtern()+
  geom_polygon(data=background, aes(x, y, z,fill=Col), alpha=0.1)+
  theme_hidegrid()+
  theme_showarrows()+
  scale_fill_manual(values=c("#0087dc", "#fdbb30","#d50000"), guide=FALSE)+
  geom_segment(aes(x=0.5, xend=1/3, y=0.5, yend=1/3, z=0, zend=1/3), colour="white")+
  geom_segment(aes(x=0.5, xend=1/3, y=0, yend=1/3, z=0.5, zend=1/3), colour="white")+
  geom_segment(aes(x=0, xend=1/3, y=0.5, yend=1/3, z=0.5, zend=1/3), colour="white")+
  geom_point(data=Engdata8305, aes(z=ConProp2001, y=LibProp2001, x=LabProp2001, colour=Winner2001), 
             alpha=0.2)+
  geom_segment(data=Engdata8305, aes(x=LabProp1997, xend=LabProp2001,
                                     y=LibProp1997, yend=LibProp2001,
                                     z=ConProp1997, zend=ConProp2001, colour=Winner2001), 
               alpha=0.2)+
  geom_point(data=subset(Engdata8305, Flip2001==1), aes(z=ConProp2001, y=LibProp2001, 
                                                        x=LabProp2001, colour=Winner2001))+
  geom_segment(data=subset(Engdata8305, Flip2001==1), aes(x=LabProp1997, xend=LabProp2001,
                                                          y=LibProp1997, yend=LibProp2001,
                                                          z=ConProp1997, zend=ConProp2001, 
                                                          colour=Winner2001))+
  scale_colour_manual(values=c("#0087dc", "#d50000", "#fdbb30"), guide=FALSE)+
  labs(x="", xarrow="Labour vote %", y="", yarrow="Lib Dem vote %", 
       z="", zarrow="Conservative vote %", 
       title="Electoral shifts between 1997 and 2001 General Elections", 
       subtitle="English constituencies only", 
       caption="Data from House of Commons library | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/199297Tern.tiff", units="in", width=8, height=8, res=500)
ggtern()+
  geom_polygon(data=background, aes(x, y, z,fill=Col), alpha=0.1)+
  theme_hidegrid()+
  theme_showarrows()+
  scale_fill_manual(values=c("#0087dc", "#fdbb30","#d50000"), guide=FALSE)+
  geom_segment(aes(x=0.5, xend=1/3, y=0.5, yend=1/3, z=0, zend=1/3), colour="white")+
  geom_segment(aes(x=0.5, xend=1/3, y=0, yend=1/3, z=0.5, zend=1/3), colour="white")+
  geom_segment(aes(x=0, xend=1/3, y=0.5, yend=1/3, z=0.5, zend=1/3), colour="white")+
  geom_point(data=Engdata8305, aes(z=ConProp1997, y=LibProp1997, x=LabProp1997, colour=Winner1997), 
             alpha=0.2)+
  geom_segment(data=Engdata8305, aes(x=LabProp1992, xend=LabProp1997,
                                     y=LibProp1992, yend=LibProp1997,
                                     z=ConProp1992, zend=ConProp1997, colour=Winner1997), 
               alpha=0.2)+
  geom_point(data=subset(Engdata8305, Flip1997==1), aes(z=ConProp1997, y=LibProp1997, 
                                                        x=LabProp1997, colour=Winner1997))+
  geom_segment(data=subset(Engdata8305, Flip1997==1), aes(x=LabProp1992, xend=LabProp1997,
                                                          y=LibProp1992, yend=LibProp1997,
                                                          z=ConProp1992, zend=ConProp1997, 
                                                          colour=Winner1997))+
  scale_colour_manual(values=c("#0087dc", "#d50000", "#fdbb30"), guide=FALSE)+
  labs(x="", xarrow="Labour vote %", y="", yarrow="Lib Dem vote %", 
       z="", zarrow="Conservative vote %", 
       title="Electoral shifts between 1992 and 1997 General Elections", 
       subtitle="English constituencies only", 
       caption="Data from House of Commons library | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/198792Tern.tiff", units="in", width=8, height=8, res=500)
ggtern()+
  geom_polygon(data=background, aes(x, y, z,fill=Col), alpha=0.1)+
  theme_hidegrid()+
  theme_showarrows()+
  scale_fill_manual(values=c("#0087dc", "#fdbb30","#d50000"), guide=FALSE)+
  geom_segment(aes(x=0.5, xend=1/3, y=0.5, yend=1/3, z=0, zend=1/3), colour="white")+
  geom_segment(aes(x=0.5, xend=1/3, y=0, yend=1/3, z=0.5, zend=1/3), colour="white")+
  geom_segment(aes(x=0, xend=1/3, y=0.5, yend=1/3, z=0.5, zend=1/3), colour="white")+
  geom_point(data=Engdata8305, aes(z=ConProp1992, y=LibProp1992, x=LabProp1992, colour=Winner1992), 
             alpha=0.2)+
  geom_segment(data=Engdata8305, aes(x=LabProp1987, xend=LabProp1992,
                                     y=LibProp1987, yend=LibProp1992,
                                     z=ConProp1987, zend=ConProp1992, colour=Winner1992), 
               alpha=0.2)+
  geom_point(data=subset(Engdata8305, Flip1992==1), aes(z=ConProp1992, y=LibProp1992, 
                                                        x=LabProp1992, colour=Winner1992))+
  geom_segment(data=subset(Engdata8305, Flip1992==1), aes(x=LabProp1987, xend=LabProp1992,
                                                          y=LibProp1987, yend=LibProp1992,
                                                          z=ConProp1987, zend=ConProp1992, 
                                                          colour=Winner1992))+
  scale_colour_manual(values=c("#0087dc", "#d50000", "#fdbb30"), guide=FALSE)+
  labs(x="", xarrow="Labour vote %", y="", yarrow="Lib Dem vote %", 
       z="", zarrow="Conservative vote %", 
       title="Electoral shifts between 1987 and 1992 General Elections", 
       subtitle="English constituencies only", 
       caption="Data from House of Commons library | Plot by @VictimOfMaths")
dev.off()

tiff("Outputs/198387Tern.tiff", units="in", width=8, height=8, res=500)
ggtern()+
  geom_polygon(data=background, aes(x, y, z,fill=Col), alpha=0.1)+
  theme_hidegrid()+
  theme_showarrows()+
  scale_fill_manual(values=c("#0087dc", "#fdbb30","#d50000"), guide=FALSE)+
  geom_segment(aes(x=0.5, xend=1/3, y=0.5, yend=1/3, z=0, zend=1/3), colour="white")+
  geom_segment(aes(x=0.5, xend=1/3, y=0, yend=1/3, z=0.5, zend=1/3), colour="white")+
  geom_segment(aes(x=0, xend=1/3, y=0.5, yend=1/3, z=0.5, zend=1/3), colour="white")+
  geom_point(data=Engdata8305, aes(z=ConProp1987, y=LibProp1987, x=LabProp1987, colour=Winner1987), 
             alpha=0.2)+
  geom_segment(data=Engdata8305, aes(x=LabProp1983, xend=LabProp1987,
                                     y=LibProp1983, yend=LibProp1987,
                                     z=ConProp1983, zend=ConProp1987, colour=Winner1987), 
               alpha=0.2)+
  geom_point(data=subset(Engdata8305, Flip1987==1), aes(z=ConProp1987, y=LibProp1987, 
                                                        x=LabProp1987, colour=Winner1987))+
  geom_segment(data=subset(Engdata8305, Flip1987==1), aes(x=LabProp1983, xend=LabProp1987,
                                                          y=LibProp1983, yend=LibProp1987,
                                                          z=ConProp1983, zend=ConProp1987, 
                                                          colour=Winner1987))+
  scale_colour_manual(values=c("#0087dc", "#d50000", "#fdbb30"), guide=FALSE)+
  labs(x="", xarrow="Labour vote %", y="", yarrow="Lib Dem vote %", 
       z="", zarrow="Conservative vote %", 
       title="Electoral shifts between 1983 and 1987 General Elections", 
       subtitle="English constituencies only", 
       caption="Data from House of Commons library | Plot by @VictimOfMaths")
dev.off()

#Constituency-specfic path
const <- "SHEFFIELD HALLAM"

tiff("Outputs/HallamTern.tiff", units="in", width=8, height=8, res=500)
ggtern()+
  geom_polygon(data=background, aes(x, y, z,fill=Col), alpha=0.1)+
  theme_hidegrid()+
  theme_showarrows()+
  scale_fill_manual(values=c("#0087dc", "#fdbb30","#d50000"), guide=FALSE)+
  geom_segment(aes(x=0.5, xend=1/3, y=0.5, yend=1/3, z=0, zend=1/3), colour="white")+
  geom_segment(aes(x=0.5, xend=1/3, y=0, yend=1/3, z=0.5, zend=1/3), colour="white")+
  geom_segment(aes(x=0, xend=1/3, y=0.5, yend=1/3, z=0.5, zend=1/3), colour="white")+
  geom_path(data=subset(Engdata, cons==const), aes(x=LabProp, y=LibProp, z=ConProp),
            arrow = arrow(angle = 15, ends="first", type = "closed", length=unit(0.1,"inches")))+
  geom_point(data=subset(Engdata, cons==const), aes(x=LabProp, y=LibProp, z=ConProp, colour=Winner))+
  scale_colour_manual(values=c(Con="#0087dc", Lab="#d50000", Lib="#fdbb30"), guide=FALSE)+
  labs(x="", xarrow="Labour vote %", y="", yarrow="Lib Dem vote %", 
       z="", zarrow="Conservative vote %", 
       title=paste("Electoral shifts in", const, "between 1983 and 2017 General Elections"), 
       caption="Data from House of Commons library | Plot by @VictimOfMaths")
dev.off()
