rm(list=ls())

library(data.table)
library(ggplot2)
library(dplyr)
library(ggridges)
library(forcats)

data <- fread("Data/population-by-age-const.csv")

sumdata <- data %>%
  group_by(PCON11NM) %>%
  summarise(MeanAge = weighted.mean(Age_year, Age_pop), MaxPop = max(Age_pop),  
            ModalAge = Age_year[which(Age_pop==MaxPop)][1])

data <- merge(data, sumdata, by="PCON11NM")
data$PCON11NM <- factor(data$PCON11NM)
data$MeanSort <- fct_reorder(data$PCON11NM, data$MeanAge)
data$ModeSort <- fct_reorder(data$PCON11NM, data$ModalAge)

png("Outputs/ConstPop.png", units="in", width=5, height=100, res=500)
ggplot(subset(data, Age_year<90), aes(x=Age_year, height=Age_percent, y=MeanSort))+
  geom_density_ridges(stat="identity", scale=2,  fill="turquoise", size=0.2)+
  theme_classic()+
  scale_x_continuous(name="Age")+
  scale_y_discrete(name="")

dev.off()