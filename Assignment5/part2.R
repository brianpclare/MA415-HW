---
title: "part2"
author: "Zirui Liu"
date: "3/21/2018"
---

library(tidyverse)
library(readxl)
library(stringr)
library(ggplot2)

veg <- read_xlsx("veg1.xlsx")
cnames.1 <- colnames(veg)
num_distinct(veg[,1])
num_distinct(veg[,2])

# count each column
count_column <- apply(veg, 2, num_distinct)
count_column1 <- names(c[count_column == 1])
count_column2 <- names(c[count_column > 1])

veg.2 <- select(veg, count_column2)
cnames.2 <- colnames(veg.2)
apply(veg.2, 2, num_distinct)
veg.3 <- dplyr::rename(veg.2, Geo = `Geo Level`, State = `State ANSI`, Data = `Data Item`, Category = `Domain Category`)
cnames.3 <- colnames(veg.3)

unique(veg.3[,"Commodity"])
unique(veg.3[,"Data"]) %>% print(n=60)
unique(veg.3[,"Domain"])
unique(veg.3[,"Category"])
unique(veg.3[,"Value"])
veg_data <- separate(veg.3, Category, into = c("label", "quant"), sep=",")

num_distinct(veg_data[,2])
unique(veg_data[,"label"]) %>% print(n=30)
Restricted_chemical <- filter(veg_data, label=="RESTRICTED USE CHEMICAL")
Restricted_chemical1 <- Restricted_chemical %>% select(label, quant) %>% unique()

# retrieve the data for each restricted chemical
Restricted_chemical2 <- Restricted_chemical1 %>% select(-label) %>% 
  separate(quant, into = c("a", "ID"), sep = "=") %>% 
  separate(a, into = c("D", "Name"), sep = "[()]") %>% 
  select(-D) %>% 
  separate(ID, into = c("ID", "D1"), sep = "[)]") %>% 
  select(-D1)

Restricted_chemical1 %>% print(n=30)

veg_data$quant
veg_data1 <- separate(veg_data, quant, into=c("Treat","Name"), sep = ":")
veg_data2 <- veg_data1 %>% filter(!Value %in% c("(D)",NA,"(Z)","(NA)"))
veg_data2 <- veg_data2 %>% select(-Domain)
veg_data2 <- veg_data2 %>% separate(Data, into = c("a", "Measurement"), sep = "-")
veg_data2 <- veg_data2 %>% select(-a)
veg_data2 <- veg_data2 %>% separate(Measurement, into = c("Measurement", "Unit of Measurement"), sep = ",")
veg_data3 <- veg_data2 %>% separate(Name, into = c("a", "ID"), sep = "=") %>% 
  separate(a, into = c("D", "Name"), sep = "[()]") %>% 
  select(-D) %>% 
  separate(ID, into = c("ID", "D1"), sep = "[)]") %>% 
  select(-D1)

# store data as csv file
write.csv(veg_data3 ,"veg_data3.csv",row.names = FALSE)
write.csv(Restricted_chemical2, "chemical.csv",row.names=F)
veg_data3$Value <- as.numeric(veg_data3$Value)

# read chemical_tox.csv file
chemical_tox <- read.csv("chemical_tox.csv")
chemical_tox <- as.tibble(chemical_tox)
chemical_tox$X <- as.integer(chemical_tox$X)
chemical_tox$Name <- as.character(chemical_tox$Name)
chemical_tox$ID <- as.character(chemical_tox$ID)
chemical_tox <- chemical_tox %>% select(-ID)

# Broccoli
broccoli <- veg_data3 %>% filter(label == "RESTRICTED USE CHEMICAL", Commodity == "BROCCOLI", `Unit of Measurement`==" MEASURED IN LB")
broccoli$Value <- as.numeric(broccoli$Value)
broccoli <- left_join(broccoli,chemical_tox,by="Name")
broccoli <- rename(broccoli,Real=Value,LD50=X)
broccoli <- broccoli %>% gather(Real , LD50 , key="Toxicity", value="value")

ggplot(broccoli, aes(x= Name, y=value )) + 
  geom_bar(stat="identity",position = "dodge",aes(fill=Toxicity)) + 
  labs(y = "Weight(LB) ",x = "Chemical") +
  coord_flip()+
  labs(title="LD50(mg/kg) and Brocoli(lb)")

# Cauliflower
Cauliflower <- veg_data3 %>% filter(label == "RESTRICTED USE CHEMICAL", Commodity == "CAULIFLOWER", `Unit of Measurement`==" MEASURED IN LB")
Cauliflower$Value <- as.numeric(Cauliflower$Value)
Cauliflower <- left_join(Cauliflower,chemical_tox,by="Name")
Cauliflower <- rename(Cauliflower,Real=Value,LD50=X)
Cauliflower <- Cauliflower %>% gather(Real , LD50 , key="Toxicity", value="value")

# plot the Cauliflower 
ggplot(Caul, aes(x= Name, y=value )) + 
  geom_bar(stat="identity",position = "dodge",aes(fill=Toxicity)) + 
  labs(y = "Weight(LB) ",x = "Chemical") +
  coord_flip()+
  labs(title="LD50(mg/kg) and Cauliflower(lb)")
