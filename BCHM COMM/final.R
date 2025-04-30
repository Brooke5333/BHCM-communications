library(tidyverse)
library(readxl)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
Raw.data <- read_excel("Dataset_R_2025.xlsx")
data <- data.frame(Raw.data)
head(data)
tail(data)
str(data)
data %>%
  gather(key=Variable, value=Values,1:4) %>%
  filter(Chem_name =="dove") %>%
  {. ->> dove} %>%
  group_by(Variable) %>%
  summarise(mean = mean(Values)) %>%
  {. ->> dove}%>%
  ggplot(., aes(x=Variable, y=mean)) + geom_bar(stat="Chem_name") -> plot.dove %>%
head(data.dove)
  

  
  