library(tidyverse)
library(dplyr)

data(iris)


iris_long <- iris %>%
  gather(key = "Attribute", value = "Measurement", -Species) %>%   
  filter(Attribute == "Petal.Length") %>% 
  group_by(Species) %>%  
  summarize(Mean_Petal_Length = mean(Measurement, na.rm = TRUE)) 


print(iris_long)

