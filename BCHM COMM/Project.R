library(tidyverse)
library(readxl)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
Raw.data <- read_excel("Dataset_R_2025.xlsx")
