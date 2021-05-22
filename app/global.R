library(plotly)

source("data_prep.R")
source("run_scenario.R")
source("graphics.R")

text <- read.table("data/text.txt", header = FALSE, sep = "|")

text1 <- text$V1[1]