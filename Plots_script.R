## Network examples
rm(list = ls()) # clear memory
setwd("/Users/ful083/Work/papers_being_written/Lenfest_EBFM/Hub_index/")

library(data.table)
library(tidyverse)
library(tidygraph)
library(ggplot2)
library(RColorBrewer)

# Load data
df <- read.table("DepletionTests.csv",header=TRUE, sep=",")

mycolor <- c("sienna1", "deepskyblue4")

ggplot(df, aes(x = SystemType, y = HR_Ratio, fill = SpeciesType)) +
  geom_bar(colour = "black", stat="identity", size = 0.3, position=position_dodge()) +
  geom_errorbar(aes(ymin=HR_Ratio-ConfidenceInterval, ymax=HR_Ratio+ConfidenceInterval), width=.2,
                position=position_dodge(.9)) +
  scale_fill_manual(values = mycolor) + 
  theme_bw() + scale_y_continuous(trans='log10') +
  labs(x="System", y = "Effect Size - Hub / Random Species Depletion") +
  theme(
    axis.text=element_text(size=14,face="bold"), 
    axis.title=element_text(size=18,face="bold"), 
    legend.title=element_text(size=16,face="bold"), 
    legend.text = element_text(size=14)
  )
  
