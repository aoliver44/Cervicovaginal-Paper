########################################
########## Supp Taxa Barplots ##########
########################################

# load libraries
library(readxl)
library(ggplot2)
library(reshape2)
library(cowplot)
library(tidyverse)

# set working directory & load files
setwd("~/Google Drive File Stream/My Drive/Github/Cervicovaginal-Paper/data/")
set.seed(999)

# Sequence Data: read in sequence data so that samples are in the first column,
# and all other columns are the taxa
midas <- read.csv("16S_rarefied.txt", check.names = FALSE, sep = "\t", row.names = NULL, header = T)

# Metadata if you have it!
metadata <- read.csv("whiteson_metadata10_cvx6x.txt", 
                     sep = "\t", header = T)


## The competition to stephens Taxonomy Solution

# Split taxonomy into different sections L1-L7
# Change where it spilts in seperate from | to ; if needed
midas_melt <- melt(midas, id.vars = "taxonomy") %>% 
  rename(., Subject_ID = variable) %>% 
  separate(., col = taxonomy, into = c("L1","L2","L3","L4","L5","L6","L7"), sep = "\\;", remove = T, extra = "drop")

# Make sure the rel abundance or counts are numeric
midas_melt$value <- as.numeric(midas_melt$value)

# Add in the metadata, make sure you merge on the correct columns!
midas_melt <- merge(metadata, midas_melt, by.x = "wSampleID", by.y = "Subject_ID")

# Summarize by L6 genus (or any other taxa group)...
# if you change make sure you change L6 and prefix (^g_)
# this is also grabbing the top 11 taxa
midas_summarize <- midas_melt %>% 
  group_by(., L5) %>% 
  filter(str_detect(L5, "f_")) %>% 
  summarise(., top_bacteria = sum(value)) %>% 
  arrange(., desc(top_bacteria)) %>% slice(., 1:5)

# group the main players (top 11) together into a list
high_abundance <- split(midas_summarize$L5, 1:NROW(midas_summarize))

# change everything that is not a main player into a other catagory
midas_melt$L5[midas_melt$L5 %in% high_abundance != "TRUE"] <- "other"

# nice color schemes!
stephen_12 <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928')

sarah_color <- c("#7F0A57", "#A64685", "#CD9ABB", "#0B447A", "#3F77AC", "#4176AA", "#74A9DD", "#007976", "#39A9AB", "#71CFC5", "#72D3C6", "#007947", "#3BAA78")

julio_color <- c("#003f5c", "#2f4b7c", "#665191", "#a05195", "#d45087", "#f95d6a", "#ff7c43", "#ffa600", "#7f0a57", "#cd9abb", "#39a9ab", "#71cfc5", "#007947", "#bebebe")
# Plot

ggplot(data = midas_melt, 
       aes(x = as.factor(TrimesterNo), weight = value, fill = L5)) +
  geom_bar(position = position_fill()) +
  theme_bw(base_size = 16) + 
  facet_wrap(. ~  PatientID) + 
  scale_fill_manual(values = stephen_12) +
  theme(panel.spacing = unit(0.1, "lines")) +   
  theme(axis.ticks.x=element_blank()) +
  labs(x = '',
       y = 'Relative Abundance') + #theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent_format()) +
  ggtitle("Family Level, 16S data") +
  theme(plot.title = element_text(hjust = 0.5), plot.margin = unit(c(0.5, 0, 0.5, 0.5), "cm"))


# metagenomics
midas <- t(read.csv("metagenomics.txt", check.names = FALSE, sep = "\t", row.names = 1, header = T))
metagenomics <- melt(midas)
tmp <- metagenomics %>% mutate(., Var3 = ifelse(value < 30, "other", as.character(Var1)))
tmp2 <- tmp %>% group_by(., Var2, Var3) %>% summarise(., abundance = sum(value))
tmp3 <- merge(metadata, tmp2, by.x = "UCIID", by.y = "Var2")
ggplot(data = tmp3) + 
  aes(x = TrimesterNo, weight = abundance, fill = Var3) + 
  geom_bar() + 
  theme_bw() + 
  facet_grid(.~PatientID) + 
  scale_fill_manual(values = stephen_12) + 
  labs(x = 'Trimester', y = 'Relative Abundance')

