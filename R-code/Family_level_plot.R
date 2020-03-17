# Family-level plot Lacto vs Bifido
library(dplyr)
library(reshape2)
library(ggplot2)
setwd("~/Google Drive File Stream/My Drive/CVF Samples/Rebuttal_analysis/")


L5 <- read.csv("L_5.txt", sep = "\t", header = T, check.names = F)
metadata <- read.csv("whiteson_metadata10_cvx6x.txt", sep = "\t", header = T)
L5_melt <- melt(L5, id.vars = "variable")
colnames(L5_melt)[1] <- "brandon_number"
L5_merge_melt <- merge(L5_melt, metadata, by.x = "brandon_number", by.y = "SampleID")

L5_plot_data <- L5_merge_melt %>% select(., 1:11, )
L5_plot_data$PatientID <- as.factor(L5_plot_data$PatientID)
L5_plot_data$TrimesterNo <- as.factor(L5_plot_data$TrimesterNo)

colors <- c("#98DB8B", "#E25256", "#8D57FA", "#E3CA6C", "#569090", "grey")
ggplot(data = L5_plot_data) +
  aes(x = TrimesterNo, fill = variable, weight = value) +
  geom_bar() +
  labs(title = 'Family-Level Taxonomy',
       x = 'Trimester Number',
       y = 'Relative Abundance',
       subtitle = '16S rRNA data') +
  theme_bw(base_size = 16) +
  facet_wrap(vars(PatientID), scales = "free_y") + scale_fill_manual(values = colors) +
  guides(fill=guide_legend(title="Bacterial Family"))



library(shiny)
runGitHub("taxonomy_solution",username = "swandro")