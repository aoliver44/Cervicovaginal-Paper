## Fig1: Taxonomic Bar plots

library(ggplot2)
library(reshape2)
library(cowplot)
library(dplyr)
setwd("~/Google Drive File Stream/My Drive/CVF Samples/Rebuttal_analysis/")

L7 <- read.csv("L_7.txt", sep = "\t", header = T, check.names = F)
metadata <- read.csv("whiteson_metadata10_cvx6x.txt", sep = "\t", header = T)
L7_melt <- melt(L7, id.vars = "sampleID")
colnames(L7_melt)[1] <- "brandon_number"
L7_merge_melt <- merge(L7_melt, metadata, by.x = "brandon_number", by.y = "SampleID")

L7_plot_data <- L7_merge_melt %>% select(., 1:11, Taxabarplot_catagories)
L7_plot_data$PatientID <- as.factor(L7_plot_data$PatientID)
L7_plot_data$TrimesterNo <- as.factor(L7_plot_data$TrimesterNo)

stephen_12 <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928')
sarah_color <- c("#7F0A57", "#A64685", "#CD9ABB", "#0B447A", "#3F77AC", "#4176AA", "#74A9DD", "#007976", "#39A9AB", "#71CFC5", "#72D3C6", "#007947", "#3BAA78")

cb12 <- c("#6973d8","#45c097",
          "#bda73a",
          "#553484",
          "#67a54f",
          "#b753a1",
          "#ba4b7d",
          "#af833d",
          "#628dd4",
          "#b95136",
          "#bc7ed4",
          "grey")


l_crispatus <- ggplot(data = subset(L7_plot_data, L7_plot_data$Taxabarplot_catagories == "L. crispatus")) +
  aes(x = TrimesterNo, fill = variable, weight = value) +
  geom_bar() +
  theme_bw() +
  facet_grid(cols = vars(PatientID), space = "free", scales = "free") + scale_fill_manual(values = cb12) +
  guides(fill=guide_legend(title="Bacterial Family")) +
  labs(x = '',
       y = 'Relative Abundance') + theme(legend.position = "none") +
    ggtitle("L. crispatus") +
    theme(plot.title = element_text(hjust = 0.5), plot.margin = unit(c(0.1, 0, 0.1, 0.1), "cm"))

l_iners <- ggplot(data = subset(L7_plot_data, L7_plot_data$Taxabarplot_catagories == "L. iners")) +
  aes(x = TrimesterNo, fill = variable, weight = value) +
  geom_bar() +
  theme_bw() +
  facet_grid(cols = vars(PatientID), space = "free", scales = "free") + scale_fill_manual(values = cb12) +
  ggtitle("L. iners") +
  labs(x = 'Trimester',
       y = '') + theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(), 
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), plot.margin = unit(c(0.1, 0, 0.1, 0.1), "cm"))

g_vaginalis <- ggplot(data = subset(L7_plot_data, L7_plot_data$Taxabarplot_catagories == "G. vaginalis")) +
  aes(x = TrimesterNo, fill = variable, weight = value) +
  geom_bar() +
  theme_bw() +
  facet_grid(cols = vars(PatientID), space = "free", scales = "free") + scale_fill_manual(values = cb12) +
  ggtitle("G. vaginalis") +
  labs(x = '',
       y = '') + theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(), 
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), plot.margin = unit(c(0.1, 0, 0.1, 0.1), "cm"))

other <- ggplot(data = subset(L7_plot_data, L7_plot_data$Taxabarplot_catagories == "other")) +
  aes(x = TrimesterNo, fill = variable, weight = value) +
  geom_bar() +
  theme_bw() +
  facet_grid(cols = vars(PatientID), space = "free", scales = "free") + scale_fill_manual(values = cb12) +
  ggtitle("other") +
  labs(x = '',
       y = '') + guides(fill=guide_legend(title="Bacterial Species")) +
  theme(plot.title = element_text(hjust = 0.5), axis.title.y=element_blank(), 
                                  axis.text.y=element_blank(),
                                  axis.ticks.y=element_blank(), plot.margin = unit(c(0.1, 0, 0.1, 0.1), "cm")) 


# Plot

plot_grid(l_crispatus, l_iners, g_vaginalis, other, labels = NULL, nrow = 1, rel_widths = c(0.7,1,0.5,1))

####### ITS section #############################

metadata_fungi <- metadata
metadata_fungi$PatientID <- factor(metadata$PatientID,levels=c("1088", "1120", "1157", "1191", "1062" , "1111", "1126" ,"1137", "1180", "1198","1222", "1018", "1089", "1146", "1103", "1202"), ordered = T)
metadata_melt <- melt(metadata_fungi, measure.vars = "DomOTU_ITS2", id.vars = c("PatientID", "TrimesterNo", "Taxabarplot_catagories"))
metadata_melt$value <- gsub("none", "Did not amplify", metadata_melt$value)

ggplot(data = metadata_melt, 
       aes(x = as.factor(TrimesterNo), y= as.factor(value))) +
  geom_tile(aes(fill = as.factor(value)), stat = "identity") +
  theme_bw(base_size = 14) +
  ggtitle("") +
  theme(legend.position = "none") +
  labs(x = '', y = '') +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_grid(.~ PatientID, scales = "free_x") + 
  scale_fill_manual(values = c("purple", "blue", "grey")) +
  guides(fill=guide_legend(title="ITS Taxonomy"))

#### Seperately (ITS) #########
Lc_its <- ggplot(data = subset(metadata_melt, metadata_melt$Taxabarplot_catagories == "L. crispatus"), 
       aes(x = as.factor(TrimesterNo), y= as.factor(value))) +
  geom_tile(aes(fill = as.factor(value))) +
  theme_bw() +
  ggtitle("L. crispatus ITS") +
  theme(legend.position = "none") +
  labs(x = '', y = '') +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_grid(cols = vars(PatientID), scales = "free_x") + 
  scale_fill_manual(values = c("purple", "blue", "grey")) +
  guides(fill=guide_legend(title="ITS Taxonomy"))

Li_its <- ggplot(data = subset(metadata_melt, metadata_melt$Taxabarplot_catagories == "L. iners"), 
                 aes(x = as.factor(TrimesterNo), y= as.factor(value))) +
  geom_tile(aes(fill = as.factor(value))) +
  theme_bw() +
  labs(x = '', y = '') +
  ggtitle("L. iners ITS") +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_grid(cols = vars(PatientID), scales = "free_x") + 
  scale_fill_manual(values = c("blue", "grey")) +
  guides(fill=guide_legend(title="ITS Taxonomy"))

gv_its <- ggplot(data = subset(metadata_melt, metadata_melt$Taxabarplot_catagories == "G. vaginalis"), 
                aes(x = as.factor(TrimesterNo), y= as.factor(value))) +
  geom_tile(aes(fill = as.factor(value))) +
  theme_bw() +
  labs(x = '', y = '') +
  ggtitle("G. vaginalis ITS") +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_grid(cols = vars(PatientID), scales = "free_x") + 
  scale_fill_manual(values = c("blue", "grey")) +
  guides(fill=guide_legend(title="ITS Taxonomy"))

other_its <- ggplot(data = subset(metadata_melt, metadata_melt$Taxabarplot_catagories == "other"), 
                 aes(x = as.factor(TrimesterNo), y= as.factor(value))) +
  geom_tile(aes(fill = as.factor(value))) +
  theme_bw() +
  labs(x = '', y = '') +
  ggtitle("other ITS") +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_grid(cols = vars(PatientID), scales = "free_x") + 
  scale_fill_manual(values = c("blue", "grey")) +
  guides(fill=guide_legend(title="ITS Taxonomy"))

plot_grid(Lc_its, Li_its, gv_its, other_its, labels = NULL, nrow = 1, rel_widths = c(0.7,1,0.5,0.5), rel_heights = c(2.1,0.7,0.7,0.7))

