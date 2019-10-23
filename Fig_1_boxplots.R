library(ggplot2)
library(cowplot)
setwd("~/Google Drive File Stream/My Drive/CVF Samples/R_stuff/")

brandon <- read.csv2("whiteson_metadata10_cvx6x.txt", sep = "\t", check.names = F)

obs_otu <- ggplot(data = brandon) +
  aes(x = TrimesterNo, y = observed_otus3_16s) +
  geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.15, colour = "deepskyblue3") + 
  labs(x = 'Trimester',
       y = 'Observed OTUs') +
  theme_classic(base_size = 14)


Evenness <- ggplot(data = brandon) +
  aes(x = TrimesterNo, y = equitability3_16s) +
  geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.15, colour = "deepskyblue3") + 
  labs(x = 'Trimester',
       y = 'Evenness') +
  theme_classic(base_size = 14)

PD <- ggplot(data = brandon) +
  aes(x = TrimesterNo, y = PD_whole_tree3_16s) +
  geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.15, colour = "deepskyblue3") + 
  labs(x = 'Trimester',
       y = 'Phylogenetic Diversity') +
  theme_classic(base_size = 14)

drop_shuttle <- subset(brandon, brandon$DomGen_16s!="Shuttleworthia")

BvsL_even <- ggplot(drop_shuttle) +
  aes(x = DomGen_16s, y = equitability3_16s) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(width = 0.15, aes(colour = drop_shuttle$DomGen_16s), show.legend = FALSE) + 
  labs(x = "", y = 'Evenness') +
  theme_classic(base_size = 14) + scale_color_manual(values=c("goldenrod3", "darkseagreen4"))

cowplot::plot_grid(BvsL_even, obs_otu, Evenness, PD, ncol = 2, labels = c("B","C", "D", "E"))

