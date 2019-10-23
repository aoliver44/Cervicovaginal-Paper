## Manni-deg Figure
library(ggplot2)
library(ggpubr)

setwd("~/Google Drive File Stream/My Drive/CVF Samples/R_stuff/")
humann_mannideg <- read.csv("manni-deg-renorm.txt", sep = "\t")
metadata <- read.csv("whiteson_metadata10_cvx6x.txt", sep = "\t")

manni_meta <- merge(humann_mannideg, metadata, by.x = "X.PWY", by.y = "SampleID")
compare <- list(c("L_crispatus", "L_iners"), c("L_crispatus", "Bifidobacteriaceae"), c("L_crispatus", "other"))

ggplot(data = manni_meta) +
       aes(x = Dom_microbe, y = MANNIDEG.PWY, fill = Dom_microbe) +
       geom_boxplot() +
       labs(title = 'ManniDeg PWY',
                       x = 'Dominant Microbe',
                       y = 'Normalized Relative Abundance') +
       theme_bw(base_size = 18) + stat_compare_means(method = "t.test", comparisons = compare, label = "p.format")

