# Mannitol operon coverage
library(ggplot2)

setwd("~/Google Drive File Stream/My Drive/CVF Samples/Rebuttal_analysis/")
mannitol_coverage <- read.csv("Mannitol_operon_coverage.txt", sep = "\t")

esquisse::esquisser(mannitol_coverage)

ggplot(data = mannitol_coverage) +
  aes(x = L_crispatus_rel_abundance, y = Mean_coverage, color = Genome_group, shape = Genome_group) +
  geom_point(size = 2.5) +
  labs(x = 'L. crispatus Relative Abundance',
       y = 'Mannitol operon mean coverage') +
  theme_bw(base_size = 14)
