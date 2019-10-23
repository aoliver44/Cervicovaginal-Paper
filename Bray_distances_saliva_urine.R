# bray curtis saliva urine analysis
setwd("~/Google Drive File Stream/My Drive/CVF Samples/Manuscript/Data/Bray_distances_saliva_urine/")

saliva <- read.csv("Saliva_bc_distances.txt", sep = "\t")


ggplot(data = saliva) +
  aes(x = direction, y = Avg_bc, fill = direction) +
  geom_boxplot() +
  labs(y = 'Average Bray Curtis Similarity', x = "") +
  theme_bw(base_size = 16) +
  facet_grid(Sample.Type ~ Chromatog) +
  stat_compare_means(method = "t.test", paired = T, label.x.npc = 0.8) + 
  theme(legend.position = "none")

