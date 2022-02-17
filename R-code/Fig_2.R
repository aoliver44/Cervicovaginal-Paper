library(ggplot2)
library(cowplot)
library(Hmisc)
library(esquisse)
library(nlme)
library(ggrepel)

setwd("~/Google Drive File Stream/My Drive/Github/Cervicovaginal-Paper/data/")

brandon <- read.csv2("whiteson_metadata10_cvx6x.txt", sep = "\t", check.names = F)
brandon$TrimesterNo <- as.factor(brandon$TrimesterNo)
brandon$pc1bc_3 <- as.numeric(brandon$pc1bc_3)
brandon$PatientID <- as.factor(brandon$PatientID)
IDconst <- factor(rep(1, each = length(brandon$PatientID)))

major_shifts <- subset(brandon, brandon$PairID %in% c("P222", "P180", "P103"))
major_shifts1 <- subset(brandon, brandon$PairID %in% c("P222", "P180", "P103", "P062", "P146", "P126"))

View(major_shifts)
shift <- ggplot() +
  geom_point(data = brandon, aes(x = TrimesterNo, y = pc1bc_3, group = PatientID), color = "grey", alpha = 0.3, show.legend = FALSE) + 
  geom_line(data = brandon, aes(x = TrimesterNo, y = pc1bc_3, group = PatientID), color = "grey", alpha = 0.3, show.legend = FALSE) + 
  geom_point(data = major_shifts1, aes(x = TrimesterNo, y = pc1bc_3, group = PatientID), color = "mediumorchid4", show.legend = FALSE) + 
  geom_line(data = major_shifts1, aes(x = TrimesterNo, y = pc1bc_3, group = PatientID), color = "mediumorchid4", show.legend = FALSE) + 
  labs(x = 'Trimester', y = 'PC1') +
  theme_classic(base_size = 14) #+ scale_color_manual(values=c("cyan4", "springgreen1"))

                   
cowplot::plot_grid(shift, labels = "B")

anova(lme(pc1bc_3 ~ as.numeric(TrimesterNo), data = brandon, random = ~ 1|PatientID, cor=corAR1()))

