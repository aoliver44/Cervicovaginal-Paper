library(ggplot2)
library(cowplot)
setwd("~/Google Drive File Stream/My Drive/Github/Cervicovaginal-Paper/data/")

brandon <- read.csv2("whiteson_metadata10_cvx6x.txt", sep = "\t", check.names = F)

give.n <- function(x){
  return(c(y = median(x)*1.05, label = length(x))) 
  # experiment with the multiplier to find the perfect position
}

obs_otu <- ggplot(data = brandon) +
  aes(x = TrimesterNo, y = observed_otus3_16s, group = TrimesterNo) +
  geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.15, colour = "deepskyblue3") + 
  labs(x = 'Trimester',
       y = 'Observed OTUs') +
  theme_classic(base_size = 14) #+
  stat_summary(fun.data = give.n, geom = "text", fun.y = median,
               position = position_dodge(width = 0.75))


Evenness <- ggplot(data = brandon) +
  aes(x = as.factor(TrimesterNo), y = as.numeric(as.character(equitability3_16s))) +
  geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.15, colour = "deepskyblue3") + 
  labs(x = 'Trimester',
       y = 'Evenness') +
  theme_classic(base_size = 14)

PD <- ggplot(data = brandon) +
  aes(x = as.factor(TrimesterNo), y = as.numeric(as.character(PD_whole_tree3_16s))) +
  geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.15, colour = "deepskyblue3") + 
  labs(x = 'Trimester',
       y = 'Phylogenetic Diversity') +
  theme_classic(base_size = 14)

drop_shuttle <- subset(brandon, brandon$DomGen_16s!="Shuttleworthia")

BvsL_even <- ggplot(drop_shuttle) +
  aes(x = DomGen_16s, y = as.numeric(as.character(equitability3_16s))) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(width = 0.15, aes(colour = drop_shuttle$DomGen_16s), show.legend = FALSE) + 
  labs(x = "", y = 'Evenness') +
  theme_classic(base_size = 14) + scale_color_manual(values=c("goldenrod3", "darkseagreen4")) #+
  stat_summary(fun.data = give.n, geom = "text", fun.y = median,
               position = position_dodge(width = 0.75))

cowplot::plot_grid(BvsL_even, obs_otu, Evenness, PD, ncol = 2, labels = NULL)

