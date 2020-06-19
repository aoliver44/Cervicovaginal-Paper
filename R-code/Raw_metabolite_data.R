# Raw data plots for mannitol and indole-3-lactate

# Load libraries
library(ggplot2)
library(esquisse)
library(cowplot)
library(ggpubr)
library(tidyverse)

# line eqn function
lm_eqn <- function(df, y, x){
  formula = as.formula(sprintf('%s ~ %s', y, x))
  m <- lm(formula, data=df);
  # formating the values into a summary string to print out
  # ~ give some space, but equal size and comma need to be quoted
  eq <- substitute(italic(target) == a + b %.% italic(input)*","~~italic(r)^2~"="~r2*","~~p~"="~italic(pvalue), 
                   list(target = y,
                        input = x,
                        a = format(as.vector(coef(m)[1]), digits = 2), 
                        b = format(as.vector(coef(m)[2]), digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3),
                        # getting the pvalue is painful
                        pvalue = format(summary(m)$coefficients[2,'Pr(>|t|)'], digits=1)
                   )
  )
  as.character(as.expression(eq));                 
}
# set wd
setwd("~/Google Drive File Stream/My Drive/CVF Samples/R_stuff/")

# import data and transpose
raw_data <- read.table("GC_CVF_std_bySample_nounannotated.txt", sep = "\t", row.names = 1, header = T)
bact_comparisons <- list(c("L. crispatus", "G. vaginalis"), c("L. crispatus", "L. iners"), c("L. crispatus", "L.sp.other"))


# make pretty plots

indole <- ggplot(data = raw_data) +
  aes(x = `X16S_dominant`, y = as.numeric(as.character(indole.3.lactate)), fill = `X16S_dominant`) +
  geom_boxplot() + geom_jitter(width = 0.2) +
  labs(x = '',
       y = 'Indole-3-lactate abundance') +
  theme_bw(base_size = 14) + scale_fill_manual(values=c("goldenrod1", "springgreen3", "turquoise3", "grey60")) +
  theme(legend.position = "none")

mannitol <- ggplot(data = raw_data) +
  aes(x = `X16S_dominant`, y = as.numeric(as.character(mannitol)), fill = `X16S_dominant`) +
  geom_boxplot() + geom_jitter(width = 0.2) +
  labs(x = 'Dominant Microbe',
       y = 'Mannitol abundance') +
  theme_bw(base_size = 14) + scale_fill_manual(values=c("goldenrod1", "springgreen3", "turquoise3", "grey60")) +
  theme(legend.position = "none")

glucose <- ggplot(data = raw_data) +
  aes(x = `X16S_dominant`, y = as.numeric(as.character(`glucose.1.phosphate`)), fill = `X16S_dominant`) +
  geom_boxplot() + geom_jitter(width = 0.2) +
  labs(x = 'Dominant Microbe',
       y = 'G1P abundance') +
  theme_bw(base_size = 14) + scale_fill_manual(values=c("goldenrod1", "springgreen3", "turquoise3", "grey60")) +
  theme(legend.position = "none") + 
  stat_compare_means(method = "t.test", comparisons = bact_comparisons)

lactic_acid <- ggplot(data = raw_data) +
  aes(x = `X16S_dominant`, y = as.numeric(as.character(`lactic.acid`)), fill = `X16S_dominant`) +
  geom_boxplot() + geom_jitter(width = 0.2) +
  labs(x = 'Dominant Microbe',
       y = 'Lactic acid abundance') +
  theme_bw(base_size = 14) + scale_fill_manual(values=c("goldenrod1", "springgreen3", "turquoise3", "grey60")) +
  theme(legend.position = "none") 

pyruvate_data <- select(raw_data, `pyruvic.acid`, `X16S_dominant`)
pyruvic_acid <- ggplot(data = pyruvate_data) +
  aes(x = `X16S_dominant`, y = `pyruvic.acid`, fill = `X16S_dominant`) +
  geom_boxplot() + geom_jitter(width = 0.2) +
  labs(x = 'Dominant Microbe',
       y = 'Pyruvic acid abundance') +
  theme_bw(base_size = 14) + scale_fill_manual(values=c("goldenrod1", "springgreen3", "turquoise3", "grey60")) +
  theme(legend.position = "none") + 
  stat_compare_means(method = "t.test", comparisons = bact_comparisons)

pyruvate_v_mannitol <- select(raw_data, `pyruvic acid`, mannitol, `glucose-1-phosphate`, `X16S_dominant`,`lactic acid`, AliqoutID)
tmp <- subset(pyruvate_v_mannitol, `X16S_dominant` == "L. crispatus")
correlation <- ggplot(data = tmp) +
  aes(x = `pyruvic acid`, y = mannitol) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  geom_text(y = 42.5, x = 20, label=lm_eqn(tmp,'`pyruvic acid`','mannitol'),color='red', parse=T) +
  theme_bw(base_size = 14) 

plot_grid(mannitol, pyruvic_acid, correlation, glucose, lactic_acid, labels = "AUTO", ncol = 3, nrow)
