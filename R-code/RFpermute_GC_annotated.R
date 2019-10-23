# Supp random forest graphs

# set working directory
setwd("/Users/andrewoliver/Google Drive File Stream/My Drive/CVF Samples/R_stuff")

# load up libraries
library(rfPermute)

# set seed
set.seed(seed = 999)

# load in data, normalized by sample. Only GC annotated metabolites. Labled by 
# dominant organism
GC_RF <- read.csv("RF_test_gc_by_sample.txt", sep = "\t")

# run random forest
CVF_RFP <- rfPermute(Dominant ~ ., data = GC_RF, proximity = TRUE, importance = TRUE)

# plot plots
proximityPlot(CVF_RFP)
varImpPlot(CVF_RFP, type = 1)
plotConfMat(CVF_RFP)
tmp_plot <- impHeatmap(CVF_RFP, alpha = 0.05, n = 30, ranks = F)
tmp_plot + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot(CVF_RFP, alpha = 0.05)


