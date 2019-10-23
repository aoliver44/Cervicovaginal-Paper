library(tidyverse)
library(vegan)
library(reshape2)

setwd("~/Google Drive File Stream/My Drive/CVF Samples/R_stuff/")

fungal_counts <- read.csv("its_counts.txt", header = T, sep = "\t", row.names = 1)
metadata <- read.csv("whiteson_metadata10_cvx6x.txt", sep = "\t", row.names = 1)
metadata$individual <- as.factor(metadata$PatientID)

fungal_data <- merge(fungal_counts, metadata, by.x = "row.names", by.y = "row.names")
rownames(fungal_data) <- fungal_data$Row.names
fungal_data$Row.names <- NULL
fungal_data$individual <- as.factor(fungal_data$PatientID)

metaphlan <- subset(fungal_data, select = c(1:37))
comm <- decostand(metaphlan, method = "total")
comm_order <- comm[, order(-colMeans(comm))]
comm_collapse <- unite(comm_order, "Other", colnames(comm_order[8]):colnames(rev(comm_order)[1]), remove = TRUE)
comm_collapse$Other <- NULL
comm_collapse$low_abundant <- (1 - rowSums(comm_collapse))
comm_meta <- merge(comm_collapse, metadata, by.x = "row.names", by.y = "row.names")
colnames(comm_meta)[2] <- "s__Candida albicans"
colnames(comm_meta)[3] <- "g__Aspergillus"
colnames(comm_collapse)[1] <- "s__Candida albicans"
colnames(comm_collapse)[2] <- "g__Aspergillus"
bacteria <- as.list(colnames(comm_collapse))

comm_melted <- melt(comm_meta, id=c("individual", "Trimester"), measure.vars = c(bacteria))
cbPalette <- rev(c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))

ggplot(comm_melted, aes(Trimester, y=value, fill = variable)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(.~ individual, scales = "free_y") + ggtitle("Taxa Relative Abundance") + 
  xlab("Individual by Trimester") + 
  ylab("Relative abundance of Fungal OTUs") +
  theme_minimal() + 
  scale_fill_manual(values=cbPalette)


