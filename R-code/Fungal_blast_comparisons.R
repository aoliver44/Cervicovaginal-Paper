# Family-level plot Lacto vs Bifido
library(dplyr)
library(ggplot2)

# import the ITS data
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

# import fungal blast data and combine
setwd("~/Google Drive File Stream/My Drive/CVF Samples/Rebuttal_analysis/")
metadata <- read.csv("whiteson_metadata10_cvx6x.txt", sep = "\t", header = T)
fungal_blast <- read.csv("Fungal_results.txt", sep = "\t", header = T)
# this is just comparing samples that amplifed for 16S...Not all the metagenomes
#fungal_comparisons <- merge(fungal_blast, comm_meta, by.x = "Sample", by.y = "Row.names")
fungal_comparisons <- merge(fungal_blast, metadata, by.x = "Sample", by.y = "SampleID")

# vizualize
ggplot(data = fungal_comparisons, aes(x = Species, weight = log2(Normalized_counts), na.rm = TRUE, fill = AtLeast1_CanDom)) +
  geom_bar(position = "dodge") +
  labs(y = 'log2(Normalized Read Counts)') +
  theme_classic(base_size = 14) +
  facet_wrap(vars(PatientID)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  guides(fill=guide_legend(title="At least 1 sample\nwith Candida"))
