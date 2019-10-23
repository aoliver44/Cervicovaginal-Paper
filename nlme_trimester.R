setwd("~/Google Drive File Stream/My Drive/CVF Samples/R_stuff/")

# Super helpful readup on lme: https://folk.uib.no/nzlkj/psychR/day4/04_lme.pdf
# Load up required packages
library(readxl)
library(nlme)
library(lattice)

# linear-mixed effects model of GC metabolite stability across trimesters
# General model structure is lme(response_var ~ Time, random = ~1 | Individual, data = df)
GC <- read_excel("GC-CVF_metabolites_diverse.xlsx")

# Done on first Euclidian Distance axis
mod.GC.stability <- lme(BC_Axis1 ~ Trimester, data = GC, 
                        random = ~ 1|Individual, cor=corAR1())
summary(mod.GC.stability)
anova(mod.GC.stability)

major_shifts1 <- subset(GC, GC$Individual %in% c("1222", "1180", "1103", "1062", "1146", "1126"))
mod.GC.shifts <- lme(BC_Axis1 ~ Trimester, data = major_shifts1, 
                        random = ~ 1|Individual, cor=corAR1())

# getting the R2 (only works in R ver 3.5 or greater)
# library(MuMIn)
# r.squaredGLMM(mod.GC.stability)
# the r2m is the r2 for the fixed factor (trimester if you use the model above). r2c is the entire model.

residuals.GC <- resid(mod.GC.stability)
plot(fitted(mod.GC.stability), residuals.GC)
abline(0,0)
qqnorm(residuals.GC)
qqline(residuals.GC)

# linear-mixed effects model of LC metabolite stability across trimesters

LC <- read_excel("LC-CVF_metabolites_diverse.xlsx")

# Done on first Euclidian Distance axis
major_shifts2 <- subset(LC, LC$Individual %in% c("1222", "1180", "1103", "1062", "1146", "1126"))

mod.LC.stability <- lme(BC_Axis1 ~ Trimester, data = major_shifts2, 
                        random = ~ 1|Individual, cor=corAR1())
summary(mod.LC.stability)
anova(mod.LC.stability)

suerresiduals.LC <- resid(mod.LC.stability)
plot(fitted(mod.LC.stability), residuals.LC)
abline(0,0)
qqnorm(residuals.LC)
qqline(residuals.LC)

# stress stuff...didnt really show anything
library(ggplot2)
ggplot(data = metadata, aes(y = simpson3_16s, x = EMA_PercievedStressScore)) + 
  geom_point(aes(color = DomOTU_16s_sp3)) + geom_smooth(method = "lm")

# linear-mixed effects model OTU table
library(labdsv)
library(vegan)
library(nlme)
library(MuMIn)
OTU <- read_excel("16S_data.xlsx")

# Get PC1
rownames(OTU) <- OTU$X__1
OTU$X__1 <- NULL
OTU_species <- OTU %>% select(., 1:33)
OTU_dist <- avgdist(OTU_species, sample = 1000, iterations = 50, 
                    meanfun = median, dmethod = "bray")
bray_distance <- as.data.frame(as.matrix(OTU_dist))
tmp.pco <- pco(bray_distance, k =2)
metadata <- merge(as.data.frame(tmp.pco$points), OTU, by.x = "row.names", by.y = "row.names")
names(metadata)[2] <- "PCO1"
names(metadata)[3] <- "PCO2"
colnames(metadata)[colnames(metadata)=="16S_dominant"] <- "dominantOrg"
mod.OTU.dominant <- lme(PCO1 ~ dominantOrg, data = metadata, random = ~ 1|Individual, cor=corAR1())
summary(mod.OTU.dominant)
anova(mod.OTU.dominant)
r.squaredGLMM(mod.OTU.dominant)
