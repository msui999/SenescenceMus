options(stringsAsFactors=F)
library(ggplot2)
library(dplyr)
library(magrittr)
library(gplots)
library(ggpubr)

#reading in first dataset
xgal <- read.table("C:/Users/cmeli/Desktop/Brem/Dose Response/12.6 Dose Response.txt",header=T,quote="",sep="\t")
xgal$Species <- sapply(strsplit(xgal$Sample,"_",fixed=T),"[",2)
xgal$Condition <- sapply(strsplit(xgal$Sample,"_",fixed=T),"[",1)
xgal$annot <- paste(xgal$Species,xgal$Condition,sep="_")
xgal$Condition <- factor(xgal$Condition, levels = c("CONT", "1000", "1500", "2000", "2500"))

xgal1 <- read.table("C:/Users/cmeli/Desktop/Brem/Dose Response/1.30 Dose Response.txt",header=T,quote="",sep="\t")
xgal1$Species <- sapply(strsplit(xgal1$Sample,"_",fixed=T),"[",2)
xgal1$Condition <- sapply(strsplit(xgal1$Sample,"_",fixed=T),"[",1)
xgal1$annot <- paste(xgal1$Species,xgal1$Condition,sep="_")

xgal2 <- read.table("C:/Users/cmeli/Desktop/Brem/Dose Response/5.28 Dose Response.txt",header=T,quote="",sep="\t")
xgal2$Species <- sapply(strsplit(xgal2$Sample,"_",fixed=T),"[",2)
xgal2$Condition <- sapply(strsplit(xgal2$Sample,"_",fixed=T),"[",1)
xgal2$annot <- paste(xgal2$Species,xgal2$Condition,sep="_")

xgal3 <- read.table("C:/Users/cmeli/Desktop/Brem/Dose Response/6.11.24 Dose Response.txt",header=T,quote="",sep="\t")
xgal3$Species <- sapply(strsplit(xgal3$Sample,"_",fixed=T),"[",2)
xgal3$Condition <- sapply(strsplit(xgal3$Sample,"_",fixed=T),"[",1)
xgal3$annot <- paste(xgal2$Species,xgal3$Condition,sep="_")

xgal4 <- read.table("C:/Users/cmeli/Desktop/Brem/Dose Response/6.27.24 Dose Response.txt",header=T,quote="",sep="\t")
xgal4$Species <- sapply(strsplit(xgal4$Sample,"_",fixed=T),"[",2)
xgal4$Condition <- sapply(strsplit(xgal4$Sample,"_",fixed=T),"[",1)
xgal4$annot <- paste(xgal2$Species,xgal4$Condition,sep="_")

#combine all replicates
alldat <- rbind(xgal,xgal1,xgal2,xgal3,xgal4)
alldat$Species <- sapply(strsplit(alldat$annot,"_",fixed=T),"[",1)
alldat$Species <- factor(alldat$Species,levels=c("M.Spretus.stf","M.MUS.musP"))
#alldat$annot <- factor(alldat$annot,levels=c("M.MUS.musP","M.MUS.musB","M.MUS.musK","M.MUS.musM","M.MUS.bl6","M.MUS.domT","M.MUS.domM","M.Spretus.stf","M.Spretus.sfm"))

#col <- c("M.MUS.musP_CONT"="dodgerblue4","M.MUS.musP_NCS"="dodgerblue2","M.MUS.musB_CONT"="cyan4","M.MUS.musB_NCS"="cyan2","M.MUS.musK_CONT"="deepskyblue4","M.MUS.musK_NCS"="deepskyblue1","M.MUS.musM_CONT"="blue4","M.MUS.musM_NCS"="blue1","M.MUS.bl6_CONT"="darkorchid4","M.MUS.bl6_NCS"="darkorchid","M.MUS.domT_CONT"="olivedrab4","M.MUS.domT_NCS"="olivedrab1","M.MUS.domM_CONT"="darkgreen","M.MUS.domM_NCS"="green","M.Spretus.stf_CONT"="red4","M.Spretus.stf_NCS"="red","M.Spretus.sfm_CONT"="pink3","M.Spretus.sfm_NCS"="pink")

# ----------------------------- average trendline with error bars 
averages <- alldat %>%
  group_by(Condition, Species) %>%
  summarize(mean = mean(Percent.Stained, na.rm = TRUE),
            sd = sd(Percent.Stained, na.rm = TRUE),
            n= n(),
            se = sd / sqrt(n),
            lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
            upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)

ggplot(data = alldat, aes(x = Condition, y = Percent.Stained, group=Species)) +
  geom_point(aes(color=Species))+#, position = 'jitter') +
  # geom_ribbon(data = averages, mapping = aes(x = Condition,ymin = lower.ci, ymax=upper.ci, group=Species, fill=Species), alpha=0.5, inherit.aes = F) + 
  #geom_pointrange(data = averages, mapping = aes(x = Condition,y=mean,ymin = lower.ci, ymax=upper.ci, group=Species, color=Species), alpha=0.5, inherit.aes = F) + 
  stat_summary(geom='line', aes (color = Species),fun = mean, size = 1.5) + 
  stat_summary(geom='point', aes (color = Species),fun = mean, size = 2.75,alpha = 0.5) +
  scale_fill_manual("Species", values = c(M.Spretus.stf='red',M.MUS.musP='royalblue')) + 
  scale_color_manual("Species", values = c(M.Spretus.stf='red',M.MUS.musP='royalblue'), guide=guide_none()) + 
  # geom_smooth(data = averages, aes(group = 1), method = "lm", se = FALSE, color = 'red', size = 2) + 
  theme(
    text = element_text(size = 20),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(angle = 45, hjust = 1),
  ) +
  ylim(0, 100) +
  ylab("% of XGal Staining") +
  xlab("XRay Dose (cGy)") +
  theme_test()

#Anova test -------------------------------------------
alldat$Condition <- factor(alldat$Condition, levels = c("CONT", "1000", "1500","2000","2500"))

two.way <-  aov(Percent.Stained ~ Species * Condition, data = alldat)
summary (two.way)

alldatC <-  alldat %>%
  filter(Condition == 'CONT')

combinedC_subset <- alldatC[, c("Species", "Percent.Stained")]

spretus_species <- combinedC_subset %>%
  filter(Species %in% c("M.Spretus.stf"))
#no species name 
spretus_species <- spretus_species %>%
  select(-Species)

others_species <- combinedC_subset %>%
  filter(Species %in% c("M.MUS.musP"))#,"M.MUS.musB","M.MUS.musC","M.MUS.musM","M.MUS.bl6","M.MUS.domT","M.MUS.domM","M.Spicilegus"))
others_species <- others_species %>%
  select(-Species)

names(spretus_species) <- NULL
names(others_species) <- NULL
others_species <- as.numeric(unlist(others_species))
spretus_species <- as.numeric(unlist(spretus_species))
wilcox_result <- wilcox.test(others_species, spretus_species,exact = FALSE)
print(wilcox_result)
