options(stringsAsFactors=F)
library(ggplot2)
library(gplots)
library(ggpubr)
library(tibble)
library(dplyr)
library(readr)

#reading in first dataset
xgal <- read.table("C:/Users/cmeli/Desktop/Brem/XGAL NCS/7.28.xgal.ncs.txt",header=T,quote="",sep="\t")
xgal$Species <- sapply(strsplit(xgal$Sample,"_",fixed=T),"[",2)
xgal$Condition <- sapply(strsplit(xgal$Sample,"_",fixed=T),"[",1)
xgal$annot <- paste(xgal$Species,xgal$Condition,sep="_")

xgal1 <- read.table("C:/Users/cmeli/Desktop/Brem/XGAL NCS/8.3.xgal.ncs.txt",header=T,quote="",sep="\t")
xgal1$Species <- sapply(strsplit(xgal1$Sample,"_",fixed=T),"[",2)
xgal1$Condition <- sapply(strsplit(xgal1$Sample,"_",fixed=T),"[",1)
xgal1$annot <- paste(xgal1$Species,xgal1$Condition,sep="_")

xgal2 <- read.table("C:/Users/cmeli/Desktop/Brem/XGAL NCS/10.2.xgal.ncs.txt",header=T,quote="",sep="\t")
xgal2$Species <- sapply(strsplit(xgal2$Sample,"_",fixed=T),"[",2)
xgal2$Condition <- sapply(strsplit(xgal2$Sample,"_",fixed=T),"[",1)
xgal2$annot <- paste(xgal2$Species,xgal2$Condition,sep="_")

xgal3 <- read.table("C:/Users/cmeli/Desktop/Brem/XGAL NCS/10.4.xgal.ncs.txt",header=T,quote="",sep="\t")
xgal3$Species <- sapply(strsplit(xgal3$Sample,"_",fixed=T),"[",2)
xgal3$Condition <- sapply(strsplit(xgal3$Sample,"_",fixed=T),"[",1)
xgal3$annot <- paste(xgal3$Species,xgal3$Condition,sep="_")

xgal4 <- read.table("C:/Users/cmeli/Desktop/Brem/XGAL NCS/12.6.xgal.ncs.txt",header=T,quote="",sep="\t")
xgal4$Species <- sapply(strsplit(xgal4$Sample,"_",fixed=T),"[",2)
xgal4$Condition <- sapply(strsplit(xgal4$Sample,"_",fixed=T),"[",1)
xgal4$annot <- paste(xgal4$Species,xgal4$Condition,sep="_")

xgal5 <- read.table("C:/Users/cmeli/Desktop/Brem/XGAL NCS/1.23.24.xgal.ncs.txt",header=T,quote="",sep="\t")
xgal5$Species <- sapply(strsplit(xgal5$Sample,"_",fixed=T),"[",2)
xgal5$Condition <- sapply(strsplit(xgal5$Sample,"_",fixed=T),"[",1)
xgal5$annot <- paste(xgal5$Species,xgal5$Condition,sep="_")

xgal6 <- read.table("C:/Users/cmeli/Desktop/Brem/XGAL NCS/1.25.24.xgal.ncs.txt",header=T,quote="",sep="\t")
xgal6$Species <- sapply(strsplit(xgal6$Sample,"_",fixed=T),"[",2)
xgal6$Condition <- sapply(strsplit(xgal6$Sample,"_",fixed=T),"[",1)
xgal6$annot <- paste(xgal6$Species,xgal6$Condition,sep="_")

xgal7 <- read.table("C:/Users/cmeli/Desktop/Brem/XGAL NCS/2.15.24.xgal.ncs.txt",header=T,quote="",sep="\t")
xgal7$Species <- sapply(strsplit(xgal7$Sample,"_",fixed=T),"[",2)
xgal7$Condition <- sapply(strsplit(xgal7$Sample,"_",fixed=T),"[",1)
xgal7$annot <- paste(xgal7$Species,xgal7$Condition,sep="_")

xgal8 <- read.table("C:/Users/cmeli/Desktop/Brem/XGAL NCS/2.20.24.xgal.ncs.txt",header=T,quote="",sep="\t")
xgal8$Species <- sapply(strsplit(xgal8$Sample,"_",fixed=T),"[",2)
xgal8$Condition <- sapply(strsplit(xgal8$Sample,"_",fixed=T),"[",1)
xgal8$annot <- paste(xgal8$Species,xgal8$Condition,sep="_")

xgal9 <- read.table("C:/Users/cmeli/Desktop/Brem/XGAL NCS/5.28.24.xgal.ncs.txt",header=T,quote="",sep="\t")
xgal9$Species <- sapply(strsplit(xgal9$Sample,"_",fixed=T),"[",2)
xgal9$Condition <- sapply(strsplit(xgal9$Sample,"_",fixed=T),"[",1)
xgal9$annot <- paste(xgal9$Species,xgal9$Condition,sep="_")

xgal10 <- read.table("C:/Users/cmeli/Desktop/Brem/XGAL NCS/5.29.24.xgal.ncs.txt",header=T,quote="",sep="\t")
xgal10$Species <- sapply(strsplit(xgal10$Sample,"_",fixed=T),"[",2)
xgal10$Condition <- sapply(strsplit(xgal10$Sample,"_",fixed=T),"[",1)
xgal10$annot <- paste(xgal10$Species,xgal10$Condition,sep="_")

xgal11 <- read.table("C:/Users/cmeli/Desktop/Brem/XGAL NCS/6.6.24.xgal.ncs.txt",header=T,quote="",sep="\t")
xgal11$Species <- sapply(strsplit(xgal11$Sample,"_",fixed=T),"[",2)
xgal11$Condition <- sapply(strsplit(xgal11$Sample,"_",fixed=T),"[",1)
xgal11$annot <- paste(xgal11$Species,xgal11$Condition,sep="_")

xgal12 <- read.table("C:/Users/cmeli/Desktop/Brem/XGAL NCS/6.4.24.xgal.ncs.txt",header=T,quote="",sep="\t")
xgal12$Species <- sapply(strsplit(xgal12$Sample,"_",fixed=T),"[",2)
xgal12$Condition <- sapply(strsplit(xgal12$Sample,"_",fixed=T),"[",1)
xgal12$annot <- paste(xgal12$Species,xgal12$Condition,sep="_")

xgal13 <- read.table("C:/Users/cmeli/Desktop/Brem/XGAL NCS/6.11.24.xgal.ncs.txt",header=T,quote="",sep="\t")
xgal13$Species <- sapply(strsplit(xgal13$Sample,"_",fixed=T),"[",2)
xgal13$Condition <- sapply(strsplit(xgal13$Sample,"_",fixed=T),"[",1)
xgal13$annot <- paste(xgal13$Species,xgal13$Condition,sep="_")

xgal14 <- read.table("C:/Users/cmeli/Desktop/Brem/XGAL NCS/7.3.24.xgal.ncs.txt",header=T,quote="",sep="\t")
xgal14$Species <- sapply(strsplit(xgal14$Sample,"_",fixed=T),"[",2)
xgal14$Condition <- sapply(strsplit(xgal14$Sample,"_",fixed=T),"[",1)
xgal14$annot <- paste(xgal14$Species,xgal14$Condition,sep="_")

#combine all replicates
alldatN <- rbind(xgal,xgal1,xgal2,xgal3,xgal4,xgal5,xgal6,xgal7,xgal8,xgal9,xgal10,xgal11,xgal12,xgal13,xgal14)
alldatN$Species <- sapply(strsplit(alldatN$annot,"_",fixed=T),"[",1)
alldatN$Species <- factor(alldatN$Species,levels=c("M.MUS.musP","M.MUS.musB","M.MUS.musK","M.MUS.musM","M.MUS.musC","M.MUS.bl6","M.MUS.domT","M.MUS.domM","M.Spicilegus","M.Spretus.sfm","M.Spretus.stf"))
#alldat$annot <- factor(alldat$annot,levels=c("M.MUS.musP","M.MUS.musB","M.MUS.musK","M.MUS.musM","M.MUS.bl6","M.MUS.domT","M.MUS.domM","M.Spretus.stf","M.Spretus.sfm"))

NCSxgal <- alldatN %>%
  filter(Condition == 'NCS')

controlNCS <- alldatN %>%
  filter(Condition == 'CONT')

  #col <- c("M.MUS.musP_CONT"="dodgerblue4","M.MUS.musP"="dodgerblue2","M.MUS.musB_CONT"="cyan4","M.MUS.musB"="cyan2","M.MUS.musK_CONT"="deepskyblue4","M.MUS.musK"="deepskyblue1","M.MUS.musM_CONT"="blue4","M.MUS.musM"="blue1","M.MUS.bl6_CONT"="darkorchid4","M.MUS.bl6"="darkorchid","M.MUS.domT_CONT"="olivedrab4","M.MUS.domT"="olivedrab1","M.MUS.domM_CONT"="darkgreen","M.MUS.domM"="green","M.Spretus.stf_CONT"="red4","M.Spretus.stf"="red","M.Spretus.sfm_CONT"="pink3","M.Spretus.sfm"="pink")
col <- c(
  "M.MUS.musP_CONT" = "dodgerblue4", "M.MUS.musP" = "dodgerblue2",
  "M.MUS.musB_CONT" = "cyan4", "M.MUS.musB" = "cyan2",
  "M.MUS.musC_CONT" = "deepskyblue4", "M.MUS.musC" = "deepskyblue1",
  "M.MUS.musM_CONT" = "blue4", "M.MUS.musM" = "blue1",
  "M.MUS.bl6_CONT" = "darkorchid4", "M.MUS.bl6" = "darkorchid",
  "M.MUS.domT_CONT" = "olivedrab4", "M.MUS.domT" = "olivedrab1",
  "M.MUS.domM_CONT" = "darkgreen", "M.MUS.domM" = "green",
  "M.Spicilegus_CONT" = "sienna", "M.Spicilegus" = "sienna1",
  "M.Spretus.sfm_CONT"="pink3","M.Spretus.sfm"="pink",
  "M.Spretus.stf_CONT" = "red4", "M.Spretus.stf" = "red"
)

averages <- NCSxgal %>%
  group_by(Condition, Species) %>%
  summarize(mean = mean(XGal.Staining, na.rm = TRUE),
            sd = sd(XGal.Staining, na.rm = TRUE),
            n= n(),
            se = sd / sqrt(n),
            lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
            upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)

ggplot(data = averages , aes(x = Species, y = mean, fill = Species)) +
  geom_bar(stat = "summary", position = "dodge") +
  stat_summary(fun = "mean", geom = "bar", position = "dodge", fun.args = list(mult = 1)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.3, position = position_dodge(0.9))+
  geom_jitter(
    data = NCSxgal,
    aes(x = Species, y = XGal.Staining, fill = Species),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9),
    alpha = 0.6 )+
  #geom_hline(yintercept = mean(alldat$XGal.Staining), linetype = "dashed", size = 0.5, color = "gray") +
  theme(
    text = element_text(size = 20),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_manual(values = col, guide = "none") +
  ylab("% of XGal Staining (NCS)")+
  coord_flip()+
  ylim(0, 100)+
  theme_test()
 

#combined controls --------------------------------------------------------------------------

alldatN <- rbind(xgal,xgal1,xgal2,xgal3,xgal4,xgal5,xgal6,xgal7,xgal8,xgal9,xgal10,xgal11,xgal12,controlIR)
alldatN$Species <- sapply(strsplit(alldatN$annot,"_",fixed=T),"[",1)
alldatN$Species <- factor(alldatN$Species,levels=c("M.MUS.musP","M.MUS.musB","M.MUS.musK","M.MUS.musM","M.MUS.musC","M.MUS.bl6","M.MUS.domT","M.MUS.domM","M.Spicilegus","M.Spretus.sfm","M.Spretus.stf"))

combinedC <- alldatN %>%
  filter(Condition == 'CONT')

control.df <- alldatN %>%
  filter(Condition == 'CONT') %>%
  group_by(Species) %>%
  summarize(XGalcont.mean = mean(XGal.Staining, na.rm = TRUE)) %>%
  ungroup()

# Perform the normalization
alldat.norm <- alldatN %>%
  left_join(control.df, by = c( "Species")) %>%
  mutate(
    XGal.Norm = XGal.Staining / XGalcont.mean
  )

#remove control (1)
removed.cont.alldat.norm <- alldat.norm %>%
  filter(Condition == 'NCS')

ggplot(data = removed.cont.alldat.norm, aes(x = Species, y = XGal.Norm, fill = annot)) +
  geom_bar(stat = "summary", position = "dodge") +
  stat_summary(fun = "mean", geom = "bar", position = "dodge", fun.args = list(mult = 1)) +
  geom_jitter(position = position_dodge(width = 0.9), stat = "identity") +
  theme(
    text = element_text(size = 20),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_manual(values = col, guide = "none") +
  ylab("XGal Staining (Combined Controls - IR)")+
  coord_flip()+
  ylim(0, 4)+
  theme_test()

averages <- removed.cont.alldat.norm %>%
  group_by(Condition, Species) %>%
  summarize(mean = mean(XGal.Staining, na.rm = TRUE),
            sd = sd(XGal.Staining, na.rm = TRUE),
            n= n(),
            se = sd / sqrt(n),
            lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
            upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)

ggplot(data = removed.cont.alldat.norm , aes(x = Species, y = XGal.Staining, fill = Species)) +
  geom_bar(stat = "summary", position = "dodge") +
  stat_summary(fun = "mean", geom = "bar", position = "dodge", fun.args = list(mult = 1)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.3, position = position_dodge(0.9))+
  geom_jitter(position = position_dodge(width = 0.9), stat = "identity") +
  #geom_hline(yintercept = mean(alldat$XGal.Staining), linetype = "dashed", size = 0.5, color = "gray") +
  theme(
    text = element_text(size = 20),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_manual(values = col, guide = "none") +
  ylab("% of XGal Staining (NCS)")+
  coord_flip()+
  ylim(0, 50)+
  theme_test()

ggplot(data = removed.cont.alldat.norm, aes(x = Species, y = XGal.Staining, fill = annot)) +
  geom_bar(stat = "summary", position = "dodge") +
  stat_summary(fun = "mean", geom = "bar", position = "dodge", fun.args = list(mult = 1)) +
  geom_jitter(position = position_dodge(width = 0.9), stat = "identity") +
  theme(
    text = element_text(size = 20),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_manual(values = col, guide = "none") +
  ylab("XGal Staining (Normalized to Controls NCS)")+
  coord_flip()+
  #ylim(0, 4)+
  theme_test()


#normalize by day -----------------------------------------
xgal$which <- "xgal"
xgal1$which <- "xgal1"
xgal2$which <- "xgal2"
xgal3$which <- "xgal3"
xgal4$which <- "xgal4"
xgal5$which <- "xgal5"
xgal6$which <- "xgal6"
xgal7$which <- "xgal7"
xgal8$which <- "xgal8"
xgal9$which <- "xgal9"
xgal10$which <- "xgal10"
xgal11$which <- "xgal11"
xgal12$which <- "xgal12"


alldat <- rbind(xgal,xgal1,xgal2,xgal3,xgal4,xgal5,xgal6,xgal7,xgal8,xgal9,xgal10,xgal11,xgal12)
alldat$Species <- sapply(strsplit(alldat$annot,"_",fixed=T),"[",1)
alldat$Species <- factor(alldat$Species,levels=c("M.MUS.musP","M.MUS.musB","M.MUS.musK","M.MUS.musM","M.MUS.musC","M.MUS.bl6","M.MUS.domT","M.MUS.domM","M.Spicilegus","M.Spretus.sfm","M.Spretus.stf"))

# Create the control data frame
control.df <- alldat %>%
  filter(Condition == 'CONT') %>%
  group_by(Species, which) %>%
  summarize(XGalcont.mean = mean(XGal.Staining, na.rm = TRUE)) %>%
  ungroup()

# Perform the normalization
alldat.norm <- alldat %>%
  left_join(control.df, by = c( "Species", "which")) %>%
  mutate(
    XGal.Norm = XGal.Staining / XGalcont.mean
  )

#remove control (1)
removed.cont.alldat.norm <- alldat.norm %>%
  filter(Condition == 'NCS')

ggplot(data = removed.cont.alldat.norm, aes(x = Species, y = XGal.Norm, fill = annot)) +
  geom_bar(stat = "summary", position = "dodge") +
  stat_summary(fun = "mean", geom = "bar", position = "dodge", fun.args = list(mult = 1)) +
  geom_jitter(position = position_dodge(width = 0.9), stat = "identity") +
  theme(
    text = element_text(size = 20),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_manual(values = col, guide = "none") +
  ylab("XGal Staining (Normalized to Controls)")+
  coord_flip()+
  ylim(0, 5)+
  theme_test()

ncs_data <- subset(alldatN, Condition == "NCS")
ncs_data_subset <- ncs_data[, c("Species", "XGal.Staining")]

spretus_species <- ncs_data_subset %>%
  filter(Species %in% c("M.Spretus.stf", "M.Spretus.sfm"))
#no species name 
spretus_species <- spretus_species %>%
  select(-Species)

others_species <- ncs_data_subset %>%
  filter(Species %in% c("M.Spicilegus"))

           #c("M.MUS.musP","M.MUS.musB","M.MUS.musC","M.MUS.musM","M.MUS.bl6","M.MUS.domT","M.MUS.domM","M.Spicilegus"))
#no species name 
others_species <- others_species %>%
  select(-Species)

#no column name & making it numeric & Wilcox
names(spretus_species) <- NULL
names(others_species) <- NULL
others_species <- as.numeric(unlist(others_species))
spretus_species <- as.numeric(unlist(spretus_species))
wilcox_result <- wilcox.test(others_species, spretus_species,exact = FALSE)
print(wilcox_result)


#anova test with species name & table form 
summary(others_species)
one.way <- aov(XGal.Staining ~ Species, data = others_species)

summary(one.way)

# two factor ANOVA for normalized NCS--------------------------------------

#two factor - do they interact species*treatment
both_species <- alldat.norm #%>%
  #filter(Species %in% c("M.Spretus.stf", "M.MUS.musP"))#"M.Spretus.sfm","M.MUS.musP","M.MUS.musB","M.MUS.musC","M.MUS.musM"))

mod <- aov(XGal.Staining ~ Species * Condition,
           data = both_species
)

# print results
summary(mod)

#Combined C Wilcox --------------------------------------------------------
combinedC_subset <- combinedC[, c("Species", "XGal.Staining")]

spretus_species <- combinedC_subset %>%
  filter(Species %in% c("M.Spretus.stf", "M.Spretus.sfm"))
#no species name 
spretus_species <- spretus_species %>%
  select(-Species)

others_species <- combinedC_subset %>%
  filter(Species %in% c("M.Spicilegus"))
           #c("M.MUS.musP","M.MUS.musB","M.MUS.musC","M.MUS.musM","M.MUS.bl6","M.MUS.domT","M.MUS.domM","M.Spicilegus"))
others_species <- others_species %>%
  select(-Species)

names(spretus_species) <- NULL
names(others_species) <- NULL
others_species <- as.numeric(unlist(others_species))
spretus_species <- as.numeric(unlist(spretus_species))
wilcox_result <- wilcox.test(others_species, spretus_species,exact = FALSE)
print(wilcox_result)

#all data Anova (two way) ---------------------
alldata <- rbind (alldatN)

two.way <-  aov(XGal.Staining ~ Species * Condition, data = alldata)
summary (two.way)

# wilcoxon for normalized NCS ------------------------------------------

spretus_species <- removed.cont.alldat.norm %>%
  filter(Species %in% c("M.Spretus.stf", "M.Spretus.sfm"))
#no species name 
spretus_species <- spretus_species %>%
  select(-Species)

musc_species <- removed.cont.alldat.norm %>%
  filter(Species %in% c("M.MUS.musP","M.MUS.musB","M.MUS.musC","M.MUS.musM","M.Spicilegus"))
#no species name 
musc_species <- musc_species %>%
  select(-Species)

spretus_species <- as.numeric(unlist(spretus_species))
musc_species <- as.numeric(unlist(musc_species))
wilcox_result <- wilcox.test(musc_species, spretus_species,exact = FALSE)
print(wilcox_result)
