options(stringsAsFactors=F)
library(ggplot2)
library(gplots)
library(ggpubr)
library(tibble)
library(dplyr)
library(readr)

#reading in first dataset
casp <- read.table("C:/Users/cmeli/Desktop/Brem/ATG/IRRAD/Caspase/8.8.23.atg.irrad.caspase.txt",header=T,quote="",sep="\t")
casp$Species <- sapply(strsplit(casp$Sample,"_",fixed=T),"[",2)
casp$Condition <- sapply(strsplit(casp$Sample,"_",fixed=T),"[",1)
casp$annot <- paste(casp$Species,casp$Condition,sep="_")

casp1 <- read.table("C:/Users/cmeli/Desktop/Brem/ATG/IRRAD/Caspase/8.11.23.atg.irrad.caspase.txt",header=T,quote="",sep="\t")
casp1$Species <- sapply(strsplit(casp1$Sample,"_",fixed=T),"[",2)
casp1$Condition <- sapply(strsplit(casp1$Sample,"_",fixed=T),"[",1)
casp1$annot <- paste(casp1$Species,casp1$Condition,sep="_")

casp2 <- read.table("C:/Users/cmeli/Desktop/Brem/ATG/IRRAD/Caspase/10.19.23.atg.irrad.caspase.txt",header=T,quote="",sep="\t")
casp2$Species <- sapply(strsplit(casp2$Sample,"_",fixed=T),"[",2)
casp2$Condition <- sapply(strsplit(casp2$Sample,"_",fixed=T),"[",1)
casp2$annot <- paste(casp2$Species,casp2$Condition,sep="_")

casp3 <- read.table("C:/Users/cmeli/Desktop/Brem/ATG/IRRAD/Caspase/10.25.23.atg.irrad.caspase.txt",header=T,quote="",sep="\t")
casp3$Species <- sapply(strsplit(casp3$Sample,"_",fixed=T),"[",2)
casp3$Condition <- sapply(strsplit(casp3$Sample,"_",fixed=T),"[",1)
casp3$annot <- paste(casp3$Species,casp3$Condition,sep="_")

casp4 <- read.table("C:/Users/cmeli/Desktop/Brem/ATG/IRRAD/Caspase/10.27.23.atg.irrad.caspase.txt",header=T,quote="",sep="\t")
casp4$Species <- sapply(strsplit(casp4$Sample,"_",fixed=T),"[",2)
casp4$Condition <- sapply(strsplit(casp4$Sample,"_",fixed=T),"[",1)
casp4$annot <- paste(casp4$Species,casp4$Condition,sep="_")

casp5 <- read.table("C:/Users/cmeli/Desktop/Brem/ATG/IRRAD/Caspase/11.2.23.atg.irrad.caspase.txt",header=T,quote="",sep="\t")
casp5$Species <- sapply(strsplit(casp5$Sample,"_",fixed=T),"[",2)
casp5$Condition <- sapply(strsplit(casp5$Sample,"_",fixed=T),"[",1)
casp5$annot <- paste(casp5$Species,casp5$Condition,sep="_")

casp7 <- read.table("C:/Users/cmeli/Desktop/Brem/ATG/IRRAD/Caspase/11.7.23.atg.irrad.caspase.txt",header=T,quote="",sep="\t")
casp7$Species <- sapply(strsplit(casp7$Sample,"_",fixed=T),"[",2)
casp7$Condition <- sapply(strsplit(casp7$Sample,"_",fixed=T),"[",1)
casp7$annot <- paste(casp7$Species,casp7$Condition,sep="_")

casp8 <- read.table("C:/Users/cmeli/Desktop/Brem/ATG/IRRAD/Caspase/1.19.24.atg.irrad.caspase.txt",header=T,quote="",sep="\t")
casp8$Species <- sapply(strsplit(casp8$Sample,"_",fixed=T),"[",2)
casp8$Condition <- sapply(strsplit(casp8$Sample,"_",fixed=T),"[",1)
casp8$annot <- paste(casp8$Species,casp8$Condition,sep="_")

casp9 <- read.table("C:/Users/cmeli/Desktop/Brem/ATG/IRRAD/Caspase/1.23.24.atg.irrad.caspase.txt",header=T,quote="",sep="\t")
casp9$Species <- sapply(strsplit(casp9$Sample,"_",fixed=T),"[",2)
casp9$Condition <- sapply(strsplit(casp9$Sample,"_",fixed=T),"[",1)
casp9$annot <- paste(casp9$Species,casp9$Condition,sep="_")

casp11 <- read.table("C:/Users/cmeli/Desktop/Brem/ATG/IRRAD/Caspase/2.20.24.atg.irrad.caspase.txt",header=T,quote="",sep="\t")
casp11$Species <- sapply(strsplit(casp11$Sample,"_",fixed=T),"[",2)
casp11$Condition <- sapply(strsplit(casp11$Sample,"_",fixed=T),"[",1)
casp11$annot <- paste(casp11$Species,casp11$Condition,sep="_")

casp12 <- read.table("C:/Users/cmeli/Desktop/Brem/ATG/IRRAD/Caspase/2.21.24.atg.irrad.caspase.txt",header=T,quote="",sep="\t")
casp12$Species <- sapply(strsplit(casp12$Sample,"_",fixed=T),"[",2)
casp12$Condition <- sapply(strsplit(casp12$Sample,"_",fixed=T),"[",1)
casp12$annot <- paste(casp12$Species,casp12$Condition,sep="_")

casp13 <- read.table("C:/Users/cmeli/Desktop/Brem/ATG/IRRAD/Caspase/2.27.24.atg.irrad.caspase.txt",header=T,quote="",sep="\t")
casp13$Species <- sapply(strsplit(casp13$Sample,"_",fixed=T),"[",2)
casp13$Condition <- sapply(strsplit(casp13$Sample,"_",fixed=T),"[",1)
casp13$annot <- paste(casp13$Species,casp13$Condition,sep="_")

casp14 <- read.table("C:/Users/cmeli/Desktop/Brem/ATG/IRRAD/Caspase/5.14.24.atg.irrad.caspase.txt",header=T,quote="",sep="\t")
casp14$Species <- sapply(strsplit(casp14$Sample,"_",fixed=T),"[",2)
casp14$Condition <- sapply(strsplit(casp14$Sample,"_",fixed=T),"[",1)
casp14$annot <- paste(casp14$Species,casp14$Condition,sep="_")

casp15 <- read.table("C:/Users/cmeli/Desktop/Brem/ATG/IRRAD/Caspase/5.15.24.atg.irrad.caspase.txt",header=T,quote="",sep="\t")
casp15$Species <- sapply(strsplit(casp15$Sample,"_",fixed=T),"[",2)
casp15$Condition <- sapply(strsplit(casp15$Sample,"_",fixed=T),"[",1)
casp15$annot <- paste(casp15$Species,casp15$Condition,sep="_")

#combine all replicates - don't have casp6, casp10
alldat <- rbind(casp,casp1,casp2,casp3,casp4,casp5,casp7,casp8,casp9,casp11,casp12,casp13,casp14,casp15)
alldat$Species <- sapply(strsplit(alldat$annot,"_",fixed=T),"[",1)
alldat$Species <- factor(alldat$Species,levels=c("M.MUS.musP","M.MUS.musB","M.MUS.musK","M.MUS.musM","M.MUS.musC","M.MUS.bl6","M.MUS.domT","M.MUS.domM","M.Spicilegus","M.Spretus.sfm","M.Spretus.stf"))
#alldat$annot <- factor(alldat$annot,levels=c("M.MUS.musP","M.MUS.musB","M.MUS.musK","M.MUS.musM","M.MUS.bl6","M.MUS.domT","M.MUS.domM","M.Spretus.stf","M.Spretus.sfm"))

alldat.normX <- alldat %>%
  as_tibble() %>%
  left_join(
    alldat %>%
      as_tibble() %>%
      filter(Condition == 'CONT') %>%
      group_by(Species) %>%
      summarize(Caspase.mean = mean(Caspase.Value)) %>%
      ungroup(),
    by = c("Species")
  ) %>%
  mutate(
    Caspase.Norm = Caspase.Value / Caspase.mean)

removed.cont.alldat.norm <- alldat.normX %>%
  filter(Condition == 'IRRAD')

averages <- removed.cont.alldat.norm %>%
  group_by(Condition, Species) %>%
  summarize(mean = mean(Caspase.Norm, na.rm = TRUE),
            sd = sd(Caspase.Norm, na.rm = TRUE),
            n= n(),
            se = sd / sqrt(n),
            lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
            upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)

XrayCasp <- alldat.normX %>%
  filter(Condition == 'IRRAD')

controlXray <- alldat.normX %>%
  filter(Condition == 'CONT')

col <- c(
  "M.MUS.musP_CONT" = "dodgerblue4", "M.MUS.musP_IRRAD" = "dodgerblue2",
  "M.MUS.musB_CONT" = "cyan4", "M.MUS.musB_IRRAD" = "cyan2",
  "M.MUS.musC_CONT" = "deepskyblue4", "M.MUS.musC_IRRAD" = "deepskyblue1",
  "M.MUS.musM_CONT" = "blue4", "M.MUS.musM_IRRAD" = "blue1",
  "M.MUS.bl6_CONT" = "darkorchid4", "M.MUS.bl6_IRRAD" = "darkorchid",
  "M.MUS.domT_CONT" = "olivedrab4", "M.MUS.domT_IRRAD" = "olivedrab1",
  "M.MUS.domM_CONT" = "darkgreen", "M.MUS.domM_IRRAD" = "green",
  "M.Spicilegus_CONT" = "sienna", "M.Spicilegus_IRRAD" = "sienna1",
  "M.Spretus.sfm_CONT"="pink3","M.Spretus.sfm_IRRAD"="pink",
  "M.Spretus.stf_CONT" = "red4", "M.Spretus.stf_IRRAD" = "red"
  
)

ggplot(data = XrayCasp, aes(x = Species, y = Caspase.Norm, fill = annot)) +
  geom_bar(stat = "summary", position = position_dodge(width = 0.9)) +
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
  ylab("Caspase Value") +
  coord_flip() + #(ylim = c(0, 4)) +
  theme_test() 


#normalized ----------------------------------------------
ggplot(data = removed.cont.alldat.norm, aes(x = Species, y = Caspase.Norm, fill = annot)) +
  geom_bar(stat = "summary", position = "dodge") +
  stat_summary(fun = "mean", geom = "bar", position = "dodge", fun.args = list(mult = 1)) +
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
  ylab("Caspase Value")+
  coord_flip()+
  theme_test()#+ coord_cartesian(ylim = c(0, 4))

#error bars ----------------------------------------------

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
  "M.Spretus.stf_CONT" = "red4", "M.Spretus.stf" = "red")

removed.cont.alldat.norm <- removed.cont.alldat.norm %>%
  mutate(Species = factor(Species, levels = names(col)))

averages <- removed.cont.alldat.norm %>%
  group_by(Species) %>%
  summarize(mean = mean(Caspase.Norm, na.rm = TRUE),
            se = sd(Caspase.Norm, na.rm = TRUE) / sqrt(n()))


ggplot(data = averages , aes(x = Species, y = mean, fill = Species)) +
  geom_bar(stat = "summary", position = "dodge") +
  stat_summary(fun = "mean", geom = "bar", position = "dodge", fun.args = list(mult = 1)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.3, position = position_dodge(0.9))+
  geom_jitter(
    data = removed.cont.alldat.norm,
    aes(x = Species, y = Caspase.Norm, fill = Species),
    position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.7),
    alpha = 0.8)+
  #geom_hline(yintercept = mean(alldat$XGal.Staining), linetype = "dashed", size = 0.5, color = "gray") +
  theme(
    text = element_text(size = 20),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_manual(values = col, guide = "none") +
  ylab("Caspase Value (XRAY Norm)")+
  coord_flip()+
  #coord_flip(ylim = c(0, 4)) +
  theme_test()
