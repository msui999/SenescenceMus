options(stringsAsFactors=F)
library(ggplot2)
library(gplots)
library(ggpubr)
library(tibble)
library(dplyr)
library(readr)

#reading in first dataset
NCSLyso <- read.table("C:/Users/cmeli/Desktop/Brem/Lyso/NCS Lyso.txt",header=T,quote="",sep="\t")
NCSLyso$Species <- sapply(strsplit(NCSLyso$Sample,"_",fixed=T),"[",2)
NCSLyso$Condition <- sapply(strsplit(NCSLyso$Sample,"_",fixed=T),"[",1)
NCSLyso$annot <- paste(NCSLyso$Species,NCSLyso$Condition,sep="_")

col <- c(
  "CONT_M.MUS.musP" = "dodgerblue2","CONT_M.Spretus.stf" = "red","Irrad_M.MUS.musP" = "dodgerblue2", "Irrad_M.Spretus.stf" = "red", "NCS_M.MUS.musP" = "dodgerblue2", "NCS_M.Spretus.stf" = "red")

allLyso <- rbind (NCSLyso, XrayLyso)

# Calculate summary statistics (mean and standard deviation)
summary_data <- allLyso %>%
  group_by(Sample) %>%
  summarise(
    mean = mean(FitC.Value, na.rm = TRUE),
    se = sd(FitC.Value, na.rm = TRUE) / sqrt(n()) # Calculate standard error
  )

ggplot(data = summary_data, aes(x = Sample, y = mean, fill = Sample)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +  # Use pre-computed means
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.2, position = position_dodge(width = 0.9)) +  # Match dodge width
  geom_jitter(
    data = allLyso,
    aes(x = Sample, y = FitC.Value),  # Use Sample for x to match bars
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9),  # Use jitterdodge
    alpha = 0.8
  ) +
  theme(
    text = element_text(size = 20),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_manual(values = col, guide = "none") +
  ylab("LysoTracker Signal (Normalized to Control M. musculus)") +
  xlab("Condition")+ 
  ylim(0, 11)+
  theme_test()

#--------------------------------------------------------------------------

ggplot(data = allLyso, aes(x = Condition, y = FitC.Value, fill = annot)) +
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
  ylab("LysoTracker Signal (FitC Normalized)")+
  #coord_flip()+
  ylim(0, 11)+
  theme_test()

#Xray --------------------------
XrayLyso <- read.table("C:/Users/cmeli/Desktop/Brem/Lyso/Xray Lyso.txt",header=T,quote="",sep="\t")
XrayLyso$Species <- sapply(strsplit(XrayLyso$Sample,"_",fixed=T),"[",2)
XrayLyso$Condition <- sapply(strsplit(XrayLyso$Sample,"_",fixed=T),"[",1)
XrayLyso$annot <- paste(XrayLyso$Species,XrayLyso$Condition,sep="_")

col <- c(
  "M.MUS.musP_CONT" = "dodgerblue4", "M.MUS.musP_Irrad" = "dodgerblue2",
  "M.Spretus.stf_CONT" = "red4", "M.Spretus.stf_Irrad" = "red")

ggplot(data = XrayLyso, aes(x = Condition, y = FitC.Value, fill = annot)) +
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
  ylab("LysoTracker Signal (Xray - FitC Normalized)")+
  #coord_flip()+
  #ylim(0, 4)+
  theme_test()



#Wilcoxon ---------------------
musncs_data <- allLyso %>%
  filter(Sample %in% c("CONT_M.Spretus.stf"))
musncs_data_numeric <- as.numeric(musncs_data$FitC.Value)

spretncs_data <- allLyso %>%
  filter(Sample %in% c("CONT_M.MUS.musP"))
spretncs_data_numeric <- as.numeric(spretncs_data$FitC.Value)

wilcox_result <- wilcox.test(musncs_data_numeric, spretncs_data_numeric,exact = FALSE)
print(wilcox_result)
