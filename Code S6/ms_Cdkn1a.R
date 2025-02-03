options(stringsAsFactors=F)
library(ggplot2)
library(gplots)
library(ggpubr)
library(tibble)
library(dplyr)
library(readr)

#reading in first dataset
Cdk1MS <- read.table("C:/Users/cmeli/Desktop/Brem/Cdkn1a_expression.txt",header=T,quote="",sep="\t")
Cdk1MS$Species <- sapply(strsplit(Cdk1MS$Sample,"_",fixed=T),"[",2)
Cdk1MS$Condition <- sapply(strsplit(Cdk1MS$Sample,"_",fixed=T),"[",1)
Cdk1MS$annot <- paste(Cdk1MS$Species,Cdk1MS$Condition,sep="_")

col <- c(
  "M.MUS.musP_CONT" = "dodgerblue4", "M.Spretus.stf_CONT" = "red4", "M.MUS.musP_Irrad" = "dodgerblue2", "M.Spretus.stf_Irrad" = "red")

ggplot(data = Cdk1MS, aes(x = Condition, y = Expression, fill = annot)) +
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
  ylab("Cdkn1a expression in purebred (reads per million)")+
  #coord_flip()+
  ylim(0,2000)+
  theme_test()

#--------------
musncs_data <- Cdk1MS %>%
  filter(Sample %in% c("Irrad_M.MUS.musP"))
musncs_data_numeric <- as.numeric(musncs_data$Expression)

spretncs_data <- Cdk1MS %>%
  filter(Sample %in% c("Irrad_M.Spretus.stf"))
spretncs_data_numeric <- as.numeric(spretncs_data$Expression)

wilcox_result <- wilcox.test(musncs_data_numeric, spretncs_data_numeric,exact = FALSE)
print(wilcox_result)

#anova ----------------------

alldata <- rbind (Cdk1MS)

two.way <-  aov(Expression ~ Species * Condition, data = alldata)
summary (two.way)
