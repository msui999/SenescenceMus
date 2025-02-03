options(stringsAsFactors=F)
library(ggplot2)
library(gplots)
library(ggpubr)
library(tibble)
library(dplyr)
library(readr)

#reading in first dataset
GLB1MS <- read.table("C:/Users/cmeli/Desktop/Brem/GLB1_expression.txt",header=T,quote="",sep="\t")
GLB1MS$Species <- sapply(strsplit(GLB1MS$Sample,"_",fixed=T),"[",2)
GLB1MS$Condition <- sapply(strsplit(GLB1MS$Sample,"_",fixed=T),"[",1)
GLB1MS$annot <- paste(GLB1MS$Species,GLB1MS$Condition,sep="_")

col <- c(
  "M.MUS.musP_CONT" = "dodgerblue4", "M.Spretus.stf_CONT" = "red4", "M.MUS.musP_Irrad" = "dodgerblue2", "M.Spretus.stf_Irrad" = "red")

ggplot(data = GLB1MS, aes(x = Condition, y = Expression, fill = annot)) +
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
  ylab("GLB1 expression in purebred (reads per million)")+
  #coord_flip()+
  ylim(0, 170)+
  theme_test()

#-------------------

musncs_data <- GLB1MS %>%
  filter(Sample %in% c("Irrad_M.MUS.musP"))
musncs_data_numeric <- as.numeric(musncs_data$Expression)

spretncs_data <- GLB1MS %>%
  filter(Sample %in% c("CONT_M.MUS.musP"))
spretncs_data_numeric <- as.numeric(spretncs_data$Expression)

wilcox_result <- wilcox.test(musncs_data_numeric, spretncs_data_numeric,exact = FALSE)
print(wilcox_result)

#anova ----------------------

alldata <- rbind (GLB1MS)

two.way <-  aov(Expression ~ Species * Condition, data = alldata)
summary (two.way)
