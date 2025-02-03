options(stringsAsFactors=F)
library(ggplot2)
library(gplots)
library(ggpubr)
library(tibble)
library(dplyr)
library(readr)

#reading in first dataset
GLB1F <- read.table("C:/Users/cmeli/Desktop/Brem/GLB1_F1_expression.txt",header=T,quote="",sep="\t")
GLB1F$Species <- sapply(strsplit(GLB1F$Sample,"_",fixed=T),"[",2)
GLB1F$Condition <- sapply(strsplit(GLB1F$Sample,"_",fixed=T),"[",1)
GLB1F$annot <- paste(GLB1F$Species,GLB1F$Condition,sep="_")

col <- c(
  "F1.M.mus_CONT" = "dodgerblue4", "F1.M.Spret_CONT" = "red4", "F1.M.mus_Irrad" = "dodgerblue2", "F1.M.Spret_Irrad" = "red")

ggplot(data = GLB1F, aes(x = Condition, y = Expression, fill = annot)) +
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
  ylab("GLB1 expression in F1 hybrids")+
  #coord_flip()+
  ylim(0, 80)+
  theme_test()

# wilcoxon --------------------------------
musncs_data <- GLB1F %>%
  filter(Sample %in% c("CONT_F1.M.Spret"))
musncs_data_numeric <- as.numeric(musncs_data$Expression)

spretncs_data <- GLB1F %>%
  filter(Sample %in% c("Irrad_F1.M.Spret"))
spretncs_data_numeric <- as.numeric(spretncs_data$Expression)

wilcox_result <- wilcox.test(musncs_data_numeric, spretncs_data_numeric,exact = FALSE)
print(wilcox_result)

#anova----------------------------------------
alldata <- rbind (GLB1F)

two.way <-  aov(Expression ~ Species * Condition, data = alldata)
summary (two.way)


