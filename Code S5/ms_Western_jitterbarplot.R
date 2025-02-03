options(stringsAsFactors=F)
library(ggplot2)
library(gplots)
library(ggpubr)
library(tibble)
library(dplyr)
library(readr)
library(rstatix)


westncs <- read.table("C:/Users/cmeli/Desktop/Brem/Western Blots/8.23.NCS.txt",header=T,quote="",sep="\t")
westncs$Sample <- factor(westncs$Sample, levels = c("Mus Control", "Spret Control", "Mus NCS", "Spret NCS"))
westncs$Species <- sapply(strsplit(westncs$Sample," ",fixed=T),"[",1)
westncs$Condition <- sapply(strsplit(westncs$Sample," ",fixed=T),"[",2)
westncs$annot <- paste(westncs$Species,westncs$Condition,sep="_")


col <- c(
  "Mus Control" = "dodgerblue2", "Spret Control" = "red","Mus NCS" = "dodgerblue2", "Spret NCS" = "red",
  "Mus Xray" = "dodgerblue2", "Spret Xray" = "red"
)


allWest <- rbind (westncs, westxray)


# Calculate summary statistics (mean and standard deviation)
summary_data <- allWest %>%
  group_by(Sample) %>%
  summarise(
    mean = mean(Ratio.TUB, na.rm = TRUE),
    se = sd(Ratio.TUB, na.rm = TRUE) / sqrt(n()) # Calculate standard error
  )

# Plot
ggplot(data = summary_data, aes(x = Sample, y = mean, fill = Sample)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.2, position = position_dodge(0.9)) +
  geom_jitter(data = allWest, aes(x = Sample, y = Ratio.TUB), position = position_dodge(width = 0.9), color = "black") +
  theme(
    text = element_text(size = 20),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_manual(values = col, guide = "none") +
  ylab("Ratio (p21/TUB)") +
  ylim(0,3)+
  theme_test()

#______________________________________________________________________________________________________

westxray <- read.table("C:/Users/cmeli/Desktop/Brem/Western Blots/8.28.Xray.txt",header=T,quote="",sep="\t")
westxray$Sample <- factor(westxray$Sample, levels = c("Mus Control", "Spret Control", "Mus Xray", "Spret Xray"))
westxray$Species <- sapply(strsplit(westxray$Sample," ",fixed=T),"[",1)
westxray$Condition <- sapply(strsplit(westxray$Sample," ",fixed=T),"[",2)
westxray$annot <- paste(westxray$Species,westxray$Condition,sep="_")

col <- c(
  "Mus Control" = "dodgerblue4", "Spret Control" = "red4","Mus Xray" = "dodgerblue2", "Spret Xray" = "red"
)


# Calculate summary statistics (mean and standard deviation)
summary_data <- westxray %>%
  group_by(Sample) %>%
  summarise(
    mean = mean(Ratio.TUB, na.rm = TRUE),
    se = sd(Ratio.TUB, na.rm = TRUE) / sqrt(n()) # Calculate standard error
  )

# Plot
ggplot(data = summary_data, aes(x = Sample, y = mean, fill = Sample)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.2, position = position_dodge(0.9)) +
  geom_jitter(data = westxray, aes(x = Sample, y = Ratio.TUB), position = position_dodge(width = 0.9), color = "black") +
  theme(
    text = element_text(size = 20),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_fill_manual(values = col, guide = "none") +
  ylab("Ratio (p21/TUB)") +
  theme_test()

#------------------------------------------------------------------------------------------------
musncs_data <- westncs %>%
  filter(Sample %in% c("Spret Control"))
musncs_data_numeric <- as.numeric(musncs_data$Ratio.TUB)

spretncs_data <- westncs %>%
  filter(Sample %in% c("Spret NCS"))
spretncs_data_numeric <- as.numeric(spretncs_data$Ratio.TUB)

wilcox_result <- wilcox.test(musncs_data_numeric, spretncs_data_numeric,exact = FALSE)
print(wilcox_result)

#------------------------------------------------------------------------------------------------
musxray_data <- westxray %>%
  filter(Sample %in% c("Spret Control"))
musxray_data_numeric <- as.numeric(musxray_data$Ratio.TUB)

spretxray_data <- westxray %>%
  filter(Sample %in% c("Spret Xray"))
spretxray_data_numeric <- as.numeric(spretxray_data$Ratio.TUB)

wilcox_result <- wilcox.test(musxray_data_numeric, spretxray_data_numeric,exact = FALSE)
print(wilcox_result)

#--------------------------------

both_species <- allWest %>%
  filter(Species %in% c("Spret", "Mus"))#"M.Spretus.sfm","M.MUS.musP","M.MUS.musB","M.MUS.musC","M.MUS.musM"))

mod <- aov(Ratio.TUB ~ Species * Condition,
           data = both_species
)

# print results
summary(mod)
