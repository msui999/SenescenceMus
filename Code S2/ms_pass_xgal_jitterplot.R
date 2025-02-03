options(stringsAsFactors=F)
library(ggplot2)
library(gplots)
library(ggpubr)
library(tibble)
library(dplyr)
library(readr)

pasX <- read.table("C:/Users/cmeli/Desktop/Brem/XGal IRRAD/passageXRay.txt",header=T,quote="",sep="\t")
pasX$Species <- sapply(strsplit(pasX$Sample,"_",fixed=T),"[",2)
pasX$Condition <- sapply(strsplit(pasX$Sample,"_",fixed=T),"[",1)
pasX$annot <- paste(pasX$Species,pasX$Condition,sep="_")

pasN <- read.table("C:/Users/cmeli/Desktop/Brem/XGal NCS/passageNCS.txt",header=T,quote="",sep="\t")
pasN$Species <- sapply(strsplit(pasN$Sample,"_",fixed=T),"[",2)
pasN$Condition <- sapply(strsplit(pasN$Sample,"_",fixed=T),"[",1)
pasN$annot <- paste(pasN$Species,pasN$Condition,sep="_")

col <- c(
  "M.MUS.musP_CONT" = "dodgerblue4", "M.MUS.musP_Irrad" = "dodgerblue2",
  "M.MUS.musB_CONT" = "cyan4", "M.MUS.musB_Irrad" = "cyan2",
  "M.MUS.musC_CONT" = "deepskyblue4", "M.MUS.musC_Irrad" = "deepskyblue1",
  "M.MUS.musM_CONT" = "blue4", "M.MUS.musM_Irrad" = "blue1",
  "M.MUS.bl6_CONT" = "darkorchid4", "M.MUS.bl6_Irrad" = "darkorchid",
  "M.MUS.domT_CONT" = "olivedrab4", "M.MUS.domT_Irrad" = "olivedrab1",
  "M.MUS.domM_CONT" = "green4", "M.MUS.domM_Irrad" = "green",
  "M.Spicilegus_CONT" = "sienna4", "M.Spicilegus_Irrad" = "sienna1",
  "M.Spretus.sfm_CONT"="pink4","M.Spretus.sfm_Irrad"="pink",
  "M.Spretus.stf_CONT" = "red4", "M.Spretus.stf_Irrad" = "red"
)
ggplot(pasX, aes(x=Passage, y= XGal.Staining , group=Condition)) + 
  geom_point()+#aes(shape=Condition))+
  scale_shape_manual(values=c(0,1))+
  geom_smooth(method = 'lm', aes(color = annot), se = FALSE, size = 0.5) +
  scale_color_manual(values = col) +
  facet_grid(
    rows = vars(Species),
    cols = vars(Condition),
    # scales = "free"
  ) + 
  theme_pubr() + 
  labs_pubr()+
  theme(strip.text.y = element_text(size = 6, angle = 360), legend.position = "none")
