options(stringsAsFactors=F)
library(ggplot2)
library(gplots)
library(ggpubr)
library(tibble)
library(dplyr)
library(readr)

#reading in first dataset
casp <- read.table("C:/Users/cmeli/Desktop/Brem/ATG/NCS/Caspase/7.7.atg.ncs.caspase.txt",header=T,quote="",sep="\t")
casp$Species <- sapply(strsplit(casp$Sample,"_",fixed=T),"[",2)
casp$Condition <- sapply(strsplit(casp$Sample,"_",fixed=T),"[",1)
casp$annot <- paste(casp$Species,casp$Condition,sep="_")

casp1 <- read.table("C:/Users/cmeli/Desktop/Brem/ATG/NCS/Caspase/7.14.atg.ncs.caspase.txt",header=T,quote="",sep="\t")
casp1$Species <- sapply(strsplit(casp1$Sample,"_",fixed=T),"[",2)
casp1$Condition <- sapply(strsplit(casp1$Sample,"_",fixed=T),"[",1)
casp1$annot <- paste(casp1$Species,casp1$Condition,sep="_")

casp2 <- read.table("C:/Users/cmeli/Desktop/Brem/ATG/NCS/Caspase/7.21.atg.ncs.caspase.txt",header=T,quote="",sep="\t")
casp2$Species <- sapply(strsplit(casp2$Sample,"_",fixed=T),"[",2)
casp2$Condition <- sapply(strsplit(casp2$Sample,"_",fixed=T),"[",1)
casp2$annot <- paste(casp2$Species,casp2$Condition,sep="_")

casp3 <- read.table("C:/Users/cmeli/Desktop/Brem/ATG/NCS/Caspase/7.28.atg.ncs.caspase.txt",header=T,quote="",sep="\t")
casp3$Species <- sapply(strsplit(casp3$Sample,"_",fixed=T),"[",2)
casp3$Condition <- sapply(strsplit(casp3$Sample,"_",fixed=T),"[",1)
casp3$annot <- paste(casp3$Species,casp3$Condition,sep="_")

casp4 <- read.table("C:/Users/cmeli/Desktop/Brem/ATG/NCS/Caspase/8.3.atg.ncs.caspase.txt",header=T,quote="",sep="\t")
casp4$Species <- sapply(strsplit(casp4$Sample,"_",fixed=T),"[",2)
casp4$Condition <- sapply(strsplit(casp4$Sample,"_",fixed=T),"[",1)
casp4$annot <- paste(casp4$Species,casp4$Condition,sep="_")

casp5 <- read.table("C:/Users/cmeli/Desktop/Brem/ATG/NCS/Caspase/8.11.atg.ncs.caspase.txt",header=T,quote="",sep="\t")
casp5$Species <- sapply(strsplit(casp5$Sample,"_",fixed=T),"[",2)
casp5$Condition <- sapply(strsplit(casp5$Sample,"_",fixed=T),"[",1)
casp5$annot <- paste(casp5$Species,casp5$Condition,sep="_")

casp6 <- read.table("C:/Users/cmeli/Desktop/Brem/ATG/NCS/Caspase/2.16.24.atg.ncs.caspase.txt",header=T,quote="",sep="\t")
casp6$Species <- sapply(strsplit(casp6$Sample,"_",fixed=T),"[",2)
casp6$Condition <- sapply(strsplit(casp6$Sample,"_",fixed=T),"[",1)
casp6$annot <- paste(casp6$Species,casp6$Condition,sep="_")

casp7 <- read.table("C:/Users/cmeli/Desktop/Brem/ATG/NCS/Caspase/2.27.24.atg.ncs.caspase.txt",header=T,quote="",sep="\t")
casp7$Species <- sapply(strsplit(casp7$Sample,"_",fixed=T),"[",2)
casp7$Condition <- sapply(strsplit(casp7$Sample,"_",fixed=T),"[",1)
casp7$annot <- paste(casp7$Species,casp7$Condition,sep="_")

casp8 <- read.table("C:/Users/cmeli/Desktop/Brem/ATG/NCS/Caspase/3.4.24.atg.ncs.caspase.txt",header=T,quote="",sep="\t")
casp8$Species <- sapply(strsplit(casp8$Sample,"_",fixed=T),"[",2)
casp8$Condition <- sapply(strsplit(casp8$Sample,"_",fixed=T),"[",1)
casp8$annot <- paste(casp8$Species,casp8$Condition,sep="_")

casp9 <- read.table("C:/Users/cmeli/Desktop/Brem/ATG/NCS/Caspase/3.5.24.atg.ncs.caspase.txt",header=T,quote="",sep="\t")
casp9$Species <- sapply(strsplit(casp9$Sample,"_",fixed=T),"[",2)
casp9$Condition <- sapply(strsplit(casp9$Sample,"_",fixed=T),"[",1)
casp9$annot <- paste(casp9$Species,casp9$Condition,sep="_")

casp10 <- read.table("C:/Users/cmeli/Desktop/Brem/ATG/NCS/Caspase/6.13.24.atg.ncs.caspase.txt",header=T,quote="",sep="\t")
casp10$Species <- sapply(strsplit(casp10$Sample,"_",fixed=T),"[",2)
casp10$Condition <- sapply(strsplit(casp10$Sample,"_",fixed=T),"[",1)
casp10$annot <- paste(casp10$Species,casp10$Condition,sep="_")

#combine all replicates
alldatN <- rbind(casp,casp1,casp2,casp3,casp4,casp5,casp6,casp7,casp8,casp9,casp10)%>% as_tibble
alldat$Species <- sapply(strsplit(alldat$annot,"_",fixed=T),"[",1)
alldat$Species <- factor(alldat$Species,levels=c("M.MUS.musP","M.MUS.musB","M.MUS.musK","M.MUS.musM","M.MUS.musC","M.MUS.bl6","M.MUS.domT","M.MUS.domM","M.Spicilegus","M.Spretus.sfm","M.Spretus.stf"))
#file_path <- "C:/Users/cmeli/Desktop/alldat.ncsCASP1.tsv"
#write.table(alldat.norm, file = file_path, sep = "\t", row.names = FALSE, quote = FALSE)

col <- c(
  "M.MUS.musP_CONT" = "dodgerblue4", "M.MUS.musP_NCS" = "dodgerblue2",
  "M.MUS.musB_CONT" = "cyan4", "M.MUS.musB_NCS" = "cyan2",
  "M.MUS.musC_CONT" = "deepskyblue4", "M.MUS.musC_NCS" = "deepskyblue1",
  "M.MUS.musM_CONT" = "blue4", "M.MUS.musM_NCS" = "blue1",
  "M.MUS.bl6_CONT" = "darkorchid4", "M.MUS.bl6_NCS" = "darkorchid",
  "M.MUS.domT_CONT" = "olivedrab4", "M.MUS.domT_NCS" = "olivedrab1",
  "M.MUS.domM_CONT" = "darkgreen", "M.MUS.domM_NCS" = "green",
  "M.Spicilegus_CONT" = "sienna", "M.Spicilegus_NCS" = "sienna1",
  "M.Spretus.sfm_CONT" = "pink3", "M.Spretus.sfm_NCS" = "pink",
  "M.Spretus.stf_CONT" = "red4", "M.Spretus.stf_NCS" = "red"
)

# Normalize the caspase values
alldat.norm <- alldatN %>%
  as_tibble() %>%
  left_join(
    alldatN %>%
      as_tibble() %>%
      filter(Condition == 'CONT')%>%
      group_by(Species) %>%
      summarize(Caspase.mean = mean(Caspase.Value)) %>%
      ungroup(),
    by = c("Species")
  ) %>%
  mutate(
    Caspase.Norm = Caspase.Value / Caspase.mean
  )

removed.cont.alldat.norm <- alldat.norm %>%
  filter(Condition == 'NCS')

averages <- removed.cont.alldat.norm %>%
  group_by(Condition, Species) %>%
  summarize(mean = mean(Caspase.Norm, na.rm = TRUE),
            sd = sd(Caspase.Norm, na.rm = TRUE),
            n= n(),
            se = sd / sqrt(n),
            lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
            upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)

NCSCasp <- alldat.norm %>%
  filter(Condition == 'NCS')

alldat <- rbind(alldat.norm, alldatnormX)
controlNCS <- alldat.norm %>%
  filter(Condition == 'CONT')

# Create the plot
ggplot(data = NCSCasp, aes(x = Species, y = Caspase.Norm, fill = annot)) +
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
  #ylim(0,5)+
  ylab("Caspase Value") +
  coord_flip()+#(ylim = c(0, 1))  +
  theme_test()




#----------------------------------------------------------
ggplot(data=alldat,aes(x=Species,y=Caspase.Value,fill=annot))+geom_bar(stat="summary",position="dodge")+
  stat_summary(fun = "mean", geom = "bar", position = "dodge", fun.args = list(mult = 1))+
  geom_jitter(data=alldat,position=position_dodge(width=0.9),stat="identity")+
  geom_hline(yintercept=1,linetype="dashed",size=0.5,color="gray")+ 
  theme(text=element_text(size=20),panel.background=element_blank(),axis.line=element_line(color="black"),
        panel.border = element_rect(color = "black", fill=NA),axis.text.x =element_text(angle = 45,hjust=1))+ #scale_fill_manual(values=col)
  scale_fill_manual(values=c("darkorchid4","darkorchid1","darkgreen","green","olivedrab4","olivedrab2","cyan4","cyan2","deepskyblue4","deepskyblue1","blue4","blue","turquoise3","turquoise","dodgerblue4","dodgerblue2","pink3","pink","red4","red"))+
  ylab("Caspase Value")+scale_fill_manual(values = col)



#fold change 
control.df <- alldat %>% 
  as_tibble %>% 
  filter(Condition == 'CONT') %>% 
  # group_by(Passage, Species) %>% # this is your "Batch" but in the future, add a Date column to your data
  group_by(Plate, Date, Species) %>% 
  summarize(Caspase.mean = mean(Caspase.Value)) %>% 
  ungroup()

alldat.norm <- alldat %>% 
  as_tibble %>% 
  left_join(
    control.df,
    # by=c('Passage','Species')
    by=c("Plate",'Date','Species')
  ) %>% 
  mutate(
    Caspase.Norm = Caspase.Value / Caspase.mean
  )

file_path <- "C:/Users/cmeli/Desktop/alldat.tsv"
write.table(alldat.norm, file = file_path, sep = "\t", row.names = FALSE, quote = FALSE)
#alldat.norm %>% write_tsv('alldat.tsv')


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
  ylab("Caspase Value (NCS Norm)")+
  #coord_flip()+
  coord_flip(ylim = c(0, 1)) +
  theme_test()

