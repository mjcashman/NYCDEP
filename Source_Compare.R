library(tidyverse)
library(readxl)
library(ggmap)
library(viridis)
library(ggpubr)

rm(list=ls())

directory <- "Y:/Access Databases/Catskills/Fingerprinting/"

palette <- c("#00AFBB", "#E7B800", "#FC4E07")

# Load raw target and source data ----
title <- "Raw Values"


target_values<-read.csv(paste0(directory,"TargetSamples_NDisRL_useMe.csv")) %>%
  mutate(Type = "ISCO")
target_info<-read.csv(paste0(directory,"TargetSamples_Info.csv"))
target <- inner_join(target_info,target_values) %>% dplyr::rename(lon = Long,lat = Lat) %>%
  mutate(lon = lon*-1)

source_values<-read.csv(paste0(directory,"SourceSamples_noRoad.csv")) 
source_info<-read.csv(paste0(directory,"SourceSamples_Info.csv"))
source <- inner_join(source_info,source_values) %>% dplyr::rename(lon = Long,lat = Lat) 

source <- source %>%
  mutate(Type=recode(Type, "Upper_Lacustrine" = "Lacustrine",
     "Lower_Lacustrine" = "Lacustrine",
             "Bank_Alluvium" = "Alluvium",
           "Terrace_Alluvium" = "Alluvium"))



# Map datasets onto google map outputs ----
sbbox <- make_bbox(lon = source$lon, lat = source$lat, f = .1)
sq_map <- get_map(location = sbbox, maptype = "terrain", source = "google")
ggmap(sq_map) + 
  geom_point(data = source, mapping = aes(x = lon, y = lat, color = Type), size=3)+
  #geom_text(data = source, aes(label = paste("  ", as.character(SampleName), sep="")), position=position_jitter(), angle = 30, hjust = 0, size = 3, color = "black")
  #scale_color_viridis(discrete=TRUE) +
  #geom_point(data = target,  mapping = aes(x = lon, y = lat, color = Collected_by), size=3,pch=7)+
  scale_colour_brewer(palette = "Set1")
ggsave("Output/SourceSampleCollection.png")
                           
# Graphical comparisons of differences between woodland/stoney clove creek----
gather_source<-source %>% gather(key=Tracer,value=value, c(12:66)) %>% mutate(Basin = recode(Basin, `Stony Clove Creek` = "Stony Clove",
                                                                                             `Woodland Creek` = "Woodland"))
# ggplot(data=gather_source, aes(x=Basin,y=value))+
#   facet_wrap(~Tracer, scales="free")+
#   geom_violin()

#Faceted Source Violin+boxplots for both basins
ggviolin(gather_source, x = "Basin", y = "value", fill = "Basin",
         palette = c("#00AFBB", "#E7B800"),
         add = "boxplot", add.params = list(fill = "white"))+
  facet_wrap(~Tracer, scales="free_y")+
  ylab("Tracer value")+
  xlab("")+
  theme(legend.position = "top") +
  stat_compare_means( aes(label = ..p.signif..), 
                      label.x = 1.5)

ggsave("Output/prefilterViolinTracer.png", width = 15, height = 10, dpi = 400)

#Faceted density plots for both basins
ggdensity(gather_source, x = "value",
          add = "mean", rug = TRUE,
          color = "Basin", fill = "Basin",
          palette = c("#00AFBB", "#E7B800"))+
  facet_wrap(~Tracer, scales="free")

   
#Bind and plot Targets compared to both sources
data <- rbind(source, target)
data <- data %>% mutate(Collection = case_when(Type == "ISCO" ~ "Target",
                                               Type != "ISCO" & Basin == "Woodland Creek" ~ "Source_Wood",
                                               Type != "ISCO" & Basin == "Stony Clove Creek" ~ "Source_SCC"))
gather_data <- data %>% gather(key=Tracer,value=value, c(10:66))
ggplot(data=gather_data, aes(x=Type, y=value, fill=Collection))+
  facet_wrap(~Tracer, scales="free")+
  geom_boxplot()

##PCA/PCO/NMDS Visual comparisons of basins ---- 
unite_source<-source %>%
  unite("Basin_Type",c("Type","Basin"))
groups <- source[, 5]

# Plot the data-----
##Apply PCA ----
pca <- prcomp(source[,12:66], center = TRUE, scale. = TRUE)
plot(pca, type = "l")

#Version 1 w/ggbiplot
library(ggbiplot)
library(viridis)

library(factoextra)

#Version2 Factoextra
library(ade4)
res.pca <- dudi.pca(source[,12:66],
                    scannf = FALSE,   # Hide scree plot
                    nf = 5 )           # Number of components kept in the results

fviz_eig(res.pca)

fviz_pca_ind(res.pca,
             col.ind = groups, # Color by the group
             axes = c(1, 2),
             pointsize = 2,
             geom = c("point"),
             addEllipses = TRUE,
             ellipse.type = "confidence",
             legend.title= "Groups",
             palette = c("#006168","#72c1c6","#8a6e00", "#EBC632","#972e04","#FC8351","#016c20","#4dca72"),
             repel = TRUE)+
  scale_shape_manual(values = c(15,15,15,15,15,15,15,15,15,15))+
  ggtitle("Raw Value PCA --  Source*Basin", subtitle = "Individuals - PCA")
#Avoid text overlapping

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE )+
        ggtitle("Raw Value PCA --  Source*Basin", subtitle = "Variables - PCA")    # Avoid text overlapping


fviz_pca_biplot(res.pca, repel = FALSE,
                geom = c("point"),
                addEllipses = TRUE,
                ellipse.level=0.95,
                ellipse.type = "confidence",
                legend.title= "Groups",
                palette = c("#006168","#72c1c6","#8a6e00", "#EBC632","#972e04","#FC8351","#016c20","#4dca72"),
                col.var = "grey", # Variables color
                alpha.var = 0.2, # Variables alpha
                col.ind = groups )+
  scale_shape_manual(values = c(15,15,15,15,15,15,15,15,15,15))+
  ggtitle("Raw Value PCA --  Source*Basin", subtitle = "PCA - Biplot") # Individuals color
ggsave("Output/RawGroupPCA.png")

#Raw LDA
# linear discriminant analysis
# example from lda in MASS package
library(ggord)
library(MASS)
ord <- lda(Type ~ ., source[,c(5,12:66)])
ggord(ord,source$Type, axes = c("1", "2"),
      ylim=c(-12,12), xlim=c(-10,14), ptslab=FALSE, arrow = NULL, txt = NULL, vec_lab = NULL)
table(predict(ord, type="class")$class, source$Type)
ggsave("Output/RawGroupLDA12.png")
ggord(ord,source$Type, axes = c("1", "3"),
      ylim=c(-12,12), xlim=c(-10,14), ptslab=FALSE, arrow = NULL, txt = NULL, vec_lab = NULL)
ggsave("Output/RawGroupLDA13.png")
ggord(ord,source$Type, axes = c("2", "3"),
      ylim=c(-12,12), xlim=c(-10,14), ptslab=FALSE, arrow = NULL, txt = NULL, vec_lab = NULL)
ggsave("Output/RawGroupLDA23.png")

##Permanova comparisons ----
library(vegan)
library(pairwiseAdonis)
rownames(source)<-source$SampleName
raw.dist<-vegdist(source[,13:66],method="euclidean")
heatmap(as.matrix(raw.dist))

adonis(source[,13:66] ~ Type*Basin, data = source)
adonis2(source[,13:66] ~ Type*Basin, data = source)
pairwise.adonis(source[,13:66], source$Type)

# Wilcoxon rank sum test with-----
detach("package:ade4", unload=TRUE)
detach("package:pairwiseAdonis", unload=TRUE)
detach("package:vegan", unload=TRUE)

library(broom)
gather_source <-   source %>% gather(key=Tracer,value=value, c(12:66))

Wilcox.Results <- gather_source %>% #Sig values no by source
  group_by(Tracer) %>%
  do(tidy(wilcox.test(value ~ Basin, data = ., exact = FALSE)))%>%
  filter(p.value<0.05)
unique(Wilcox.Results$Tracer)

Wilcox.Results <- gather_source %>%  #Sig values  by source
                    group_by(Tracer,Type) %>%
                    do(tidy(wilcox.test(value ~ Basin, data = ., exact = FALSE))) %>%
  filter(p.value<0.05)
unique(Wilcox.Results$Tracer)

wilcox.sig<- Wilcox.Results %>%
  filter(p.value<0.05) %>%
  arrange(desc(Type))
write.csv(wilcox.sig, "Output/wilcoxsig_basinDiff.csv")
unique(wilcox.sig$Tracer)

Wilcox.median.diff.results <- source %>%  #Look at median differences
  group_by(Basin,Type) %>%
  dplyr::select(-1,-2,-4,-6:-11) %>%
  summarise_all(funs(median)) %>%
  dplyr::select(c(unique(wilcox.sig$Tracer),Type)) %>%
  arrange(Type)

t.medians<-t(Wilcox.median.diff.results)
pc.diff<- Wilcox.median.diff.results %>%
  gather(key=Tracer,value=value, c(2:11)) %>%
  spread(Basin,value) %>%
  mutate(SCCDiff = (`Stony Clove Creek`-`Woodland Creek`)/`Woodland Creek`*100,
        WCCDiff = (`Woodland Creek`-`Stony Clove Creek`)/`Stony Clove Creek`*100) %>%
  dplyr::filter(Tracer %in% diff)
diff <- unique(wilcox.sig$Tracer)

#Permanova test with Wilcoxon identified tracers removed ----
source.remove<-source %>%
  dplyr::select(-one_of((diff)))
library(vegan)

adonis(source.remove[,12:55] ~ Type*Basin, data = source)

library(devtools)
install_github('pmartinezarbizu/pairwiseAdonis/pairwiseAdonis')
library(pairwiseAdonis)

pairwise.adonis(source.remove[,12:55], source$Type)

# Removed Tests and plots ----

##PCA/PCO/NMDS Visual comparisons of basins ---- 
unite_source.remove<-source.remove %>%
  unite("Basin_Type",c("Type","Basin"))
unite_groups <- unite_source.remove[, 3]
groups <- source.remove[, 5]


# Plot the data-----
##Apply PCA ----
pca <- prcomp(source.remove[,12:56], center = TRUE, scale. = TRUE)
plot(pca, type = "l")

#Version 1 w/ggbiplot
library(ggbiplot)
library(viridis)
library(factoextra)

#Version2 Factoextra
library(ade4)
res.pca <- dudi.pca(source.remove[,12:56],
                    scannf = FALSE,   # Hide scree plot
                    nf = 5 )           # Number of components kept in the results

fviz_eig(res.pca)

fviz_pca_ind(res.pca,
             col.ind = groups, # Color by the group
             axes = c(1, 2),
             pointsize = 2,
             geom = c("point"),
             addEllipses = TRUE,
             ellipse.type = "confidence",
             legend.title= "Groups",
             palette = c("#006168","#72c1c6","#8a6e00", "#EBC632","#972e04","#FC8351","#016c20","#4dca72"),
             repel = TRUE)+
  scale_shape_manual(values = c(15,15,15,15,15,15,15,15,15,15))
#Avoid text overlapping

palette = c("#00AFBB", "#E7B800", "#FC4E07")

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = FALSE )    # Avoid text overlapping

#Split Groups
fviz_pca_biplot(res.pca, repel = FALSE,
                axes = c(1, 2),
                geom = c("point"),
                addEllipses = TRUE,
                ellipse.level=0.95,
                ellipse.type = "confidence",
                legend.title= "Groups",
             #   palette = c("#006168","#72c1c6","#8a6e00", "#EBC632","#972e04","#FC8351","#016c20","#4dca72"),
                col.var = "grey", # Variables color
                alpha.var = 0.2, # Variables alpha
                col.ind = unite_groups )+
  scale_shape_manual(values = c(15,15,15,15,15,15,15,15,15,15))+ # Individuals color
  ggtitle("Equal-Basin PCA --  Source*Basin", subtitle = "PCA - Biplot") # Individuals color
ggsave("Output/EqualBasinSourceBasinPCA12.png")

#Unified Groups
install_github('kassambara/factoextra')
library(factoextra)
fviz_pca_biplot(res.pca, repel = FALSE,
                #habillage = groups,
                axes = c(1, 2),
                #Individuals
                geom.ind = ("point"),
                col.ind = "white",
                fill.ind = groups,
                pointshape = 21,
                pointsize = 3, 
                #Ellipse
                addEllipses = TRUE,
                ellipse.level=0.95,
                ellipse.alpha=0.4,
                ellipse.type = "confidence",
                #Variable
                col.var = "darkgrey", # Variables color
                alpha.var = 0.2, # Variables alpha
                legend.title= "Source"
)
 
# scale_colour_manual(values = c("white","white","white","white"))+# Individuals color
 # shapcale_fill_manual(values = c("blue","red","green","yellow"))+# Individuals color
  
ggtitle("Equal-Basin PCA --  Source", subtitle = "PCA - Biplot") # Individuals color
ggsave("Output/EqualBasinSourcePCA12.png")

#LDA
# linear discriminant analysis
# example from lda in MASS package
library(ggord)
library(MASS)
ord <- lda(Type ~ ., source.remove[,c(5,12:56)])
ggord(ord,source.remove$Type, axes = c("1", "2"),
      ylim=c(-10,10), xlim=c(-12,10), ptslab=FALSE, arrow = NULL, txt = NULL, vec_lab = NULL)
table(predict(ord, type="class")$class, source.remove$Type)
print("Classified Correctly:")
sum(36+25+21+37)/sum(36+25+21+37+1+2)*100
ggsave("Output/EqualBasinGroupLDA12.png")
ggord(ord,source.remove$Type, axes = c("1", "3"),
      ylim=c(-10,10), xlim=c(-12,10), ptslab=FALSE, arrow = NULL, txt = NULL, vec_lab = NULL)
ggsave("Output/EqualBasinGroupLDA13.png")
ggord(ord,source.remove$Type, axes = c("2", "3"),
      ylim=c(-10,10), xlim=c(-12,10), ptslab=FALSE, arrow = NULL, txt = NULL, vec_lab = NULL)
ggsave("Output/EqualBasinGroupLDA23.png")


#Tracer by Tracer Violin plots - Equal Basin ----
gather_source<-source.remove %>% gather(key=Tracer,value=value, c(12:56)) %>%
  mutate(Basin = recode(Basin, `Stony Clove Creek` = "Stony Clove",`Woodland Creek` = "Woodland")) %>%
  mutate(Type = fct_recode(Type, "Glacial\nTill" = "Glacial_Till"))

#Faceted Source Violin+boxplots for both basins
my_comparisons <- list( c("Alluvium", "Forest"), c("Alluvium", "Glacial\nTill"), c("Alluvium", "Lacustrine"),
                        c("Forest", "Glacial\nTill"), c("Forest", "Lacustrine"),
                        c("Glacial\nTill", "Lacustrine"))
ggviolin(gather_source, x = "Type", y = "value", fill = "Type",
         #palette = c("#00AFBB", "#E7B800"),
         add = "boxplot", add.params = list(fill = "white"))+
  facet_wrap(~Tracer, scales="free_y")+
  theme(axis.text.x = element_text(size=7)) + 
  theme(axis.text.y = element_text(size=7)) + 
  ggtitle("Stony Clove Creek + Woodland Sediment Source Library")+
  ylab("Tracer value")+
  xlab("")+
  theme(legend.position = "top") 
ggsave("Output/EqualBasinpostfilterViolinTracer.png", width = 15, height = 10, dpi = 400)
