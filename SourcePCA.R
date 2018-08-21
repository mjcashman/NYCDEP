library(tidyverse)
library(readxl)

rm(list=ls())

# Load raw target and source data ----
title <- "Raw Values"
mix.filename <- "Y:/Access Databases/Catskills/Fingerprinting/TargetSamples_NDisRL_useMe.csv"
target<-read.csv(mix.filename) %>%
  mutate(Type = "ISCO") 
source.filename <- "Y:/Access Databases/Catskills/Fingerprinting/SourceSamples_noRoad.csv"
source<-read.csv(source.filename) %>%
  select(-Trib)
data<-rbind(source,target)

# Load Size and Organic corrected target and source data
  # title <- "Size and Organic Corrected Values"
  # mix.filename <- "Z:/Files/Projects/NYC DEP/Sed_SatOutput/7.19.2018_alltracer_allsource_1000mcmc/ImportData/"
  # target<-read_excel(paste0(mix.filename,"FluvialImport.xlsx")) %>%
  #   mutate(Type = "ISCO")
  # source.filename <- "Z:/Files/Projects/NYC DEP/Sed_SatOutput/7.19.2018_alltracer_allsource_1000mcmc/Step5/"
  # source<-read_excel(paste0(source.filename,"DataPOSTBracket.xlsx"))
  # data<-rbind(source,target)

#Alter Groups to combine lacustrine and alluvium
source <- source %>%
  mutate(Type=recode(Type, "Upper_Lacustrine" = "Lacustrine",
                     "Lower_Lacustrine" = "Lacustrine",
                     "Bank_Alluvium" = "Alluvium",
                     "Terrace_Alluvium" = "Alluvium"))

groups <- source %>%
  unite("Type_Basin", c("Type","Basin"))
groups <- groups[,2]
basin <- source[,2]
type <- source[,3]


# Plot the data-----

##Apply PCA ----
# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
pca <- prcomp(source[,6:59], center = TRUE, scale. = TRUE)
plot(pca, type = "l")

#Version 1 w/ggbiplot
library(ggbiplot)
library(viridis)
library(factoextra)
ggbiplot(pca, choices=c(1,2),obs.scale = 1, var.scale = 1, 
              groups = groups, ellipse = TRUE, 
              circle = TRUE, var.axes = TRUE) +
 # scale_shape_manual(name="Source Type",values=c(21:25,8,10))+
  #scale_fill_viridis(name="Source Type",discrete=TRUE) +
  #scale_color_viridis(name="Source Type",discrete=TRUE) +
  geom_point(aes(color=groups,fill=groups, shape=groups),size=3)+
  theme(legend.direction = 'vertical', legend.position = 'right') +
  ggtitle(title)

#Version2 Factoextra
library(ade4)
res.pca <- dudi.pca(source[,6:59],
                    scannf = FALSE,   # Hide scree plot
                    nf = 5 )           # Number of components kept in the results

fviz_eig(res.pca)

fviz_pca_ind(res.pca,
             col.ind = groups,
             habillage = groups,
             axes = c(1, 2),
             geom = c("point"),
             addEllipses = TRUE,
             ellipse.type = "confidence",
             legend.title= "Groups",
             palette = "lancet",
             #gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)+ # Avoid text overlapping
  scale_shape_manual(values = c(15,17,15,17,15,17,15,17,15,17,15,17))


fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE )    # Avoid text overlapping


fviz_pca_biplot(res.pca, repel = FALSE,
                geom = c("point"),
                addEllipses = TRUE,
                ellipse.type = "confidence",
                legend.title= "Groups",
                col.var = "#2E9FDF", # Variables color
                col.ind = groups ) # Individuals color


##Apply PCO ------
library(dplyr)
library(ggpubr)
mds <- data %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()
colnames(mds) <- c("Dim.1", "Dim.2")

# Plot MDS
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = groups,
          size = 1,
          colour = groups,
          mean.point = TRUE,
          ellipse=TRUE,
          palette = "jco",
          repel = TRUE)

# Compute MDS
library(MASS)
nmds <- data %>%
  dist() %>%          
  isoMDS() %>%
  .$points %>%
  as_tibble()
colnames(mds) <- c("Dim.1", "Dim.2")
# Plot MDS
cbind(mds,groups)
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = groups,
          colour = groups,
          ellipse=TRUE,
          mean.point=TRUE,
          star.plot=TRUE,
          size = 1,
          repel = TRUE)
library(devtools)
install_github('jfq3/ggordiplots')
library(ggordiplots)

#RDA
ord <- rda(data[,3:56])
gg_ordiplot(mds, groups = data$Type, pt.size = 3)

#tes
library(devtools)
install_github('fawda123/ggord')
library(ggord)
ord <- prcomp(data[,3:56], center = TRUE, scale. = TRUE)
p <- ggord(ord, data$Type)
p

#NMDS
library(vegan)
ord <- metaMDS(data[,3:53], k=2,autotransform=TRUE,trymax=500)
stressplot(ord)
plot(ord)
ordiplot(ord,type="n")
orditorp(ord,display="species",col="red",air=0.01)
orditorp(ord,display="sites",cex=1.25,air=0.01)

ggord(ord, data$Type, vec_ext = 0, txt = NULL, arrow = 0)

#LDA
# linear discriminant analysis
# example from lda in MASS package
ord <- lda(Type ~ ., data[,-1], prior = rep(1, 5)/5)
ggord(ord, data$Type,arrow=NULL)

# correspondence analysis
# dudi.coa
ord <- dudi.coa(data[,3:56], scannf = FALSE, nf = 4)
ggord(ord, data$Type)

# correspondence analysis
# ca
library(ca)
ord <- ca(data[,3:56])

ggord(ord, data$Type)

#Cluster analysis
mydata <- scale(data[,3:56]) 
row.names(data)<-(data$Field.ID)
d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D") 
plot(fit,cex=0.75 ) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")

data$group <- groups

