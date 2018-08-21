library(tidyverse)
library(readxl)
library(ggmap)
library(viridis)
library(scatterpie)

rm(list=ls())

baseDir <- "D:/AutoSed_SAT/SedimentFingerprinting_R/AutoOutput/"

#All
Runlist <- c("8.8.18.Auto.AllCorrect.NoBasinDiff.noRoad_groupedSource",
             "8.8.18.Auto.NoOrgCorrect.NoBasinDiff.noRoad_groupedSource",
             "8.9.18.Auto.NoSize.NoBasinDiff.noRoad_groupedSource",
             "8.9.18.Auto.NoCorrect.NoBasinDiff.noRoad_groupedSource",
             "8.9.18.Auto.NoOrgCorrect.NoBasinDiff.noRoad.ULoutlierremoved_groupedSource",
             "8.9.18.Auto.AllCorrect.NoBasinDiff.noRoad.ULoutlierremoved_individSource",
             "8.15.18.Auto.NoOrgCorrect.NoBasinDiff.noRoad.ULoutlierremoved_individSource"
)

#Individual
#Runlist <-  "8.15.18.Auto.NoOrgCorrect.NoBasinDiff.noRoad.ULoutlierremoved_individSource"

#Loop for grouped source results----
for (i in seq(Runlist)){
  Run<-Runlist[i]

#Plot Mixing Model Results Grouped----
data <- read.csv(paste0(baseDir,Run,"/Step8/MixingModelResults.csv")) %>% 
  dplyr::select(-Error)
Source_no <- length(colnames(data))-4

if(Source_no == 4){ 
  data<-data %>%
    gather(Source,value,Alluvium:Lacustrine) %>%
    mutate(Source = fct_recode(Source, "Glacial\nTill" = "Glacial_Till"))
  data$Source = factor(data$Source, levels = c("Forest","Alluvium","Glacial\nTill","Lacustrine"))
} else if (Source_no == 6){
  data<-data %>%
    gather(Source,value,Bank_Alluvium:Upper_Lacustrine) %>%
    mutate(Source = fct_recode(Source, "Glacial\nTill" = "Glacial_Till",
                               "Bank\nAlluvium" = "Bank_Alluvium",
                               "Terrace\nAlluvium" = "Terrace_Alluvium",
                               "Lower\nLacustrine" = "Lower_Lacustrine",
                               "Upper\nLacustrine" = "Upper_Lacustrine"))
  data$Source = factor(data$Source, levels = c("Forest","Bank\nAlluvium","Terrace\nAlluvium","Glacial\nTill","Lower\nLacustrine","Upper\nLacustrine"))
}

data$TSample = factor(data$TSample, levels = c("CHICH7ISC102917", "JANSS9ISC102917", "OXCLO6ISC102917", "WARNE8ISC102917",
                                               "PHONE3ISC102917", "PHONE4ISC102917", "PHONE5ISC102917", "PHONE0ISC103017"))
  data<-data %>%
    mutate(TSample = fct_recode(TSample, "Stony Clove at Chichester" = "CHICH7ISC102917",
                                "Stony Clove at Janssen Rd" = "JANSS9ISC102917",
                                "Ox Clove near Mouth" = "OXCLO6ISC102917",
                                "Warner near Chichester" = "WARNE8ISC102917",
                                "Woodland Rise @ 15:35, 32 cfs" = "PHONE3ISC102917",
                                "Woodland Peak @ 23:05, 1440 cfs"= "PHONE4ISC102917",
                                "Woodland Peak @ 23:40, 2180 cfs" = "PHONE5ISC102917",
                                "Woodland Peak @ 02:00, 1420 cfs" = "PHONE0ISC103017"))
  
  ggplot(data=data)+
    geom_col(aes(x=Source, y=value, fill=Source))+
    facet_wrap(~TSample, nrow = 2)+
    theme_bw()+
    theme(axis.text.x = element_text(size=8)) + 
    theme(legend.position = "top") +
    guides(fill=guide_legend(title="Source"))+
    xlab("")+
    ylab("Source Apportionment (%)")+
    scale_y_continuous(expand = c(0,0), lim = c(0,100)) +
    scale_fill_brewer(palette = "Set1") +
    ggtitle("Target Source Unmixing", subtitle = Run)
  ggsave(paste0("Output/MixResults",Run,".png"))
  
#Plot Error/SVT Model Results----
  if(Source_no == 4){ 
    data <- read.csv(paste0(baseDir,Run,"/Step8/ErrorAnalysis.csv")) %>%
      dplyr::select(-Error) %>%
      gather(Source,value,Alluvium:Lacustrine) %>%
      mutate(Source = fct_recode(Source, "Glacial\nTill" = "Glacial_Till"))  %>%
      mutate(SourceType = fct_recode(SourceType, "Glacial\nTill" = "Glacial_Till"))  %>%
      group_by(TSample,SourceType,Source) %>%
      dplyr::summarise(mean=mean(value))
    data$Source = factor(data$Source, levels = c("Forest","Alluvium","Glacial\nTill","Lacustrine"))
    data$SourceType = factor(data$SourceType, levels = c("Forest","Alluvium","Glacial\nTill","Lacustrine"))
    
  } else if (Source_no == 6){
    data <- read.csv(paste0(baseDir,Run,"/Step8/ErrorAnalysis.csv")) %>%
      dplyr::select(-Error) %>%
      gather(Source,value,Bank_Alluvium:Upper_Lacustrine) %>%
      mutate(Source = fct_recode(Source, "Glacial\nTill" = "Glacial_Till",
                                 "Bank\nAlluvium" = "Bank_Alluvium",
                                 "Terrace\nAlluvium" = "Terrace_Alluvium",
                                 "Lower\nLacustrine" = "Lower_Lacustrine",
                                 "Upper\nLacustrine" = "Upper_Lacustrine")) %>%
      mutate(SourceType = fct_recode(SourceType, "Glacial\nTill" = "Glacial_Till",
                                     "Bank\nAlluvium" = "Bank_Alluvium",
                                     "Terrace\nAlluvium" = "Terrace_Alluvium",
                                     "Lower\nLacustrine" = "Lower_Lacustrine",
                                     "Upper\nLacustrine" = "Upper_Lacustrine")) %>%
      group_by(TSample,SourceType,Source) %>%
      dplyr::summarise(mean=mean(value))
      data$Source = factor(data$Source, levels = c("Forest","Bank\nAlluvium","Terrace\nAlluvium","Glacial\nTill","Lower\nLacustrine","Upper\nLacustrine"))
    data$SourceType = factor(data$SourceType, levels = c("Forest","Bank\nAlluvium","Terrace\nAlluvium","Glacial\nTill","Lower\nLacustrine","Upper\nLacustrine"))
    }
  
  data$TSample = factor(data$TSample, levels = c("CHICH7ISC102917", "JANSS9ISC102917", "OXCLO6ISC102917", "WARNE8ISC102917",
                                                 "PHONE3ISC102917", "PHONE4ISC102917", "PHONE5ISC102917", "PHONE0ISC103017"))

  data<-data %>%
    ungroup() %>%
    mutate(TSample = fct_recode(TSample, "Stony Clove at Chichester" = "CHICH7ISC102917",
                                "Stony Clove at Janssen Rd" = "JANSS9ISC102917",
                                "Ox Clove near Mouth" = "OXCLO6ISC102917",
                                "Warner near Chichester" = "WARNE8ISC102917",
                                "Woodland Rise @ 15:35, 32 cfs" = "PHONE3ISC102917",
                                "Woodland Peak @ 23:05, 1440 cfs"= "PHONE4ISC102917",
                                "Woodland Peak @ 23:40, 2180 cfs" = "PHONE5ISC102917",
                                "Woodland Peak @ 02:00, 1420 cfs" = "PHONE0ISC103017"))
  
  ggplot(data)+
    geom_col(aes(x=SourceType, y=mean, fill=Source))+
    facet_wrap(~TSample, nrow = 2)+
    theme_bw()+
    theme(axis.text.x = element_text(size=8)) + 
    guides(fill=guide_legend(title="Source"))+
    xlab("Collected Source")+
    ylab("Source Verification Prediction (%)")+
    theme(legend.position = "top") +
    scale_y_continuous(expand = c(0,0)) +
    scale_fill_brewer(palette = "Set1") +  ggtitle("Source Verification Test", subtitle = Run)
  ggsave(paste0("Output/SVTResults",Run,".png"))
  
}



#Map Mixing Model Results----
directory <- "Y:/Access Databases/Catskills/Fingerprinting/"

source_values<-read.csv(paste0(directory,"SourceSamples_noRoad.csv"))
source_info<-read.csv(paste0(directory,"SourceSamples_Info.csv"))
source <- inner_join(source_info,source_values) %>% rename(lon = Long,lat = Lat) %>% filter(Basin == "Stony Clove Creek")

sbbox <- make_bbox(lon = source$lon, lat = source$lat, f = .1)
sq_map <- get_map(location = sbbox, maptype = "terrain", source = "google")

target_data <- read.csv(paste0(baseDir,Run,"Step8/MixingModelResults.csv")) %>%
  dplyr::select(-Error) %>%
  gather(Source,value,Alluvium:Lacustrine) %>%
  mutate(Source = fct_recode(Source, "Glacial\nTill" = "Glacial_Till"))

target_data$Source <- fct_inorder(target_data$Source)
target_data$TSample = factor(target_data$TSample, levels = c("CHICH7ISC102917", "JANSS9ISC102917", "OXCLO6ISC102917", "WARNE8ISC102917",
                                               "PHONE3ISC102917", "PHONE4ISC102917", "PHONE5ISC102917", "PHONE0ISC103017"))

target_info<-read.csv(paste0(directory,"TargetSamples_Info.csv"))
target <- left_join(target_data,target_info, by=c("TSample"="Field.ID")) %>% rename(lon = Long,lat = Lat) %>%
  mutate(lon = lon*-1) %>%
  spread(Source, value)

ggmap(sq_map) +
  #geom_scatterpie(data = target, aes(x = lon, y = lat, r = 0.005), cols=colnames(target[16:19]),color="NA")+
  #geom_text(data = source, aes(label = paste("  ", as.character(SampleName), sep="")), position=position_jitter(), angle = 30, hjust = 0, size = 3, color = "black")
  #scale_color_viridis(discrete=TRUE) +
  geom_point(data = target,  mapping = aes(x = lon, y = lat, color = Collected_by), size=3,pch=7)+
  scale_fill_brewer(palette = "Set1")+ coord_equal()

