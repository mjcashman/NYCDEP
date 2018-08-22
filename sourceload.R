library(tidyverse)
library(readxl)
library(viridis)
library(dataRetrieval)

SCC_Chichester <- '01362370'
SCC_Janssen <- '01362336' 
OxClove <- '01362368'
Warner <- '01362357'
Woodland <-  '0136230002' 

sitelist<- c(OxClove, SCC_Chichester, SCC_Janssen, Warner, Woodland)
pCodes <- c("00060","63680") 
start <- "2017-10-29"
end <- "2017-11-29"

siteInfo<-readNWISsite(sitelist)

gageGPS<-readNWISsite(sitelist)[,c(2,3,7,8)]


wideMulti <- readNWISuv(sitelist,pCodes, start, end,tz = "America/New_York") %>%
  dplyr::select(-ends_with("_cd"))
wideMulti<-renameNWISColumns(wideMulti)
wideMulti$TurbLoad <- wideMulti$Flow_Inst*wideMulti$Turb_Inst

#Diagnostic plot for identifying gaps 
gather(wideMulti, variable, value, -site_no, -dateTime) %>%
  mutate(variable = as.factor(variable)) %>%
  mutate(site_no = as.factor(site_no)) %>%
ggplot(aes(x=dateTime, y=value, color=site_no))+
  geom_path(size=1)+
  facet_grid(variable~., scales="free")+
  scale_colour_brewer(palette = "Set1")


cumload <- (wideMulti)
cumload <- cumload %>% 
  group_by(site_no) %>%
  mutate(cumTurbLoad = cumsum(replace_na(TurbLoad,0))) %>%
  mutate(cumTurbLoadpc = cumTurbLoad/max(cumTurbLoad)*100) %>%
  ungroup()


siteInfo <- attr(wideMulti, "siteInfo")
paramInfo <- attr(wideMulti, "variableInfo")

longMulti <- gather(cumload, variable, value, -site_no, -dateTime) %>%
  mutate(variable = as.factor(variable)) %>%
  mutate(site_no = as.factor(site_no))

levels(longMulti$site_no) <- siteInfo$station_nm
levels(cumload$site_no) <- siteInfo$station_nm

peak<-which.max(cumload$Flow_Inst)

longMulti %>%
  filter(variable != "Flow_Inst"&variable != "Turb_Inst") %>%
  filter(dateTime < "2017-11-1") %>%
  mutate(variable = fct_recode(variable, "Cumulative Turbidity Load" = "cumTurbLoad",
                                                                     "Percent of cumulative turbidity load" = "cumTurbLoadpc",
                                                                     "Instantaneous Turbidity Load" = "TurbLoad")) %>%
#cumulative turbidity load plots
  ggplot(aes(dateTime, value, color=site_no)) +
  geom_path(size=1.1) + xlab("") +
  geom_vline(xintercept = as.numeric(cumload$dateTime[peak]), linetype=2) +
  facet_wrap(variable ~ .,scales= "free", nrow=2) + 
  theme(strip.background =element_rect(fill=usgsgreen),strip.text = element_text(size=14,colour = 'white')) +
  theme(legend.position = c(0.55, 0.35), legend.text = element_text(size=11)) +
  scale_colour_brewer(name="Sites",palette = "Set1")
ggsave("Output/CumulativeGroupFacetPlot.png",width = 12, height = 9)


#Hysteresis Plots----
siteInfo <- attr(wideMulti, "siteInfo")
paramInfo <- attr(wideMulti, "variableInfo")

levels(cumload$site_no) <- siteInfo$station_nm
levels(cumload$site_no) <- siteInfo$station_nm

usgsgreen<-"#006F41"

xintercept = as.numeric(cumload$dateTime[peak])

test<-cumload %>%
  separate(dateTime,c("Date","Time"), sep = " ") %>%
  left_join(siteInfo,by="site_no") %>%
  filter(Date < "2017-11-1")
test1<-test %>%
  group_by(site_no) %>%
  na.omit() %>%
  slice(which.max(Flow_Inst)) %>%
  unite(DateTime,Date,Time, sep=" ")
test1$DateTime <- as.POSIXct(test1$DateTime)

joined_test<-left_join(test, test1, by="site_no") %>%
  unite(InstDateTime,Date,Time, sep=" ") %>%
  mutate(limb = case_when(InstDateTime <= DateTime ~ "Rise",
                          #InstDateTime == DateTime ~ "Peak",
                          InstDateTime >= DateTime  ~ "Fall")) %>%
  separate(InstDateTime,c("Date","Time"), sep = " ") 
  
joined_test$Date<-as.date(joined_test$Date)
library(chron)
joined_test$Time<-chron(times = joined_test$Time)
firstday<-joined_test %>%
  group_by(station_nm.x,Date) %>%
  slice(which.min(Time))

ggplot(joined_test, aes(Flow_Inst.x, Turb_Inst.x, color=(limb))) +
  geom_path(size=1.5) + xlab("") +
  facet_wrap(station_nm.x ~ .) + 
  theme_bw() +
#  scale_x_continuous(trans="log10", limits=c(NA,2000), breaks=c(1,10,100,1000,2000)) +
 # scale_y_continuous(trans="log10", limits=c(NA,2000), breaks=c(1,10,100,1000,2000)) +
 # coord_trans(x ="log10", y="log10") +
  theme(strip.background =element_rect(fill=usgsgreen),strip.text = element_text(size=6,colour = 'white')) +
  theme(legend.position = c(0.85, 0.21), legend.title = element_text(size=14), legend.text = element_text(size=12)) +
  scale_color_manual(name="Limb",values=c('#E69F00','#56B4E9')) + 
  ylab("Instantaneous Turbidity (FNU)") +
  xlab("Instantaneous Discharge (CFS)") +
  geom_point(data=firstday,color="black")
ggsave("Output/SedTransportCurv.png")
  

#Load Source Diagrams ----
longMulti %>%
  filter(dateTime < "2017-11-1") %>%
  #filter(site_no != "WOODLAND CREEK ABOVE MOUTH AT PHOENICIA NY") %>%
ggplot(aes(dateTime, value, color=site_no)) +
  geom_path(size=1.1) + xlab("") +
  theme_bw()+
  geom_vline(xintercept = as.numeric(cumload$dateTime[peak]), linetype=2) +
  facet_wrap(variable ~ .,scales= "free") + 
  theme(strip.background =element_rect(fill=usgsgreen),strip.text = element_text(colour = 'white')) +
  theme(legend.position = c(0.7, 0.3), legend.text = element_text(size=8)) +
  scale_colour_brewer(name="Sites",palette = "Set1")
ggsave("Output/CumulativeGroupFacetPlot.png",width = 12, height = 9)



baseDir <- "D:/AutoSed_SAT/SedimentFingerprinting_R/AutoOutput/"
Run <- "8.9.18.Auto.NoOrgCorrect.NoBasinDiff.noRoad.ULoutlierremoved_groupedSource"

data <- read.csv(paste0(baseDir,Run,"/Step8/MixingModelResults.csv")) %>% 
  dplyr::select(-Error)   %>%
  mutate(SampleName = fct_recode(SampleName, "STONY CLOVE CREEK BLW OX CLOVE AT CHICHESTER NY" = "CHICH7ISC102917",
                                 "STONY CLOVE CR AT JANSSEN RD AT LANESVILLE NY" = "JANSS9ISC102917",
                                 "OX CLOVE NEAR MOUTH AT CHICHESTER NY" = "OXCLO6ISC102917",
                                 "WARNER CREEK NEAR CHICHESTER NY" = "WARNE8ISC102917",
                                 "WOODLAND CREEK @ 15:35, 32 cfs" = "PHONE3ISC102917",
                                 "WOODLAND CREEK @ 23:05, 1440 cfs" = "PHONE4ISC102917",
                                 "WOODLAND CREEK @ 23:40, 2180 cfs" = "PHONE5ISC102917",
                                 "WOODLAND CREEK @ 02:00, 1420 cfs" = "PHONE0ISC103017"))   %>%
  gather(Source,value,Alluvium:Lacustrine) %>%
  mutate(Source = fct_recode(Source, "Glacial\nTill" = "Glacial_Till"))


data$Source = factor(data$Source, levels = c("Forest","Alluvium","Glacial\nTill","Lacustrine"))

p<-ggplot(data=subset(data,SampleName == "WOODLAND CREEK @ 02:00, 1420 cfs"))+
  geom_col(aes(x=Source, y=value, fill=Source))+
  #facet_wrap(~SampleName, nrow = 2)+
  theme_bw()+
  guides(fill=FALSE) +
  theme(axis.text.x = element_text(size=8)) + 
  theme(legend.position = "top") +
 # guides(fill=guide_legend(title="Source"))+
  xlab("")+
  ylab("Source Apportionment (%)")+
  scale_y_continuous(expand = c(0,0), lim = c(0,100)) +
  scale_fill_brewer(palette = "Set1") +
  theme(strip.background =element_rect(fill=usgsgreen),strip.text = element_text(colour = 'white'))
my_g <- grobTree(rectGrob(gp=gpar(fill="#006F41")),
                 textGrob("WOODLAND CREEK @ 02:00, 1420 cfs", x=0.5, hjust=0.5,
                          gp=gpar(col="white", cex=0.9)))
g<-arrangeGrob(my_g, p, heights=c(1,9),nrow=2,ncol=1)

grid.draw(g) 
#save_plot(g ,"Output/SCC_Chichester_Source.png")

g<-cumload %>%
  group_by(site_no) %>%
  summarize(max=max(cumTurbLoad))  %>%
  mutate(site_no = fct_recode(site_no, "WOODLAND CREEK ABOVE MOUTH AT PHOENICIA NY" = "0136230002",
                                                                  "STONY CLOVE CR AT JANSSEN RD AT LANESVILLE NY" = "01362336",
                                                                  "WARNER CREEK NEAR CHICHESTER NY" = "01362357",
                                                                  "OX CLOVE NEAR MOUTH AT CHICHESTER NY" = "01362368",
                                                                "STONY CLOVE CREEK BLW OX CLOVE AT CHICHESTER NY" = "01362370")) %>%
  left_join(data,by=c("site_no" = "SampleName")) %>%
  na.omit() %>%
  mutate(source_load = value*max) %>%
  filter(site_no == "WARNER CREEK NEAR CHICHESTER NY") %>% 
  ggplot()+
  geom_col(aes(x=Source, y=source_load, fill=Source))+
  #facet_wrap(~site_no, nrow = 2)+
  theme_bw()+
  guides(fill=FALSE) +
  theme(axis.text.x = element_text(size=8)) + 
  theme(legend.position = "top") +
  xlab("")+
  ylab("Estimated Source Loading (FNU*CFS)")+
  theme(strip.background =element_rect(fill=usgsgreen),strip.text = element_text(colour = 'white'), axis.title.y = element_text(size=10)) +
  scale_y_continuous(expand = c(0,0), lim = c(0,4500000000)) +
  scale_fill_brewer(palette = "Set1") 
my_g <- grobTree(rectGrob(gp=gpar(fill="#006F41")),
                 textGrob("WARNER CREEK NEAR CHICHESTER NY", x=0.5, hjust=0.5,
                          gp=gpar(col="white", cex=0.8)))
g<-arrangeGrob(my_g, g, heights=c(1,9),nrow=2,ncol=1)
grid.draw(g) 


# Wighted loads for woodland----
#' Woodland Rise 1535-1625-1900-2035-2130-2235
#' middle @ 2250
#' woodland Peak 2305 - 2320
#' @ 2330
#' WOODLAND peak2 2340-0110
#' @ 0135
#' Woodland Fall 0200 - 0305 - 0505

sourcumload<-wideMulti %>%
  filter(dateTime < "2017-11-1") %>%
  mutate(site_no = fct_recode(site_no, "WOODLAND CREEK ABOVE MOUTH AT PHOENICIA NY" = "0136230002")) %>%
  filter(site_no == "WOODLAND CREEK ABOVE MOUTH AT PHOENICIA NY") %>% 
  mutate(forest.pc = case_when(dateTime <= "2017-10-29 22:50:00" ~ "20.604832", #sample 1
                               dateTime >"2017-10-29 22:50:00" & dateTime <= "2017-10-29 23:30:00" ~ "6.725934", #sample 2
                               dateTime >"2017-10-29 23:30:00" & dateTime <= "2017-10-30 01:35:00" ~ "13.469539", #sample 3
                               dateTime >"2017-10-30 01:35:00" ~ "7.456763")) %>% #sample 4
  mutate(alluvium.pc = case_when(dateTime <= "2017-10-29 22:50:00" ~ "38.35535873",
                                 dateTime >"2017-10-29 22:50:00" & dateTime <= "2017-10-29 23:30:00" ~  "54.15459003",
                                 dateTime >"2017-10-29 23:30:00" & dateTime <= "2017-10-30 01:35:00" ~ "44.81831169",
                                 dateTime >"2017-10-30 01:35:00" ~ "0.00003352")) %>%
  mutate(glacialtill.pc = case_when(dateTime <= "2017-10-29 22:50:00" ~ "37.38168",
                                    dateTime >"2017-10-29 22:50:00" & dateTime <= "2017-10-29 23:30:00" ~  "39.02704", 
                                     dateTime >"2017-10-29 23:30:00" & dateTime <= "2017-10-30 01:35:00" ~ "35.86378",
                                     dateTime >"2017-10-30 01:35:00" ~ "35.00058")) %>%
  mutate(lacustrine.pc = case_when(dateTime <= "2017-10-29 22:50:00" ~ "3.65813349",
                                   dateTime >"2017-10-29 22:50:00" & dateTime <= "2017-10-29 23:30:00" ~  "0.09243998",
                                   dateTime >"2017-10-29 23:30:00" & dateTime <= "2017-10-30 01:35:00" ~ "5.84837040",
                                   dateTime >"2017-10-30 01:35:00" ~ "57.54262542")) %>%
  gather(Source,value,forest.pc:lacustrine.pc) 

sourcumload1<-sourcumload %>%
  na.omit() %>%
  mutate(sourceTurbLoad = as.numeric(TurbLoad)*as.numeric(value)) %>%
  group_by(Source) %>%
  summarize(cumsum = sum(sourceTurbLoad)) %>%
  ungroup() %>%
  mutate(Source = fct_recode(Source, "Alluvium" = "alluvium.pc",
                             "Forest" = "forest.pc",
                             "Glacial\nTill" = "glacialtill.pc",
                             "Lacustrine" = "lacustrine.pc"))
sourcumload1$Source = factor(sourcumload1$Source, levels = c("Forest","Alluvium","Glacial\nTill","Lacustrine"))
library(gridExtra)
library(grid)
p<-ggplot(data=subset(sourcumload1))+
  geom_col(aes(x=Source, y=cumsum, fill=Source))+
  #facet_wrap(~SampleName, nrow = 2)+
  theme_bw()+
  guides(fill=FALSE) +
  theme(axis.text.x = element_text(size=12)) + 
  theme(legend.position = "top") +
  # guides(fill=guide_legend(title="Source"))+
  xlab("")+
  ylab("Estimated Source Loading (FNU*CFS)")+
  scale_y_continuous(expand = c(0,0), lim = c(0,2500000000)) +
  scale_fill_brewer(palette = "Set1") +
  theme(strip.background =element_rect(fill=usgsgreen),strip.text = element_text(colour = 'white'))
my_g <- grobTree(rectGrob(gp=gpar(fill="#006F41")),
                 textGrob("WOODLAND CREEK ABOVE MOUTH AT PHOENICIA NY\nESTIMATED SOURCE LOADINGS FROM OCT-29 TO NOV-1", x=0.5, hjust=0.5,
                          gp=gpar(col="white", cex=1.1)))
g<-arrangeGrob(my_g, p, heights=c(1,9),nrow=2,ncol=1)
grid.draw(g) 

                            