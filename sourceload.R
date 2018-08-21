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
end <- "2017-11-4"

readNWISsite(sitelist)

gageGPS<-readNWISsite(sitelist)[,c(2,3,7,8)]


wideMulti <- readNWISuv(sitelist,pCodes, start, end,tz = "America/New_York") %>%
  dplyr::select(-ends_with("_cd"))
wideMulti<-renameNWISColumns(wideMulti)
wideMulti$TurbLoad <- wideMulti$Flow_Inst*wideMulti$Turb_Inst

cumload <- na.omit(wideMulti)
cumload <- cumload %>% 
  group_by(site_no) %>%
  mutate(cumTurbLoad = cumsum(TurbLoad)) %>%
  mutate(cumTurbLoadpc = (cumsum(TurbLoad)/max(cumsum(TurbLoad))*100)) %>%
  ungroup()

siteInfo <- attr(wideMulti, "siteInfo")
paramInfo <- attr(wideMulti, "variableInfo")

longMulti <- gather(cumload, variable, value, -site_no, -dateTime) %>%
  mutate(variable = as.factor(variable)) %>%
  mutate(site_no = as.factor(site_no))

levels(longMulti$variable) <- paramInfo$param_units
levels(longMulti$site_no) <- siteInfo$station_nm
levels(cumload$site_no) <- siteInfo$station_nm

peak<-which.max(cumload$Flow_Inst)


ggplot(longMulti, 
             aes(dateTime, value, color=site_no)) +
  geom_path(size=1.5) + xlab("") +
  geom_vline(xintercept = as.numeric(cumload$dateTime[peak])) +
  facet_wrap(variable ~ .,scales= "free") + 
  theme(legend.title=element_blank(),
        legend.position='bottom',legend.direction = "vertical")


ggplot(cumload, 
       aes(Flow_Inst, Turb_Inst, color=(dateTime))) +
  geom_path(size=1.5) + xlab("") +
  facet_wrap(site_no ~ .) + 
  theme(legend.title=element_blank(),
        legend.position='bottom',legend.direction = "vertical")+
  scale_color_viridis()
