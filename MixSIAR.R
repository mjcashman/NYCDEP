library(MixSIAR)

# Load mix data
library(readxl)
mix.filename <- "Y:/Access Databases/Catskills/Fingerprinting/TargetSamples_NDisRL_useMe.csv"
target<-read.csv(mix.filename) %>%
  select(-Basin)

# Load raw source data
source.filename <- "Y:/Access Databases/Catskills/Fingerprinting/SourceSamples_noRoad.csv"
source<-read.csv(source.filename)

#Bracket test
library(tidyverse)

source.bracket <- source %>% mutate(Type = "Source")
target.bracket <- target %>% mutate(Type = "Target")
bracket<-rbind(source.bracket,target.bracket)

minmax <- bracket %>%
  select(-Field.ID) %>%
  group_by(Type) %>% summarize_all(funs(min,max)) %>%
  group_by(Type) %>%
  gather(key=Tracer, value="Value",D50_min:Mass_Fraction_N_max) %>% spread(Type,Value) %>% 
  mutate(Diff = Source-Target)
  
###Only Ag and d13C failed this bracket

#Group source data for means
    source3 <- source %>%
      group_by(Type) %>% select (-Field.ID, -Ag, -Delta_13C)

    grouped_source<-source3 %>%
      ungroup()%>%
      mutate(Type=recode(Type, "Upper_Lacustrine" = "Lacustrine",
             "Lower_Lacustrine" = "Lacustrine",
             "Bank_Alluvium" = "Alluvium",
             "Terrace_Alluvium" = "Alluvium")) %>%
      group_by(Type) %>%
      summarize_all(funs(mean, sd))

    write.csv(grouped_source,"Y:/Access Databases/Catskills/Fingerprinting/grouped_source.csv", row.names=FALSE)

    source.count <- source3 %>%
      mutate(Type=recode(Type, "Upper_Lacustrine" = "Lacustrine",
                         "Lower_Lacustrine" = "Lacustrine",
                         "Bank_Alluvium" = "Alluvium",
                         "Terrace_Alluvium" = "Alluvium")) %>%
          group_by(Type) %>% tally()
    grouped_source<-read.csv("Y:/Access Databases/Catskills/Fingerprinting/grouped_source.csv")
    grouped_source<-left_join(grouped_source,source.count,by="Type")
    

    Mean_Tracers<-paste0("Mean",colnames(source3)[-1])
    SD_Tracers<-paste0("SD",colnames(source3)[-1])
    group_tracer_names<-c("Type", Mean_Tracers,SD_Tracers,"n")
    colnames(grouped_source) <- group_tracer_names

    write.csv(grouped_source,"Y:/Access Databases/Catskills/Fingerprinting/grouped_source_n.csv", row.names=FALSE)

mix.filename <- "Y:/Access Databases/Catskills/Fingerprinting/TargetSamples_NDisRL_useMe.csv"
target<-read.csv(mix.filename)
target<-target %>%  select (-Ag, -Delta_13C, -Basin) 
tracers<-colnames(target)[-1]
write.csv(target,"Y:/Access Databases/Catskills/Fingerprinting/target_bracket_remove.csv", row.names=FALSE)

#Load Final Versions for Target and Sources
mix.filename <- "Y:/Access Databases/Catskills/Fingerprinting/target_bracket_remove.csv"

mix <- load_mix_data(filename=mix.filename,
                     iso_names=tracers,
                     factors="Field.ID",
                     fac_random=FALSE,
                     fac_nested=FALSE,
                     cont_effects=NULL)

source.filename <- "Y:/Access Databases/Catskills/Fingerprinting/grouped_source_n.csv"
source <- load_source_data(filename=source.filename,
                           source_factors=NULL,
                           conc_dep=FALSE,
                           data_type="means",
                           mix)

# Load discrimination/TDF data

# Note that Galloway et al. [5] conducted feeding trials to create a “resource
# library”. In the mixing model, the sources are actually consumers fed exclusively
# each of the sources. This allowed them to set the discrimination = 0 (see
# “cladocera_discrimination.csv”).

discr.filename <- "Y:/Access Databases/Catskills/Fingerprinting/Catskills_discrimination.csv"
group.disc<-read.csv(discr.filename) %>%   
  mutate(Source=recode(Source, "Upper_Lacustrine" = "Lacustrine",
                                                     "Lower_Lacustrine" = "Lacustrine",
                                                     "Bank_Alluvium" = "Alluvium",
                                                     "Terrace_Alluvium" = "Alluvium")) 
group.disc1<-group.disc[-2:-3,]
write.csv(group.disc1, "Y:/Access Databases/Catskills/Fingerprinting/Catskills_discrimination_group.csv", row.names=FALSE)
discr.filename.group <- "Y:/Access Databases/Catskills/Fingerprinting/Catskills_discrimination_group.csv"

discr <- load_discr_data(filename=discr.filename.group, mix)

# DON'T make isospace plot - MixSIAR makes plots of each pairwise combination
# of tracers.  Here we have 22, so that would be '22 choose 2' = 231 biplots.

# Plot your prior
plot_prior(alpha.prior=1,source,plot_save_png=TRUE)

# Define model structure and write JAGS model file
model_filename <- "MixSIAR_model.txt"
resid_err <- FALSE
process_err <- TRUE
write_JAGS_model(model_filename, resid_err, process_err, mix, source)

# Run the JAGS model ("test" first, then "normal")
#jags.1 <- run_model(run="test", mix, source, discr, model_filename, alpha.prior=1, resid_err, process_err)

## "normal" took my laptop ~30 minutes to run
jags.1 <- run_model(run="normal", mix, source, discr, model_filename, alpha.prior=1, resid_err, process_err)

# Process diagnostics, summary stats, and posterior plots
# Note that since we fit “id” as a fixed effect, there is no inference on diet at
# the overall population level (no p.global). You should see posterior plots for
# all 14 mixture samples.
output_JAGS(jags.1, mix, source)
