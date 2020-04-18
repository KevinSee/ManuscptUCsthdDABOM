# Author: Kevin See
# Purpose: Take cleaned capture histories and run DABOM
# Created: 4/25/2018
# Last Modified: 12/5/19
# Notes: For steelhead starting at Priest Rapids dam, in spawn year 2019

#-----------------------------------------------------------------
# install some packages
devtools::install_github("KevinSee/DABOM@v0.1.0")

# load needed libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(magrittr)
library(jagsUI)
library(DABOM)

#-----------------------------------------------------------------
# set species / year
spp = 'Steelhead'

# data.frame of lower and upper sites (upper sites excluded)
pairedSites = tibble(Lower = c('NAL',
                               'CHL',
                               'PES',
                               'ICL',
                               'ICL',
                               'MRW',
                               'CRW',
                               'SA1',
                               'OMK',
                               'ENA',
                               'ENA',
                               'ENA',
                               'ZSL',
                               'ZSL',
                               'ZSL'),
                     Upper = c('NAU',
                               'CHU',
                               'PEU',
                               'ICM',
                               'ICU',
                               'WFC',
                               'CRU',
                               'SA0',
                               'OBF',
                               'ENM',
                               'ENS',
                               'ENF',
                               'TON',
                               'NMC',
                               'OKC'))

# loop over 2 years
for(yr in 2016:2017) {
  # loop over whether to include upstream sites or not
  for(upSites in c(T, F)) {

    # load prepped data
    load(paste0('analysis/data/derived_data/DABOMready/UC_', spp, '_', yr, '.rda'))

    # if excluding upstream sites, drop detections from them.
    if(!upSites) {
      proc_list$ProcCapHist %<>%
        filter(!SiteID %in% pairedSites$Upper)
    }

    # get data on origin of each tag
    bio_df = read_excel('analysis/data/raw_data/WDFW/AllBioData.xlsx',
                        sheet = yr) %>%
      mutate(TagID = ifelse(!is.na(`PIT (Pelvic)`), `PIT (Pelvic)`, `PIT (Unknown)`)) %>%
      filter(!is.na(TagID)) %>%
      select(TagID,
             TrapDate = SurveyDate,
             Sex = `Sex(final)`,
             Origin = `Origin(final)`,
             ForkLength,
             Weight,
             FinalAge,
             Age = `Age (scales)`) %>%
      distinct()

    #--------------------------------------------------------------------------
    # Run DABOM
    #--------------------------------------------------------------------------
    # turn into wide version of capture histories, and add origin
    dabom_df = createDABOMcapHist(proc_list$ProcCapHist %>%
                                    filter(UserProcStatus),
                                  proc_list$NodeOrder,
                                  split_matrices = F) %>%
      # add origin information
      left_join(bio_df %>%
                  select(TagID, Origin) %>%
                  distinct()) %>%
      select(TagID, Origin, everything())

    # split observations into matrices to feed directly to JAGS
    dabom_list = createDABOMcapHist(proc_list$ProcCapHist %>%
                                      filter(UserProcStatus),
                                    proc_list$NodeOrder,
                                    split_matrices = T)

    if(sum(grepl('WVT', names(dabom_list$Entiat))) == 0) {
      dabom_list$Entiat %<>%
        mutate(WVTB0 = 0,
               WVTA0 = 0)
    }

    # add biological data to JAGS data
    dabom_list$fishOrigin = dabom_df %>%
      mutate(Origin = recode(Origin,
                             'W' = 1,
                             'H' = 2)) %>%
      pull(Origin)

    #------------------------------------------------------
    # write initial model
    basic_modNm = 'analysis/modelFiles/PRA_DABOM.txt'
    writeDABOM_PRA(file_name = basic_modNm)

    # mark some detection probabilities 0 or 100%
    if(upSites) {
      mod_path = paste0('analysis/modelFiles/PRA_DABOM_', spp, '_', yr, '.txt')
    }
    if(!upSites) {
      mod_path = paste0('modelFiles/PRA_DABOM_', spp, '_', yr, '_NoUppSites.txt')
    }

    fixNoFishNodes(basic_modNm,
                   mod_path,
                   proc_list$ProcCapHist %>%
                     filter(UserProcStatus),
                   proc_list$NodeOrder)

    # pull together all data to feed to JAGS, in a named list
    jags_data = createJAGSinputs_PRA(dabom_list)

    # create a function to spit out initial values for MCMC chains
    init_fnc = setInitialValues_PRA(dabom_list)

    # set parameters to save
    jags_params = setSavedParams_PRA()

    #----------------------------------------
    # run JAGS model
    set.seed(7)
    ptm <- proc.time()
    dabom_mod <- jags.basic(data = jags_data,
                            inits = init_fnc,
                            parameters.to.save = jags_params,
                            model.file = mod_path,
                            n.chains = 4,
                            n.iter = 5000,
                            n.burnin = 2500,
                            n.thin = 10,
                            # n.chains = 1,
                            # n.adapt = 2,
                            # n.iter = 2,
                            # n.burnin = 1,
                            # n.thin = 1,
                            parallel = T,
                            DIC = TRUE)
    proc.time() - ptm

    #----------------------------------------
    # save stuff
    if(upSites) {
      fileNm = paste0('analysis/modelFits/PRA_', spp, '_', yr, '_DABOM.rda')
    }
    if(!upSites) {
      fileNm = paste0('analysis/modelFits/PRA_', spp, '_', yr, '_DABOM_NoUppSites.rda')
    }
    save(dabom_mod, dabom_df, parent_child, proc_list, configuration, site_df, site_list, bio_df,
         file = fileNm)


  }
}
