# Author: Kevin See
# Purpose: Take cleaned capture histories and run DABOM
# Created: 4/25/2018
# Last Modified: 12/5/19
# Notes: For steelhead starting at Priest Rapids dam, in spawn year 2019

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(magrittr)
library(jagsUI)
library(RPushbullet)
library(DABOM)

#-----------------------------------------------------------------
# set species / year
spp = 'Steelhead'
yr = 2019

# for(yr in 2019:2017) {
  
  # load prepped data
  load(paste0('data/DABOMready/UC_', spp, '_', yr, '.rda'))
  
  #-----------------------------------------------------------------
  # update with reviewed capture histories
  if(yr %in% c(2017:2018)) {
    proc_ch = read_excel(paste0('data/WDFW/UC_Steelhead_', yr, '_BT.xlsx')) %>%
      mutate_at(vars(matches('ProcStatus')),
                funs(as.logical)) %>%
      mutate_at(vars(TrapDate),
                funs(ymd)) %>%
      mutate_at(vars(matches('ObsDate')),
                funs(ymd_hms)) %>%
      mutate_at(vars(Group),
                funs(as.factor))
  }
  
  if(yr == 2018) {
    proc_ch2 = read_excel('data/WDFW/UC_Steelhead_2018_BT20181231.xlsx') %>%
      mutate_at(vars(matches('ProcStatus')),
                funs(as.logical)) %>%
      mutate_at(vars(TrapDate),
                funs(ymd)) %>%
      mutate_at(vars(matches('ObsDate')),
                funs(ymd_hms)) %>%
      mutate_at(vars(Group),
                funs(as.factor))
    
    CLKtags = proc_ch2 %>%
      filter(SiteID == 'CLK') %>%
      select(TagID) %>%
      distinct() %>%
      as.matrix() %>% as.character()
  }
  
  # fix one tag
  if(yr == 2019){
    proc_ch = proc_list$ProcCapHist %>%
      mutate(UserProcStatus = if_else(TagID == "3DD.0077A0D6E8" & Node %in% c("ENAB0", "ENAA0"),
                                      F, UserProcStatus))
  }
    
  
  # update proc_list
  proc_list$ProcCapHist = proc_ch
  
  # get data on origin of each tag
  if(yr == 2017) bioFileNm = 'Steelhead_PRD_BY2017_FlatFile.xlsx'
  if(yr == 2018) bioFileNm = 'BY18 BioData.xlsx'
  if(yr == 2019) bioFileNm = 'BY19 BioData.xlsx'
  
  bio_df = read_excel(paste0('data/WDFW/', bioFileNm)) %>%
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
    select(Origin) %>%
    mutate(Origin = recode(Origin,
                           'W' = 1,
                           'H' = 2)) %>%
    as.matrix() %>%
    as.vector()
  
  #------------------------------------------------------
  # write initial model
  basic_modNm = 'modelFiles/PRA_DABOM.txt'
  writeDABOM_PRA(file_name = basic_modNm)
  
  # mark some detection probabilities 0 or 100%
  mod_path = paste0('modelFiles/PRA_DABOM_', spp, '_', yr, '.txt')
  # mod_path = paste0('modelFiles/PRA_DABOM_', spp, '_', yr, '_NoUppSites.txt')
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
  
  pbPost('note',
         paste('UC Sthd', yr),
         paste('JAGS model has finished running. It took', round(c(proc.time() - ptm)[3] / 3600, 1), 'hours to run.'),
         recipients = NA,
         apikey = "dCj80OM9XSYx1xRUyvCp5KlN4aT456Kg")
  
  #----------------------------------------
  # save stuff
  fileNm = paste0('modelFits/PRA_', spp, '_', yr, '_DABOM.rda')
  save(dabom_mod, dabom_df, parent_child, proc_list, configuration, site_df, site_list, bio_df,
       file = fileNm)
  
# }
