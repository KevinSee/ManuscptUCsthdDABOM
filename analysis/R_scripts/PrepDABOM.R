# Author: Kevin See
# Purpose: Prep data for dabom, 2011 -
# Created: 1/9/2019
# Last Modified: 1/9/2019
# Notes: 

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(lubridate)
library(magrittr)
library(PITcleanr)
library(WriteXLS)

#-----------------------------------------------------------------
# set species / year
spp = 'Steelhead'

# build configuration table (requires internet connection)
org_config = buildConfig()

# manually add site for Colockum Creek (not in PTAGIS)
org_config = org_config %>%
  bind_rows(tibble(SiteID = 'CLK',
                   ConfigID = 100,
                   AntennaID = 'A1',
                   Node = 'CLK',
                   ValidNode = T,
                   # making these up
                   StartDate = as.POSIXct(ymd('20150101')),
                   SiteType = 'INT',
                   SiteName = 'Colockum Creek',
                   AntennaGroup = 'Single Colockum Ck',
                   SiteDescription = 'Tempoary single antenna.',
                   SiteTypeName = 'Instream Remote Detection System',
                   RKM = '740.001',
                   RKMTotal = 741))

# dataframe of sites for PRD DABOM model with some indication of network structure
site_df = writePRDNodeNetwork()

# customize some nodes based on DABOM framework
configuration = org_config %>%
  filter(!(SiteID == 'WAN' & SiteType == 'MRR'),
         !(SiteID == 'TMF' & SiteType == 'MRR'),
         !(SiteID == 'PRO' & SiteType == 'MRR')) %>%
  mutate(Node = ifelse(SiteID %in% c('RIA', 'RRF', 'WEA', 'PRV'),
                       SiteID,
                       Node)) %>%
  mutate(Node = ifelse(SiteID == 'PRDLD1',
                       'PRA',
                       Node)) %>%
  mutate(Node = ifelse(SiteID %in% c('TUF', 'TUMFBY', 'TUM'),
                       'TUM',
                       Node),
         Node = ifelse(SiteID == 'LNF' & AntennaID %in% c('01', '02'),
                       'LNFA0',
                       Node),
         Node = ifelse(SiteID == 'LNF' & AntennaID %in% c('03', '04'),
                       'LNFB0',
                       Node),
         Node = ifelse(SiteID == 'LEAV',
                       'LNFA0',
                       Node),
         Node = ifelse(SiteID == 'ICL' & ConfigID == 100,
                       'ICLB0',
                       Node),
         Node = ifelse(SiteID == 'CHIWAC',
                       'CHWA0',
                       Node),
         Node = ifelse(SiteID == 'CHIWAR',
                       'CHLA0',
                       Node),
         Node = ifelse(SiteID == 'CHIKAC',
                       'CHUA0',
                       Node),
         Node = ifelse(SiteID == 'WHITER',
                       'WTLA0',
                       Node),
         Node = ifelse(SiteID == 'LWENAT',
                       'LWNA0',
                       Node),
         Node = ifelse(SiteID == 'NASONC',
                       'NALA0',
                       Node),
         # any fish seen at Dryden dam should also be seen at LWE
         Node = ifelse(SiteID == 'DRY',
                       'LWEA0',
                       Node),
         # any fish seen at Chiwawa acclimation pond gets moved to CHL
         Node = ifelse(SiteID == 'CHP',
                       'CHLA0',
                       Node),
         Node = ifelse(SiteID == 'EBO',
                       'RRF',
                       Node),
         Node = ifelse(SiteID == 'EHL' & ConfigID == 100 & AntennaID == '02',
                       'EHLB0',
                       Node),
         Node = ifelse(SiteID == 'EHL' & ConfigID == 100 & AntennaID == '01',
                       'EHLA0',
                       Node),
         Node = ifelse(SiteID == 'EHL' & ConfigID == 110 & AntennaID == '03',
                       'EHLB0',
                       Node),
         Node = ifelse(SiteID == 'EHL' & ConfigID == 110 & AntennaID %in% c('01', '02'),
                       'EHLA0',
                       Node),
         Node = ifelse(SiteID == 'WEA' & AntennaID == 'C1',
                       'WVTB0',
                       Node),
         Node = ifelse(SiteID == 'WEA' & AntennaID == 'C2',
                       'WVTA0',
                       Node),
         Node = ifelse(SiteID == 'LBC' & ConfigID == 100,
                       'LBCB0',
                       Node),
         Node = ifelse(SiteID == 'MRC',
                       'MRCB0',
                       Node),
         Node = ifelse(SiteID %in% c('SSC', '18N', 'MHB', 'M3R', 'MWF'),
                       'MRCA0',
                       Node),
         # Node = ifelse(SiteID == 'TWISPW',
         #               'TWRA0',
         #               Node),
         Node = ifelse(SiteID == 'MSH' & AntennaID %in% c('02', '03'),
                       'MSHB0',
                       Node),
         Node = ifelse(SiteID == 'MSH' & AntennaID %in% c('01'),
                       'MSHA0',
                       Node),
         Node = ifelse(SiteID == 'MSH' & AntennaID == '00',
                       'METHB0',
                       Node),
         Node = ifelse(SiteID == 'METH',
                       'METHA0',
                       Node),
         Node = ifelse(SiteID == 'LLC' & ConfigID == 100,
                       ifelse(AntennaID == 'D3',
                              'LLCB0',
                              'LLCA0'),
                       Node),
         Node = ifelse(SiteID %in% c('OFB', 'OMF'),
                       'OMKA0',
                       Node),
         Node = ifelse(SiteID == 'ZSL',
                       ifelse(grepl('Weir 3', AntennaGroup, ignore.case = T),
                              'ZSLB0',
                              'ZSLA0'),
                       Node),
         Node = ifelse(SiteID == 'SA1' & ConfigID == 110,
                       'SA1B0',
                       Node),
         Node = ifelse(SiteID == 'OKC' & ConfigID == 100,
                       'OKCB0',
                       Node),
         Node = ifelse(SiteID == 'RCT' & ConfigID == 100,
                       'RCTB0',
                       Node),
         Node = ifelse(SiteID == 'BPC' & ConfigID == 100,
                       ifelse(AntennaID %in% c('C3'),
                              'BPCB0',
                              'BPCA0'),
                       Node),
         Node = ifelse(SiteID == 'PRH' & AntennaID %in% c('F1', 'F2', 'F3', 'F4'),
                       'PRHB0',
                       Node),
         Node = ifelse((SiteID == 'PRH' & AntennaID %in% c('F5', 'F6', '01', '02')) | SiteID %in% c('DDM', 'DM', 'UM', 'UUM', 'UP'),
                       'PRHA0',
                       Node),
         Node = ifelse(SiteID == 'PRO' & SiteType == 'INT',
                       'PROB0',
                       Node),
         Node = ifelse(SiteID %in% c('CHANDL', 'SAT', 'TOP', 'SUN', 'LNR', 'ROZ', 'LMC', 'TAN') | SiteID == 'PRO' & SiteType == 'MRR',
                       'PROA0',
                       Node),
         Node = ifelse(SiteID == 'ICH',
                       'ICHB0',
                       Node),
         Node = ifelse(grepl('522\\.', RKM) & RKMTotal > 538,
                       'ICHA0',
                       Node),
         Node = ifelse(SiteID == 'MDR',
                       'MDRB0',
                       Node),
         Node = ifelse(SiteID %in% c('LWD', 'BGM', 'NBA', 'MCD'),
                       'MDRA0',
                       Node),
         Node = ifelse(SiteID == 'HST',
                       'HSTB0',
                       Node),
         Node = ifelse(SiteID %in% c('BBT', 'COP', 'PAT'),
                       'HSTA0',
                       Node),
         Node = ifelse(SiteID == 'JD1',
                       'JD1B0',
                       Node),
         Node = ifelse(SiteID %in% c('30M', 'BR0', 'JDM', 'SJ1', 'SJ2', 'MJ1'),
                       'JD1A0',
                       Node),
         Node = ifelse(SiteID != 'JD1' & as.integer(stringr::str_split(RKM, '\\.', simplify = T)[,1]) < 351,
                       'BelowJD1',
                       Node)) %>%
  distinct()

# correct a couple RKM values
configuration = configuration %>%
  mutate(RKM = ifelse(SiteID == 'SA1',
                      '858.041.003',
                      RKM),
         RKMTotal = ifelse(SiteID == 'SA1',
                           902,
                           RKMTotal)) %>%
  mutate(RKM = ifelse(SiteID == 'TON',
                      '858.133.001',
                      RKM),
         RKMTotal = ifelse(SiteID == 'TON',
                           992,
                           RKMTotal)) %>%
  mutate(RKM = ifelse(grepl('WVT', Node),
                      '829.001',
                      RKM),
         RKMTotal = ifelse(grepl('WVT', Node),
                           830,
                           RKMTotal))


# group sites appropriately
site_list = vector('list', 5)
names(site_list) = c('BelowPriest', 'Wenatchee', 'Entiat', 'Methow', 'Okanogan')
for(grp in names(site_list)) {
  site_list[[grp]] = site_df %>%
    filter(grepl(grp, path)) %>%
    select(SiteID) %>%
    as.matrix() %>%
    as.character()
}
site_list[['Wenatchee']] = c('RIA', site_list[['Wenatchee']])
site_list[['Entiat']] = c('RRF', 'WVT', site_list[['Entiat']])
site_list[['Methow']] = c('WEA', site_list[['Methow']])

#-----------------------------------------------------------------
# run through each year
for(yr in 2011:2017) {
  
  # start date is June 1 of the previous year
  startDate = paste0(yr - 1, '0601')
  
  # build parent-child table
  parent_child = createParentChildDf(site_df,
                                     configuration,
                                     startDate = startDate)
  # get raw observations from PTAGIS
  # These come from running a saved query on the list of tags to be used
  observations = read_csv(paste0('data/PTAGIS/UC_Sthd_', yr, '_CTH.csv'))
  # filter out observations with wonky dates
  observations %<>%
    filter(mdy_hms(`Event Date Time Value`) < ymd(startDate) + years(1) + months(1) | mdy_hms(`Event Release Date Time Value`) < ymd(startDate) + years(1) + months(1))
  
  # get processed obs from WDFW
  wdfw_obs = read_csv(paste0('data/WDFW/Steelhead_PRD_BY', yr, '.csv')) %>%
    rename(TagID = Tag_ID,
           TrapDate = Date,
           TUM = TUF) %>%
    mutate(TrapDate = mdy(TrapDate))
  
  names(wdfw_obs) %<>%
    str_replace('_down$', 'B0') %>%
    str_replace('_up$', 'A0')
  
  if('MSH' %in% names(wdfw_obs)) wdfw_obs %<>% rename(METHB0 = MSH)
  if('METH' %in% names(wdfw_obs)) wdfw_obs %<>% rename(METHA0 = METH)
  if('Hatchery_Wild' %in% names(wdfw_obs)) wdfw_obs %<>% rename(Origin = Hatchery_Wild)
  if(!'Sex' %in% names(wdfw_obs)) wdfw_obs %<>% mutate(Sex = NA)
  if(!'Age' %in% names(wdfw_obs)) wdfw_obs %<>% mutate(Age = NA)
  
  wdfw_obs %>%
    filter(is.na(TrapDate)) %>%
    nrow() %>%
    cat(paste('There are', ., 'tags without a trap date in', yr))
  
  bio_df = wdfw_obs %>%
    select(TagID,
           Origin) %>%
    distinct()
  
  valid_obs = wdfw_obs %>%
    select(TagID,
           TrapDate,
           everything(),
           -(Sex:Age)) %>%
    gather(Node, seen, -TagID, -TrapDate, -Origin) %>%
    filter(seen > 0)
  
  myObs = observations %>%
    select(TagID = `Tag Code`,
           Origin = `Mark Rear Type Name`,
           SiteID = `Event Site Code Value`,
           ObsTime = `Event Date Time Value`,
           ObsRel = `Event Release Date Time Value`,
           AntennaID = `Antenna ID`,
           ConfigID = `Antenna Group Configuration Value`) %>%
    mutate_at(vars(ObsTime, ObsRel),
              funs(mdy_hms)) %>%
    rowwise() %>%
    mutate(ObsDate = max(ObsTime, ObsRel, na.rm = T)) %>%
    ungroup() %>%
    select(-ObsTime, -ObsRel)
  
  myObs %>%
    mutate(Origin = recode(Origin,
                           'Hatchery Reared' = 'H',
                           'Wild Fish or Natural Production' = 'W')) %>%
    left_join(configuration %>%
                filter(StartDate > ymd(startDate) |
                         is.na(EndDate)) %>%
                select(SiteID:Node)) %>%
    group_by(TagID, Origin, SiteID, Node) %>%
    summarise_at(vars(ObsDate),
                 funs(max),
                 na.rm = T) %>%
    ungroup() %>%
    rename(lastObsDate = ObsDate) %>%
    full_join(valid_obs) %>%
    filter(is.na(lastObsDate)) %>% xtabs(~ Node, .)
    # filter(is.na(seen)) %>% xtabs(~ Node, .)
    
  
}