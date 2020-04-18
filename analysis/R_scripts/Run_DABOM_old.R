# Author: Kevin See
# Purpose: Take cleaned capture histories and run DABOM
# Created: 4/25/2018
# Last Modified: 12/17/2018
# Notes: For steelhead starting at Priest Rapids dam, in spawn year 2017

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(lubridate)
library(stringr)
library(forcats)
library(readxl)
library(magrittr)
library(PITcleanr)
library(DABOM)
library(jagsUI)
library(WriteXLS)

#-----------------------------------------------------------------
# set species / year
spp = 'Steelhead'
yr = 2018
startDate = paste0(yr - 1, '0601')


#-----------------------------------------------------------------
# get biological data
if(yr == 2018) {
  bio_raw = read_csv('data/WDFW/BY18 TagList.csv') %>%
    select(TagID = P4,
           Origin = `P4 Origin`)
  # write list of tag codes
  bio_raw %>%
    select(TagID) %>%
    write_tsv(path = paste0('data/TagCodes/UC_Sthd_tags_', yr, '.txt'),
              col_names = F)
}
if(yr == 2017) {
  bio_raw = read_excel(paste0('data/WDFW/Steelhead_PRD_BY', yr, '_FlatFile.xlsx'))
}
if(yr == 2016) {
  bio_raw = read_excel('data/WDFW/Steelhead_PRD_BY2016_QCI.xlsx',
                     'BioData')
}


bio_df = bio_raw %>%
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

bio_df %<>%
  filter(TagID %in% TagID[duplicated(TagID)],
         !is.na(Age)) %>%
  bind_rows(bio_df %>%
              filter(!TagID %in% TagID[duplicated(TagID)]))

#-----------------------------------------------------------------
# pre-processing
# build configuration table
org_config = buildConfig()

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
         Node = ifelse(SiteID == 'EBO',
                       'RRF',
                       Node),
         # Node = ifelse(SiteID == 'EBO' & ConfigID == 100,
         #               'EBOB0',
         #               Node),
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

# correct a couple RKM value
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

# build parent-child table
parent_child = createParentChildDf(site_df,
                                   configuration,
                                   startDate = startDate)

#--------------------------------------------------------------------------
# get raw observations from PTAGIS
observations = read_csv(paste0('analysis/data/PTAGIS/UC_Sthd_', yr, '_CTH.csv'))

proc_list = processCapHist_PRD(startDate = startDate,
                               configuration = configuration,
                               parent_child = parent_child,
                               observations = observations %>%
                                 filter(mdy_hms(`Event Date Time Value`) < ymd(startDate) + years(1) + months(1) | mdy_hms(`Event Release Date Time Value`) < ymd(startDate) + years(1) + months(1)),
                               site_df = site_df,
                               truncate = T,
                               save_file = T,
                               file_name = paste0('data/PITcleanr/UC_', spp, '_', yr, '.xlsx'))


# valid_paths = proc_list$ValidPaths
# valid_obs = proc_list$ValidObs
# node_order = proc_list$NodeOrder %>%
#   select(-Group, -BranchNum) %>%
#   left_join(stack(site_list) %>%
#               tbl_df() %>%
#               select(Group = ind,
#                      NodeSite = values) %>%
#               mutate(BranchNum = as.integer(Group))) %>%
#   distinct()
# node_list = createNodeList(node_order)
# proc_ch = proc_list$ProcCapHist %>%
#   mutate(UserProcStatus = AutoProcStatus) %>%
#   filter(UserProcStatus)


#--------------------------------------------------------------------------



# read in processed observaions from WDFW
uc_data = read_csv(paste0('analysis/data/raw_data/WDFW/Steelhead_PRD_BY', yr, '.csv')) %>%
  rename(TagID = Tag_ID,
         TUM = TUF)
names(uc_data) = gsub('_down$',
                      'B0',
                      gsub('_up$',
                           'A0',
                           names(uc_data)))
uc_data %<>%
  rename(METHB0 = MSH,
         METHA0 = METH)

bio_df = uc_data %>%
  select(TagID,
         Origin = Hatchery_Wild) %>%
  distinct()


# # remove some upper sites
# uc_data %<>%
#   mutate_at(vars(matches('NAU'),
#                  matches('CHU'),
#                  matches('PEU'),
#                  matches('ICM'),
#                  matches('ICU'),
#                  matches('WFC'),
#                  matches('CRU'),
#                  matches('SA0'),
#                  matches('OBF'),
#                  matches('ENM'),
#                  matches('ENS'),
#                  matches('ENF'),
#                  matches('TON'),
#                  matches('NMC'),
#                  matches('OKC')),
#             funs(ifelse(. == 1, 0, 0)))


# uc_data %>%
#   mutate(PRA = 1) %>%
#   select(-(Sex:Age)) %>%
#   rename(TrapDate = Date) %>%
#   gather(Node, seen, -TagID, -TrapDate) %>%
#   mutate(Node = recode(Node,
#                        'BelowJD' = 'BelowJD1',
#                        'FOS' = 'FST',
#                        'TUF_rm' = 'TUM')) %>%
#   filter(seen == 1) %>%
#   distinct() %>%
#   left_join(proc_list$ProcCapHist %>%
#               group_by(TagID, Node) %>%
#               filter(ObsDate == max(ObsDate)) %>%
#               slice(1) %>%
#               distinct() %>%
#               mutate(TrapDate = floor_date(TrapDate, unit = 'days')) %>%
#               select(-TrapDate)) %>%
#   arrange(TagID, ObsDate)



valid_obs = uc_data %>%
  select(TagID,
         TrapDate = Date,
         everything(),
         -(Sex:Age)) %>%
  gather(Node, ObsDate, -TagID, -TrapDate) %>%
  mutate(Node = recode(Node,
                       'BelowJD' = 'BelowJD1',
                       'FOS' = 'FST',
                       'TUF_rm' = 'TUM')) %>%
  filter(ObsDate > 0) %>%
  distinct() %>%
  bind_rows(uc_data %>%
              select(TagID,
                     TrapDate = Date) %>%
              distinct() %>%
              mutate(Node = 'PRA',
                     ObsDate = 1)) %>%
  left_join(configuration %>%
              filter(StartDate > ymd(startDate) |
                       is.na(EndDate)) %>%
              select(Node, SiteID, SiteType, SiteName, SiteDescription) %>%
              distinct() %>%
              group_by(Node) %>%
              slice(1) %>%
              ungroup()) %>%
  arrange(TagID) %>%
  mutate(ObsDate = if_else(Node == 'PRA',
                           TrapDate,
                           TrapDate + days(1)))

# generate valid paths
valid_paths = getValidPaths(parent_child)

# create node order and list of nodes within sevaral population groups
node_order = createNodeOrder(valid_paths,
                             configuration) %>%
  left_join(stack(site_list) %>%
              tbl_df() %>%
              select(Group = ind,
                     NodeSite = values) %>%
              mutate(BranchNum = as.integer(Group))) %>%
  distinct()


# node_list = createNodeList(node_order)

# table(node_order$Node %in% unique(configuration$Node))
# node_order$Node[!node_order$Node %in% unique(configuration$Node)]



# process the capture histories
proc_ch = uc_data %>%
  mutate(PRA = 1) %>%
  select(-(Sex:Age)) %>%
  rename(TrapDate = Date) %>%
  gather(Node, seen, -TagID, -TrapDate) %>%
  mutate(Node = recode(Node,
                       'BelowJD' = 'BelowJD1',
                       'FOS' = 'FST',
                       'TUF_rm' = 'TUM')) %>%
  filter(seen == 1) %>%
  distinct() %>%
  mutate(ObsDate = if_else(Node == 'PRA',
                           TrapDate,
                           TrapDate + days(1))) %>%
  mutate(UserProcStatus = T)


# proc_ch = uc_data %>%
#   mutate(PRA = 1) %>%
#   select(-(Sex:Age)) %>%
#   rename(TrapDate = Date) %>%
#   gather(Node, seen, -TagID, -TrapDate) %>%
#   mutate(Node = recode(Node,
#                        'BelowJD' = 'BelowJD1',
#                        'FOS' = 'FST',
#                        'TUF_rm' = 'TUM')) %>%
#   filter(seen == 1) %>%
#   distinct() %>%
#   left_join(proc_list$ProcCapHist %>%
#               group_by(TagID, Node) %>%
#               filter(ObsDate == max(ObsDate)) %>%
#               slice(1) %>%
#               distinct() %>%
#               mutate(TrapDate = floor_date(TrapDate, unit = 'days'))) %>%
#   arrange(TrapDate, TagID, ObsDate) %>%
#   select(-seen) %>%
#   mutate(UserProcStatus = T)

proc_list = list('ValidPaths' = valid_paths,
                 'NodeOrder' = node_order,
                 'ValidObs' = valid_obs,
                 'ProcCapHist' = proc_ch)

#--------------------------------------------------------------------------
# Run DABOM
#--------------------------------------------------------------------------
# turn into wide version of capture histories, and add origin
dabom_df = createDABOMcapHist(proc_ch,
                              node_order,
                              split_matrices = F) %>%
  # add origin information
  left_join(bio_df %>%
              select(TagID, Origin)) %>%
  select(TagID, Origin, everything())

# split observations into matrices to feed directly to JAGS
dabom_list = createDABOMcapHist(proc_ch,
                                node_order,
                                split_matrices = T)

# identical(node_list, lapply(dabom_list, names))
# stack(node_list) %>%
#   bind_cols(stack(lapply(dabom_list, names))) %>%
#   filter(values != values1)

# add biological data
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
               proc_ch,
               node_order)

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
                        n.iter = 10000,
                        n.burnin = 4000,
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


if(grepl('NoUppSites', mod_path)) {
  fileNm = paste0('modelFits/PRA_', spp, '_', yr, '_DABOM_NoUppSites.rda')
}

if(!grepl('NoUppSites', mod_path)) {
  fileNm = paste0('modelFits/PRA_', spp, '_', yr, '_DABOM.rda')
}

save(dabom_mod, dabom_df, parent_child, proc_list, configuration, site_df, site_list, bio_df,
     file = fileNm)


#----------------------------------------------------------------
# for adding data from double arrays at LWE, ENL and LMR
# (WDFW data originally consolidated them into single arrays)
#----------------------------------------------------------------
# this is for the dataset Lynn Waterhouse is using, for BY 2012
uc_data = read_excel('/Users/kevin/Dropbox/Personal/ResearchPapers/2016_UC_BranchingModel/Data/Steelhead_PRD_BY2012.csv-1.xlsx') %>%
  rename(TagID = Tag_ID)
names(uc_data) = gsub('_down$',
                      'B0',
                      gsub('_up$',
                           'A0',
                           names(uc_data)))


ucData_updated = uc_data %>%
  select(-Date, -Hatchery_Wild) %>%
  gather(Node, seen, -TagID) %>%
  filter(seen == 1) %>%
  filter(!Node %in% c('LWE', 'ENL', 'LMR')) %>%
  bind_rows(uc_data %>%
              select(-Date, -Hatchery_Wild) %>%
              gather(Node, seen, -TagID) %>%
              filter(seen == 1) %>%
              filter(Node %in% c('LWE', 'ENL', 'LMR')) %>%
              rename(SiteID = Node) %>%
              left_join(proc_ch %>%
                          filter(AutoProcStatus) %>%
                          select(TagID, SiteID, Node) %>%
                          mutate(seen = 1) %>%
                          filter(SiteID %in% c('LWE', 'ENL', 'LMR'))) %>%
              distinct() %>%
              select(-SiteID)) %>%
  filter(!is.na(Node)) %>%
  spread(Node, seen, fill = 0) %>%
  full_join(uc_data %>%
              select(TagID:Hatchery_Wild)) %>%
  select(TagID, Date, Hatchery_Wild, everything())

# add any sites/nodes that didn't see ANY fish
missNodes = setdiff(names(uc_data), names(ucData_updated))
missNodes = missNodes[-match(c('LWE', 'ENL', 'LMR'), missNodes)]

if(length(missNodes) > 0) {
  for(node in missNodes) {
    ucData_updated[,node] = 0
  }
}
ucData_updated %<>%
  mutate_at(vars(-(TagID:Hatchery_Wild)),
            funs(if_else(is.na(.), 0, .))) %>%
  select(TagID, Date, Hatchery_Wild, LWEB0, LWEA0, one_of(names(uc_data)), everything()) %>%
  select(TagID:LWNA0, ENLB0, ENLA0, MADB0:SA1A0, LMRB0, LMRA0, MRWB0:GLCA0)

setdiff(names(uc_data), names(ucData_updated))
setdiff(names(ucData_updated), names(uc_data))

names(uc_data)[which(colSums(uc_data[,-c(1:3)]) == 0) + 3]

# change names of arrays to match older UC data (for Lynn Waterhouse)
names(ucData_updated) = str_replace(names(ucData_updated), 'B0$', '_down')
names(ucData_updated) = str_replace(names(ucData_updated), 'A0$', '_up')
write_csv(ucData_updated,
          path = paste0('/Users/kevin/Dropbox/Personal/ResearchPapers/2016_UC_BranchingModel/Outgoing/Steelhead_PRD_BY', yr, '_updated.csv'))

