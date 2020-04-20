# Author: Kevin See
# Purpose: Summarise results of DABOM
# Created: 4/25/2018
# Last Modified: 12/19/2018
# Notes: For steelhead starting at Priest Rapids dam, in spawn year 2017 and 2018

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
# library(lubridate)
# library(stringr)
# library(forcats)
library(readxl)
library(magrittr)
library(msm)
library(PITcleanr)
library(DABOM)
library(STADEM)
library(jagsUI)
library(WriteXLS)

#-----------------------------------------------------------------
# set species / year
spp = 'Steelhead'
yr = 2019

#-----------------------------------------------------------------
# load results
load(paste0('modelFits/PRA_', spp, '_', yr, '_DABOM.rda'))
# load(paste0('modelFits/PRA_', spp, '_', yr, '_DABOM_PITcleanr.rda'))

if('Date' %in% names(bio_df)) {
  bio_df %<>%
    rename(TrapDate = Date)
}

# fix a few prefixes for tags with known issues (Ben Truscott had to enter them incorrectly in his database)
bio_df %>%
  filter(!TagID %in% dabom_df$TagID)

bio_df %<>%
  mutate(TagID = ifelse(!TagID %in% dabom_df$TagID,
                        str_replace(TagID, 
                                    '^3DD', 
                                    '3DA'),
                        TagID))

#------------------------------------------------------
# estimate final spawning location
tag_summ = summariseTagData(proc_list$ProcCapHist %>%
                              filter(UserProcStatus) %>%
                              mutate(lastObsDate = ObsDate) %>%
                              left_join(proc_list$NodeOrder %>%
                                          mutate(Group = fct_expand(Group, 'WellsPool'),
                                                 Group = if_else(Node == 'WEA',
                                                                 'WellsPool',
                                                                 as.character(Group)),
                                                 Group = ifelse(NodeSite %in% c('FST', 'RRF', 'WVT', 'RIA', 'CLK'),
                                                                NA,
                                                                Group),
                                                 Group = factor(Group,
                                                                levels = c('Wenatchee', 'Entiat', 'Methow', 'Okanogan', 'WellsPool', 'BelowPriest')))) %>%
                              mutate(SiteID = NodeSite),
                            trap_data = bio_df %>%
                              mutate(Age = str_replace(Age, 'r', 'R'))) %>%
  rename(Branch = Group)

tag_summ %>% 
  filter(is.na(TrapDate)) %>% 
  select(TagID)

proc_list$NodeOrder %>%
  mutate(Group = fct_expand(Group, 'WellsPool'),
         Group = if_else(Node == 'WEA',
                         'WellsPool',
                         as.character(Group)),
         Group = ifelse(NodeSite %in% c('FST', 'RRF', 'WVT', 'RIA', 'CLK'),
                        NA,
                        Group),
         Group = factor(Group,
                        levels = c('Wenatchee', 'Entiat', 'Methow', 'Okanogan', 'WellsPool', 'BelowPriest'))) %>%
  select(Group, NodeSite, RKM) %>%
  distinct() %>%
  arrange(Group, RKM) %>%
  # as.data.frame()
  split(list(.$Group)) %>%
  map(.f = function(x) x$NodeSite)

tag_summ %>%
  filter(is.na(Branch)) %>%
  # xtabs(~ AssignSpawnNode, .)
  xtabs(~ AssignSpawnSite, .)


bioList = list('Origin' = tag_summ %>%
                 filter(!is.na(Branch)) %>%
                 group_by(Branch, Origin) %>%
                 summarise(nTags = n_distinct(TagID)) %>%
                 spread(Origin, nTags,
                        fill = as.integer(0)) %>%
                 ungroup() %>%
                 mutate(propW = W / (W + H),
                        propH = 1 - propW,
                        propOrgSE = sqrt((propW * (1 - propW)) / (W + H))),
               'AllSex' = tag_summ %>%
                 filter(!is.na(Branch)) %>%
                 group_by(Branch, Origin, Sex) %>%
                 summarise(nTags = n_distinct(TagID[!is.na(Sex)])) %>%
                 filter(!is.na(Sex)) %>%
                 ungroup() %>%
                 spread(Sex, nTags,
                        fill = as.integer(0)) %>%
                 mutate(total_sexed = F + M,
                        propF = F / (F + M),
                        propM = 1 - propF,
                        propSexSE = sqrt((propF * (1 - propF)) / (M + F))) %>%
                 select(Branch, Origin, total_sexed:propSexSE) %>%
                 arrange(Origin, Branch),
               'AllAge' = tag_summ %>%
                 filter(!is.na(Branch)) %>%
                 group_by(Branch, Origin, Age) %>%
                 summarise(nTags = n_distinct(TagID)) %>%
                 ungroup() %>%
                 group_by(Branch, Origin) %>%
                 mutate(total_aged = sum(nTags)) %>%
                 spread(Age, nTags,
                        fill = 0) %>%
                 select(Branch, Origin, total_aged,
                        not_aged = `<NA>`,
                        everything()) %>%
                 mutate_at(vars(-c(1:2)),
                           list(as.integer)) %>%
                 arrange(Origin, Branch))

# save biological summary as Excel
# WriteXLS(bioList,
#          paste0('outgoing/PRA_BioSumm_', spp, '_', yr, '.xlsx'))


#-----------------------------------------------------------------
# # detection probabilities
detect_summ = summariseDetectProbs(dabom_mod, proc_list$ProcCapHist)

detect_summ %>%
  filter(sd > 0) %>%
  arrange(desc(sd))

# movement probabilities
trans_df = compileTransProbs_PRA(dabom_mod)

trans_summ = trans_df %>%
  group_by(Origin, param) %>%
  summarise(mean = mean(value),
            median = median(value),
            mode = estMode(value),
            sd = sd(value),
            skew = moments::skewness(value),
            kurtosis = moments::kurtosis(value),
            lowerCI = coda::HPDinterval(coda::as.mcmc(value))[,1],
            upperCI = coda::HPDinterval(coda::as.mcmc(value))[,2]) %>%
  mutate_at(vars(mean, median, mode, sd, matches('CI$')),
            funs(ifelse(. < 0, 0, .))) %>%
  ungroup()

#-----------------------------------------------------------------
# total escapement past Priest
# window count
totWinCnt = getWindowCounts(dam = 'PRD',
                            spp = 'Steelhead',
                            start_date = paste0(yr-1, '0601'),
                            end_date = paste0(yr, '0531')) %>%
  summarise_at(vars(win_cnt),
               funs(sum)) %>%
  as.matrix() %>%
  as.integer()

# reascension data
reascData = queryPITtagData(damPIT = 'PRA',
                            spp = 'Steelhead',
                            start_date = paste0(yr-1, '0601'),
                            end_date = paste0(yr, '0531'))

# adjust for re-ascension and origin
totEscape = reascData %>%
  mutate(SpawnYear = yr,
         TagIDAscentCount = ifelse(is.na(TagIDAscentCount),
                                   0, TagIDAscentCount),
         ReAscent = ifelse(TagIDAscentCount > 1, T, F)) %>%
  group_by(Species, SpawnYear, Date) %>%
  summarise(tot_tags = n_distinct(TagID),
            reascent_tags = n_distinct(TagID[ReAscent])) %>%
  ungroup() %>%
  group_by(Species, SpawnYear) %>%
  summarise_at(vars(matches('tags')),
               funs(sum),
               na.rm = T) %>%
  ungroup() %>%
  mutate(reascRate = reascent_tags / tot_tags,
         reascRateSE = sqrt(reascRate * (1 - reascRate) / tot_tags),
         totWinCnt = totWinCnt,
         adjWinCnt = totWinCnt * (1 - reascRate),
         adjWinCntSE = totWinCnt * reascRateSE) %>%
  bind_cols(bio_df %>%
              group_by(Origin) %>%
              summarise(nTags = n_distinct(TagID)) %>%
              spread(Origin, nTags,
                     fill = as.integer(0)) %>%
              ungroup() %>%
              mutate(propW = W / (W + H),
                     propH = 1 - propW,
                     propOrgSE = sqrt((propW * (1 - propW)) / (W + H))))
  
totEscape %>%
  mutate(#alpha_reasc = ((1 - reascRate) / adjWinCntSE^2 - 1 / reascRate) * reascRate^2,
    alpha_reasc = ((reascRate) / adjWinCntSE^2 - 1 / (1 - reascRate)) * (1 - reascRate)^2,
    beta_reasc = alpha_reasc * (1 / reascRate),
    alpha_W = ((1 - propW) / propOrgSE^2 - 1 / propW) * propW^2,
    beta_W = alpha_W * (1 / propW - 1)) %>%
  select(Species, SpawnYear, totWinCnt, alpha_reasc:beta_W)

mean(rbeta(1000, 326, 1092))
mean(rbeta(1000, -0.963, -25.9))

orgEscapeTmp = totEscape %>%
  mutate(Hescp = propH * adjWinCnt,
         HescpSE = deltamethod(~ x1 * x2,
                               mean = c(propH, adjWinCnt),
                               cov = diag(c(propOrgSE, adjWinCntSE)^2))) %>%
  mutate(Wescp = propW * adjWinCnt,
         WescpSE = deltamethod(~ x1 * x2,
                               mean = c(propW, adjWinCnt),
                               cov = diag(c(propOrgSE, adjWinCntSE)^2))) %>%
  select(Species, SpawnYear, matches('escp'))

orgEscape = orgEscapeTmp %>%
  gather(var, value, -(Species:SpawnYear)) %>%
  filter(!grepl('SE', var)) %>%
  rename(Origin = var,
         TotEscp = value) %>%
  mutate(Origin = recode(Origin,
                         'Hescp' = 'Hatchery',
                         'Wescp' = 'Natural')) %>%
  left_join(orgEscapeTmp %>%
              gather(var, value, -(Species:SpawnYear)) %>%
              filter(grepl('SE', var)) %>%
              rename(Origin = var,
                     TotEscpSE = value) %>%
              mutate(Origin = recode(Origin,
                                     'HescpSE' = 'Hatchery',
                                     'WescpSE' = 'Natural')))

#-----------------------------------------------------------------
# escapement estimates
#-----------------------------------------------------------------
# using delta method
# escape_summ = trans_summ %>%
#   select(Origin, param, move = median, sd) %>%
#   left_join(orgEscape) %>%
#   rowwise() %>%
#   mutate(Est = move * TotEscp,
#          SE = deltamethod(~ x1 * x2,
#                           mean = c(move, TotEscp),
#                           cov = diag(c(sd, TotEscpSE)^2))) %>%
#   ungroup() %>%
#   select(Species, SpawnYear, Origin, Area = param, Est, SE) %>%
#   mutate(CV = SE / Est)

# bootstrap posteriors
nSamps = trans_df %>%
  group_by(Origin, param) %>%
  summarise(nIters = n()) %>%
  ungroup() %>%
  select(nIters) %>%
  distinct() %>%
  as.matrix() %>%
  as.numeric()
  
# nSamps = 2400
set.seed(5)  
escape_summ = orgEscape %>%
  split(list(.$Origin)) %>%
  map_df(.id = 'Origin',
         .f = function(x) {
           tibble(totEsc = rnorm(nSamps, x$TotEscp, x$TotEscpSE)) %>%
             mutate(iter = 1:nSamps)
         }) %>%
  left_join(trans_df %>%
              group_by(Origin, param) %>%
              mutate(iter = 1:n()) %>%
              select(-chain) %>%
              rename(prob = value) %>%
              slice(sample.int(max(iter), nSamps)) %>%
              ungroup()) %>%
  mutate(value = totEsc * prob) %>%
  group_by(Origin, param) %>%
  summarise(mean = mean(value),
            median = median(value),
            mode = estMode(value),
            sd = sd(value),
            skew = moments::skewness(value),
            kurtosis = moments::kurtosis(value),
            lowerCI = coda::HPDinterval(coda::as.mcmc(value))[,1],
            upperCI = coda::HPDinterval(coda::as.mcmc(value))[,2]) %>%
  mutate_at(vars(mean, median, mode, sd, matches('CI$')),
            funs(ifelse(. < 0, 0, .))) %>%
  ungroup()

# generate population level estimates
pop_summ = escape_summ %>%
  filter(param %in% c("past_LWE", 'past_ENL', 'past_LMR', 'past_OKL', 'dwnStrm', 'WEA_bb')) %>%
  mutate(Population = recode(param,
                             'past_LWE' = 'Wenatchee',
                             'past_ENL' = 'Entiat',
                             'past_LMR' = 'Methow',
                             'past_OKL' = 'Okanogan',
                             'dwnStrm' = 'BelowPriest',
                             'WEA_bb' = 'WellsPool')) %>%
  select(Population, Origin, Escape = median, EscSE = sd) %>%
  arrange(Origin, Population)

  
#-----------------------------------------------------------------
# combine biological data with escapement estimates
#-----------------------------------------------------------------
bioSumm = tag_summ %>%
  group_by(Stream = Branch, 
           Origin,
           Sex, 
           Age) %>%
  summarise(nTags = n_distinct(TagID),
            meanFL = mean(ForkLength, na.rm = T)) %>%
  full_join(expand.grid(list(Stream = unique(tag_summ$Branch)[!is.na(unique(tag_summ$Branch))],
                             Origin = unique(tag_summ$Origin),
                             Sex = unique(tag_summ$Sex),
                             Age = unique(tag_summ$Age)))) %>%
  mutate_at(vars(nTags),
            funs(if_else(is.na(.),
                         as.integer(0), .))) %>%
  group_by(Stream, Origin) %>%
  mutate(totTags = sum(nTags)) %>%
  ungroup() %>%
  mutate(prop = nTags / totTags,
         propSE = sqrt((prop * (1 - prop)) / (totTags))) %>%
  mutate(Stream = as.character(Stream)) %>%
  select(Stream, Origin, totTags, Sex, Age, nTags, prop, propSE, meanFL) %>%
  arrange(Stream, Origin, Sex, Age)

fullSumm = pop_summ %>%
  mutate(Origin = recode(Origin,
                         'Natural' = 'W',
                         'Hatchery' = 'H')) %>%
  mutate(Species = spp,
         SpawnYear = yr) %>%
  select(Species, SpawnYear, Population, Origin, Escape, EscSE) %>%
  left_join(bioSumm) %>%
  rowwise() %>%
  mutate(Est = Escape * prop,
         EstSE = deltamethod(~ x1 * x2,
                             mean = c(Escape, prop),
                             cov = diag(c(EscSE, propSE)^2))) %>%
  ungroup() %>%
  mutate(Est = round(Est),
         Est = as.integer(Est)) %>%
  select(Species, SpawnYear, Population, Origin, Sex, Age, nTags, meanFL, prop, Est, EstSE)


#-----------------------------------------------------------------
# save results to Excel for export
#-----------------------------------------------------------------
save_list = c(list('Population Escapement' = pop_summ,
                   'All Escapement' = escape_summ,
                   'Detection' = detect_summ %>%
                     rename(Estimate = median,
                            SE = sd) %>%
                     mutate_at(vars(Estimate:upperCI),
                               funs(round),
                               digits = 3) %>%
                     select(-mean, -mode),
                   'Biological Summary' = fullSumm %>%
                     filter(nTags > 0)),
              bioList)


WriteXLS(save_list,
         paste0('outgoing/PRA_', spp, '_', yr, '_', format(Sys.Date(), '%Y%m%d'), '.xlsx'),
         AdjWidth = T,
         AutoFilter = T,
         BoldHeaderRow = T,
         FreezeRow = 1)
