# Author: Kevin See
# Purpose: Compare results of DABOM using upstream detections at some sites, or not
# Created: 4/25/2018
# Last Modified: 4/27/2018
# Notes: For steelhead starting at Priest Rapids dam, in spawn year 2017

#-----------------------------------------------------------------
# install correct version of DABOM to work with this script
devtools::install_github("KevinSee/DABOM@v0.1.0")
# load needed libraries
library(tidyverse)
library(ggrepel)
library(DABOM)
library(STADEM)
library(msm)
library(gridExtra)
library(WriteXLS)

theme_set(theme_bw())

#-----------------------------------------------------------------
# set species / year
spp = 'Steelhead'
yr = 2016

# define some other patches in the model to look at for escapement
otherBoxes = c('TUM_bb', 'past_TUM', 'LWE_bb', 'past_LWE', 'past_ENL', 'ENL_bb', 'MRC_bb', 'OKL_bb', 'past_OKL')

#-----------------------------------------------------------------
# load results, using missing upstream detections
load(paste0('modelFits/PRA_', spp, '_', yr, '_DABOM_NoUppSites.rda'))

# # detection probabilities
detect_miss = summariseDetectProbs(dabom_mod, proc_list$ProcCapHist)

# cummulative movement probabilities
trans_miss = compileTransProbs_PRA(dabom_mod)

transSumm_miss = trans_miss %>%
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

# using movement probabilities only from lower site
trans_mat = as.matrix(dabom_mod,
                      iters = T,
                      chains = T) %>%
  as.data.frame() %>%
  tbl_df() %>%
  # remove detection parameters
  select(-matches('_p$'))

trans_w = trans_mat %>%
  select(CHAIN, ITER, matches('\\[1'))
trans_h = trans_mat %>%
  select(CHAIN, ITER, matches('\\[2'))
# change names of paramters
names(trans_w) = renameTransParams_PRA(names(trans_w))
names(trans_h) = renameTransParams_PRA(names(trans_h))

move_miss = list('Natural' = trans_w,
                  'Hatchery' = trans_h) %>%
  purrr::map_df(.id = 'Origin',
                .f = function(x) {
                  x %>%
                    mutate(iter = 1:n()) %>%
                    tidyr::gather(param, value, -CHAIN, -ITER, -iter) %>%
                    select(chain = CHAIN,
                           iter,
                           param,
                           value)
                })


moveSumm_miss = move_miss %>%
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

rm(trans_mat, trans_h, trans_w)

# load results, using all detections
load(paste0('modelFits/PRA_', spp, '_', yr, '_DABOM.rda'))

# # detection probabilities
detect_all = summariseDetectProbs(dabom_mod, proc_list$ProcCapHist)

# cummulative movement probabilities
trans_all = compileTransProbs_PRA(dabom_mod)

transSumm_all = trans_all %>%
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

# using movement probabilities only from lower site
trans_mat = as.matrix(dabom_mod,
                      iters = T,
                      chains = T) %>%
  as.data.frame() %>%
  tbl_df() %>%
  # remove detection parameters
  select(-matches('_p$'))

trans_w = trans_mat %>%
  select(CHAIN, ITER, matches('\\[1'))
trans_h = trans_mat %>%
  select(CHAIN, ITER, matches('\\[2'))
# change names of paramters
names(trans_w) = renameTransParams_PRA(names(trans_w))
names(trans_h) = renameTransParams_PRA(names(trans_h))

move_all = list('Natural' = trans_w,
                 'Hatchery' = trans_h) %>%
  purrr::map_df(.id = 'Origin',
                .f = function(x) {
                  x %>%
                    mutate(iter = 1:n()) %>%
                    tidyr::gather(param, value, -CHAIN, -ITER, -iter) %>%
                    select(chain = CHAIN,
                           iter,
                           param,
                           value)
                })


moveSumm_all = move_all %>%
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

rm(trans_mat, trans_h, trans_w)

#-----------------------------------------------------------------
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

#-----------------------------------------------------------------
# compare detection probability estimates and precision
#-----------------------------------------------------------------
lowNodeComp = pairedSites %>%
  select(Lower) %>%
  distinct() %>%
  split(list(.$Lower)) %>%
  map_df(.id = 'LowerSite',
         .f = function(x) {
           detect_all %>%
             filter(grepl(x$Lower[1], Node)) %>%
             mutate(source = 'All') %>%
             bind_rows(detect_miss %>%
                         filter(grepl(x$Lower[1], Node)) %>%
                         mutate(source = 'Miss'))
         }) %>%
  arrange(LowerSite, Node, source)

uppStrmDetects = pairedSites %>%
  split(list(.$Lower)) %>%
  map_df(.id = 'LowerSite',
         .f = function(x) {
           proc_list$ProcCapHist %>%
             left_join(proc_list$NodeOrder %>%
                         select(Node, NodeSite)) %>%
             filter(NodeSite %in% x$Upper) %>%
             summarise(upTags = n_distinct(TagID))
         })

detEst_p = lowNodeComp %>%
  left_join(uppStrmDetects) %>%
  select(LowerSite, Node, upTags, source, median) %>%
  spread(source, median) %>%
  mutate(pos = ifelse(grepl('A0$', Node),
                            'Up',
                            'Down')) %>%
  ggplot(aes(x = All,
             y = Miss)) +
  geom_abline(linetype = 2,
              color = 'gray') +
  geom_point(aes(color = pos,
                 size = upTags)) +
  geom_text_repel(aes(label = LowerSite),
                  size = 3) +
  labs(x = 'All Detections',
       y = 'Missing Upstream Sites',
       color = 'Array Position',
       size = 'Upstream Tags',
       title = 'Detection Probability Estimate')
  

detSE_p = lowNodeComp %>%
  left_join(uppStrmDetects) %>%
  select(LowerSite, Node, upTags, source, sd) %>%
  spread(source, sd) %>%
  mutate(pos = ifelse(grepl('A0$', Node),
                      'Up',
                      'Down')) %>%
  ggplot(aes(x = All,
             y = Miss)) +
  geom_abline(linetype = 2,
              color = 'gray') +
  geom_point(aes(color = pos,
                 size = upTags)) +
  geom_text_repel(aes(label = LowerSite),
                  size = 3) +
  labs(x = 'All Detections',
       y = 'Missing Upstream Sites',
       color = 'Array Position',
       size = 'Upstream Tags',
       title = 'Detection Probability Std. Error')
  
#-----------------------------------------------------------------
# compare movement probability estimates and precision
#-----------------------------------------------------------------
lowNodeMove = pairedSites %>%
  select(Lower) %>%
  distinct() %>%
  split(list(.$Lower)) %>%
  map_df(.id = 'LowerSite',
         .f = function(x) {
           moveSumm_all %>%
             filter(grepl(x$Lower[1], param),
                    grepl('^past', param)) %>%
             mutate(source = 'All') %>%
             bind_rows(moveSumm_miss %>%
                         filter(grepl(x$Lower[1], param),
                                grepl('^past', param)) %>%
                         mutate(source = 'Miss'))
         }) %>%
  bind_rows(moveSumm_all %>%
              filter(param %in% otherBoxes) %>%
              mutate(LowerSite = param,
                     source = 'All') %>%
              bind_rows(moveSumm_miss %>%
                          filter(param %in% otherBoxes) %>%
                          mutate(LowerSite = param,
                                 source = 'Miss'))) %>%
  arrange(LowerSite, param, Origin, source)


moveEst_p = lowNodeMove %>%
  left_join(uppStrmDetects) %>%
  select(LowerSite, Origin, param, upTags, source, median) %>%
  filter(!is.na(upTags)) %>%
  spread(source, median) %>%
  ggplot(aes(x = All,
             y = Miss)) +
  geom_abline(linetype = 2,
              color = 'gray') +
  geom_point(aes(color = Origin,
                 size = upTags)) +
  geom_text_repel(aes(label = LowerSite),
                  size = 3) +
  labs(x = 'All Detections',
       y = 'Missing Upstream Sites',
       color = 'Origin',
       size = 'Upstream Tags',
       title = 'Movement Probability Estimate')


moveSE_p = lowNodeMove %>%
  left_join(uppStrmDetects) %>%
  select(LowerSite, Origin, param, upTags, source, sd) %>%
  filter(!is.na(upTags)) %>%
  spread(source, sd) %>%
  ggplot(aes(x = All,
             y = Miss)) +
  geom_abline(linetype = 2,
              color = 'gray') +
  geom_point(aes(color = Origin,
                 size = upTags)) +
  geom_text_repel(aes(label = LowerSite),
                  size = 3) +
  labs(x = 'All Detections',
       y = 'Missing Upstream Sites',
       color = 'Origin',
       size = 'Upstream Tags',
       title = 'Movement Probability Std. Error')

#-----------------------------------------------------------------
# compare transition probability estimates and precision
#-----------------------------------------------------------------
lowNodeTrans = pairedSites %>%
  select(Lower) %>%
  distinct() %>%
  split(list(.$Lower)) %>%
  map_df(.id = 'LowerSite',
         .f = function(x) {
           transSumm_all %>%
             filter(grepl(x$Lower[1], param),
                    grepl('^past', param)) %>%
             mutate(source = 'All') %>%
             bind_rows(transSumm_miss %>%
                         filter(grepl(x$Lower[1], param),
                                grepl('^past', param)) %>%
                         mutate(source = 'Miss'))
         }) %>%
  bind_rows(transSumm_all %>%
              filter(param %in% otherBoxes) %>%
              mutate(LowerSite = param,
                     source = 'All') %>%
              bind_rows(transSumm_miss %>%
                          filter(param %in% otherBoxes) %>%
                          mutate(LowerSite = param,
                                 source = 'Miss'))) %>%
  arrange(LowerSite, param, Origin, source)


transEst_p = lowNodeTrans %>%
  left_join(uppStrmDetects) %>%
  select(LowerSite, Origin, param, upTags, source, median) %>%
  filter(!is.na(upTags)) %>%
  spread(source, median) %>%
  ggplot(aes(x = All,
             y = Miss)) +
  geom_abline(linetype = 2,
              color = 'gray') +
  geom_point(aes(color = Origin,
                 size = upTags)) +
  geom_text_repel(aes(label = LowerSite),
                  size = 3) +
  labs(x = 'All Detections',
       y = 'Missing Upstream Sites',
       color = 'Origin',
       size = 'Upstream Tags',
       title = 'Transition Probability Estimate')


transSE_p = lowNodeTrans %>%
  left_join(uppStrmDetects) %>%
  select(LowerSite, Origin, param, upTags, source, sd) %>%
  filter(!is.na(upTags)) %>%
  spread(source, sd) %>%
  ggplot(aes(x = All,
             y = Miss)) +
  geom_abline(linetype = 2,
              color = 'gray') +
  geom_point(aes(color = Origin,
                 size = upTags)) +
  geom_text_repel(aes(label = LowerSite),
                  size = 3) +
  labs(x = 'All Detections',
       y = 'Missing Upstream Sites',
       color = 'Origin',
       size = 'Upstream Tags',
       title = 'Transition Probability Std. Error')


#-----------------------------------------------------------------
# Estimate fish and compared differences
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


lowNodeFish = lowNodeTrans %>%
  select(LowerSite, source, Origin, param, mean, sd) %>%
  left_join(orgEscape) %>%
  rowwise() %>%
  mutate(Est = mean * TotEscp,
         SE = deltamethod(~ x1 * x2,
                          mean = c(mean, TotEscp),
                          cov = diag(c(sd, TotEscpSE)^2))) %>%
  ungroup() %>%
  select(Species, SpawnYear, Origin, source, LowerSite, Est, SE) %>%
  mutate(CV = SE / Est) %>%
  distinct()


fishEst_p = lowNodeFish %>%
  select(-SE, -CV) %>%
  spread(source, Est) %>%
  ggplot(aes(x = All,
             y = Miss)) +
  geom_abline(linetype = 2,
              color = 'gray') +
  geom_point(aes(color = Origin)) +
  geom_text_repel(aes(label = LowerSite),
                  size = 3) +
  labs(x = 'All Detections',
       y = 'Missing Upstream Sites',
       color = 'Origin',
       size = 'Upstream Tags',
       title = 'Escapement Estimate')

fishEst_p2 = lowNodeFish %>%
  filter(!LowerSite %in% c('past_LWE', 'past_OKL', 'past_TUM', 'past_ENL')) %>%
  select(-SE, -CV) %>%
  spread(source, Est) %>%
  ggplot(aes(x = All,
             y = Miss)) +
  geom_abline(linetype = 2,
              color = 'gray') +
  geom_point(aes(color = Origin)) +
  geom_text_repel(aes(label = LowerSite),
                  size = 3) +
  labs(x = 'All Detections',
       y = 'Missing Upstream Sites',
       color = 'Origin',
       size = 'Upstream Tags',
       title = 'Escapement Estimate (no LWE)')



fishSE_p = lowNodeFish %>%
  select(-Est, -CV) %>%
  spread(source, SE) %>%
  ggplot(aes(x = All,
             y = Miss)) +
  geom_abline(linetype = 2,
              color = 'gray') +
  geom_point(aes(color = Origin)) +
  geom_text_repel(aes(label = LowerSite),
                  size = 3) +
  labs(x = 'All Detections',
       y = 'Missing Upstream Sites',
       color = 'Origin',
       size = 'Upstream Tags',
       title = 'Escapement Std. Error')


fishBias_p = lowNodeFish %>%
  select(-SE, -CV) %>%
  spread(source, Est) %>%
  mutate(bias = Miss - All,
         relBias = bias / All) %>%
  ggplot(aes(x = relBias)) +
  geom_histogram(fill = 'darkblue',
                 bins = 15) +
  geom_vline(xintercept = 0,
             linetype = 2,
             color = 'red') +
  facet_wrap(~ Origin) +
  labs(x = '(Missing Sites - All Sites) / All Sites',
       y = 'Frequency',
       title = 'Relative Bias')


#-----------------------------------------------------------------
# save plots
#-----------------------------------------------------------------
allPlots = marrangeGrob(grobs = list(detEst_p, detSE_p, 
                                     moveEst_p, moveSE_p,
                                     transEst_p, transSE_p,
                                     fishEst_p, fishEst_p2, fishSE_p,
                                     fishBias_p),
                        ncol = 1,
                        nrow = 1,
                        top = NULL)

ggsave(paste0('outgoing/UpstrmSiteComp_', yr, '.pdf'),
       allPlots,
       height = 7,
       width = 7)

# save some tables
saveList = list('DetectionProb' = lowNodeComp %>%
                  select(LowerSite, Node, source,
                         everything()),
                'EscapementEst' = lowNodeFish %>%
                  mutate_at(vars(Est),
                            funs(round),
                            digits = 0) %>%
                  mutate_at(vars(SE),
                            funs(round),
                            digits = 1) %>%
                  mutate_at(vars(CV),
                            funs(round),
                            digits = 3))
  
WriteXLS(x = saveList,
         ExcelFileName = paste0('outgoing/UpstrmSiteComp_', yr, '.xlsx'),
         BoldHeaderRow = T,
         AdjWidth = T,
         AutoFilter = T,
         FreezeRow = 1)

#-----------------------------------------------------------------
# Look at capture histories
#-----------------------------------------------------------------

allDetects = pairedSites %>%
  split(list(.$Lower)) %>%
  map_df(.id = 'LowerSite',
         .f = function(x) {
           y = proc_list$ProcCapHist %>%
             left_join(proc_list$NodeOrder %>%
                         select(Node, NodeSite)) %>%
             filter(NodeSite %in% x$Lower) %>%
             select(TagID, Node) %>%
             mutate(seen = 1) %>%
             spread(Node, seen,
                    fill = 0) %>%
             select(TagID, matches('B0$'), matches('A0$'))
           names(y)[2:3] = c('LowArray', 'UppArray')
           
           z = y %>%
             full_join(proc_list$ProcCapHist %>%
                         left_join(proc_list$NodeOrder %>%
                                     select(Node, NodeSite)) %>%
                         filter(NodeSite %in% x$Upper) %>%
                         select(TagID) %>%
                         mutate(UpStrm = 1) %>%
                         distinct()) %>%
             mutate_at(vars(LowArray:UpStrm),
                       funs(ifelse(is.na(.), 0, .)))
           z %>%
             group_by(LowArray, UppArray, UpStrm) %>%
             summarise(freq = n_distinct(TagID)) %>%
             ungroup()
         }) %>%
  mutate(CapHist = paste0(LowArray, UppArray, UpStrm))

allDetects %>%
  filter(LowArray == 1 |
           UppArray == 1) %>%
  group_by(LowerSite) %>%
  summarise(nLowTags = sum(freq)) %>%
  full_join(allDetects %>%
              group_by(LowerSite) %>%
              summarise(nTotTags = sum(freq))) %>%
  mutate(diff = nTotTags - nLowTags,
         relDiff = diff / nLowTags) %>%
  arrange(desc(relDiff))

allDetects %>%
  group_by(LowerSite) %>%
  summarise(nTags = sum(freq))

allDetects %>%
  filter(LowerSite == 'CHL')

allDetects %>%
  select(LowerSite, CapHist, freq)


proc_list$ProcCapHist %>%
  filter(grepl('CHL', Node))
