# Author: Kevin See
# Purpose: Summarise results of DABOM
# Created: 1/23/2019
# Last Modified: 2/15/2019
# Notes: For steelhead starting at Priest Rapids dam, in spawn year 2016

#-----------------------------------------------------------------
# install the correct version of DABOM for this model
devtools::install_github("KevinSee/DABOM@v0.1.0")

# load needed libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(magrittr)
library(ggpubr)
library(msm)
library(PITcleanr)
library(DABOM)
library(STADEM)
library(jagsUI)
library(WriteXLS)
library(coda)


#-----------------------------------------------------------------
# set species / year
spp = 'Steelhead'
yr = 2016

#-----------------------------------------------------------------
# load results
load(paste0('analysis/modelFits/PRA_', spp, '_', yr, '_DABOM.rda'))

# bio_df = read_excel('/Users/kevin/Dropbox/ISEMP/Analysis_Projects/UpperColumbia_AdultEscapement/UC_Steelhead/2011_2018_DABOM/data/WDFW/AllBioData.xlsx',
#                     as.character(yr))

bio_df = read_excel('analysis/data/raw_data/WDFW/AllBioData.xlsx',
                    as.character(yr))


# fix a few prefixes for tags with known issues (Ben Truscott had to enter them incorrectly in his database)
bio_df %<>%
  mutate(TagID = ifelse(!TagID %in% dabom_df$TagID,
                        str_replace(TagID,
                                    '^3DD',
                                    '3DA'),
                        TagID))


summary(dabom_mod)$quantiles %>%
  as.data.frame() %>%
  mutate(param = row.names(.)) %>%
  select(param, everything()) %>%
  filter(grepl('p_pop_WEA', param))

#-----------------------------------------------------------------
# model fitting diagnositcs
gr = gelman.diag(dabom_mod,
                 multivariate = FALSE)
gr$psrf %>%
  as_tibble(rownames = 'param') %>%
  summarise(nParam = n_distinct(param),
            nNA = sum(is.na(`Point est.`)),
            nonNA = nParam - nNA)

length(which(colMeans(dabom_mod[[1]]) %in% c(0, 1)))
length(which(colMeans(dabom_mod[[1]]) %in% c(0)))
length(which(colMeans(dabom_mod[[1]]) %in% c(1)))


gr$psrf %>%
  as_tibble(rownames = 'param') %>%
  summarise_at(vars(`Point est.`, `Upper C.I.`),
               list(max),
               na.rm = T)

gr$psrf %>%
  as_tibble(rownames = 'param') %>%
  filter(`Point est.` >= 1.01)

#------------------------------------------------------
# estimate final spawning location
tag_summ = summariseTagData(proc_list$ProcCapHist %>%
                              mutate(lastObsDate = ObsDate) %>%
                              select(-BranchNum, -Group, -NodeOrder) %>%
                              left_join(proc_list$NodeOrder %>%
                                          mutate(Group = fct_expand(Group, 'WellsPool'),
                                                 Group = if_else(Node == 'WEA',
                                                                 'WellsPool',
                                                                 as.character(Group)),
                                                 # Group = ifelse(NodeSite %in% c('FST', 'RRF', 'WVT', 'RIA', 'CLK'),
                                                 #                NA,
                                                 #                Group),
                                                 Group = ifelse(NodeSite %in% c('RRF', 'WVT', 'RIA', 'CLK'),
                                                                NA,
                                                                Group),
                                                 Group = fct_relevel(Group,
                                                                     c('Wenatchee', 'Entiat', 'Methow', 'Okanogan', 'WellsPool', 'BelowPriest')))) %>%
                              mutate(SiteID = NodeSite),
                            trap_data = bio_df %>%
                              mutate(Age = str_replace(Age, 'r', 'R'))) %>%
  rename(Branch = Group) %>%
  mutate(Branch = fct_explicit_na(Branch)) %>%
  select(-(ForkLength:Age), -TrapDate) %>%
  distinct() %>%
  filter(TagID %in% dabom_df$TagID)


xtabs(~ Branch, tag_summ) %>%
  addmargins()
  prop.table()

tag_summ %>%
  filter(!TagID %in% dabom_df$TagID)

tag_summ %>%
  xtabs(~ (TagPath == 'PRA'), .) %>%
  # xtabs(~ Origin + (TagPath == 'PRA'), .) %>%
  prop.table() %>%
  addmargins()



proc_list$NodeOrder %>%
  mutate(Group = fct_expand(Group, 'WellsPool'),
         Group = if_else(Node == 'WEA',
                         'WellsPool',
                         as.character(Group)),
         # Group = ifelse(NodeSite %in% c('FST', 'RRF', 'WVT', 'RIA', 'CLK'),
         #                NA,
         #                Group),
         Group = ifelse(NodeSite %in% c('RRF', 'WVT', 'RIA', 'CLK'),
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
  filter(Branch == '(Missing)') %>%
  xtabs(~ AssignSpawnNode, .)


# bioList = list('Origin' = tag_summ %>%
#                  filter(!is.na(Branch)) %>%
#                  group_by(Branch, Origin) %>%
#                  summarise(nTags = n_distinct(TagID)) %>%
#                  spread(Origin, nTags,
#                         fill = as.integer(0)) %>%
#                  ungroup() %>%
#                  mutate(propW = W / (W + H),
#                         propH = 1 - propW,
#                         propOrgSE = sqrt((propW * (1 - propW)) / (W + H))),
#                'AllSex' = tag_summ %>%
#                  filter(!is.na(Branch)) %>%
#                  group_by(Branch, Origin, Sex) %>%
#                  summarise(nTags = n_distinct(TagID[!is.na(Sex)])) %>%
#                  filter(!is.na(Sex)) %>%
#                  ungroup() %>%
#                  spread(Sex, nTags,
#                         fill = as.integer(0)) %>%
#                  mutate(total_sexed = F + M,
#                         propF = F / (F + M),
#                         propM = 1 - propF,
#                         propSexSE = sqrt((propF * (1 - propF)) / (M + F))) %>%
#                  select(Branch, Origin, total_sexed:propSexSE) %>%
#                  arrange(Origin, Branch),
#                'AllAge' = tag_summ %>%
#                  filter(!is.na(Branch)) %>%
#                  group_by(Branch, Origin, Age) %>%
#                  summarise(nTags = n_distinct(TagID)) %>%
#                  ungroup() %>%
#                  group_by(Branch, Origin) %>%
#                  mutate(total_aged = sum(nTags)) %>%
#                  spread(Age, nTags,
#                         fill = 0) %>%
#                  select(Branch, Origin, total_aged,
#                         not_aged = `<NA>`,
#                         everything()) %>%
#                  mutate_at(vars(-c(1:2)),
#                            list(as.integer)) %>%
#                  arrange(Origin, Branch))

#-----------------------------------------------------------------
# detection probabilities
detect_summ = summariseDetectProbs(dabom_mod,
                                   proc_list$ProcCapHist %>%
                                     filter(UserProcStatus))

detect_summ %>%
  filter(sd > 0) %>%
  arrange(desc(sd))

detect_summ %>%
  filter(mean < 0.5,
         mean != 0)


detect_summ %>%
  filter(mean != 0,
         mean != 1) %>%
  xtabs(~ (mean >= 0.7), .) %>%
  prop.table()

detect_summ %>%
  filter(mean != 0,
         mean != 1) %>%
  qplot(mean, data = .)

detect_summ %>%
  filter(mean != 0,
         mean != 1) %>%
  arrange(mean)

# re-create Table 1 of manuscript
tab1 = proc_list$NodeOrder %>%
  group_by(NodeSite) %>%
  summarise(n_arrays = n_distinct(Node)) %>%
  full_join(proc_list$ProcCapHist %>%
              left_join(proc_list$NodeOrder %>%
                          select(Node, NodeSite) %>%
                          distinct()) %>%
              group_by(NodeSite) %>%
              summarise(n_tags = n_distinct(TagID))) %>%
  full_join(detect_summ %>%
              left_join(proc_list$NodeOrder %>%
                          select(Node, NodeSite) %>%
                          distinct()) %>%
              mutate_at(vars(median, sd),
                        list(round),
                        digits = 3) %>%
              select(NodeSite, Node, median) %>%
              filter(!is.na(NodeSite)) %>%
              group_by(NodeSite) %>%
              mutate(nodeNum = 1:n()) %>%
              select(-Node) %>%
              spread(nodeNum, median) %>%
              mutate(joinDet = if_else(!is.na(`2`),
                                       1 - ((1 - `1`) * (1 - `2`)),
                                       `1`)) %>%
              mutate(detection_median = paste(`1`, `2`, sep = ', ')) %>%
              select(-(`1`:`2`)) %>%
              left_join(detect_summ %>%
                          left_join(proc_list$NodeOrder %>%
                                      select(Node, NodeSite) %>%
                                      distinct()) %>%
                          mutate_at(vars(median, sd),
                                    list(round),
                                    digits = 3) %>%
                          select(NodeSite, Node, sd) %>%
                          filter(!is.na(NodeSite)) %>%
                          group_by(NodeSite) %>%
                          mutate(nodeNum = 1:n()) %>%
                          select(-Node) %>%
                          spread(nodeNum, sd) %>%
                          mutate(detection_sd = paste(`1`, `2`, sep = ', ')) %>%
                          select(-(`1`:`2`))) %>%
              mutate_at(vars(matches('^detection')),
                        list(str_replace),
                        pattern = ', NA$',
                        replacement = '')) %>%
  filter(!is.na(n_tags))

tab1 %>%
  filter(!is.na(joinDet),
         joinDet != 1) %>%
  xtabs(~ (joinDet > 0.8), .)

tab1 %>%
  filter(joinDet < 0.6)

qplot(x = joinDet, data = tab1)
tab1 %>%
  arrange(joinDet)

tab1 %>%
  # filter(n_arrays > 1) %>%
  # filter(n_arrays < 2) %>%
  xtabs(~ (joinDet > 0.8), .) %>%
  addmargins()
  # prop.table()

# save table
config = buildConfig()
tab1 %>%
  mutate_at(vars(joinDet),
            list(round),
            digits = 3) %>%
  rename(SiteID = NodeSite) %>%
  left_join(config %>%
              select(SiteID, SiteName) %>%
              distinct() %>%
              filter(!(SiteName %in% c('Prosser Diversion Dam, Yakima River',
                                       'Three Mile Falls Dam (Umatilla River)',
                                       'Wanapum Dam')))) %>%
  mutate(SiteName = str_remove(SiteName, 'Instream Array'),
         SiteName = str_remove(SiteName, 'Temporary Array'),
         SiteName = str_remove(SiteName, '\\(Combined\\)'),
         SiteName = str_remove(SiteName, 'Combined'),
         SiteName = str_trim(SiteName)) %>%
  mutate(SiteName = if_else(SiteID == 'BelowJD1',
                            'Below site JD1',
                            SiteName)) %>%
  select(Abbreviation = SiteID,
         `Full Name` = SiteName,
         `Num. Arrays` = n_arrays,
         `Num. Tags Detected` = n_tags,
         `Joint Detection` = joinDet,
         `Median Detection` = detection_median,
         `Std. Error Detection` = detection_sd) %>%
  write_csv('DetectionProb_Table.csv')


#-----------------------------------------------------------------
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
            list(~ifelse(. < 0, 0, .))) %>%
  ungroup()

#-----------------------------------------------------------------
# total escapement past Priest
# window count
totWinCnt = getWindowCounts(dam = 'PRD',
                            spp = 'Steelhead',
                            start_date = paste0(yr-1, '0601'),
                            end_date = paste0(yr, '0531')) %>%
  summarise_at(vars(win_cnt),
               list(sum)) %>%
  pull(win_cnt)

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
               list(sum),
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

# mean(rbeta(1000, 634, 1565))
# mean(rbeta(1000, -0.963, -25.9))
#
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
# # using delta method
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
escape_post = orgEscape %>%
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
  mutate(value = totEsc * prob)

escape_summ = escape_post %>%
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
            list(~ ifelse(. < 0, 0, .))) %>%
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
  select(Population, Origin, Estimate = median, SE = sd, matches('CI$')) %>%
  arrange(Origin, Population)

escape_summ %>%
  filter(param %in% c('PRA_bb', 'RRF_bb', 'RIA_bb')) %>%
  group_by(Origin) %>%
  summarise_at(vars(mean:mode, matches('CI$')),
               list(sum))

escape_summ %>%
  filter(param == 'past_WEA') %>%
  group_by(param) %>%
  summarise_at(vars(mean:mode, matches('CI$')),
               list(sum)) %>%
  summarise_at(vars(mean:mode, matches('CI$')),
               list(round))

escape_summ %>%
  filter(param == 'past_TUM') %>%
  group_by(param) %>%
  summarise_at(vars(mean:mode, matches('CI$')),
               list(sum)) %>%
  mutate_at(vars(mean:mode, matches('CI$')),
               list(round))

pop_summ %>%
  filter(Population %in% c('Entiat', 'Wenatchee')) %>%
  group_by(Population) %>%
  summarise_at(vars(Estimate, matches('CI$')),
               list(sum))

pop_summ %>%
  filter(Population %in% c('WellsPool', 'Methow', 'Okanogan')) %>%
  group_by(Population) %>%
  summarise_at(vars(Estimate, matches('CI$')),
               list(sum)) %>%
  pull(Estimate) / 8079

escape_summ %>%
  filter(param %in% c('ENL_bb', 'past_ENA', 'past_MAD', 'past_EHL', 'past_RCT')) %>%
  select(Origin, param, mean, median) %>%
  left_join(pop_summ %>%
              filter(Population == 'Entiat')) %>%
  # mutate(perc = median / Estimate) %>%
  mutate(perc = mean / Estimate) %>%
  group_by(Origin) %>%
  summarise_at(vars(perc),
               list(sum))

escape_summ %>%
  filter(param %in% c('TUM_bb', 'past_CHW', 'past_CHL', 'past_UWE')) %>%
  # group_by(Origin) %>%
  summarise_at(vars(mean, median, mode, lowerCI, upperCI),
               list(sum))


# recreate Table 2
tab2 = escape_summ %>%
  filter(param %in% c("past_LWE", 'past_ENL', 'past_WEA', 'past_TUM', 'past_CHL', 'past_PES')) %>%
  mutate(Branch = recode(param,
                         'past_LWE' = 'Wenatchee',
                         'past_ENL' = 'Entiat',
                         'past_WEA' = 'Wells',
                         'past_TUM' = 'Tumwater',
                         'past_CHL' = 'Chiwawa',
                         'past_PES' = 'Peshastin'),
         Branch = factor(Branch,
                         levels = c('Entiat', 'Wells', 'Wenatchee', 'Tumwater', 'Chiwawa', 'Peshastin'))) %>%
  mutate_at(vars(mean, median, lowerCI, upperCI),
            list(as.integer)) %>%
  mutate(CredInt_95 = paste0('(', lowerCI, ', ', upperCI, ')')) %>%
  select(Origin, Branch, Mean = mean, Median = median, CredInt_95) %>%
  arrange(Origin, Branch)
tab2 %>%
  write_csv('EscapeEst_Table2.csv')

plotDf = escape_post %>%
  filter(param %in% c("past_LWE", 'past_ENL', 'past_WEA', 'past_TUM', 'past_CHL', 'past_PES')) %>%
  mutate(Branch = recode(param,
                         'past_LWE' = 'Wenatchee',
                         'past_ENL' = 'Entiat',
                         'past_WEA' = 'Wells',
                         'past_TUM' = 'Tumwater',
                         'past_CHL' = 'Chiwawa',
                         'past_PES' = 'Peshastin'),
         Branch = factor(Branch,
                         levels = c('Entiat', 'Wells', 'Wenatchee', 'Tumwater', 'Chiwawa', 'Peshastin')))

# p1 = plotDf %>%
#   ggplot(aes(x = Branch,
#              y = value,
#              fill = Origin)) +
#   geom_boxplot() +
#   theme_bw() +
#   scale_fill_grey() +
#   theme(legend.position = c(0.8, 0.8)) +
#   theme(panel.grid = element_blank()) +
#   # theme(legend.position = 'bottom') +
#   labs(y = 'Estimated # Fish')
#
# p2 = plotDf %>%
#   ggplot(aes(x = Branch,
#              y = value)) +
#   geom_violin(fill = 'gray') +
#   geom_boxplot(width = 0.5) +
#   theme_bw() +
#   facet_wrap(~ Origin + Branch,
#              scales = 'free',
#              nrow = 2) +
#   labs(y = 'Estimated # Fish')

p1 = plotDf %>%
  ggplot(aes(x = Branch,
             y = value,
             fill = Origin)) +
  geom_boxplot() +
  theme_bw() +
  scale_fill_brewer(palette = 'Set1') +
  theme(legend.position = c(0.8, 0.8)) +
  theme(panel.grid = element_blank()) +
  labs(y = 'Estimated # Fish')

p2 = plotDf %>%
  ggplot(aes(x = Branch,
             y = value)) +
  geom_violin(aes(fill = Origin)) +
  scale_fill_brewer(palette = 'Set1') +
  geom_boxplot(width = 0.5) +
  theme_bw() +
  theme(legend.position = 'none',
        strip.text = element_text(size = 7)) +
  facet_wrap(~ Origin + Branch,
             scales = 'free',
             nrow = 2) +
  labs(y = 'Estimated # Fish')

ggarrange(plotlist = list(p1, p2),
          ncol = 1,
          nrow = 2,
          labels = 'AUTO') %>%
  ggsave('analysis/figures/Figure5.pdf',
         plot = .,
         width = 6,
         height = 8,
         dpi = 600)

# another version
dodge_pos = 0.9
p3 = plotDf %>%
  ggplot(aes(x = Branch,
             y = value)) +
  geom_violin(aes(fill = Origin),
              scale = 'width',
              draw_quantiles = 0.5,
              position = position_dodge(width = dodge_pos)) +
  # geom_boxplot(position = position_dodge(width = dodge_pos)) +
  theme_bw() +
  scale_fill_brewer(palette = 'Set1') +
  theme(legend.position = c(0.75, 0.75),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  # scale_y_continuous(trans = 'log') +
  labs(y = 'Estimated # Fish',
       x = "Location")

ggsave('analysis/figures/Figure5_v2.pdf',
       p3,
       width = 3,
       height = 4,
       dpi = 600)


dodge_pos = 0.9
plotDf %>%
  ggplot(aes(x = Branch,
             y = value)) +
  geom_violin(aes(fill = Origin),
              scale = 'width',
              position = position_dodge(width = dodge_pos)) +
  scale_fill_brewer(palette = 'Set1') +
  geom_boxplot(aes(group = Origin),
               position = position_dodge(width = dodge_pos)) +
  theme_bw() +
  theme(legend.position = 'none',
        strip.text = element_text(size = 7)) +
  facet_wrap(~ Branch,
             scales = 'free',
             # scales = 'free_x',
             nrow = 2) +
  labs(y = 'Estimated # Fish')


plotDf %>%
  ggplot(aes(x = value,
             fill = Origin,
             color = Origin)) +
  # geom_density(alpha = 0.3) +
  geom_histogram(alpha = 0.3) +
  theme_bw() +
  scale_fill_brewer(palette = 'Set1') +
  scale_color_brewer(palette = 'Set1') +
  facet_wrap(~ Branch,
             scales = 'free')

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
            list(if_else(is.na(.),
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
  select(Species, SpawnYear, Population, Origin, Estimate, SE) %>%
  left_join(bioSumm) %>%
  rowwise() %>%
  mutate(Est = Estimate * prop,
         SE = deltamethod(~ x1 * x2,
                          mean = c(Estimate, prop),
                          cov = diag(c(SE, propSE)^2))) %>%
  ungroup() %>%
  mutate(Est = round(Est),
         Est = as.integer(Est)) %>%
  select(Species, SpawnYear, Population, Origin, Sex, Age, nTags, meanFL, prop, Est, SE)


#-----------------------------------------------------------------
# save results to Excel for export
#-----------------------------------------------------------------
save_list = c(list('Population Escapement' = pop_summ %>%
                     mutate_at(vars(Estimate, matches('CI$')),
                               list(round)) %>%
                     mutate_at(vars(Estimate, matches('CI$')),
                               list(as.integer)) %>%
                     mutate_at(vars(SE),
                               list(round),
                               digits = 1),
                   'All Escapement' = escape_summ %>%
                     rename(Estimate = mean,
                            SE = sd) %>%
                     mutate_at(vars(Estimate, matches('CI$')),
                               list(round)) %>%
                     mutate_at(vars(Estimate, matches('CI$')),
                               list(as.integer)) %>%
                     mutate_at(vars(SE),
                               list(round),
                               digits = 1) %>%
                     select(-median, -mode, -skew, -kurtosis),
                   'Detection' = detect_summ %>%
                     rename(Estimate = mean,
                            SE = sd) %>%
                     mutate_at(vars(Estimate:upperCI),
                               list(round),
                               digits = 3) %>%
                     select(-median, -mode),
                   'Biological Summary' = fullSumm %>%
                     filter(nTags > 0)),
              bioList)


WriteXLS(save_list,
         paste0('PRA_', spp, '_', yr, '_', format(Sys.Date(), '%Y%m%d'), '.xlsx'),
         AdjWidth = T,
         AutoFilter = T,
         BoldHeaderRow = T,
         FreezeRow = 1)

#-----------------------------------------------------------------
# Tumwater estimates
#-----------------------------------------------------------------
WDFWcnt = read_excel('../Data/STHD_TMW Run Escapement_by Run YearUpdate .xlsx',
                     sheet = '2016 Brood',
                     range = 'A11:T391',
                     col_names = F) %>%
  select(Date = '..1',
         Count = '..20') %>%
  mutate_at(vars(Date),
            list(as.Date))

STADEMcnt = getWindowCounts(dam = 'TUM',
                         spp = 'Steelhead',
                         start_date = paste0(yr-1, '0612'),
                         end_date = paste0(yr, '0626'))

STADEMcnt %>%
  full_join(WDFWcnt) %>%
  mutate(diff = Count - win_cnt) %>%
  summarise_at(vars(win_cnt:diff),
               list(sum))
  filter(win_cnt != Count) %>%
  arrange(desc(abs(diff)))


# # %>%
#   summarise_at(vars(win_cnt),
#                list(sum)) %>%
#   pull(win_cnt)


tumCnt = read_excel('../Data/STHD_TMW Run Escapement_by Run YearUpdate .xlsx',
                    sheet = '2016 Brood',
                    range = 'M5',
                    col_names = F) %>%
  as.numeric

tumReasc = read_excel('../Data/Tumwater fallback 2016.xlsx') %>%
  rename(fallback = '..19') %>%
  filter(!is.na(`Tag ID`))

tumReascDf = tumReasc %>%
  mutate(reasc = if_else(`Location Days` < 365 & `Location Days` > 2,
                         T, F)) %>%
  xtabs(~ reasc + SpRRT, .) %>%
  as_tibble() %>%
  spread(reasc, n) %>%
  rename(reasc = `TRUE`,
         Origin = SpRRT) %>%
  mutate(total = `FALSE` + reasc) %>%
  select(Origin, reasc, total) %>%
  mutate(Origin = recode(Origin,
                         '32H' = 'Hatchery',
                         '32W' = 'Natural')) %>%
  # summarise_at(vars(reasc, total),
  #              list(sum)) %>%
  mutate(rate = reasc / total,
         rateSE = sqrt(rate * (1 - rate) / total))

tumReascDf = tibble(reasc = 14,
                    total = 228) %>%
  mutate(rate = reasc / total,
         rateSE = sqrt(rate * (1 - rate) / total))

tumReascDf %>%
  mutate(totCnt = tumCnt) %>%
  mutate(nFish = totCnt * (1 - rate),
         nFishSE = totCnt * rateSE,
         lowCI = nFish + qnorm(0.025) * nFishSE,
         uppCI = nFish + qnorm(0.975) * nFishSE) %>%
  mutate_at(vars(nFish),
            list(round))


#-----------------------------------------------------------------
# Old method estimates
#-----------------------------------------------------------------
oldEst = read_excel('../Data/UCR Steelehad run escapement using old RT data.xlsx',
                    skip = 3) %>%
  rename(Year = '..1') %>%
  select(-'..5',
         -'..9',
         -'..13')
names(oldEst)[-1] = paste(rep(c('Wenatchee', 'Entiat', 'Methow', 'Okanogan'), each = 3),
                          rep(c('Total', 'Hatchery', 'Wild'), 4), sep = '_')
oldEst %<>%
  gather(var, estOld, -Year) %>%
  mutate(Population = str_split(var, '_', simplify = 'T')[,1],
         Type = str_split(var, '_', simplify = 'T')[,2]) %>%
  select(Year, Population, Type, estOld) %>%
  mutate(Origin = recode(Type,
                       'Wild' = 'Natural'))

newEst = excel_sheets('/Users/kevin/Dropbox/ISEMP/Analysis_Projects/UpperColumbia_AdultEscapement/UC_Steelhead/2011_2018_DABOM/outgoing/Escapement_2011-2018.xlsx') %>%
  as.list() %>%
  map_df(.f = function(x) {
    read_excel('/Users/kevin/Dropbox/ISEMP/Analysis_Projects/UpperColumbia_AdultEscapement/UC_Steelhead/2011_2018_DABOM/outgoing/Escapement_2011-2018.xlsx',
               sheet = x[1]) %>%
      filter(Area %in% c('Wenatchee', 'Entiat', 'Methow', 'Okanogan')) %>%
      mutate_at(vars(CV),
                list(as.numeric))
  })

compDf = newEst %>%
  bind_rows(newEst %>%
              group_by(Year, Area) %>%
              summarise_at(vars(Estimate, Low_95CI, Upp_95CI),
                           list(sum)) %>%
              left_join(newEst %>%
                          group_by(Year, Area) %>%
                          summarise(SE = sqrt(sum(SE^2)))) %>%
              mutate(Origin = 'Total',
                     CV = SE / Estimate)) %>%
  arrange(Year, Area, Origin) %>%
  rename(Population = Area) %>%
  left_join(oldEst %>%
              select(-Type))

compDf %>%
  ggplot(aes(x = estOld,
             y = Estimate,
             color = Population)) +
  geom_abline(linetype = 2,
              color = 'gray') +
  geom_errorbar(aes(ymin = Low_95CI,
                    ymax = Upp_95CI)) +
  geom_point() +
  scale_color_brewer(palette = 'Set1') +
  facet_grid(Year ~ Origin) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(x = 'Old Method',
       y = 'DABOM')


compDf %>%
  filter(Origin != 'Total') %>%
  ggplot(aes(x = estOld,
             y = Estimate,
             # color = Population,
             shape = Origin)) +
  geom_abline(linetype = 2,
              color = 'darkgray') +
  # geom_errorbar(aes(ymin = Low_95CI,
  #                   ymax = Upp_95CI)) +
  geom_point() +
  # scale_color_brewer(palette = 'Set1') +
  scale_shape_manual(values = c('Hatchery' = 1,
                                'Natural' = 19)) +
  # facet_grid(Year ~ Origin) +
  scale_x_continuous(limits = c(0, 6000)) +
  scale_y_continuous(limits = c(0, 6000)) +
  theme_bw() +
  # theme(legend.position = 'bottom') +
  theme(legend.position = c(0.15, 0.9)) +
  labs(x = 'Number of steelhead based on dam counts',
       y = 'Number of steelhead based on PIT tag model')

ggsave('../Figures/MethodComp.pdf',
       width = 5,
       height = 5)

compDf %>%
  mutate(diff = Estimate - estOld,
         relDiff = diff / estOld) %>%
  ggplot(aes(x = relDiff,
             fill = Population)) +
  geom_histogram(binwidth = 0.5,
                 position = 'dodge') +
  scale_fill_brewer(palette = 'Set1') +
  geom_vline(xintercept = 0,
             linetype = 2) +
  theme_bw() +
  facet_wrap(~ Origin,
             scales = 'free') +
  labs(x = '(New - Old) / Old')

