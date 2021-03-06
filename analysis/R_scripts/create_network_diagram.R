# Author: Kevin See
# Purpose: Create network diagram for DABOM
# Created: 5/6/2020
# Last Modified: 5/6/2020
# Notes: For steelhead starting at Priest Rapids dam, in spawn year 2016

#-----------------------------------------------------------------
library(PITcleanr)
library(tidyverse)
library(magrittr)

#-----------------------------------------------------------------
# dataframe of sites for PRD DABOM model with some indication of network structure
site_df = writePRDNodeNetwork()

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
                   StartDate = as.POSIXct(ymd('20100101')),
                   SiteType = 'INT',
                   SiteName = 'Colockum Creek',
                   AntennaGroup = 'Single Colockum Ck',
                   SiteDescription = 'Tempoary single antenna.',
                   SiteTypeName = 'Instream Remote Detection System',
                   RKM = '740.001',
                   RKMTotal = 741))

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
  mutate(Node = ifelse(Node == "LWE",
                       'LWEB0',
                       Node),
         Node = ifelse(SiteID %in% c('TUF', 'TUMFBY', 'TUM'),
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
         Node = ifelse(Node == "LMR",
                       'LMRB0',
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
         Node = ifelse(Node == "SCP",
                       'SCPB0',
                       Node),
         Node = ifelse(Node == "OMK",
                       'OMKB0',
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
# Build network diagram
#-----------------------------------------------------------------
library(tidygraph)
library(ggraph)
library(netplot)
library(igraph)

# build parent-child table
# which spawn year are we dealing with?
yr = 2017
# start date is July 1 of the previous year
start_date = paste0(yr - 1, '0701')

# build parent-child table
par_ch_node = createParentChildDf(site_df,
                                  configuration,
                                  startDate = start_date)

root_node = par_ch_node %>%
  filter(nodeOrder == 1) %>%
  pull(ParentNode)

par_ch_site = createParentChildDf(site_df,
                                  configuration %>%
                                    mutate(Node = ifelse(grepl('A0$', Node) | grepl('B0$', Node),
                                                         SiteID,
                                                         Node)) %>%
                                    distinct(),
                                  startDate = start_date) %>%
  rename(ParentSite = ParentNode,
         ChildSite = ChildNode) %>%
  left_join(stack(site_list) %>%
              tbl_df() %>%
              select(Group = ind,
                     ChildSite = values) %>%
              bind_rows(tibble(Group = root_node,
                               ChildSite = root_node)) %>%
              mutate(Group = factor(Group,
                                    levels = c(root_node, names(site_list)))) %>%
              mutate(BranchNum = as.integer(Group))) %>%
  left_join(configuration %>%
              select(ChildSite = SiteID,
                     lat = Latitude,
                     long = Longitude) %>%
              distinct()) %>%
  mutate(lat = if_else(is.na(lat) & ChildSite == 'BelowJD1',
                       configuration %>%
                         filter(SiteID == 'CHINOR') %>%
                         pull(Latitude),
                       lat),
         long = if_else(is.na(long) & ChildSite == 'BelowJD1',
                        configuration %>%
                          filter(SiteID == 'CHINOR') %>%
                          pull(Longitude),
                        long))

# # add nodes for black boxes
# bb_nodes = par_ch_site %>%
#   group_by(ParentSite) %>%
#   summarise(nChild = n_distinct(ChildSite)) %>%
#   filter(nChild > 1) %>%
#   left_join(par_ch_site %>%
#               select(-ParentSite,
#                      ParentSite = ChildSite,
#                      SiteType:long)) %>%
#   mutate(SiteType = 'BB') %>%
#   mutate(ChildSite = paste0(ParentSite, '_bb')) %>%
#   select(-nChild) %>%
#   distinct()
#
# par_ch_site %<>%
#   bind_rows(bb_nodes)

# build table of nodes
nodes = par_ch_site %>%
  select(ParentSite, ChildSite) %>%
  gather(type, node) %>%
  select(node) %>%
  distinct() %>%
  left_join(par_ch_site %>%
              rename(node = ChildSite) %>%
              select(-starts_with("Parent"))) %>%
  mutate(Group = as.factor(Group),
         Group = fct_relevel(Group, root_node)) %>%
  arrange(Group, RKM, nodeOrder) %>%
  mutate(index = 1:n()) %>%
  select(index, label = node, everything())

nodes %<>%
  mutate(Group = if_else(label %in% c('PRA', 'PRA_bb', 'RIA', 'RRF', 'WEA'),
                         'Mainstem',
                         as.character(Group)),
         Group = factor(Group,
                        levels = c('Mainstem',
                                   'BelowPriest',
                                   'Wenatchee',
                                   'Entiat',
                                   'Methow',
                                   'Okanogan')),
         Group = fct_drop(Group))

nodes %<>%
  left_join(par_ch_site %>%
              group_by(label = ParentSite) %>%
              summarise(nChilds = n_distinct(ChildSite)) %>%
              bind_rows(par_ch_site %>%
                          filter(!ChildSite %in% ParentSite) %>%
                          select(label = ChildSite) %>%
                          mutate(nChilds = 0)) %>%
              mutate(nodeType = if_else(nChilds == 0,
                                        'Terminal',
                                        if_else(nChilds == 1,
                                                'PassThru', 'Branch')))) %>%
  mutate(nodeType = if_else(SiteType == 'BB',
                            'BB', nodeType),
         nodeType = factor(nodeType,
                           levels = c('Branch',
                                      'PassThru',
                                      'Terminal',
                                      'BB')))


# build table of edges (connecting nodes)
edges = par_ch_site %>%
  filter(ParentSite != ChildSite) %>%
  select(from = ParentSite,
         to = ChildSite) %>%
  distinct() %>%
  mutate(edgeID = 1:n()) %>%
  gather(direction, label, -edgeID) %>%
  left_join(nodes %>%
              select(index, label)) %>%
  select(-label) %>%
  spread(direction, index) %>%
  select(-edgeID)

# one graph with all sites
myGraph = tbl_graph(nodes = nodes,
                    edges = edges)

#--------------------------------------------------
# igraph
l = igraph::layout_as_tree(myGraph,
                           flip.y = F)

# set of colors
myColors = RColorBrewer::brewer.pal(nlevels(nodes$Group), 'Set1')
# myColors = viridis::plasma(nlevels(nodes$Group))
# myColors = viridis::viridis(nlevels(nodes$Group))
# myColors = gray.colors(nlevels(nodes$Group), start = 0.3, end = 0.9)
# myColors = c(rep('darkgray', 3), 'black')

# this will open up a Quartz window and let you edit the layout with your mouse
id = tkplot(myGraph,
            layout = l,
            canvas.width = 1200,
            canvas.height = 900,
            vertex.color = myColors[nodes$Group],
            frame.color = 'black',
            # vertex.color = 'gray90',
            vertex.shape = 'circle',
            vertex.size = 25,
            # label = nodes$label,
            label.size = 1,
            # label.cex = 8,
            edge.color = 'black')

# for saving
tk_postscript(tkp.id = id)

tk_close(tkp.id = id)
tk_off()

