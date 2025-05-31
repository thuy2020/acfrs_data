library(tidyverse)
library(readr)
library(stringr)
library(tidyr)
library(dplyr)
library(stringdist)
source("src/data_processing/nces.R")

# # HSD = High School District
# # CHSD = Community High School District
# # CUSD = Community Unit School District
# # 
# # CCSD = Community Consolidated School District
# # Spec Educ Coop = Special Education Cooperative
# # ROE = Regional Office of Education
# # sd = public schools district
# # USD = School District
# # UD =  Consolidated School District
# 
# # NCES
# # NCES list only has 13,713: data downloaded Jan 27, 2020. Not including charter schools. Student > 1.
# nces_old <- rio::import(here::here("data", "ncesdata_DBBFFFC.xlsx"), skip = 14) %>%
#   select(`NCES District ID`, `District Name`, `County Name*`, City, State) %>%
#   rename(nces_original_name = `District Name`,
#          county_nces = `County Name*`,
#          state = State,
#          ncesid = `NCES District ID`,
#          city_nces = City
#   )
# 
# 
# ####Example of 63 School Districts####
# #This is a hand-matched example of 63 school district in NCES data & ACFRs. 
# #Analyzing the pattern of match between these 2 datasets to help improving match in the full dataset. 
# 
# example_match_60sd <- rio::import(here::here("data", "Mappings for Largest School Districts (2).xlsx")) %>% 
#   rename(name = `Name in ACFR System`, 
#          nces_name = `District Name`) %>% 
#   select(name, nces_name, `State District ID`, `State...7`) 
# 
# example_match_60sd_clean <- example_match_60sd %>% 
#   mutate(nces_name = str_to_lower(nces_name),
#          name = str_to_lower(name)) %>% 
#   
#   # remove terms in acfrs, 1st time
#   mutate(name = str_remove_all(name, "(the)?\\s*school district\\s*(of)?|county|independent school district|district school board|\\s*board of education\\s* (of the)?|public schools|the school board of|board of education|public school system")) %>% 
#   
#   # remove terms in acfrs, second time
#   mutate(name = str_trim(name, side = "both")) %>% 
#   mutate(name = str_remove_all(name, "^of\\s*|-$|municipalno.*|\\.|'s|’s|[0-9]*$")) %>% 
#   mutate(name = str_trim(name, side = "both")) %>% # need to repeat b/c after removing words, space remains
#   
#   
#   # remove terms in nces, 1st time
#   mutate(nces_name = str_remove_all(nces_name, "(the)?\\s*school district\\s*(of)?|independent school district|district( school board)?|board of education|isd|public schools|schools|(co)? pblc schs|county|(city )?sd")) %>% 
#   
#   # remove terms in nces, 2nd time
#   
#   mutate(nces_name = str_trim(nces_name, side = "both")) %>% 
#   mutate(nces_name = str_remove_all(nces_name, "^of\\s*|-$|municipalno.*|\\.|'s|’s|[0-9]*$")) %>% 
#   mutate(nces_name = str_trim(nces_name, side = "both")) 
# 
# example_matched <- example_match_60sd_clean %>% 
#   # check of the 2 name cols are identical
#   mutate(same_name = ifelse(name == nces_name, TRUE, FALSE)) %>% 
#   filter(same_name == TRUE)
# 
# examples_NOT_matched <- example_match_60sd_clean %>% 
#   # check of the 2 name cols are identical
#   mutate(same_name = ifelse(name == nces_name, TRUE, FALSE)) %>% 
#   filter(same_name == FALSE)
# 
# 
# 
# ####ACFRs name####
# acfrs_school_districts <- readRDS("data/acfrs_data.RDS") %>% 
#   filter(category == "School District") %>% 
#   select(state.abb, name, id) %>% 
#   rename(state = state.abb) %>% 
#   distinct() %>%  mutate(acfrs_original_name = name) %>% 
# 
#   #clean
#   mutate(name = str_to_lower(acfrs_original_name)) %>% 
#   mutate(name = str_remove_all(name, "no\\.|#|'")) %>% 
#   mutate(name = str_replace_all(name, "/", " ")) %>%
#   mutate(name = str_replace_all(name, "\\.", " ")) %>% 
#   mutate(name = str_replace_all(name, "-", " ")) %>% 
#   
#   mutate(name = str_remove_all(name, "(community consolidated school district)|(community consolidated schools district)|(joint unified school district)|(center unified school district)"),
#          #name = str_remove_all(name, ""),
#          name = str_remove_all(name, "(consolidated high school district)|(consolidated independent school district)"),
#          name = str_remove_all(name, "(union high school district)|(city school district)|(union elementary school district)|(union school district)|(county unified school district)|(joint unified school district)"), # cali
#          name = str_remove_all(name, "(county school district)|(county independent school district)"),
#          name = str_remove_all(name, "(community unit school district)|(community unit district)"),
#          name = str_remove_all(name, "(public school district)|(public schools district)|(independent school district)|(district school board)"),
#          #OH
#          name = str_remove_all(name, "(union exempted village school district)|(exempted village school district)|(county joint vocational school district)"), 
#          name = str_remove_all(name, "(high school district)|(local school district)"),
#          name = str_remove_all(name, "educational service district"),
#          name = str_remove_all(name, "fractional township"), 
#          name = str_remove_all(name, "(the school districts of)|(board of education)|(the school board of)|(public school system)"),
#          name = str_remove_all(name, "unified school district"),
#          name = str_remove_all(name, "(community school district)|(comm unit school)|(community school dist)"),
#          name = str_remove_all(name,"(elementary school district)|(elementary scool district)"),
#          name = str_remove_all(name,"public schools"),
#          name = str_remove_all(name,"grade school district"),
#          name = str_remove_all(name, "(school district)|(comm sch dist)|(elem sch dist)|(sch dist)|(ind sch dist)")) %>% 
#   mutate(name = str_squish(name))
# 
# #### Middle file - Govt unit names with NCES ID####
# 
# #It is hard to join directly between Acfrs data and NCES data because: 
# # 1) there's no common id. 
# # 2) the name are not exact mapped
# # 
# # It is better to use this government name file as a middle file. 
# #First, it has gov unit names - which are most close to Acfrs name. 
# #Second, it has NCES id - which can be use to link to NCES data. 
# 
# 
# # File Paul sent to Marc: "dataformarc" file. Email Sep 7, 2022
# govname_nces_id <- rio::import("data/censusID_necesID_link.xlsx") %>% 
#   # name in this file is government unit name - also the name in file "Govt_Units_2021_Final.xlsx", sheet 3 + sheet 4
#   rename(gov_unit_original_name = name) %>% 
#   mutate(gov_unit_original_name = str_to_lower(gov_unit_original_name)) %>% 
#   mutate(name = str_trim(gov_unit_original_name)) %>% 
#   rename(ncesid = `NCES Agency Identification Number`, 
#          censusid = idcensus) %>% 
# select(censusid, ncesid, gov_unit_original_name, name) 
# 
# 
# # Only get those 13,713 in NCES to match with ACFRs: #nces %>% left_join(govname_nces_id) 
# census_gov_unit <- govname_nces_id %>% left_join(nces_old, by = "ncesid") %>% 
#   mutate(name = str_replace_all(name, "\\.", " ")) %>% 
#   mutate(name = str_replace_all(name, "/", " ")) %>% 
#   mutate(name = str_replace_all(name, "-|&", " ")) %>% 
#   
#   mutate(name = str_remove_all(name, "(community consolidated school district)"),
#          name = str_remove_all(name, "(community unit school district)|(community unit)"),
#          name = str_remove_all(name, "consolidated school district"), 
#          name = str_remove_all(name, "county school district"),
#          name = str_remove_all(name, "community consolidated schools district"),
#          name = str_remove_all(name, "(community high school district)|(high school district)"), 
#          name = str_remove_all(name, "community consolidated school district"),
#          name = str_remove_all(name, "consolidated high school district"),
#          name = str_remove_all(name,"(city unified sch dist)|(joint unified school district)"), 
#         name = str_remove_all(name, "(unified school district)|(union high school dist)|(co office of ed)|(unified sch dist)|(unified school dist)|(union elem sch dist)|(co unif sch dist)|(union elementary sch dist)"),
#          
#          name = str_remove_all(name,"(public school district)|(public schools)|(unit school district)|(union school district)"),
#          name = str_remove_all(name,"(elementary school district)|(elementary scool district)|(elem school district)|(elementary school dist)"),
#         # Ohio
#         name = str_remove_all(name, "(local school district)|(local sch dist)|(local school dist)|(jt voc sch dist)|(exempted sch dist)|(city sch dist)|(ex vlg sch dist)|(union sch dist)|(ex vlg school dist)"), 
#         name = str_remove_all(name, "co jt voc sch dist"),
#         
#          name = str_remove_all(name, "(community school district)|(community unit district)|(comm college district)|(uni sch dist)|(un sch dist)"),
#          name = str_remove_all(name, "(co ind sch dist)|(unif school dist)|(unif sch dist)|(union elem sch dt)|(jt unified sch dist)|(jt union high school dist)|(elem sch district)|(jt elem sch dist)"), #
#          name = str_remove_all(name, "(district school board)|(ind sch district)|(ind sch dist)|(cons sch dist)|(ind school district)"),
#          
#          name = str_remove_all(name, "(school district)|(district)|(comm sch dist)|(elem sch dist)|(sch dist)|(fr t h school district)|(elem sch dt)|(union el sch d)|(jt uni sch dist)")
#          ) %>% 
# 
# # Texas 
# mutate(name = ifelse(state == "TX", str_remove_all(name, "[0-9]"), name)) %>% 
#   mutate(name = str_squish(name))
# 
# 
# #### Round 1####
# round1 <- acfrs_school_districts %>% left_join(census_gov_unit, by = c("state", "name")) %>% 
#   drop_na(censusid)
# 
# #### Round 2 ####
# 
# acfrs_sd_2 <- acfrs_school_districts %>% 
#   filter(!id %in% round1$id) %>% 
#   mutate(name = str_replace_all(name, "-|,|&|#|_", " ")) %>%  
#   mutate(name = str_remove_all(name, "(school district of the city of)|(consolidated school district)|(consolidated schools)|(community schools)|intermediate|(office of education)|(city sch dist)|(independent public school district)"),
#          name = str_remove_all(name, "^(the)"),
#     name = str_remove_all(name, "central|(union free)|(counties boces)|(county board of cooperative educational services)|centre|(community school)|(community high school)"),
#   #Michigan
#   name = str_remove_all(name, "(union free school district)|(city school district)|(board of cooperative educational services of)|(schools)|township|(district schools)|(union schools)|(public school of)"),
#   
#   name = str_remove_all(name, "public school"),
#   name = str_remove_all(name, "^of "),
#   name = str_remove_all(name, "(school)|county|consolidated|(isd)|( joint)|( district)|( community)|(union districit)$")) %>% 
#   mutate(name = ifelse(state == "OK", str_replace_all(name, " 00", " "), name)) %>% 
#   mutate(name = ifelse(state == "OK", str_replace_all(name, "( i )|(c0)|( c)|( c )|( 0)|( 1 )", " "), name)) %>%
# mutate(name = ifelse(state == "OK", str_replace_all(name, "( ity)|( 0)", " "), name)) %>%
#   mutate(name = str_remove_all(name, "number|(independent)")) %>% 
#   mutate(name = str_replace_all(name, "( d )|( no )|( o[0-9])", " ")) %>% 
#   mutate(name = str_squish(name))
# 
# 
# ## Gov Units
# 
# census_sd_2 <- census_gov_unit %>% filter(!ncesid %in% round1$ncesid) %>% 
# mutate(name = str_replace_all(name, "-|,|&|#|_", " "),
#        name = str_remove_all(name, "'")) %>%  
#   mutate(name = ifelse(state == "MI", str_remove_all(name, "[0-9]"), name)) %>% 
# mutate(name = str_remove_all(name, "(consolidated school district)|(cons school)|(joint community college)|(county community school corporation)"),
#        
#        name = str_remove_all(name, "(union free school district)|(uf sch dist)|(central sch dist)|(union free)|(ctl high school dist)|(pt ool dist)|central|(centre union free school dist)"),
#   name = str_remove_all(name, "(city school dist)|(central sch)|(comm college)|(community college)|(ctl sch dist)|(ctl school dist)|(co comm coll)|(community high school)|(pub sch dist)|(comm sch dist)"),
#   name = str_remove_all(name, "(school district)|central|( ool dist)|(u f school dist)|(ctl sch)|(uf school dist)|(school dist)|(comm schs)|(city sch dist)|(township sch dist)"),
#   
#   name = str_remove_all(name,"(community sch dist)|(comm school dist)"),
#   name = str_remove_all(name, "^of "),
#   
#   name = str_remove_all(name, "(u f)|(twp)$"),
#   name = str_remove_all(name, "(uf)|(isd)$"),
#   name = str_remove_all(name, "( ool)|( pt)|( csd)$"),
#   name = str_remove_all(name, "( ctl)|(c s d)|( schs)$"),
#   
#   name = str_remove_all(name, "(comm schools)|(schs dist)|(public school)|(consolidated school)|(community schools)|(joint union)"),
#   name = str_remove_all(name, 
# "( schools)|( public)|(co schools)|(township)|(pub)|(twp)|( comm)|( community)|( cmty)|(twp f)|(pub fr)|(consol)|( sch)|( scools)|(college)|(township f)|(twp fr)|( co)|( union)|( joint)$"),
#   name = str_replace_all(name, "( i 00)", " ")) %>% 
# 
#   mutate(name = str_squish(name)) 
# 
# round2 <- acfrs_sd_2 %>% left_join(census_sd_2) %>% drop_na(censusid) 
# 
# 
# #### Round 3####
# round1_2 <- rbind(round1, round2)
# # after round 2, how many ACFRS left in total NOT matched
# acfrs_sd_3 <- acfrs_school_districts %>% filter(!id %in% round1_2$id) %>% 
#   mutate(name = str_remove_all(name, "(community schools district)|(county schools district)"),
#     name = str_remove_all(name, "(school disrict)|(community schools)|(community schools)"),
#     name = str_remove_all(name, "( r)|( county)|( consolidated)$")) %>% 
#     mutate(name = ifelse(state == "NE", str_remove_all(name, " [0-9]+$"), name)) %>% 
#   mutate(name = str_remove_all(name, "( municipal)|( city)|( union)$")) %>% 
#   mutate(name = str_squish(name))
# 
# # after round 2, how many census left in total NOT matched
# census_sd_3 <- census_gov_unit %>% filter(!ncesid %in% round1_2$ncesid) %>% 
#   mutate(name = str_remove_all(name,"(ind school dist)|(independent rict)|(community college dist)|(community college)|(br school dist)")) %>% 
#   mutate(name = str_remove_all(name, "( rict )|(county unified school system)"),
#          name = str_remove_all(name, "(city sd)|(city pub[0-9])|(city pub)"),
#          name = str_replace_all(name, "( 0)", " "),
#          name = str_replace_all(name, "serv", "service")) %>% 
#   mutate(name = ifelse(state == "NE", str_remove_all(name, " [0-9]+$"), name)) %>% 
#   mutate(name = str_remove_all(name,"( college)|( independent)|( cons)|( i s)|( rict)|( co cons)|( school)|( city)|( co)|( comm)|( ind sh)|( indep)$"),
#   name = str_remove(name, "(olidated)|( munc)$")
#   ) %>% 
#    mutate(name = str_squish(name))
# 
# round3 <- acfrs_sd_3 %>% left_join(census_sd_3) %>% drop_na(censusid)
# 
# 
# #### Round 4####
# round123 <- rbind(round1, round2, round3)
# #after round 3, how many ACFRS left in total NOT matched
# acfrs_sd_4 <- acfrs_school_districts %>% filter(!id %in% round123$id) 
# 
# # census left in total NOT matched
# census_sd_4 <- census_gov_unit %>% filter(!ncesid %in% round123$ncesid)
# 
# # From here can't join anymore --> need to deal with names differences state by state
# acfrs_sd_4 %>% left_join(census_sd_4) %>% drop_na(ncesid)
# 
# ## IN
# in_acfrs <- acfrs_sd_4 %>% filter(state == "IN") %>% 
#     mutate(name = str_remove_all(name, ", inc"),
#     name = str_remove_all(name, "(community schools ecas)|(community school corporation ecas)|(community schools inc)|(com school corporation)|(consol school corporation eca)|(comm school corporation ecas)|(community school corporation of)|(community school corporation)"),
#          name = str_remove_all(name, "(county schools)|(school corporation ecas)|(school corporation)|(community schools eca)|(community schools)|(metropolitan of)|(school city of)|(county consolidated)"),
#          name = str_remove_all(name, "( ecas)|(cnty cmnty)|(consolidated)|( schools)$"),
#          # change acfrs name to census name
#          name = ifelse(name == "n vermillion eca", "north vermillion", name),
#          name = str_squish(name))
# 
# in_census <- census_sd_4 %>% filter(state == "IN") %>% 
#   mutate(name = str_remove_all(name, "(community school inc)|(community school corporation)|(community schools of)|(county consolidated school corporation)|(consolidated school corporation)|(county community school corporation)|(consolidated schools)"),
#          name = str_remove_all(name, "(school corporation)|(community schools)"),
#          name = str_remove_all(name, "( county)|( metropolitan)|( school corp)$"),
#          name = str_squish(name)) %>% arrange(name)
# 
# in_matched <- in_acfrs %>% left_join(in_census) %>% drop_na(censusid)
# 
# in_acfrs_matched <- in_matched %>% select(state, acfrs_original_name, name)
# 
# ## CA
# 
# ca_acfrs <- acfrs_sd_4 %>% filter(state == "CA") %>% 
#   mutate(name = str_remove_all(name, "(county office of education)|(schools district)|(county superintendent of schools)|(county education office)|(union school distict)|(union elementary)"),
#          name = str_remove_all(name, "( valley)|( joint)$"),
#          
#          # change name to a common to meet census
#      acfrs_original_name = str_squish(acfrs_original_name), # MUST squish original name
#      name = ifelse(acfrs_original_name == "galt joint union elementary school district", "galt elementary", name),
#      name = ifelse(acfrs_original_name == "galt joint union high school district", "galt high", name),
#     name = ifelse(acfrs_original_name == "gold oak union elementary school district", "gold oak element", name),
#     name = case_when(acfrs_original_name == "huntington beach city school district" ~ "huntington elementary",
#                      acfrs_original_name == "huntington beach union high school district" ~ "huntington high",
#                      acfrs_original_name == "rim of the world unified school district" ~ "rim world",
#                      acfrs_original_name == "san rafael city elementary school district" ~ "san rafael elementary",
#                      acfrs_original_name == "san rafael city high school district" ~ "san rafael high", 
#                      name == "three rivers" ~ "three river", 
#                      TRUE ~ name),
#          name = str_squish(name)) #%>% filter(str_detect(name, "galt"))
#   
# 
# ca_census <- census_sd_4 %>% filter(state == "CA") %>% #filter(str_detect(name, "union high"))
#   mutate(name = str_remove_all(name, "(joint unified)|(un high school dist)|(union elem school dist)|(co spl schs oper by co supt)|(co spl sch oper by co supt)|(jt unif school district)|(valley jt unified sch dist)|(co off of education)|(valley unified sch dt)|(unified school d)|(county office of education)|(valley jt unif sch dist)|(elem school dist)|(county special schools operated by co supt)"),
#          name = str_remove_all(name, "(jt high)|(union elem)|(elementary sch)|(joint union high)|(jt union high sch)|(union high)|(un school dist)|(unified school dst)|(co office of education)|(union sch dt)|(cmty unif sch dist)|(co spl schs)"),
#          name = str_remove_all(name, "(union elementary)|(school dist)|(valley elem)|(co special schools)|(elem sch)|(elem school dist)|(elementary sch dist)|(county special schools)|(unified sch dt)|(joint union)|(unif sch dist)|(county selpa)"),
#          name = str_remove_all(name, "( valley)||( el)$"),
#          name = str_remove_all(name, "( jt)|( ctr)|(em sch)|( rict)$"),
#          name = str_remove_all(name, "( val)|(em sch)|( county)|( cy)$"),
#          name = str_remove_all(name, "( j t)|( sch)|(school dist)$"),
#          name = str_remove_all(name, "( vly)|( ist)|( un)$"),
#          
#          # change name to a common to meet acfrs
#          gov_unit_original_name = str_squish(gov_unit_original_name),
#          name = ifelse(gov_unit_original_name == "galt jt union elem sch dist", "galt elementary", name),
#          name = ifelse(gov_unit_original_name == "galt jt union high school dist", "galt high", name),
#          name = ifelse(gov_unit_original_name == "gen shafter elem sch dist", "general shafter", name),
#          name = ifelse(gov_unit_original_name == "gold oak un elem sch dist", "gold oak element", name),
#          
#          name = ifelse(gov_unit_original_name == "calexico unif sch dist", "calexico", name),
#          name = ifelse(gov_unit_original_name == "el segundo uni sch dist", "e l segundo", name),
#          name = ifelse(gov_unit_original_name == "grass valley elem school dist", "grass", name),
#          
#          name = case_when(gov_unit_original_name == "howell mt elem sch dist" ~ "howell mountain", 
#                           gov_unit_original_name == "huntington bch city elem school dist" ~ "huntington elementary",
#                           gov_unit_original_name == "huntington beach uhs dist" ~ "huntington high",
#                           gov_unit_original_name == "san francisco unif sch dist" ~ "an francisco",
#                           name == "pacific" & county_nces == "Fresno County" ~ "pacific (fresno county)",
#                           name == "pacific" & county_nces == "Humboldt County" ~ "pacific (humboldt county)",
#                           
#                           gov_unit_original_name == "san rafael elementary district" ~ "san rafael elementary",
#                           gov_unit_original_name == "san rafael high school district" ~ "san rafael high",
#                           gov_unit_original_name == "victor valley jt union high school dist" ~ "victor",
#                           gov_unit_original_name == "south bay union elem sch dist" & county_nces == "Humboldt County"~ "south bay (humboldt county)",
# gov_unit_original_name == "south bay union school district"  & county_nces == "San Diego County" ~ "south bay (san diego county)",
#                           TRUE ~ name)) %>% 
#          
#         mutate( name = str_squish(name)) %>% 
#   arrange(name) #%>% filter(str_detect(name, "willow"))
# 
# ca_matched <- ca_acfrs %>% left_join(ca_census) %>% drop_na(censusid) 
# 
# ca_acfrs_matched <- ca_matched %>% select(state, acfrs_original_name, name)
# ca_matched %>% select(acfrs_original_name, gov_unit_original_name, name)
# 
# ## OH
# 
# oh_acfrs <- acfrs_sd_4 %>% filter(state == "OH") %>% arrange(name) %>% 
#   mutate(name = str_remove_all(name, "heights university heights"),
#          #name = str_remove_all(name, ""),
#          name = str_remove_all(name, "(central)$"),
#          name = str_replace_all(name, "mc donald", "mcdonald"),
#          # change acfrs name to census name
#          name = ifelse(name == "", "", name),
#          name = str_squish(name))
# 
# oh_census <-  census_sd_4 %>% filter(state == "OH") %>% arrange(name) %>% 
#   mutate(name = str_remove_all(name, "(hgts univ hgts)|(city schools)|(city sch dis t)|(ex vill school dist)|(cent loc sch dist)|(city school dist)|(loc sch dist)|(city school dist)|(local sch dt)"),
#          #name = str_remove_all(name, ""),
#         name = str_remove_all(name, "( loc)|( exem)|( loc)|( ex village)$"), 
#          name = str_remove_all(name, "(ex vil)|(city sch)|( lo)|( cty)|(cal sch dt)$"),
#          name = str_replace_all(name, "hgts", "heights"),
#          
#          name = case_when(gov_unit_original_name == "buckeye cent loc sch dist" ~ "buckeye", 
#                        gov_unit_original_name == "pymatuning vall loc sch dist" ~ "pymatuning valley",
#                        gov_unit_original_name == "reading cmnty city sch dist" ~ "reading community",
#         
#                        gov_unit_original_name == "ripley union lewis local sch dist" ~ "ripley union lewis huntington",
#                        TRUE ~ name),
#         name = case_when(name == "north olmstead" ~ "north olmsted", 
#                          name == "washington courthouse" ~ "washington court house",
#                          name == "yellow spgs" ~ "yellow springs",
#                          TRUE ~ name),
#          name = str_squish(name)) #%>% filter(str_detect(name, "tri"))
# 
# oh_matched <- oh_acfrs %>% left_join(oh_census) %>% drop_na(censusid)
# 
# oh_matched %>% select(acfrs_original_name, gov_unit_original_name, name)
# 
# oh_acfrs_matched <- oh_matched %>% select(state, acfrs_original_name, name)
#  
# 
# ## NE
# ne_acfrs <-  acfrs_sd_4 %>% filter(state == "NE") %>% arrange(name) %>% 
#   mutate(name = str_remove_all(name, "(public schools)|"),
#          name = str_remove_all(name, "(public school)"),
#          name = str_remove_all(name, "[0-9]{1,2}$"),
#          name = str_replace_all(name, "mc donald", "mcdonald"),
#          
#          # change acfrs name to census name
#          acfrs_original_name = str_squish(acfrs_original_name),
#          name = case_when(acfrs_original_name == "blue hill school district no. 91-0074" ~ "blue hill 74", 
#                        acfrs_original_name == "don iphan-trumbu ll public schools district no. 40-0126" ~ "doniphan-trumbull",
#                       acfrs_original_name == "dorchester school district no. 44" ~ "dorchester 44",
#                        TRUE ~ name),
#          name = str_squish(name))# %>% filter(str_detect(name, "blue hill"))
#   
# ne_census <-  census_sd_4 %>% filter(state == "NE") %>% arrange(name) %>% 
#   # use NCES instead of gov unit names
#   mutate(name = str_to_lower(nces_original_name)) %>% 
#   mutate(name = str_replace_all(name, "-", " "),
#          name = str_remove_all(name, "(public schools)|(comm schools)|(community schools)|(public schs)"),
#         name = str_remove_all(name, "$"),
#          name = str_remove_all(name, "(schools)$"),
#   name = case_when(gov_unit_original_name == "blue hill vill school dist 74" ~ "blue hill 74",
#                        gov_unit_original_name == "doniphan-trumbull public schools" ~ "doniphan-trumbull",
#                        gov_unit_original_name == "dorchester vlg sch di 44" ~ "dorchester 44",
#                       TRUE ~ name),
#          name = str_squish(name)) #%>% filter(str_detect(name, "hill"))
# 
# ne_matched <- ne_acfrs %>% left_join(ne_census) %>% drop_na(censusid)
# 
# ne_matched %>% select(acfrs_original_name, gov_unit_original_name, name)
#  
# ne_acfrs_matched <- ne_matched %>% select(state, acfrs_original_name, name)
# 
# 
# ## OK
#    
# ok_acfrs <-  acfrs_sd_4 %>% filter(state == "OK") %>% arrange(name) %>% 
#   mutate(name = str_remove_all(name, "(public schools)|(school district no. i-095)"),
#          name = str_remove_all(name, "(public school)|(i 95)|(i 11)|(c 29 pottawatomie)|(i 27)|(i 2)|( i 10)|(55 c029)|(i 90)|(no i 365)|(d 29)|(i 51)|(c 32)|(60 i 103)"),
#          name = str_remove_all(name, "(i 4)|(  c 9)$"),
#          name = str_replace_all(name, "mc donald", "mcdonald"),
#          # change acfrs name to census name
#          acfrs_original_name = str_squish(acfrs_original_name),
#          name = case_when(acfrs_original_name == "davidson school district no. c-9" ~ "davidson", 
#                        acfrs_original_name == "don iphan-trumbu ll public schools district no. 40-0126" ~ "doniphan-trumbull",
#                       acfrs_original_name == "dorchester school district no. 44" ~ "dorchester 44",
#                        TRUE ~ name),
#          name = str_squish(name)) #%>% filter(str_detect(name, "lamont"))
#   
# ok_census <-  census_sd_4 %>% filter(state == "OK") %>% arrange(name) %>% 
#   # use NCES instead of gov unit names
#   mutate(name = str_to_lower(nces_original_name)) %>% 
# 
#   mutate(name = str_replace_all(name, "-", " "),
#          name = str_remove_all(name, "(public schools)|(comm schools)|(community schools)|(public schs)|(high school)"),
#         name = str_remove_all(name, "$"),
#          name = str_remove_all(name, "(schools)$"),
#   name = case_when(gov_unit_original_name == "" ~ "",
#                       TRUE ~ name),
#          name = str_squish(name)) #%>% filter(str_detect(name, "hill"))
# 
# ok_matched <- ok_acfrs %>% left_join(ok_census) %>% drop_na(censusid)
# ok_matched %>% select(acfrs_original_name, gov_unit_original_name)
# 
# ok_acfrs_matched <- ok_matched %>% select(state, acfrs_original_name, name)
#  
# 
# ## MO
# 
#    
# mo_acfrs <- acfrs_sd_4 %>% filter(state == "MO") %>% arrange(name) %>% 
#   mutate(name = str_remove_all(name, "(public schools)|(school district no. i-095)|(consolidated no)"),
#          name = str_remove_all(name, "(county)|(reorganized 2)|(124)|(reorganized r 2)"),
#          name = str_remove_all(name, "(schools)|( 81)$"),
#         
#          name = str_replace(name, "de soto 73", "desoto 73"),
#          name = str_replace(name, "salem r80", "salem r 80"),
#          # change acfrs name to census name
#          acfrs_original_name = str_squish(acfrs_original_name),
#          name = case_when(acfrs_original_name == "campbell reorganized school district no.2" ~ "campbell r ii",
#                        acfrs_original_name == "fredericktown r-1 school district" ~ "fredericktown r i",
#                       acfrs_original_name == "hayti reorganized school district no.2" ~ "hayti r ii",
#                        TRUE ~ name),
#          
#          name = str_squish(name)) #%>% filter(str_detect(name, "lamont"))
#   
# 
# mo_census <-  census_sd_4 %>% filter(state == "MO") %>% arrange(name) %>% 
#   # use NCES instead of gov unit names
#   mutate(name = str_to_lower(nces_original_name)) %>% 
# 
#   mutate(name = str_replace_all(name, "-|\\.", " "),
#          name = str_remove_all(name, "(public schools)|(comm schools)|(community schools)|(public schs)|(high school)|(co.)|(florissant r ii)"),
#         name = str_remove_all(name, "( 101)|( 58)$"),
#          name = str_remove_all(name, "(schools)|( of warren)$"),
#         
#   name = case_when(gov_unit_original_name == "puxico sch dist r 8" ~ "puxico r viii",
#                       TRUE ~ name),
#          name = str_squish(name)) #%>% filter(str_detect(name, "hill"))
# 
# mo_matched <- mo_acfrs %>% left_join(mo_census) %>% drop_na(censusid)
# mo_matched %>% select(acfrs_original_name, gov_unit_original_name, nces_original_name)
#    
# mo_acfrs_matched <- mo_matched %>% select(state, acfrs_original_name, name)
#  
# 
# ## OR 
# 
# or_acfrs <-  acfrs_sd_4 %>% filter(state == "OR") %>% arrange(name) %>% 
#   mutate(name = str_remove_all(name, "(public schools)|(consolidated no)"),
#          name = str_remove_all(name, "(jt)$"),
#          name = str_remove_all(name, "(29j)|(10jt)|( 2 c)|j$"),
#         
#          name = str_replace(name, "de soto 73", "desoto 73"),
#          name = str_replace(name, "salem r80", "salem r 80"),
#          # change acfrs name to census name
#          acfrs_original_name = str_squish(acfrs_original_name),
#          name = case_when(acfrs_original_name == "centennial school district no. 28jt" ~ "centennial 28",
#                        acfrs_original_name == "north santiam school district no. 29j" ~ "north santiam 29",
#                       acfrs_original_name == "pendleton school district 16r" ~ "pendleton 16",
#                        TRUE ~ name),
#          
#          name = str_squish(name)) #%>% filter(str_detect(name, "centennial"))
#   
# or_census <-  census_sd_4 %>% filter(state == "OR") %>% arrange(name) %>% 
#   # use NCES instead of gov unit names
#   mutate(name = str_to_lower(nces_original_name)) %>% 
# 
#   mutate(name = str_replace_all(name, "-|\\.", " "),
#          name = str_remove_all(name, "(co unit)|(school dist 10j)|(school dist)|(bay )|(unified 7)|(county)"),
#          name = str_remove_all(name, "( sd)"),
#          name = str_remove_all(name, "( 2c)|j$"),
#         name = str_remove_all(name, "( 57)|( 40)|(511)|( 8)|( city)|(10)|(county)$"),
#          
#         
#   name = case_when(gov_unit_original_name == "corvallis sch dist 509-j" ~ "corvallis 509",
#                    gov_unit_original_name == "crow-apple gate-lorane school dist 66" ~ "crow applegate lorane 66",
#                    gov_unit_original_name == "grant admin school dist 3" ~ "grant 3",
#                    gov_unit_original_name == "harney county school dist 4" ~ "harney 4",
#                    gov_unit_original_name == "helix school district #1r" ~ "helix 1", 
# gov_unit_original_name == "jewell sch dist 8" ~ "jewell",
# gov_unit_original_name == "morrow co sch dist 1" ~ "morrow",
# gov_unit_original_name == "perrydale sch dist 21" ~ "perrydale",
# gov_unit_original_name == "pilot rock sch dist 2" ~ "pilot rock",
# gov_unit_original_name == "scio school dist 95" ~ "scio",
# gov_unit_original_name == "seaside sch dist 10" ~ "seaside", 
# gov_unit_original_name == "silver falls school district 4j" ~ "silver falls",
# TRUE ~ name),
# name = str_squish(name)) #%>% filter(str_detect(name, "centennial"))
# 
# or_matched <- or_acfrs %>% left_join(or_census) %>% drop_na(censusid)
# 
# or_matched %>% select(acfrs_original_name, nces_original_name, gov_unit_original_name, name)
# 
# or_acfrs_matched <- or_matched %>% select(state, acfrs_original_name, name)
# 
# ## NJ
#    
# nj_acfrs <-  acfrs_sd_4 %>% filter(state == "NJ") %>% arrange(name) %>% 
#   mutate(name = str_remove_all(name,"(school district of the borough of)|(school district of)|(school district of town of )|(of the)"), 
#          name = str_remove_all(name, "(public schools)|(borough of)|(school distrcit)|(township of )"),
#          name = str_remove_all(name, "(county)|(township)"),
#          name = str_remove_all(name, "^(city of)"),
#          name = str_remove_all(name, "(^(of )|(town of ))"),
#          name = str_remove_all(name, "(schools)|(school)|(borough)$"),
#          
#          # change acfrs name to census name
#          acfrs_original_name = str_squish(acfrs_original_name),
#          name = case_when(acfrs_original_name == "caldwell-west caldwell school district" ~ "caldwell west",
#                           acfrs_original_name == "matawan-abredeen regional school district"~ "matawan aberdeen regional",
#                           acfrs_original_name == "passaic board of education-passaic public schools" ~ "passaic",
#                           acfrs_original_name == "scotch plains-fanwood regional school district" ~ "scotch plains fanwood regional",
#                           acfrs_original_name == "south orange and maplewood school district board of education" ~ "south orange maplewood",
#                           TRUE ~ name),
#          #  
#          name = str_squish(name)) #%>% filter(str_detect(name, "chathams"))
# 
# 
# nj_census <-  census_sd_4 %>% filter(state == "NJ") %>% arrange(name) %>% 
#   # use NCES instead of gov unit names
#   mutate(name = str_to_lower(nces_original_name)) %>% 
#   
#   mutate(name = str_replace_all(name, "-|\\.", " "),
#          name = str_remove_all(name, "(township school district)|(city school district)|(boro school district)|(public school district)|(borough school district)|(high school district)|(school district of )"),
#          name = str_remove_all(name, "( 101)|( 58)|(high school)|(borough)$"),
#          name = str_remove_all(name, "(schools)|(school district)|(township)|( city)|(public)$"),
#          
#          name = case_when(gov_unit_original_name == "paulsboro boro sch dist" ~ "paulsboro",
#                           TRUE ~ name),
#          name = str_squish(name)) #%>% filter(str_detect(name, "hill"))
# 
# nj_matched <- nj_acfrs %>% left_join(nj_census) %>% drop_na(censusid)
# nj_acfrs_matched <- nj_matched %>% select(state, acfrs_original_name, name)
# 
#  
# ## MI
#    
# mi_acfrs <-  acfrs_sd_4 %>% filter(state == "MI") %>% arrange(name) %>% 
#   mutate(name = str_replace_all(name, "-|\\.|#|_", " "),
#          name = str_remove_all(name, "(school district of the city of )|(union schools district)"),
#          name = str_remove_all(name, "(public school)|(community schools)|(community district)|(school system)|(area schools of gogebic county)"),
#          #name = str_remove_all(name, ""),
#          name = str_remove_all(name, "(schools)|(school)$"),
#          
#          # name = str_replace(name, "", ""),
#          # name = str_replace(name, "", ""),
#          # change acfrs name to census name
#          acfrs_original_name = str_squish(acfrs_original_name),
#          name = case_when(acfrs_original_name == "" ~ "",
#                           acfrs_original_name == "detroit public schools community district" ~ "detroit community district",
#                           acfrs_original_name == "" ~ "",
#                           TRUE ~ name),
#          
#          name = str_squish(name)) #%>% filter(str_detect(name, "lamont"))
# 
# mi_census <-  census_sd_4 %>% filter(state == "MI") %>% arrange(name) %>% 
#   # use NCES instead of gov unit names
#   mutate(name = str_to_lower(nces_original_name)) %>% 
#   mutate(name = str_replace_all(name, "-|\\.|#", " "),
#          name = str_replace_all(name, "\\(|\\)", ""),
#          name = str_remove_all(name, "(township school district)|(public school district)|(community schools)|(in the counties of oakland and lapee)|(area schools of gogebic county)|(union schools district )"),
#          name = str_remove_all(name, "(school district)|(public schools)|(s/d )"),
#          name = str_remove_all(name, "(schools)$"),
#          
#          name = case_when(gov_unit_original_name == "" ~ "",
#                           TRUE ~ name),
#          name = str_squish(name)) #%>% filter(str_detect(name, "macomb"))
# 
# mi_matched <- mi_acfrs %>% left_join(mi_census) %>% drop_na(censusid)
# mi_acfrs_matched <- mi_matched %>% select(state, acfrs_original_name, name)
# 
# 
# ## AR
#    
# ar_acfrs <-  acfrs_sd_4 %>% filter(state == "AR") %>% arrange(name) %>% 
#   mutate(name = str_remove_all(name, "[0-9]{1,2}"),
#          name = str_remove_all(name, "consolidated"),
#          acfrs_original_name = str_squish(acfrs_original_name),
#          name = case_when(acfrs_original_name == "cave city school district no. 2a" ~ "cave 2a",
#                           acfrs_original_name == "" ~ "",
#                           acfrs_original_name == "" ~ "",
#                           TRUE ~ name),
#          name = str_squish(name)) 
# 
# ar_census <-  census_sd_4 %>% filter(state == "AR") %>% arrange(name) %>% 
#   # use NCES instead of gov unit names
#   mutate(name = str_to_lower(nces_original_name)) %>% 
#   
#   mutate(name = str_replace_all(name, "-|\\.|\\/", " "),
#          name = str_remove_all(name, "(public schools)|(comm schools)|(community schools)|(public schs)|(high school)|(central sch dist)|(city school district)|(county school dist)|(is central sch dist)"),
#          name = str_remove_all(name, "(school district)|(sch dist)|(school dist)|(cons school dist)"),
#          name = str_remove_all(name, "[0-9]{1,2}"),
#          #name = str_remove_all(name, "$"),
#          
#          name = case_when(gov_unit_original_name == "cave city sch dist 2 a" ~ "cave 2a",
#                           gov_unit_original_name == "south conway co sch dist" ~ "south conway",
#                           gov_unit_original_name == "south side school district" & county_nces == "Van Buren County" ~ "south side (van buren)",
#                           gov_unit_original_name == "texarkana sch dist 7" & county_nces == "Miller County" ~ "texarkana (miller county)",
#                           TRUE ~ name),
#          name = str_squish(name)) #%>% filter(str_detect(name, "hill"))
# 
# ar_matched <- ar_acfrs %>% left_join(ar_census) %>% drop_na(censusid)
# ar_acfrs_matched <- ar_matched %>% select(state, acfrs_original_name, name)
# 
# 
# ## MN
# 
#    
# mn_acfrs <-  acfrs_sd_4 %>% filter(state == "MN") %>% arrange(name) %>% 
#   mutate(name = str_remove_all(name, "(independent school dist)|(community schools)"),
#          name = str_remove_all(name, "^[0-9]{1,4}"),
#          name = str_remove_all(name, "[0-9]{1,4}$"),
#          # 
#          # name = str_replace(name, "-", " "),
#          # name = str_replace(name, "", ""),
#          # # change acfrs name to census name
#          acfrs_original_name = str_squish(acfrs_original_name),
#          name = case_when(acfrs_original_name == "" ~ "",
#                           acfrs_original_name == "burnsville-eagan-savage independent school district 191" ~ "burnsville",
#                           acfrs_original_name == "eastern carver county schools independent school district no. 112" ~ "eastern 112",
#                           TRUE ~ name),
#          
#          name = str_squish(name)) #%>% filter(str_detect(name, "lamont"))
# 
# mn_census <-  census_sd_4 %>% filter(state == "MN") %>% arrange(name) %>% 
#   # keep GOV UNIT
#   mutate(name = str_to_lower(nces_original_name)) %>% 
#   
#   mutate(name = str_replace_all(name, "-|\\.", " "),
#          name = str_remove_all(name, "county public school"),
#          name = str_remove_all(name, "(public schools)|(comm schools)|(community schools)|(public schs)|(high school)|(public school district)|(ind school dist)|(public school dist)|(school district)|(public sch)|(public sc)|(ind sch dist)"),
#          name = str_remove_all(name, "(school dist)|(sch dist)|(isd)"),
#          name = str_remove_all(name, "$"),
#          
#          name = case_when(gov_unit_original_name == "bagley sch district 162" ~ "bagley 162",
#                           gov_unit_original_name == "dassel-cokato public sch district 466" ~ "dassel cokato 466",
#                           gov_unit_original_name == "eastern carver county isd 112" ~ "eastern 112",
#                           gov_unit_original_name == "faribault sch dist 656" ~ "fairbault 656",
#                           TRUE ~ name),
#          name = str_squish(name)) #%>% filter(str_detect(name, "hill"))
# 
# mn_matched <- mn_acfrs %>% left_join(mn_census) %>% drop_na(censusid)
# 
# mn_acfrs_matched <- mn_matched %>% select(state, acfrs_original_name, name)
# 
#  
# ## NY
# 
# ny_acfrs <-  acfrs_sd_4 %>% filter(state == "NY") %>% arrange(name) %>% 
#   mutate(name = str_remove_all(name, "(board of cooperative educational services of )|(supervisory district of )|(board of education city of)"),
#          name = str_remove_all(name, "(board of cooperative educational services)|(enlarged of )|(of the city of )"),
#          name = str_remove_all(name, "(county)|(enlarged)|(union free)|(boces)|(central)"),
#          name = str_remove_all(name, "^(of )"),
#          
#          # change acfrs name to census name
#          acfrs_original_name = str_squish(acfrs_original_name),
#          name = case_when(acfrs_original_name == "leroy central school district" ~ "le roy",
#                           TRUE ~ name),
#          name = str_squish(name)) #%>% filter(str_detect(name, "lamont"))
# 
# ny_census <-  census_sd_4 %>% filter(state == "NY") %>% arrange(name) %>% 
#   # use NCES instead of gov unit names
#   mutate(name = str_to_lower(nces_original_name)) %>% 
#   
#   mutate(name = str_replace_all(name, "-|\\.", " "),
#          name = str_remove_all(name, "(city school districts)|(union free school district)|(public schs)|(city school district)|(city central school district)"),
#          name = str_remove_all(name, "(csd)|(school distict)|(town of)|(school district)|(central school district)"),
#          name = str_remove_all(name, "$"),
#          
#          name = case_when(gov_unit_original_name == "" ~ "",
#                           TRUE ~ name),
#          name = str_squish(name)) #%>% filter(str_detect(name, "hill"))
# 
# ny_matched <- ny_acfrs %>% left_join(ny_census) %>% drop_na(censusid)
# 
#    
# ny_acfrs_matched <- ny_matched %>% select(state, acfrs_original_name, name)
# 
# 
# ## IL
#    
# il_acfrs <-  acfrs_sd_4 %>% filter(state == "IL") %>% arrange(name) %>% #filter(str_detect(name, "148"))
#   mutate(name = str_remove_all(name, "(community)|(counties regional office of education)|(counties special education district 801)|(union elementary consolidated)|(community schools unit district)|(of the city of )|(township high school disrtict )|(cooperative association for special education)"),
#          name = str_remove_all(name, "(cc school dist)|(ccsd)|(special education district joint agreement 865)|(city schools district)|(community consl)|(counties special education association)|(elementary school district)"),
#          name = str_remove_all(name, "(county)|(community)"), 
#          name = str_replace(name, " and ", " and"),
#          #        name = str_replace(name, "", ""),
#          # change acfrs name to census name
#          acfrs_original_name = str_squish(acfrs_original_name)) %>% 
#   # IL
#   mutate(
#     name = case_when(acfrs_original_name == "alsip hazelgreen and oak lawn school district 126" ~ "alsip hazlgrn oaklwn 126",
#                      acfrs_original_name == "berwyn south school district 100" ~ "berwyn 100",
#                      acfrs_original_name == "carmi-white county community unit school district no.5" ~ "carmi 5",
#                      acfrs_original_name == "chicago board of education" ~ "chicago 299",
#                      acfrs_original_name == "country club hill school district 160" ~ "country club hills 160", 
#                      acfrs_original_name == "dolton west school district 148" ~  "dolton 148",
#                      acfrs_original_name == "kildeer countryside consolidated school district" ~ "kildeer countryside community consl 96",
#                      acfrs_original_name == "lagrange elementary school district 102" ~ "la grange 102", 
#                      acfrs_original_name == "lemont-bromberek combined school district 113a"~ "lemont bromberek combined 113 a", 
#                      acfrs_original_name == "moline coal valley school dist 40" ~ "moline 40",
#                      acfrs_original_name == "posen-robbins elementary school district 143 1/2" ~ "posen robbins 143 5", 
#                      acfrs_original_name == "prairie-Hills elementary school district 144" ~ "praire hills 144",
#                      #acfrs_original_name == "Thornton Fractional Township High School District No. 215" ~ name = "",
#                      acfrs_original_name == "school district u-46" ~ "u 46 (elgin area)",
#                      acfrs_original_name == "valley view public schools community unit district" ~ "valley view 365 u",
#                      acfrs_original_name == "bureau henry and stark counties regional office of education no. 28" ~"bureau henry stark",
#                      acfrs_original_name == "dewitt livingston logan and mclean counties regional office of education no. 17" ~ "dewitt livingston",
#                      acfrs_original_name =="lemont-bromberek combined school district 113a" ~ "lemont bromberek csd 113a",
#                      acfrs_original_name == "park ridge-niles school district 64" ~ "park ridge 64",
#                      acfrs_original_name == "speed s.e.j.a. 802" ~ "speed seja 802",
#                      TRUE ~ name),
#     name = str_squish(name)) #%>% filter(str_detect(name, "lamont"))
# 
# #mutate(case_when(name== "calumetlaurium keweenaw" ~ "calumet laureium keweenaw"))
# 
# il_census <-  census_sd_4 %>% filter(state == "IL") %>% arrange(name) %>% #filter(str_detect(name, "100"))
#   # use NCES instead of gov unit names
#   mutate(name = str_to_lower(nces_original_name)) %>% 
#   
#   mutate(name = str_replace_all(name, "-|\\.|\\/|#", " "),
#          name = str_remove_all(name, "(public schools)|(comm schools)|(community schools)|(public schs)|(high school)|(chsd)|(ccsd)|(cusd)|(union sd)|(twp hsd)|(cons sd)|(usd)|(cons hsd)|(sp ed district)|(union e cons d)|(township hsd)|(coop spec ed)|(cntys sp ed assoc)|(city of)"),
#          name = str_remove_all(name, "(roe)|(county)|(cud)|(county spec ed dist)|(esd)|(city)"),
#          name = str_replace_all(name, " sd ", " "),
#          name = str_replace_all(name, "spec educ assoc", "special education association"),
#          name = str_replace_all(name, "365 u", "365u"),
#          name = case_when(gov_unit_original_name == "berwyn school district 100" ~ "berwyn 100",
#                           gov_unit_original_name == "bureau/henry/stark roe" ~ "bureau henry stark",
#                           gov_unit_original_name == "dewitt/livingstn/logan/mclean roe" ~ "dewitt livingston", 
#                           gov_unit_original_name == "massac unit district 1" ~ "massac 1",
#                           TRUE ~ name),
#          name = str_squish(name)) #%>% filter(str_detect(name, "chicago"))
# 
# il_matched <- il_acfrs %>% left_join(il_census) %>% drop_na(censusid)
# 
# il_acfrs_matched <- il_matched %>% select(state, acfrs_original_name, name)
# 
# 
# ## TX
# 
# tx_acfrs <-  acfrs_sd_4 %>% filter(state == "TX") %>% arrange(name) %>% 
#   mutate(name = str_remove_all(name, "indepedent"),
#          name = str_remove_all(name, "(county consolidated common)"),
#          # name = str_remove_all(name, "$"),
#          # 
#          name = str_replace(name, "mountain", "mt"),
#          # name = str_replace(name, "", ""),
#          # # change acfrs name to census name
#          # acfrs_original_name = str_squish(acfrs_original_name),
#          name = case_when(acfrs_original_name == "fort sam houston independent school district" ~ "ft sam houston",
#                           acfrs_original_name == "pharrsan juanalamo independent school district" ~ "pharr san juan alamo",
#                           #              acfrs_original_name == "" ~ "",
#                           TRUE ~ name),
#          # 
#          name = str_squish(name)) #%>% filter(str_detect(name, "lamont"))
# 
# tx_census <-  census_sd_4 %>% filter(state == "TX") %>% arrange(name) %>% 
#   # use NCES instead of gov unit names
#   mutate(name = str_to_lower(nces_original_name)) %>% 
#   
#   mutate(name = str_remove_all(name, "'"),
#          name = str_replace_all(name, "-|\\.|-|\\/|#", " "),
#          name = str_remove_all(name, "(public schools)|(comm schools)|(community schools)|(public schs)|(high school)|(county consolidated csd)|( cisd)"),
#          name = str_remove_all(name, "( isd)|( csd)|(county)$"),
#          name = str_remove_all(name, "$"),
#          
#          name = case_when(gov_unit_original_name == "" ~ "",
#                           TRUE ~ name),
#          name = str_squish(name)) #%>% filter(str_detect(name, "hill"))
# 
# tx_matched <- tx_acfrs %>% left_join(tx_census) %>% drop_na(censusid)
# 
# tx_acfrs_matched <- tx_matched %>% select(state, acfrs_original_name, name)
# 
#  
# ## PA
#    
# pa_acfrs <-  
#   acfrs_sd_4 %>% filter(state == "PA") %>% arrange(name) %>% 
#   
#   mutate(name = str_remove_all(name, "(intermediate)|(school)|(unit)"),
#          name = str_remove_all(name, "(of borough of )|(county)|(of the city of )|(borough)"),
#          name = str_remove_all(name, "^(the )"),
#          name = str_replace_all(name, "iv", "4"),
#          
#          #name = str_replace(name, "heights", "hts"),
#          #name = str_replace(name, "", ""),
#          # change acfrs name to census name
#          acfrs_original_name = str_squish(acfrs_original_name),
#          name = case_when(acfrs_original_name == "harborcreek school district" ~ "harbor creek",
#                           acfrs_original_name == "" ~ "",
#                           acfrs_original_name == "" ~ "",
#                           TRUE ~ name),
#          
#          name = str_squish(name)) #%>% filter(str_detect(name, "lamont"))
# 
# 
# pa_census <-  census_sd_4 %>% filter(state == "PA") %>% arrange(name) %>% 
#   # use NCES instead of gov unit names
#   mutate(name = str_to_lower(nces_original_name)) %>% 
#   
#   mutate(name = str_replace_all(name, "-|\\.|\\/|#", " "),
#          name = str_remove_all(name, "(public schools)|(comm schools)|(community schools)|(public schs)|(high school)|(school dist)"),
#          name = str_remove_all(name, "(intermediate)|(school)|(unit)|(county)|(borough)"),
#          name = str_remove_all(name, "( sd)|(city)$"),
#          name = str_replace_all(name, " iu ", " "),
#          
#          # name = case_when(gov_unit_original_name == "" ~ "",
#          #                     TRUE ~ name),
#          name = str_squish(name)) #%>% filter(str_detect(name, "hill"))
# 
# pa_matched <- pa_acfrs %>% left_join(pa_census) %>% drop_na(censusid)
# 
# pa_acfrs_matched <- pa_matched %>% select(state, acfrs_original_name, name)
# 
# 
# ## TN
#    
# tn_acfrs <-  
#   acfrs_sd_4 %>% filter(state == "TN") %>% arrange(name) %>% 
#   
#   mutate(name = str_remove_all(name, "(board of education)|(community school system)|(city school board of education)"),
#          name = str_remove_all(name, "(community schools)|(city schools)|(school system)|(county schools)"),
#          name = str_remove_all(name, "(city school)|(special school district)|(county)"),
#          name = str_squish(name)) #%>% filter(str_detect(name, "lamont"))
# 
# 
# tn_census <-  census_sd_4 %>% filter(state == "TN") %>% arrange(name) %>% 
#   # use NCES instead of gov unit names
#   mutate(name = str_to_lower(nces_original_name)) %>% 
#   
#   mutate(name = str_replace_all(name, "-|\\.|#", " "),
#          name = str_remove_all(name, "(public schools)|(comm schools)|(community schools)|(public schs)|(high school)|(town schs)|(co sp dist)|(sp dist)|(special school district)"),
#          name = str_remove_all(name, "(county)"),
#          name = str_remove_all(name, "$"),
#          
#          name = case_when(gov_unit_original_name == "" ~ "",
#                           TRUE ~ name),
#          name = str_squish(name)) #%>% filter(str_detect(name, "hill"))
# 
# tn_matched <- tn_acfrs %>% left_join(tn_census) %>% drop_na(censusid)
#    
# tn_acfrs_matched <- tn_matched %>% select(state, acfrs_original_name, name)
# 
#  
# ## LA
# 
# la_acfrs <-  acfrs_sd_4 %>% filter(state == "LA") %>% arrange(name) %>% 
#   mutate(name = str_remove_all(name, "(parish school board)|(city school board)|(community school system)"),
#          name = str_remove_all(name, "(city of )"),
#          name = str_remove_all(name, "(school board)"),
#          # # change acfrs name to census name
#          acfrs_original_name = str_squish(acfrs_original_name),
#          name = case_when(acfrs_original_name == "central community school system" ~ "central community",
#                           acfrs_original_name == "" ~ "",
#                           acfrs_original_name == "" ~ "",
#                           TRUE ~ name),
#          
#          name = str_squish(name)) #%>% filter(str_detect(name, "lamont"))
# 
# la_census <-  census_sd_4 %>% filter(state == "LA") %>% arrange(name) %>% 
#   # use NCES instead of gov unit names
#   mutate(name = str_to_lower(nces_original_name)) %>% 
#   
#   mutate(name = str_replace_all(name, "-|\\.", " "),
#          name = str_remove_all(name, "(city of)|(comm schools)|(community schools)|(public schs)|(high school)|"),
#          name = str_remove_all(name, "( parish)$"),
#          name = str_remove_all(name, "(school district)"),
#          
#          name = case_when(gov_unit_original_name == "" ~ "",
#                           TRUE ~ name),
#          name = str_squish(name)) #%>% filter(str_detect(name, "hill"))
# 
# la_matched <- la_acfrs %>% left_join(la_census) %>% drop_na(censusid)
#  
#    
# la_acfrs_matched <- la_matched %>% select(state, acfrs_original_name, name)
# 
# 
# ## CO
#    
# co_acfrs <-  acfrs_sd_4 %>% filter(state == "CO") %>% arrange(name) %>% 
#   mutate(name = str_remove_all(name, "(board of cooperative educational services)"),
#          name = str_remove_all(name, "(counties of )|(county schools district)|(of the counties of)"),
#          name = str_remove_all(name, "(county)|(boces)|(number)"),
#          
#          name = str_replace(name, " and ", " "),
#          name = str_replace(name, " no ", " "),
#          # change acfrs name to census name
#          acfrs_original_name = str_squish(acfrs_original_name),
#          name = case_when(acfrs_original_name == "school district no. 1 in the city and county of denver and state of colorado" ~ "denver 1",
#                           acfrs_original_name == "bayfield school district 10-jtr" ~ "bayfield 10",
#                           acfrs_original_name == "" ~ "",
#                           TRUE ~ name),
#          
#          name = str_squish(name)) #%>% filter(str_detect(name, "lamont"))
# 
# 
# co_census <-  census_sd_4 %>% filter(state == "CO") %>% arrange(name) %>% 
#   # use NCES instead of gov unit names
#   mutate(name = str_to_lower(nces_original_name)) %>% 
#   
#   mutate(name = str_replace_all(name, "-|\\.", " "),
#          name = str_remove_all(name, "(public schools)|(comm schools)|(community schools)|(public schs)|(high school)|(of the count)|(consolidated school district no)|(board of cooperative educational services)"),
#          name = str_remove_all(name, "(school district)|(school district no)|(sch dist)"),
#          name = str_remove_all(name, "(county)|(consolidated)|(schools)|(in the of e)"),
#          name = str_replace_all(name, " no ", " "),
#          
#          name = case_when(gov_unit_original_name == "denver sch dist 1" ~ "denver 1",
#                           gov_unit_original_name == "bayfield sch dist 10 jt" ~ "bayfield 10",
#                           
#                           gov_unit_original_name == "delta co sch dist j 50" ~ "delta joint 50j",
#                           gov_unit_original_name == "eagle co sch dist re-50" ~ "eagle re50j",
#                           gov_unit_original_name == "falcon sch dist 49" ~ "el paso 49",
#                           gov_unit_original_name == "florence sch dist re-2" ~ "fremont re 2",
#                           gov_unit_original_name == "gunnison watershed sch dist re-1j" ~ "gunnison watershed re 1",
#                           gov_unit_original_name == "mapleton sch dist 1" ~ "mapleton adams 1",
#                           gov_unit_original_name == "monte vista sch dist c-8" ~ "monte vista 8",
#                           gov_unit_original_name == "gunnison watershed sch dist re-1j" ~ "gunnison watershed re 1",
#                           TRUE ~ name),
#          name = str_squish(name)) #%>% filter(str_detect(name, "hill"))
# 
# co_matched <- co_acfrs %>% left_join(co_census) %>% drop_na(censusid)
# co_acfrs_matched <- co_matched %>% select(state, acfrs_original_name, name)
# 
# ## KS 
#    
# ks_acfrs <-  acfrs_sd_4 %>% filter(state == "KS") %>% arrange(name) %>% 
#   mutate(name = str_remove_all(name, "^(no )"),
#          name = str_remove_all(name, "(number )|(county special education cooperative)|"),
#          name = str_remove_all(name, "(special education interlocal)|(unified school distrct)"),
#          name = str_squish(name)) #%>% filter(str_detect(name, "lamont"))
# 
# 
# ks_census <-  census_sd_4 %>% filter(state == "KS") %>% arrange(name) %>% 
#   # use NCES instead of gov unit names
#   mutate(name = str_to_lower(gov_unit_original_name)) %>% 
#   
#   mutate(name = str_replace_all(name, "-|\\.", " "),
#          name = str_remove_all(name, "(public schools)|(public school district)|(community schools)|(public schs)|(high school)|(co comm schools)|(co community)|(county unified school district)"),
#          name = str_remove_all(name, "(unified school district)|(comm schools)|(county schools)|(public school)"),
#          name = str_remove_all(name, "( schools)|( city)|(county)$"),
#          
#          name = case_when(gov_unit_original_name == "" ~ "",
#                           TRUE ~ name),
#          name = str_squish(name)) #%>% filter(str_detect(name, "367"))
# 
# ks_matched <- ks_acfrs %>% left_join(ks_census) %>% drop_na(censusid)
# 
# 
# # round 4 state by state
# round4 <- in_matched %>% 
#   rbind(oh_matched) %>% 
#   rbind(ca_matched) %>% 
#   rbind(ne_matched) %>% 
#   rbind(ok_matched) %>% 
#   rbind(mo_matched) %>% 
#   rbind(or_matched) %>% 
#   rbind(nj_matched) %>% 
#   rbind(mi_matched) %>% 
#   rbind(ar_matched) %>% 
#   rbind(mn_matched) %>% 
#   rbind(ny_matched) %>% 
#   rbind(il_matched) %>% 
#   rbind(tx_matched) %>% 
#   rbind(pa_matched) %>% 
#   rbind(tn_matched) %>% 
#   rbind(la_matched) %>% 
#   rbind(co_matched) %>% 
#   rbind(ks_matched)
# 
# ####Result after 4 rounds####
# 
# round1234 <- round123 %>% rbind(round4) %>% 
#   select(id, ncesid, state, acfrs_original_name) %>% 
#   rename(name = acfrs_original_name,
#          state.abb = state,
#          ncesID = ncesid)
# 
# 
# ####Manual mapping####
# round5 <- read.csv("data/_dictionary_5.csv") %>% 
#   filter(!is.na(ncesID)) %>% 
#   select(-ncesName) %>% 
#   rename(state.abb = state) %>% 
#     mutate(name = str_to_lower(name))
# 
# # this file has number of students, NCES names, and other notes about the entities
# round6 <- readxl::read_xlsx("data/_dictionary_6_manually_created.xlsx") %>% select(id, name, ncesID, state.abb)
# round7 <- read_csv("data/_dictionary_7_manually_created.csv") %>% select(-1)
# 
# ####Exceptions####
#   #NE Omaha City School District 1 = 	Douglas County School District No. 0001	35480
#   
# # This dictionary was constructed using an other NCES list that is similar but not identical to the ELSI list
# dictionary_old <- readRDS("data/dictionary_old.RDS") %>% rename(state.abb = state)
# 
#   
# dictionary_1234567_old <- rbind(round1234, round5, round6, round7, dictionary_old) %>% 
#   mutate(name = str_to_lower(name)) %>% 
#   mutate(ncesID = ifelse(nchar(ncesID) < 7, paste0("0", ncesID), ncesID)) %>% 
#   drop_na() %>% distinct() %>% 
# # keep the inflated rows out
#   add_count(id) %>% filter(n==1) %>% select(-n) %>%
# 
# # take out some acfrs already deleted from acfrs database
#   filter(!id %in% c("30408", "76973", "87263")) %>% 
#   
# # filter out wrong matches
#   filter(!ncesID %in% c("0618810"))
# 
# 
# 
# #check inflated id   
# rbind(round1234, round5, round6, round7, dictionary_old) %>% 
#   mutate(name = str_to_lower(name)) %>% 
#   mutate(ncesID = ifelse(nchar(ncesID) < 7, paste0("0", ncesID), ncesID)) %>% 
#   drop_na() %>% distinct() %>% 
#   add_count(id) %>% filter(n>1)  %>% 
#   left_join(nces) %>% 
#   select(state.abb, id, ncesID, name, name_nces, county) -> inflated_dictionary
# 
# #inflated_dictionary %>% arrange(state.abb, name) %>% 
# #  writexl::write_xlsx("tmp/inflated_dictionary.xlsx")
# 
# #check inflated ncesID  - not a real problem. Essentially duplicated due to variation in names. 
# # TODO: some acfrs id not existing. If there's a longer & a shorter acfrs id. the shorter does not exist. 
# rbind(round1234, round5, round6, round7, dictionary_old) %>% 
#   mutate(name = str_to_lower(name)) %>% 
#   mutate(ncesID = ifelse(nchar(ncesID) < 7, paste0("0", ncesID), ncesID)) %>% 
#   drop_na() %>% distinct() %>% 
#   add_count(ncesID) %>% filter(n>1)  %>% 
#   left_join(nces) %>% 
#   select(state.abb, id, ncesID, name, name_nces, county) %>% 
#   arrange(state.abb, name) -> inflated_ncesID 
# 
# # corrected the inflated 
# inflated_dictionary_corrected <- readxl::read_xlsx("data/_inflated_dictionary_corrected_July2024.xlsx") %>% 
#   select(state.abb, id, ncesID, name)
# 
# 
# ####Minesota####
# # mn_wiki <- readxl::read_excel("data/Minesota_school_districts_wiki_list.xlsx") %>% 
# #   mutate(number = str_extract(name_wiki, "\\d+")) %>% drop_na() 
# # mn_acfrs_needNCESid <- testc %>% mutate(number = str_extract(name, "\\d+")) %>% 
# #   select(id, name, number) %>% distinct()
# # mn_acfrs_needNCESid %>% left_join(mn_wiki, by = "number") %>% View()
# 
# round10_minesota <- read.csv("data/_round10_minesota.csv") %>% select(-1)
# 
# # Manual input 
# round11 <- read.csv("data/_acfrs_without_ncesID_Aug2024.csv") %>% 
#   drop_na(ncesID) %>% 
#   select(state.abb, id, name, ncesID)
# 
# round12 <- read.csv("data/_dictionary_12_manually_created.csv") %>% 
#   select(-1, -name_nces) %>% 
#   drop_na(ncesID)
#   
#   #### Result####
# # adding back the corrected inflated join  
# dictionary_tmp <- dictionary_1234567_old %>% 
#   rbind(inflated_dictionary_corrected) %>% 
#   rbind(round10_minesota) %>% 
#   rbind(round11)  %>% 
#   rbind(round12) %>% 
#   #filter out entities that were removed from database OR wrong matches
#   filter(!id %in% c("168053", "1268310")) %>% 
# 
#   #fixing wrong pairs
#   mutate(ncesID = case_when(id == "225139" ~ "3406540",
#                             id == "42008" ~ "3412060",
#                             id == "37130" ~ "3904778",
#                             id == "37201" ~ "3904849",
#                             id == "37134" ~ "3904945",
#                             id == "37260" ~ "3904790",
#                             id == "37149" ~ "3904809",
#                             id == "37307" ~ "3905058",
#                             id == "49866" ~ "3904627",
#                             id == "42545" ~ "4810140",
#                             id == "39070" ~ "4813410",
#                             id == "39183" ~ "4813680",
#                             id == "163540" ~ "4816380",
#                             id == "38814" ~ "4818120",
#                             id == "45440" ~ "4835560",
#                             id == "163660" ~ "4823730",
#                             id == "65007" ~ "3904853",
#                             id == "85769" ~ "4111910",
#                             TRUE ~ ncesID)) 
#   
# saveRDS(dictionary_tmp, "data/dictionary_tmp.RDS")

#TODO: Update anything after this. DO NOT run the above code again. 


dictionary_tmp <- readRDS("data/dictionary_tmp.RDS") %>% 
  #removed entity no longer exist in acfr database
  filter(!id %in% c("1268507", "34840", "30371", "31164", "1133206", 
                    "31398", "189042", "69024", "105267", "196794",
                    "1267815", "171323", "1267835", "58299", "34840",
                    "1268507", "34887", "1240701", "46287", "1240220",
                    "82195", "36553", "147069", "37500", "38854", "200059",
                    "197654", "197659", "197665", "88825", "197671", "32051")) %>% 
  

  mutate(id = case_when(ncesID == "4700148" ~ "87787",
                        ncesID == "3622050" ~ "36342",
                        ncesID == "0805370" ~ "1266165",
                        TRUE ~ id)) 
 
####Fuzzy match####
# Normalize function ---
normalize_name <- function(name) {
  name %>%
    tolower() %>%
    gsub("school district", "", .) %>%
    gsub("elem", "elementary", .) %>%
    gsub("no\\.?|districts?|schools?|public|and", "", .) %>%
    gsub("[^a-z0-9 ]", "", .) %>%
    trimws()
}

 acfr_as_missing_ncesID <- school_districts_all %>% 
    select(state.abb, state.name, name, id, ncesID) %>% 
    filter(is.na(ncesID)) %>% distinct() 
  
acfr_as_missing_ncesID_normalized <- acfr_as_missing_ncesID %>%
    mutate(name_clean = normalize_name(name))
  
  nces_normalized <- nces %>%
    #padding a leading 0, ncesID should have 7 digit
    mutate(ncesID = sprintf("%07s", as.character(ncesID))) %>% 
    
    mutate(name_clean = normalize_name(name_nces))
  
  # Match within state to improve accuracy ---
  match_within_state <- function(state_df, nces_df) {
    dist_matrix <- stringdistmatrix(state_df$name_clean, nces_df$name_clean, method = "jw")
    best_match_index <- apply(dist_matrix, 1, which.min)
    
    matched_nces <- nces_df[best_match_index, ]
    
    state_df %>%
      mutate(
        filled_ncesID = nces_df$ncesID[best_match_index],
        matched_name_nces = nces_df$name_nces[best_match_index],
        state_agency_id = matched_nces$state_agency_id,
        county_nces = matched_nces$county_nces,
        match_score = mapply(function(i, j) dist_matrix[i, j], 
                             seq_along(best_match_index), best_match_index)
      )
  }
  
  # --- 5. Apply fuzzy match by state ---
  matched_all_states <- acfr_as_missing_ncesID_normalized %>%
    group_split(state.abb) %>%
    purrr::map_dfr(function(state_group) {
      state_abbr <- unique(state_group$state.abb)
      
      nces_state <- filter(nces_normalized, state.abb == state_abbr)
      match_within_state(state_group, nces_state)
    })
  
  
  # keep only new matches and strong ones ---
  strong_matches <- matched_all_states %>%
    filter(is.na(ncesID) & match_score < 0.15) %>% 
    
    #need to treat Montana separately
    filter(state.abb != "MT") %>% 

    # fix some error
    filter(name != "oakes public schools") %>% 
    mutate(filled_ncesID = case_when(name == "fremont county school district no. 25"
                                     ~ "5605220",
                      
           TRUE ~ as.character(filled_ncesID))) %>% 
    select(-ncesID) %>% 
    
    rename(ncesID = filled_ncesID) %>% 
    select(id, ncesID, state.abb, name)
  
  # view some beyond 0.15, < 0.2
  
  #TODO: review and weed out by hand this set
  matched_all_states %>% 
    filter(is.na(ncesID) &  0.15 < match_score & match_score < 0.2) %>% View()
  
#TODO: need to find a way to match these week_matches
  # weak_matches
  weak_matches <- matched_all_states %>%
    filter(is.na(ncesID) & match_score >= 0.15) %>% 
    
    #need to treat Montana separately
    filter(state.abb != "MT") 
    
  # --- 7. Save result ---
  write_csv(weak_matches, "tmp/us_missing_ncesID_weak_matches.csv")
  

  
####Final merge####
 
 dict_13 <- readxl::read_xls("data/_dictionary_13.xls") %>% 
   select(id, ncesID, state.abb, name) %>% 
   mutate(across(everything(), as.character))
 
 # In Montana, some separate school districts are reported in one acfrs
 # need to combine students from these sd to reflect true number of students that these acfrs cover
 dict_montana <- readxl::read_xlsx("data/_dictionary_montana_school_districts.xlsx") %>% 
   select(id, ncesID, state.abb, name) %>% 
   mutate(across(everything(), as.character))
 
  
  # id in dictionary but no longer exist in acfr database
  id_nolonger_exist <- anti_join(dictionary_tmp, school_districts_all, by = "id") 
  
  dictionary <- dictionary_tmp %>% 
    filter(!id %in% id_nolonger_exist$id) %>% 
    add_count(id) %>% filter(n == 1) %>% select(-n) %>% 
    add_count(ncesID) %>% filter(n == 1) %>% select(-n) %>% 
    
  
    # add fuzzy match result - strong match
    rbind(strong_matches) %>% 
    rbind(dict_13) %>% 
    rbind(dict_montana) %>% distinct() %>% 
    
    
    #final correction: 
    mutate(ncesID = case_when(id == "161618" ~ "2622320",
                              TRUE ~ ncesID
                              ))
    
  
  # update changes here
  saveRDS(dictionary, "data/dictionary.RDS")
  dictionary %>% write.csv("tmp/ncesID_acfrsID_dictionary.csv")
  
####Final summary###
  nces_not_matched <- nces_normalized %>% filter(!ncesID %in% dictionary$ncesID) %>% 
    select(state.abb, name_nces, ncesID, county_nces, state_agency_id, agency_type, enrollment_23) 
  
  sum(nces_not_matched$enrollment_23, na.rm = TRUE)/
    sum(nces$enrollment_23, na.rm = TRUE)
  