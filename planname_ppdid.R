library(dplyr)

# queried from legacy database: 
#SELECT * 
#FROM public.master_attribute_view
#WHERE data_source_name = 'Reason'

ppd <- rio::import("data/data-1709916516851.csv") %>% 
  select(ppd_id, admin_govt_name) %>% distinct() %>% 
  mutate(admin_govt_name = str_remove(admin_govt_name, " State"))


ppd_id_reason_plan <- rio::import("data/data-1709916516851.csv") %>% select(plan_id, plan_name, ppd_id) %>% 
  distinct()

planname_ppdid <- ppd_id_reason_plan %>% left_join(ppd) %>% filter(ppd_id != "NULL")
write.csv(planname_ppdid, "output/planname_ppdid.csv")
