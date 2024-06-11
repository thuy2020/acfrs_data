
## These values will be imported to the database later. 

# currently there's no revenues field in database

#Metropolitan Nashville Public Schools TN
revenues = case_when(id == "107203" & year == "2021"~ 1333733574, # 982397940 + 213091395 + 138244239) %>% # page B-10
                     id == "107203" & year == "2020" ~ 1135186800,   #883724814 + 138389571 + 113072415      
                     TRUE ~ revenues)


