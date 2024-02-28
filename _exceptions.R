#NOTE: 
#This is for reference only. All result from this file has been input back to the portal. 


### Boston 2021
The following parts deal with exceptions of school districts. Except for New York, results for other entities were entered manually to the database. 

## Boston

- It looks like Boston Public Schools finances are consolidated with the city. They city's FY21 ACFR is here: ACFR_01.31.22_FINAL.pdf (boston.gov). It does not provide discreet reporting for schools, so we'll have to make some assumptions. 

- Total FY21 expenses were $2,083,935,000 and program revenues total to $568,994,000 (p. 20). It lists FTE employees at 10,149 (9,528+621) in FY21 (p. 150). That's 53.82% of total city FTEs. 

- Citywide pension liability is $1,406,402,000 and OPEB is $2,196,724,000. If we apply that ratio, the calculated pension liability is $756,926,000 and OPEB is $1,182,277,000. We can't calculate anything else.

fte_employee_share = .5382 #FTE employees at 10,149 (9,528+621) in FY21 (p. 150)

boston_city_2021 <- readRDS("data/data_from_dbsite_2021.RDS") %>% 
  filter(category == "General Purpose" & name == "Boston" & state == "MA") %>% 
  mutate(id = as.character(id)) %>% 
  select(state, name,id, year, total_liabilities, net_pension_liability, net_opeb_liability, expenses, revenues)

boston_2021 <- nces %>% filter(nces_original_name == "Boston" & state == "MA") %>% 
  mutate(
    id = NA, 
    year = 2021,
    
    # apportion sd from city
    total_liabilities = 0, # can't do this city_total_liability * fte_employee_share
    net_opeb_liability = boston_city_2021$net_opeb_liability*fte_employee_share,
    net_pension_liability =  boston_city_2021$net_pension_liability*fte_employee_share,
    
    # manual insert
    expenses = 2083935000, #schools line, page 20
    revenues = 539271000 + 18727000 + 10996000) %>% # sum of 3 cols program revenues, school line, p20, school district run by the city
  # 
  select(ncesID, nces_original_name, county_name, city, state, id, students, year, total_liabilities, net_opeb_liability, net_pension_liability, expenses, revenues)
```

### Boston 2020
```{r}
##### 2020
fte_employee_share = .5382 #10149

boston_city_2020 <- readRDS("data/data_from_dbsite_2020.RDS") %>% 
  filter(category == "General Purpose" & name == "Boston" & state == "MA") %>% 
  mutate(id = as.character(id)) %>% 
  select(state, name,id, year, total_liabilities, net_pension_liability, net_opeb_liability, expenses, revenues)

boston_2020 <- nces %>% filter(nces_original_name == "Boston" & state == "MA") %>% 
  mutate(
    id = NA, 
    year = 2020,
    
    # apportion sd from city
    total_liabilities = 0, # can't do this: city_total_liability * fte_employee_share
    net_opeb_liability = boston_city_2020$net_opeb_liability*fte_employee_share,
    net_pension_liability =  boston_city_2020$net_pension_liability*fte_employee_share,
    
    # manual insert
    expenses = 1874077000, #schools line, page A-17, school line
    revenues = 9444000 + 60527000 + 10200000) %>% ## sum of 3 cols program revenues, school line, page A-17
  # 
  select(ncesID, nces_original_name, county_name, city, state, id, students, year, total_liabilities, net_opeb_liability, net_pension_liability, expenses, revenues)
```

## Chesterfield
Chesterfield County schools are consolidated with the county, but reported as a component unit: 762 (chesterfield.gov). 
- Expenses and revenues can be found on p. 227. 
- Balance sheet items are on p. 224. 
- Net pension liability is broken out on p. 169.

```{r}
chesterfield_2021 <- nces %>% filter(nces_original_name == "CHESTERFIELD CO PBLC SCHS" & state == "VA") %>%
  mutate(
    id = NA, 
    total_assets = 122955915, #page 224
    year = 2021,
    total_liabilities = 62869374,#page 224
    net_opeb_liability = 0,## How about page 179, Schedule of changes in Net OPEB?? -> have both 2020, 2021
    net_pension_liability = 4167372, #Geoff: should use the net pension liability on p. 169. That seems to focus on teachers. File Chesterfield ACFRS 2022
    
    expenses = 709920047, # #Geoff: For revenues and expenses, use the values on p. 225; those on p. 227 are just a subcomponent.  
    revenues = 704737890) %>% 
  # 
  select(ncesID, nces_original_name, county_name, city, state, id, students, year, total_liabilities, net_opeb_liability, net_pension_liability, expenses, revenues)

#######

chesterfield_2020 <- nces %>% filter(nces_original_name == "CHESTERFIELD CO PBLC SCHS" & state == "VA") %>%
  mutate(
    id = NA, 
    total_assets = 121514512, # page 224, Schedule C-1, Discretely presented Component Unit- School board
    year = 2020,
    total_liabilities = 63252560, # page 224,Schedule C-1, Discretely presented Component Unit- School board
    net_opeb_liability = 0, # page
    
    net_pension_liability = 20311386, #page 169, school board component unit, column 2020, file Chesterfield ACFRS 2021 
    
    expenses = 659875816, # page 225, column School operating, Schedule C-2, Discretely presented Component Unit- School board - Statement of Revenues, Expenditure and changes in Fund Balance
    revenues = 658178834) %>% # page 225,
  # 
  select(ncesID, nces_original_name, county_name, city, state, id, students, year, total_liabilities, net_opeb_liability, net_pension_liability, expenses, revenues)

#
```

## Nashville

Nashville schools are consolidated with Davidson County: ACFRFY21_01_21_2022_Upload.pdf (nashville.gov). Balance sheet items are on p. B-6 and should be the sum of columns called "General Purpose School," "Education Services," and "GSD School Purposes Debt Service."  Revenues and expenditures are on p. B-10 and includes the same columns.

- We can apportion OPEB as we did for Boston. On p. H-35, the employee headcounts show 9,055 out of 18,548 are in education. That's 48.8%. Total liabilities are listed on p. B-142. 
 
 --> Page B-143 shows "school professional employees' 

- For pensions, they're showing a net asset (it's overfunded) on p. B-140. It looks like there are two plans, so we should add those together: $56,699,625 + $5,853,778 = $62,553,403.

```{r}
employee_share = .4848
county_opep_liabilities_2021 = 3240451063 # p. B-142.
county_opep_liabilities_2020 = 3064106607 # p. B-142.

davidson_county_2021 <- nces %>% filter(nces_original_name == "Davidson County" & state == "TN") %>%
  mutate(
    id = NA, 
    total_assets = 649639619 + 118474605 + 124590923, # sum of columns called "General Purpose School," "Education Services," and "GSD School Purposes Debt Service."
    year = 2021,
    total_liabilities = 44660790 + 89820946 + 212989, # sum of above 3 columns 
    net_opeb_liability = county_opep_liabilities_2021*employee_share,
    
    #OR use page B-143 school professional employees
    #net_opeb_liability = 1061732808,
    
    net_pension_liability = 0, 
    expenses = 758516604 + 363357551 + 108450334, #B-10
    revenues = 982397940 + 213091395 + 138244239) %>% # page B-10
  # 
  select(ncesID, nces_original_name, county_name, city, state, id, students, year, total_liabilities, net_opeb_liability, net_pension_liability, expenses, revenues)

##### year 2020

davidson_county_2020 <- nces %>% filter(nces_original_name == "Davidson County" & state == "TN") %>%
  mutate(
    id = NA, 
    total_assets = 56699625 + 5853778, #For pensions, they're showing a net asset (it's overfunded) on p. B-140. It looks like there are two plans, so we should add those together: 
    year = 2020,
    total_liabilities = 44660790 + 89820946 + 212989, # sum of above 3 columns 
    net_opeb_liability = county_opep_liabilities_2020*employee_share,
    
    #OR use page B-143 school professional employees
    #net_opeb_liability = 1196937989,
    
    net_pension_liability = -(56699625 + 5853778), #?? 
    expenses = NA, 
    revenues = NA) %>% 
  # 
  select(ncesID, nces_original_name, county_name, city, state, id, students, year, total_liabilities, net_opeb_liability, net_pension_liability, expenses, revenues)


## Henrico
Henrico County schools are a component unit of Henrico County: Henrico-County-VA-FY21-ACFR-Final.pdf. page 25 - 26

henrico_2021 <- nces %>% filter(nces_original_name == "HENRICO CO PBLC SCHS" & state == "VA") %>%
  mutate(
    id = NA, 
    total_assets = 384459131, 
    year = 2021,
    total_liabilities = 650708572,
    net_opeb_liability = 76925442,
    net_pension_liability = 520851160,
    expenses = 614221032, 
    revenues = 157,496 + 389,076,269) %>% # page 26
  # 
  select(ncesID, nces_original_name, county_name, city, state, id, students, year, total_liabilities, net_opeb_liability, net_pension_liability, expenses, revenues)

######

henrico_2020 <- nces %>% filter(nces_original_name == "HENRICO CO PBLC SCHS" & state == "VA") %>%
  mutate(
    id = NA, 
    total_assets = 393231084,
    year = 2020,
    total_liabilities = 602815832,
    net_opeb_liability = 81756137,
    net_pension_liability = 462836320,
    expenses = 574231130, 
    revenues = 5,030,669 + 336,336,784) %>% #page 26, line total general revenues
  # 
  select(ncesID, nces_original_name, county_name, city, state, id, students, year, total_liabilities, net_opeb_liability, net_pension_liability, expenses, revenues)


