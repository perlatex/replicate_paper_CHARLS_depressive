library(tidyverse)
library(haven)

community <- read_dta("./data/community2011.dta")
psu       <- read_dta("./data/psu2011.dta")
demogr11  <- read_dta("./data/Demographic_Background2011.dta")
demogr13  <- read_dta("./data/Demographic_Background2013.dta")
health    <- read_dta("./data/Health_Status_and_Functioning2013.dta")




##############################################
# how may types of facility in this community?
##############################################

facility <- community %>% 
  select(
    communityID, 
    sub_commuID, 
    jb029_1_1_,
    jb029_1_2_,
    jb029_1_3_,
    jb029_1_4_,
    jb029_1_5_,
    jb029_1_6_,
    jb029_1_11_,
    jb029_1_14_,
  ) %>% 
  mutate( across(starts_with("jb029_1_"), ~ if_else(.x %in% c(2, NA), 0, .x)) ) %>% 
  rowwise() %>% 
  mutate( facility = sum(c_across(starts_with("jb029_1_"))) ) %>% 
  ungroup() %>% 
  select(communityID, facility) %>% 
  full_join(
    psu %>% select(communityID, city, urban_nbs, areatype, versionID), 
    by = "communityID"
  )



##############################################
# 人口学
##############################################

demo <- demogr13 %>% 
  select(
    ID, 
    householdID, 
    communityID, 
    birth_year_2013 = ba002_1, 
    sex             = ba000_w2_3, 
    martial         = be001, 
    edu             = bd001
  ) %>% 
  left_join(
    demogr11 %>% 
      mutate(ID = paste(householdID, substr(ID, 10, 11), sep = "0")) %>% 
      select(ID, birth_year_2011 = ba002_1), 
    by = "ID"
  ) %>% 
  mutate(birth_year = coalesce(birth_year_2013, birth_year_2011)) %>% 
  mutate(age = 2013 - birth_year) %>% 
  select(ID, householdID, communityID, sex, age, martial, edu)





##############################################
# health status and functioning data 2013 for physical activity data
# PA          da051_1_:da055_3_
# IADL        db016:db020 instrumental activities of daily living (IADLs)
# depression  dc009:dc018
##############################################


behavior <- health %>% 
  select(
    ID, 
    da051_1_:da055_3_,        # PA
    num_range("db0", 16:20),  # Functional Health Status(IADL)
    dc009:dc018               # depressive symptoms
  ) %>% 
  mutate(
    vig = case_when(
      da051_1_ == 2 ~ 0,
      da054_1_ == 1 ~ 1,
      da054_1_ == 2 ~ 2,
      da055_1_ == 1 ~ 3,
      da055_1_ == 2 ~ 4,
      .default      = NA
    )
  ) %>% 
  mutate(da052_1_ = if_else(vig == 0, 0, da052_1_)) %>% 
  mutate(vig_total = vig*da052_1_) %>% 
  
  mutate(
    vig_leisure = case_when(
      da051_1_1_ == 1 ~ 0,
      #da054_1_1_ == 4 ~ 0,
      .default        = vig_total
    )
  ) %>% 
  mutate(
    vig_nonLeisure = case_when(
      da051_1_1_ == 2 ~ 0,
     # da054_1_1_ == 3 ~ 0,
      .default        = vig_total
    )
  ) %>% 
  mutate(
    mod = case_when(
      da051_2_ == 2 ~ 0,
      da054_2_ == 1 ~ 1,
      da054_2_ == 2 ~ 2,
      da055_2_ == 1 ~ 3,
      da055_2_ == 2 ~ 4,
      .default      = NA
    )
  ) %>% 
  mutate(da052_2_ = if_else(mod == 0, 0, da052_2_)) %>% 
  mutate(mod_total = mod*da052_2_) %>% 
  mutate(
    mod_leisure = case_when(
      da051_1_2_ == 1 ~ 0,
     # da054_1_2_ == 4 ~ 0,
    .default          = mod_total
    )) %>% 
  mutate(
    mod_nonLeisure = case_when(
      da051_1_2_ == 2 ~ 0,
      #da054_1_2_ == 3 ~ 0,
      .default        = mod_total
    )) %>% 
  
  mutate(
   walk = case_when(
     da051_3_ == 2 ~ 0,
     da054_3_ == 1 ~ 1,
     da054_3_ == 2 ~ 2,
     da055_3_ == 1 ~ 3,
     da055_3_ == 2 ~ 4,
     .default      = NA
  )) %>% 
  mutate(da052_3_ = if_else(walk == 0, 0, da052_3_)) %>% 
  mutate(walk_total = walk*da052_3_) %>% 
  mutate(
    walk_leisure = case_when(
      da051_1_3_== 1 ~ 0,
      #da054_1_3_== 4 ~ 0,
      .default        = walk_total
  )) %>% 
  mutate(
    walk_nonLeisure = case_when(
     da051_1_3_== 2 ~ 0,
     #da054_1_3_== 3 ~ 0,
    .default         = walk_total
    )
  ) %>% 
  
  select(
    ID,
    vig_total,
    mod_total,
    walk_total,
    vig_leisure,
    mod_leisure,
    walk_leisure,
    vig_nonLeisure,
    mod_nonLeisure,
    walk_nonLeisure,
    
    db016:db020, 
    dc009:dc018
  )





##############################################
# 合并
##############################################

mydata <- demo %>% 
  mutate(ID = as.character(ID)) %>% 
  left_join(behavior, by = "ID") %>% 
  filter(!if_all(c(vig_total, mod_total, walk_total), is.na)) %>% 
  left_join(facility, by = "communityID") %>% 
  distinct(ID, .keep_all = TRUE)

mydata %>% sjPlot::view_df()

mydata <- mydata %>% filter(age >= 45)



##############################################
# 插值（先插值、再变形，后统计。）
##############################################

library(mice)

mydataImp1 <- mydata %>% 
  haven::zap_labels() %>% 
  mice::mice(method = "pmm", m = 2, seed = 1024) %>% 
  mice::complete() %>% 
  as_tibble() %>% 
  mutate(
    LPA = 8 * vig_leisure + 4 * mod_leisure + 3.3 * walk_leisure,
    TPA = 8 * vig_total   + 4 * mod_total   + 3.3 * walk_total
  ) 



##############################################
# 保存
##############################################

readr::write_rds(mydataImp1, "mydataImp1.rds")

