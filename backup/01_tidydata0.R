library(tidyverse)

# 人口学数据
demo <- haven::read_dta("F:/CHARLS/CHARLS2020r/Demographic_Background.dta")


demo %>% sjPlot::view_df()



# sex, age, marriage, edu
d <- demo %>%
  select(
    ID,
    sex      = ba001,
    year     = ba003_1,
    marriage = ba011,
    edu      = ba010,
    place    = ba008
  ) %>%
  drop_na() %>% 
  mutate(age = 2020 - year) %>% 
  mutate(sex_group = labelled::to_factor(sex)) %>% 
  mutate(
    age_group = case_when(
      age >= 44 & age <= 54 ~ "44–54",
      age >= 55 & age <= 64 ~ "55–64",
      age >= 65 & age <= 74 ~ "65–74",
      age >= 75             ~ "75+",
      .default              =  NA
    )
  ) %>% 
  mutate(marriage_group = case_when(
    marriage %in% c(1, 2)    ~ "married",
    .default                 =  "Others"
  ) ) %>% 
  
  mutate(edu_group = case_when(
    edu == 1            ~ "文盲",
    edu %in% c(2, 3)    ~ "能识字",
    edu == 4            ~ "小学",
    edu == 5            ~ "初中",
    edu >= 6            ~ "高中+",
    .default            = NA
  ) ) %>% 
  mutate(
    place_group = case_when(
      place %in% c(1,2)  ~ "城镇",
      .default           = "农村"
    )
  )



d %>% 
  select(ends_with("_group")) %>% 
  gtsummary::tbl_summary(
    by = place_group
  ) %>% 
  gtsummary::add_p() %>% 
  gtsummary::add_overall() 









birth2011 <- demo %>% 
  select(ID, year = ba003_1, month = ba003_2, day = ba003_3)



# 健康
health <- haven::read_dta("./CHARLS2020r/Health_Status_and_Functioning.dta")

health %>% sjPlot::view_df()
