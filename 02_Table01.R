library(tidyverse)
library(gtsummary)



mydataIMP <- read_rds("mydataImp1.rds")
glimpse(mydataIMP)




##############################################
# Table 01

mydataIMP %>% count(sex)
mydataIMP %>% count(martial)
mydataIMP %>% count(urban_nbs)
mydataIMP %>% count(edu)


d <- mydataIMP %>%
  select(
    ID,
    sex,
    age,
    martial,
    edu,
    urban_nbs
  ) %>%
  mutate(sex_group = sex) %>% 
  mutate(
    age_group = case_when(
      age >= 44 & age <= 54 ~ "44–54",
      age >= 55 & age <= 64 ~ "55–64",
      age >= 65 & age <= 74 ~ "65–74",
      age >= 75             ~ "75+",
      .default              =  NA
    )
  ) %>% 
  mutate(
    marriage_group = case_when(
      martial %in% c(1, 2)    ~ "married",
      .default                = "Others"
    )
  ) %>% 
  mutate(edu_group = factor(
    case_when(
      edu == 1            ~ "文盲",
      edu == 2            ~ "能识字",
      edu %in% c(3, 4)    ~ "小学",
      edu == 5            ~ "初中",
      edu >= 6            ~ "高中+",
      .default            = NA
    ),
    levels = c("文盲", "能识字", "小学", "初中", "高中+")
  ) ) %>% 
  mutate(
    place_group = case_when(
      urban_nbs == 0  ~ "农村",
      urban_nbs == 1  ~ "城镇"
    )
  ) 


d %>% 
  select(ends_with("_group")) %>% 
  gtsummary::tbl_summary(
    by = place_group
  ) %>% 
  gtsummary::add_p(
    test = everything() ~ "chisq.test",
    pvalue_fun = function(x) style_pvalue(x, digits = 2)
  ) %>% 
  gtsummary::add_overall() %>% 
  gtsummary::modify_table_body(
    ~.x %>% relocate(stat_0, .after = stat_2)
  )
##############################################




