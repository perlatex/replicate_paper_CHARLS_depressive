library(tidyverse)
library(lavaan)

mydata <- read_rds("mydata.rds")

sjPlot::view_df(mydata)


mydata %>% count(sex)
mydata %>% count(martial)
mydata %>% count(urban_nbs)



data <- mydata %>% 
  haven::zap_labels() %>% 
  mice::mice(method = "pmm") %>% 
  mice::complete()



d <- mydata %>%
  select(
    ID,
    sex,
    age,
    martial,
    urban_nbs
  ) %>%
  mutate(across(everything(), labelled::remove_labels)) %>% 
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
  gtsummary::add_p() %>% 
  gtsummary::add_overall() 




# Table 02

##############################################
# health status and functioning data 2013 for physical activity data
# PA          da051_1_:da055_3_
# physiFunc   db001:db0019
# IADL        db016:db020
# depression  dc009:dc018
##############################################


d1 <- mydata %>% 
  select(dc009:dc018, urban_nbs) %>%
  mutate(across(everything(), labelled::remove_labels)) 

# DC009 I was bothered by things that don’t usually bother me. 我因一些小事而烦恼。
# DC010 I had trouble keeping my mind on what I was doing. 我在做事时很难集中精力。
# DC011 I felt depressed. 我感到情绪低落。
# DC012 I felt everything I did was an effort. 我觉得做任何事都很费劲。
#DC013 I felt hopeful about the future. 我对未来充满希望。
# DC014 I felt fearful. 我感到害怕。
# DC015 My sleep was restless. 我的睡眠不好。
#DC016 I was happy. 我很愉快
# DC017 I felt lonely. 我感到孤独。
# DC018 I could not get ”going.” 我觉得我无法继续我的生活。


cfamodel <- "
  depression =~ dc009 + dc010 + dc011 + dc012 + dc014 + dc015 + dc017 + dc018 
"

fit_cfa1 <- cfa(cfamodel, 
                data      = d1 %>% filter(urban_nbs == 0),
                estimator = "MLR", 
                mimic     = "Mplus")

fit_cfa1 %>% 
  parameterestimates(standardized = TRUE) %>% 
  filter(op == "=~") %>% 
  mutate(pvalue = gtools::stars.pval(pvalue)) %>% 
  select(lhs, rhs, est, se, z, pvalue, std.all) %>% 
  flextable::flextable() %>% 
  flextable::colformat_double(digits = 3) %>% 
  flextable::autofit()


fit_cfa2 <- cfa(cfamodel, 
                data      = d1 %>% filter(urban_nbs == 1),
                estimator = "MLR", 
                mimic     = "Mplus")

fit_cfa2 %>% 
  parameterestimates(standardized = TRUE) %>% 
  filter(op == "=~") %>% 
  mutate(pvalue = gtools::stars.pval(pvalue)) %>% 
  select(lhs, rhs, est, se, z, pvalue, std.all) %>% 
  flextable::flextable() %>% 
  flextable::colformat_double(digits = 3) %>% 
  flextable::autofit()
