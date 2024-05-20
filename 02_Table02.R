library(tidyverse)
library(gtsummary)
library(lavaan)
   

mydataIMP <- read_rds("mydataImp1.rds") 

glimpse(mydataIMP)




##############################################
# Table 02

# health status and functioning data 2013 for physical activity data
# PA          da051_1_:da055_3_
# IADL        db016:db020
# depression  dc009:dc018
##############################################


d1 <- mydataIMP %>% 
  select(db016:db020, dc009:dc018, TPA, LPA, urban_nbs) %>%
  as_tibble() 

d1 %>% 
  count(urban_nbs)

d1 %>% 
  filter(if_any(everything(), is.na))


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

d1 %>% 
  filter(urban_nbs == 0) %>% 
  select(
    "Bothered by things"      = dc009,
    "Trouble keeping mind"    = dc010,
    "Felt depressed"          = dc011,
    "Everything as an effort" = dc012,
    "Felt fearful"            = dc014,
    "Restless sleep"          = dc015,
    "Felt lonely"             = dc017,
    "Could not get going"     = dc018,
    "Doing household chores"  = db016,
    "Preparing hot meals"     = db017,
    "Shopping for groceries"  = db018,
    "Managing money"          = db019,
    "Taking medications"      = db020
  ) %>% 
  psych::describe() %>% 
  round(2) %>% 
  as.data.frame() %>% 
  rownames_to_column("variable") %>% 
  select(variable, mean, sd)
  


d1 %>% 
  filter(urban_nbs == 0) %>% 
  select(
    dc009:dc012, dc014:dc015, dc017:dc018, 
    db016:db020, 
    TPA,
    LPA
  ) %>% 
  psych::describe() %>% 
  round(2) %>% 
  as.data.frame() %>% 
  rownames_to_column("variable") %>% 
  select(variable, mean, sd)

d1 %>% 
  filter(urban_nbs == 1) %>% 
  select(
    dc009:dc012, dc014:dc015, dc017:dc018, 
    db016:db020, 
    TPA,
    LPA
  ) %>%
  psych::describe() %>% 
  round(2) %>% 
  as.data.frame() %>% 
  rownames_to_column("variable") %>% 
  select(variable, mean, sd)


# 或者，使用rstatix
mydataIMP %>% 
  select(
    dc009:dc012, dc014:dc015, dc017:dc018, 
    db016:db020, 
    TPA,
    LPA,
    urban_nbs
  ) %>% 
  group_by(urban_nbs) %>% 
  rstatix::get_summary_stats(type = "mean_sd") %>% 
  mutate(mean_sd = str_c(mean, "(", sd, ")")) %>% 
  select(urban_nbs, variable, mean_sd) %>% 
  pivot_wider(
    names_from = urban_nbs,
    values_from = mean_sd,
    names_prefix = "urban"
  ) %>% 
  flextable::flextable() %>% 
  flextable::autofit()


#####################################################################
# Standardized factor loadings for the multigroup measurement model
# 注意
# urban (0=rural 1=urban)
# dc013, dc016 未使用


cfamodel <- "

  depression =~ dc009 + dc010 + dc011 + dc012 + dc014 + dc015 + dc017 + dc018 
  IADL       =~ db016 + db017 + db018 + db019 + db020
  db016      ~~ db017

"


fit_cfa <- cfa(cfamodel, 
               data        = mydataIMP,
               int.lv.free = TRUE,
               group       = "urban_nbs",
               estimator   = "ML", 
               mimic       = "Mplus")



fit_cfa %>% 
  standardizedsolution() %>% 
  filter(op == "=~") %>% 
  mutate(pvalue = gtools::stars.pval(pvalue)) %>% 
  select(group, lhs, rhs, est.std, se, z, pvalue) %>% 
  flextable::flextable() %>% 
  flextable::colformat_double(digits = 3) %>% 
  flextable::hline(i = 13) %>% 
  flextable::autofit()

##################################################################



