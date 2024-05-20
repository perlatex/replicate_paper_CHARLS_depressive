library(tidyverse)
library(gtsummary)
library(lavaan)
   

# 先插值、再变形，后统计。后续使用的是mydataImp1数据集
mydataIMP <- read_rds("mydataImp1.rds") %>% 
  rename(facility = type_number)


glimpse(mydataIMP)




##############################################
# Table 01

mydataIMP %>% count(sex)
mydataIMP %>% count(martial)
mydataIMP %>% count(urban_nbs)
mydataIMP %>% count(facility) 


d <- mydataIMP %>%
  select(
    ID,
    sex,
    age,
    martial,
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







##############################################
# Table 02

# health status and functioning data 2013 for physical activity data
# PA          da051_1_:da055_3_
# physiFunc   db001:db009
# IADL        db016:db020
# depression  dc009:dc018
##############################################
glimpse(mydataIMP)

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
#####################################################################


cfamodel <- "

  depression =~ dc009 + dc010 + dc011 + dc012 + dc014 + dc015 + dc017 + dc018 
  IADL       =~ db016 + db017 + db018 + db019 + db020
  db016      ~~ db017

"


fit_cfa <- cfa(cfamodel, 
               data        = d1,
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




fit_mi <- measurementInvariance(
  model     = cfamodel, 
  data      = d1,
  group     = "urban_nbs",
  estimator = "ML"
)

comp <- compareFit(fit_mi$fit.configural, fit_mi$fit.loadings, fit_mi$fit.intercepts)


comp@fit %>% 
  as.data.frame() %>% 
  rownames_to_column("Models") %>% 
  mutate(Models = c("Configural invariant model", "Metric invariant model", "Scalar invariant model")) %>% 
  select(Models, chisq, df, pvalue, cfi, rmsea, rmsea.ci.lower, rmsea.ci.upper, rmsea.ci.level, srmr) %>% 
  flextable::flextable() %>% 
  flextable::colformat_double(digits = 3)
##################################################################







##################################################################
semmodel_LPA_unconstr <- "

  depression =~ dc009 + dc010 + dc011 + dc012 + dc014 + dc015 + dc017 + dc018 
  IADL       =~ db016 + db017 + db018 + db019 + db020
  db016      ~~ db017

  LPA        ~ facility
  IADL       ~ LPA + facility
  depression ~ IADL + LPA + facility

"

fit_sem_LPA_unconstr <- sem(semmodel_LPA_unconstr,
      data        = mydataIMP,
      group       = "urban_nbs",
      estimator   = "ML",
      mimic       = "Mplus"
)


semmodel_LPA_constr <- "

  depression =~ dc009 + dc010 + dc011 + dc012 + dc014 + dc015 + dc017 + dc018 
  IADL       =~ db016 + db017 + db018 + db019 + db020
  db016      ~~ db017

  LPA        ~ facility
  IADL       ~ LPA + facility
  depression ~ c(a, a) * IADL + LPA + c(b, b) * facility

"


fit_sem_LPA_constr <- sem(semmodel_LPA_constr,
      data          = mydataIMP,
      group         = "urban_nbs",
      estimator     = "ML",
      mimic         = "Mplus"
)

fit_sem_LPA_unconstr %>% 
  fitMeasures(c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.ci.level", "srmr"))


fit_sem_LPA_constr %>% 
  fitMeasures(c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.ci.level", "srmr"))


# fit_sem_LPA %>%
#   standardizedsolution() %>%
#   filter(op == "~") %>%
#   mutate(pvalue = gtools::stars.pval(pvalue)) %>%
#   select(group, lhs, rhs, est.std, se, z, pvalue) %>%
#   flextable::flextable() %>%
#   flextable::colformat_double(digits = 3) %>%
#   flextable::hline(i = 6) %>%
#   flextable::autofit()
# 
# 
# sem_mi_LPA <- measurementInvariance(
#   model         = semmodel_LPA, 
#   data          = mydataIMP,
#   group.partial = c("depression ~ type_number", 
#                     "depression ~ IADL"
#                     ),
#   group         = "urban_nbs",
#   estimator     = "ML",
#   se            = "bootstrap",
#   bootstrap     = 1000
# )


# sem_mi_LPA$fit.intercepts %>% 
#   standardizedsolution() %>% 
#   filter(op == "~") %>% 
#   mutate(pvalue = gtools::stars.pval(pvalue)) %>% 
#   select(group, lhs, rhs, est.std, se, z, pvalue) %>% 
#   flextable::flextable() %>% 
#   flextable::colformat_double(digits = 3) %>% 
#   flextable::hline(i = 6) %>% 
#   flextable::autofit()
# 
# 
# 
# comp_LPA <- compareFit(sem_mi_LPA$fit.configural, sem_mi_LPA$fit.loadings, sem_mi_LPA$fit.intercepts)
# 
# comp_LPA@fit %>% 
#   as.data.frame() %>% 
#   rownames_to_column("Models") %>% 
#   mutate(Models = c("Configural invariant model", "Metric invariant model", "Scalar invariant model")) %>% 
#   select(Models, chisq, df, pvalue, cfi, rmsea, rmsea.ci.lower, rmsea.ci.upper, rmsea.ci.level, srmr) %>% 
#   flextable::flextable() %>% 
#   flextable::colformat_double(digits = 3)

##################################################################





##################################################################
semmodel_TPA_unconstr <- "

  depression =~ dc009 + dc010 + dc011 + dc012 + dc014 + dc015 + dc017 + dc018 
  IADL       =~ db016 + db017 + db018 + db019 + db020
  db016      ~~ db017

  TPA        ~ facility
  IADL       ~ TPA + facility
  depression ~ IADL + TPA + facility

"

fit_sem_TPA_unconstr <- sem(semmodel_TPA_unconstr,
            data        = mydataIMP,
            group       = "urban_nbs",
            estimator   = "ML",
            mimic       = "Mplus")




semmodel_TPA_unconstr <- "

  depression =~ dc009 + dc010 + dc011 + dc012 + dc014 + dc015 + dc017 + dc018 
  IADL       =~ db016 + db017 + db018 + db019 + db020
  db016      ~~ db017

  TPA        ~ facility
  IADL       ~ c(a, a) * TPA + facility
  depression ~ c(b, b) * IADL + c(d, d) * TPA + c(e, e) * facility

"

fit_sem_TPA_constr <- sem(semmodel_TPA_unconstr,
            data          = mydataIMP,
            group         = "urban_nbs",
            estimator     = "ML",
            mimic         = "Mplus")



fit_sem_TPA_unconstr %>% 
  fitMeasures(c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.ci.level", "srmr"))


fit_sem_TPA_constr %>% 
  fitMeasures(c("chisq", "df", "pvalue", "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.ci.level", "srmr"))




# fit_sem_TPA %>%
#   standardizedsolution() %>%
#   filter(op == "~") %>%
#   mutate(pvalue = gtools::stars.pval(pvalue)) %>%
#   select(group, lhs, rhs, est.std, se, z, pvalue) %>%
#   flextable::flextable() %>%
#   flextable::colformat_double(digits = 3) %>%
#   flextable::hline(i = 6) %>%
#   flextable::autofit()
# 
# 
# sem_mi_TPA <- measurementInvariance(
#   model         = semmodel_TPA, 
#   data          = mydataIMP,
#   group.partial = c("depression ~ type_number", 
#                     "IADL       ~ TPA",
#                     "depression ~ TPA",
#                     "depression ~ IADL"
#                     ),
#   group         = "urban_nbs",
#   estimator     = "ML",
#   se            = "bootstrap",
#   bootstrap     = 1000
# )
# 
# 
# sem_mi_TPA$fit.intercepts %>% 
#   standardizedsolution() %>% 
#   filter(op == "~") %>% 
#   mutate(pvalue = gtools::stars.pval(pvalue)) %>% 
#   select(group, lhs, rhs, est.std, se, z, pvalue) %>% 
#   flextable::flextable() %>% 
#   flextable::colformat_double(digits = 3) %>% 
#   flextable::hline(i = 6) %>% 
#   flextable::autofit()
# 
# 
# 
# comp_TPA <- compareFit(sem_mi_TPA$fit.configural, sem_mi_TPA$fit.loadings, sem_mi_TPA$fit.intercepts)
# 
# comp_TPA@fit %>% 
#   as.data.frame() %>% 
#   rownames_to_column("Models") %>% 
#   mutate(Models = c("Configural invariant model", "Metric invariant model", "Scalar invariant model")) %>% 
#   select(Models, chisq, df, pvalue, cfi, rmsea, rmsea.ci.lower, rmsea.ci.upper, rmsea.ci.level, srmr) %>% 
#   flextable::flextable() %>% 
#   flextable::colformat_double(digits = 3)
##################################################################


