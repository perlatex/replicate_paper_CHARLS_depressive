library(tidyverse)
library(gtsummary)
library(lavaan)
library(semTools)  
   

mydataIMP <- read_rds("mydataImp1.rds") 

glimpse(mydataIMP)




#####################################################################
library(processR)
labels = list(X = "facility", M1 = "LPA", M2 = "IADL", Y = "depression")
statisticalDiagram(6, labels = labels)
#####################################################################




#####################################################################
# Table 05
# Standardized factor loadings for the multigroup measurement model
# 注意
# urban (0=rural 1=urban)
# dc013, dc016 未使用
#####################################################################




##################################################################
semmodel_LPA <- "
  # measurement models
  depression =~ dc009 + dc010 + dc011 + dc012 + dc014 + dc015 + dc017 + dc018 
  IADL       =~ db016 + db017 + db018 + db019 + db020
  db016      ~~ db017

  # regression models
  LPA        ~ c(a1_x, a1_y) * facility
  IADL       ~ c(a2_x, a2_y) * facility + c(d21_x, d21_y) * LPA
  depression ~ c(b1_x, b1_y) * LPA + c(b2_x, b2_y) * IADL + c(cprime_x, cprime_y) * facility

  # effect values x group
  indirect_eff_PA_FHS_DS_x     := d21_x*b2_x
  indirect_eff_ARC_PA_FHS_DS_x := a1_x*d21_x*b2_x
  total_eff_PA_DS_x            := d21_x*b2_x + b1_x
  total_eff_ARC_DS_x           := a1_x*d21_x*b2_x + a1_x*b1_x + a2_x*b2_x + cprime_x

  # effect values y group
  indirect_eff_PA_FHS_DS_y     := d21_y*b2_y
  indirect_eff_ARC_PA_FHS_DS_y := a1_y*d21_y*b2_y
  total_eff_PA_DS_y            := d21_y*b2_y + b1_y
  total_eff_ARC_DS_y           := a1_y*d21_y*b2_y + a1_y*b1_y + a2_y*b2_y + cprime_y
"

fit_sem_LPA <- sem(semmodel_LPA,
  data        = mydataIMP,
  group       = "urban_nbs",
  estimator   = "ML",
  se          = "bootstrap",
  bootstrap   = 1000
)


fit_sem_LPA %>%
  standardizedsolution() %>%
  filter(op == "~") %>%
  mutate(pvalue = gtools::stars.pval(pvalue)) %>%
  select(group, lhs, rhs, label, est.std, ci.lower, ci.upper) %>%
  flextable::flextable() %>%
  flextable::colformat_double(digits = 3) %>%
  flextable::hline(i = 6) %>%
  flextable::autofit()
##################################################################




##################################################################
semmodel_TPA <- "
  # measurement models
  depression =~ dc009 + dc010 + dc011 + dc012 + dc014 + dc015 + dc017 + dc018 
  IADL       =~ db016 + db017 + db018 + db019 + db020
  db016      ~~ db017

  # regression models
  TPA        ~ c(a1_x, a1_y) * facility
  IADL       ~ c(a2_x, a2_y) * facility + c(d21_x, d21_y) * TPA
  depression ~ c(b1_x, b1_y) * TPA + c(b2_x, b2_y) * IADL + c(cprime_x, cprime_y) * facility

  # effect values x group
  indirect_eff_PA_FHS_DS_x     := d21_x*b2_x
  indirect_eff_ARC_PA_FHS_DS_x := a1_x*d21_x*b2_x
  total_eff_PA_DS_x            := d21_x*b2_x + b1_x
  total_eff_ARC_DS_x           := a1_x*d21_x*b2_x + a1_x*b1_x + a2_x*b2_x + cprime_x

  # effect values y group
  indirect_eff_PA_FHS_DS_y     := d21_y*b2_y
  indirect_eff_ARC_PA_FHS_DS_y := a1_y*d21_y*b2_y
  total_eff_PA_DS_y            := d21_y*b2_y + b1_y
  total_eff_ARC_DS_y           := a1_y*d21_y*b2_y + a1_y*b1_y + a2_y*b2_y + cprime_y

"


fit_sem_TPA <- sem(semmodel_TPA,
  data        = mydataIMP,
  group       = "urban_nbs",
  estimator   = "ML",
  se          = "bootstrap",
  bootstrap   = 1000
)


fit_sem_TPA %>%
  standardizedsolution() %>%
  filter(op == "~") %>%
  mutate(pvalue = gtools::stars.pval(pvalue)) %>%
  select(group, lhs, rhs, label, est.std, ci.lower, ci.upper) %>%
  flextable::flextable() %>%
  flextable::colformat_double(digits = 3) %>%
  flextable::hline(i = 6) %>%
  flextable::autofit()
##################################################################







##################################################################
# Direct effect/Indirect effect/Total effect

res_rural_LPA <- fit_sem_LPA %>%
  standardizedsolution() %>%
  filter(str_detect(label, "_x")) %>%
  mutate(across(where(is.numeric), function(x) format(round(x, 3), nsmall = 3)) ) %>% 
  mutate(label = str_remove(label, "_x")) %>% 
  mutate(Rural_est = str_c(est.std, " (", ci.lower, ", ", ci.upper, ")") ) %>% 
  select(label, Rural_est, Rural_pvalue = pvalue) 


res_urban_LPA <- fit_sem_LPA %>%
  standardizedsolution() %>%
  filter(str_detect(label, "_y")) %>%
  mutate(across(where(is.numeric), function(x) format(round(x, 3), nsmall = 3)) ) %>% 
  mutate(label = str_remove(label, "_y")) %>% 
  mutate(Urban_est = str_c(est.std, " (", ci.lower, ", ", ci.upper, ")") ) %>% 
  select(label, Urban_est, Urban_pvalue = pvalue)




res_rural_TPA <- fit_sem_TPA %>%
  standardizedsolution() %>%
  filter(str_detect(label, "_x")) %>%
  mutate(across(where(is.numeric), function(x) format(round(x, 3), nsmall = 3)) ) %>% 
  mutate(label = str_remove(label, "_x")) %>% 
  mutate(Rural_est = str_c(est.std, " (", ci.lower, ", ", ci.upper, ")") ) %>% 
  select(label, Rural_est, Rural_pvalue = pvalue) 


res_urban_TPA <- fit_sem_TPA %>%
  standardizedsolution() %>%
  filter(str_detect(label, "_y")) %>%
  mutate(across(where(is.numeric), function(x) format(round(x, 3), nsmall = 3)) ) %>% 
  mutate(label = str_remove(label, "_y")) %>% 
  mutate(Urban_est = str_c(est.std, " (", ci.lower, ", ", ci.upper, ")") ) %>% 
  select(label, Urban_est, Urban_pvalue = pvalue)



tbl_orders <- c("b2", "d21", "b1", "a1", "a2", "cprime", 
                "indirect_eff_PA_FHS_DS",   
                "indirect_eff_ARC_PA_FHS_DS", 
                "total_eff_PA_DS",           
                "total_eff_ARC_DS")        


res_rural_LPA %>% 
  left_join(res_urban_LPA, by = join_by(label)) %>%
  arrange(factor(label, levels = tbl_orders)) %>% 
  flextable::flextable() %>%
  flextable::hline(i = c(6, 8)) %>% 
  flextable::autofit()


res_rural_TPA %>% 
  left_join(res_urban_TPA, by = join_by(label)) %>%
  arrange(factor(label, levels = tbl_orders)) %>% 
  flextable::flextable() %>%
  flextable::hline(i = c(6, 8)) %>% 
  flextable::autofit()

##################################################################






##################################################################
# Model fit indices

indices <- c("chisq", "df", "cfi", "rmsea", 
             "rmsea.ci.lower", "rmsea.ci.upper", 
             "rmsea.ci.level", "srmr")


lst(fit_sem_LPA, fit_sem_TPA) %>% 
  map(
    ~ .x %>% 
      fitMeasures(indices) %>% 
      as_tibble_row()
  ) %>% 
  list_rbind(names_to = "models")  %>%
  flextable::flextable() %>%
  flextable::colformat_double(digits = 3) %>%
  flextable::autofit()
##################################################################





##################################################################
# R square (explained variance (R2) in depressive symptoms)

lst(fit_sem_LPA, fit_sem_TPA) %>% 
  map(
    ~ .x %>% 
      lavInspect(what = "rsquare") %>% 
      set_names("rural", "urban") %>% 
      map(as_tibble_row) %>% 
      list_rbind(names_to = "group") %>%  
      mutate(across(-group, as.double)) 
  ) %>% 
  list_rbind(names_to = "models") %>% 
  select(-matches("db|dc")) %>%
  flextable::flextable() %>%
  flextable::colformat_double(digits = 3) %>%
  flextable::color(j = "depression", color = "red") %>%
  flextable::autofit()
##################################################################
