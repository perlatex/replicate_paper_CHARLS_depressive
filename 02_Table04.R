library(tidyverse)
library(lavaan)
library(semTools) 


mydataIMP <- read_rds("mydataImp1.rds") 

glimpse(mydataIMP)



########################################################
## 本文不是采用“求均值后再算相关系数”的方法
d <- mydataIMP %>% 
  rowwise() %>% 
  transmute(
    depression = mean(c_across(dc009:dc018)),
    IADL       = mean(c_across(db016:db020)),
    LPA        = LPA,
    TPA        = TPA,
    ARC        = facility,
    urban_nbs  = urban_nbs
  ) %>% 
  ungroup() 
 


d %>%   
  select(depression, LPA, IADL, ARC, urban_nbs) %>% 
  group_by(urban_nbs) %>% 
  group_map(
    ~.x %>% 
     corrr::correlate() %>% 
     corrr::shave()
  )


d %>%   
  select(depression, TPA, IADL, ARC, urban_nbs) %>% 
  group_by(urban_nbs) %>% 
  group_map(
    ~.x %>% 
      corrr::correlate() %>% 
      corrr::shave()
  )
########################################################





########################################################
## 文章使用lavInspect()函数来检查潜变量和显变量之间的相关系数

model <- "

  depression =~ dc009 + dc010 + dc011 + dc012 + dc014 + dc015 + dc017 + dc018 
  IADL       =~ db016 + db017 + db018 + db019 + db020


  LPA        ~ facility
  IADL       ~ LPA + facility
  depression ~ IADL + LPA + facility

"


fit <- sem(model, 
           data      = mydataIMP,
           group     = "urban_nbs",
           estimator = "ML",
           mimic     = "Mplus")
########################################################





########################################################
## 呈现方法1

constructs <- c("depression", "LPA", "IADL", "facility")

ms <- lavInspect(fit, what = "cor.all")
ms %>%
  map(function(m) {
    m <- m[constructs, constructs]
    m[upper.tri(m)] <- NA
    m %>%
      as.data.frame() %>%
      rownames_to_column(var = "name") %>%
      flextable::flextable() %>%
      flextable::colformat_double(digits = 3) %>%
      flextable::autofit()
  })
########################################################






########################################################
## 呈现方法2，不是很完美

fit %>%
  lavInspect(what = "cor.all") %>%
  map(
    ~ .x %>%
      corrr::as_cordf() %>%
      corrr::focus(-matches("db|dc"), mirror = T) %>%
      corrr::shave() %>%
      flextable::flextable() %>%
      flextable::colformat_double(digits = 3) %>%
      flextable::autofit()
  )
########################################################






