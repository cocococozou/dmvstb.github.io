Final\_virsion \_annie
================
Annie Yu (xy2404)
11/15/2018

``` r
load("dm.Rdata")

df_final <- dm_base %>% 
  janitor::clean_names() %>% 
  rename(dmtime = quezhensj) %>% 
  rename(birthyear  = birth_year) %>% 
  rename(birthmon = birth_mon) %>% 
  rename(dmdatayear = rucu_year) %>% 
  rename(dmdatamon = rucu_mon) %>% 
  rename(dmdataage = rucuage)
```
