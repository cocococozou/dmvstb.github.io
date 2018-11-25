Drugpart
================
Coco
11/14/2018

``` r
dm_base_df = dm_base %>% 
  rename(drug_insulin = insulin,
         drug_oral_sulfo = sulfonylurea,
         drug_oral_biguanide = biguanide,
         drug_oral_glu = glu_inhib) %>% 
  mutate(drug_oral = case_when(drug_oral_biguanide == 0 & drug_oral_biguanide == 0 & drug_oral_glu == 0 ~0,
                               TRUE ~ 1),
         drug = case_when(drug_oral == 0 & drug_insulin ==0 ~ 0,
                          TRUE ~ 1))
```
