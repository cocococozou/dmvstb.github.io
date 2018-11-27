complications\_DZ
================
DZ
November 14, 2018

``` r
load('./dm.Rdata')
df <- dm_base %>% 
  rename(retina = reti, skin = derm, vessel = vesl, nerve = neur,kidney = neph, depression = depress ) %>%
  mutate(retina = as.numeric(retina),
         skin = as.numeric(skin),
         vessel = as.numeric(vessel),
         nerve = as.numeric(nerve),
         kidney = as.numeric(kidney)) %>% 
  mutate(complications = retina + skin + vessel + nerve + kidney + depression) %>%
  mutate(complications = as.factor(complications))
levels(df$complications) <- list(none=0,one=1,more_than_two=c(2:6))
```
