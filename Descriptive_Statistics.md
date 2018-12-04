Descriptive Statistics
================
Rui Huang
December 3, 2018

``` r
load('./data/df_combine.Rdata')
df = df_combine %>% 
  filter(district != "") %>% 
  mutate(tb = fct_recode(tb, '1'= 'Yes', '0'='No')) %>% 
  mutate(tb=as.character(tb),
         tb=as.numeric(tb)) 

# person_years
summary(df$days)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       0     638    1485    1403    1970    4001

``` r
mean_follow_up_year = mean(df$days)/365
sum_follow_up_year = sum(df$days)/365

# tb summary
summary(df$tb)
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## 0.000000 0.000000 0.000000 0.004607 0.000000 1.000000

``` r
sum_tb = sum(df$tb)

# all participants
nrow(df)
```

    ## [1] 170381

``` r
# overall incidence
overall_incidence = sum_tb/sum_follow_up_year

# male incidence
df_male = df %>% filter(gender == 1)
male_incidence = sum(df_male$tb)/sum(df_male$days)/365

# female incidence
df_female = df %>% filter(gender == 2)
female_incidence = sum(df_female$tb)/sum(df_female$days)/365
```

With an average following-up period of 3.8440697 year (range: 0 to
10.9616438 years, 785 TB cases were recorded among 170381 T2DM patients
from 6.549564310^{5} person-years follow-up. The overall incident rate
of TB was 119.8553004 per 100 000 person-years with 224.2033743 per 100
000 person-years for men, and 51.3442211 per 100 000 person-years for
women.
