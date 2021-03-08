Acral\_Melanoma
================
Chen Lu
2/16/2021

``` r
setwd('/Users/chenyuelu/Desktop/things/MBI/research/SemenovLab/Melanoma_Pearl/')
read_excel_allsheets <- function(filename, tibble = TRUE) {
    # I prefer straight data.frames
    # but if you like tidyverse tibbles (the default with read_excel)
    # then just pass tibble = TRUE
    sheets <- readxl::excel_sheets(filename)
    x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
    if(!tibble) x <- lapply(x, as.data.frame)
    names(x) <- sheets
    x
}
df_all <- read_excel_allsheets('Melanoma AGO 2021_20_02_notes.xlsx')
```

    ## Warning in read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet, :
    ## Expecting numeric in A15 / R15C1: got a date

    ## New names:
    ## * `` -> ...4
    ## * `` -> ...5
    ## * `` -> ...6
    ## * `` -> ...7
    ## * `` -> ...8
    ## * ...

``` r
df <- df_all$`Use this Sheet!`
head(df)
```

    ## # A tibble: 6 x 19
    ##       ID Gender Insurance `Self-pay-0, Me… Zip   `Race/Ethnicity`
    ##    <dbl> <chr>  <chr>                <dbl> <chr> <chr>           
    ## 1 1.03e8 Female ANTHEM B…                2 04856 White           
    ## 2 1.06e8 Female INTERNAT…                3 @     Black or Africa…
    ## 3 1.11e8 Female CIGNA, U…                2 03053 White           
    ## 4 1.07e8 Female TUFTS MC…                2 02130 White           
    ## 5 1.03e8 Female CIGNA, U…                2 32081 White           
    ## 6 1.09e8 Male   MEDICARE…                2 02543 Not Recorded    
    ## # … with 13 more variables: `Marital Status` <chr>, DOB <dttm>, `vital status
    ## #   (1 = alive, 0 = dead)` <chr>, `Age of last f/u or death` <dttm>, `Age at
    ## #   diagnosis` <chr>, Location <chr>, Laterality <chr>, `Date of
    ## #   diagnosis` <chr>, `Final Stage` <chr>, immunotherapy <chr>,
    ## #   `chemotherapy?` <chr>, Column1 <chr>, Column2 <lgl>

``` r
df_clean <- df %>%
    mutate(race_ethnicity = case_when(`Race/Ethnicity` == 'White' ~ 'Non-Hispanic White',
           `Race/Ethnicity` == 'Black or African American' ~ 'Non-Hispanic Black',
            `Race/Ethnicity` == 'Not Recorded' ~ 'Unknown',
            `Race/Ethnicity` == 'Hispanic-Other' ~ 'Hispanic',
            `Race/Ethnicity` == 'Asian' ~ 'Asian',
            `Race/Ethnicity` == 'American Indian' ~ 'Other',
            `Race/Ethnicity` == 'Native Hawaiian or Pacific Islander' ~ 'Other',
            `Race/Ethnicity` == 'Hispanic-Black' ~ 'Hispanic')) %>%
    mutate(white = case_when(`Race/Ethnicity` == 'White' ~ 'Non-Hispanic White',
                             `Race/Ethnicity` == 'Not Recorded' ~ 'Unknown',
                             TRUE ~ 'POC')) %>%
    mutate(insurance_type = case_when(`Self-pay-0, Medicaid-1, Other-2, International-3` == 0 ~ 'Self-pay',
                                      `Self-pay-0, Medicaid-1, Other-2, International-3` == 1 ~ 'Medicaid',
                                      `Self-pay-0, Medicaid-1, Other-2, International-3` == 2 ~ 'Private_or_public',
                                      `Self-pay-0, Medicaid-1, Other-2, International-3` == 3 ~ 'International')) %>%
    mutate(dx_date = as.Date(as.numeric(`Date of diagnosis`), origin = '1899-12-30')) %>% # some formatting necessary for R to read dates
    select(ID, Gender, race_ethnicity, white, `Marital Status`, insurance_type, DOB, dx_date, `Final Stage`, Location, Laterality, `Age  of last f/u or death`, `vital status (1 = alive, 0 = dead)`, immunotherapy, `chemotherapy?`)
```

    ## Warning: Problem with `mutate()` input `dx_date`.
    ## ℹ NAs introduced by coercion
    ## ℹ Input `dx_date` is `as.Date(as.numeric(`Date of diagnosis`), origin = "1899-12-30")`.

    ## Warning in as.Date(as.numeric(`Date of diagnosis`), origin = "1899-12-30"): NAs
    ## introduced by coercion

``` r
colnames(df_clean) <- c('ID', 'Gender', 'Race_ethnicity','White_or_POC', 'Marital_Status', 'insurance_type', 'DOB', 'dx_date', 'Final_Stage', 'Location', 'Laterality', 'follow_up_date', 'Alive', 'immuno','chemo')

# define reference group for each variable
df_clean$Race_ethnicity <- factor(df_clean$Race_ethnicity, levels=c("Non-Hispanic White","Non-Hispanic Black","Hispanic","Asian","Other","Unknown"))
df_clean$Race_ethnicity = relevel(df_clean$Race_ethnicity, ref = "Non-Hispanic White")

df_clean$White_or_POC <- factor(df_clean$White_or_POC, levels=c("Non-Hispanic White","POC","Unknown"))
df_clean$White_or_POC = relevel(df_clean$White_or_POC, ref = "Non-Hispanic White")

df_clean$Gender <- factor(df_clean$Gender, levels=c("Male", "Female"))
df_clean$Gender = relevel(df_clean$Gender, ref = "Male")

df_clean$Final_Stage <- factor(df_clean$Final_Stage, levels=c("0","1",'2','3','4','na'))
df_clean$Final_Stage = relevel(df_clean$Final_Stage, ref = "0")

df_clean$insurance_type <- factor(df_clean$insurance_type, levels=c("Private_or_public","Self-pay",'Medicaid','International'))
df_clean$insurance_type = relevel(df_clean$insurance_type, ref = "Private_or_public")

df_clean$immuno <- factor(df_clean$immuno, levels=c("Y","N"))
df_clean$immuno = relevel(df_clean$immuno, ref = "N")

df_clean$chemo <- factor(df_clean$chemo, levels=c("Y","N"))
df_clean$chemo = relevel(df_clean$chemo, ref = "N")
```

``` r
race_counts <- df_clean %>%
    group_by(Race_ethnicity) %>%
    summarize(Total_Counts = n())
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
race_counts
```

    ## # A tibble: 6 x 2
    ##   Race_ethnicity     Total_Counts
    ##   <fct>                     <int>
    ## 1 Non-Hispanic White          341
    ## 2 Non-Hispanic Black           15
    ## 3 Hispanic                     17
    ## 4 Asian                        10
    ## 5 Other                         3
    ## 6 Unknown                      21

``` r
stage_counts <- df_clean %>%
    group_by(Final_Stage) %>%
    summarize(Total_Counts = n())
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
stage_counts
```

    ## # A tibble: 6 x 2
    ##   Final_Stage Total_Counts
    ##   <fct>              <int>
    ## 1 0                     82
    ## 2 1                    104
    ## 3 2                     79
    ## 4 3                     94
    ## 5 4                     18
    ## 6 na                    30

``` r
df_gender <- df_clean %>%
    group_by(Race_ethnicity,Gender) %>% 
    summarise(counts=n()) %>% 
    group_by(Race_ethnicity) %>% 
    mutate(Total=sum(counts)) %>%
    filter(Gender=='Male') %>% 
    select(Race_ethnicity, counts, Total) %>%
    rename('Males'=counts) %>%
    mutate(percentage = round(100*Males/Total,2))
```

    ## `summarise()` regrouping output by 'Race_ethnicity' (override with `.groups` argument)

``` r
# Let's see the breakdown 
df_gender
```

    ## # A tibble: 5 x 4
    ## # Groups:   Race_ethnicity [5]
    ##   Race_ethnicity     Males Total percentage
    ##   <fct>              <int> <int>      <dbl>
    ## 1 Non-Hispanic White   136   341       39.9
    ## 2 Non-Hispanic Black     6    15       40  
    ## 3 Hispanic               4    17       23.5
    ## 4 Asian                  3    10       30  
    ## 5 Unknown                9    21       42.9

``` r
# overall percentages
print(paste('total number of males:',sum(df_clean$Gender == 'Male')))
```

    ## [1] "total number of males: 158"

``` r
print(paste('total number of people with gender labels:',length(df_clean$Gender)))
```

    ## [1] "total number of people with gender labels: 407"

``` r
prop.test(sum(df_clean$Gender == 'Male'), length(df_clean$Gender), p = 0.5)
```

    ## 
    ##  1-sample proportions test with continuity correction
    ## 
    ## data:  sum(df_clean$Gender == "Male") out of length(df_clean$Gender), null probability 0.5
    ## X-squared = 19.902, df = 1, p-value = 8.153e-06
    ## alternative hypothesis: true p is not equal to 0.5
    ## 95 percent confidence interval:
    ##  0.3409259 0.4376314
    ## sample estimates:
    ##         p 
    ## 0.3882064

``` r
# separate race groups 
prop.test(df_gender$Males, df_gender$Total)
```

    ## Warning in prop.test(df_gender$Males, df_gender$Total): Chi-squared
    ## approximation may be incorrect

    ## 
    ##  5-sample test for equality of proportions without continuity
    ##  correction
    ## 
    ## data:  df_gender$Males out of df_gender$Total
    ## X-squared = 2.2958, df = 4, p-value = 0.6815
    ## alternative hypothesis: two.sided
    ## sample estimates:
    ##    prop 1    prop 2    prop 3    prop 4    prop 5 
    ## 0.3988270 0.4000000 0.2352941 0.3000000 0.4285714

``` r
# White vs POC
POC_gender <- df_clean %>%
    group_by(White_or_POC,Gender) %>% 
    summarise(counts=n()) %>% 
    group_by(White_or_POC) %>% 
    mutate(Total=sum(counts)) %>%
    filter(Gender=='Male') %>%
    select(White_or_POC, counts, Total) %>%
    rename('Males'=counts)%>%
    mutate(percentage = round(100*Males/Total,2))
```

    ## `summarise()` regrouping output by 'White_or_POC' (override with `.groups` argument)

``` r
POC_gender 
```

    ## # A tibble: 3 x 4
    ## # Groups:   White_or_POC [3]
    ##   White_or_POC       Males Total percentage
    ##   <fct>              <int> <int>      <dbl>
    ## 1 Non-Hispanic White   136   341       39.9
    ## 2 POC                   13    45       28.9
    ## 3 Unknown                9    21       42.9

``` r
# testing if there is a difference between white and POC in gender breakdown
prop.test(POC_gender$Males, POC_gender$Total)
```

    ## 
    ##  3-sample test for equality of proportions without continuity
    ##  correction
    ## 
    ## data:  POC_gender$Males out of POC_gender$Total
    ## X-squared = 2.175, df = 2, p-value = 0.3371
    ## alternative hypothesis: two.sided
    ## sample estimates:
    ##    prop 1    prop 2    prop 3 
    ## 0.3988270 0.2888889 0.4285714

**Gender**

**1. Looking at overall percentage of males in the dataset, we see less
than 50% were males. p-value is significant. Confidence interval does
not include 0.5 (the assumption that there is equal number of males and
females). **

**2. Comparing white and individual race groups. Here the null
hypothesis is that the proportion of males is the same in each race
group: p1 = p2 = p3 = p4 = p5. The p-value here is 0.6815, which is not
significant, so we cannot reject the null hypothesis. Simply put, the
gender proportions across the race groups are the same. **

**3. Comparing white and POC (combining individual race groups) and
Unknown. The p-value is 0.3371, still not significant.**

``` r
df_location <- df_clean %>%
    group_by(Race_ethnicity,Location) %>% 
    summarise(counts=n()) %>% 
    group_by(Race_ethnicity) %>% 
    mutate(Total=sum(counts)) %>%
    filter(Location=='foot') %>% 
    select(Race_ethnicity, counts, Total) %>%
    rename('Foot'=counts) %>%
    mutate(percentage = round(100*Foot/Total,2))
```

    ## `summarise()` regrouping output by 'Race_ethnicity' (override with `.groups` argument)

``` r
df_location
```

    ## # A tibble: 6 x 4
    ## # Groups:   Race_ethnicity [6]
    ##   Race_ethnicity      Foot Total percentage
    ##   <fct>              <int> <int>      <dbl>
    ## 1 Non-Hispanic White   284   341       83.3
    ## 2 Non-Hispanic Black    10    15       66.7
    ## 3 Hispanic              17    17      100  
    ## 4 Asian                  9    10       90  
    ## 5 Other                  3     3      100  
    ## 6 Unknown               14    21       66.7

``` r
print(paste('total number of foot acral melanoma:',sum(df_clean$Location == 'foot')))
```

    ## [1] "total number of foot acral melanoma: 337"

``` r
print(paste('total number of people with location labels:',sum(df_clean$Location!='na')))
```

    ## [1] "total number of people with location labels: 404"

``` r
prop.test(sum(df_clean$Location == 'foot'), sum(df_clean$Location!='na'), p = 0.5)
```

    ## 
    ##  1-sample proportions test with continuity correction
    ## 
    ## data:  sum(df_clean$Location == "foot") out of sum(df_clean$Location != "na"), null probability 0.5
    ## X-squared = 179.11, df = 1, p-value < 2.2e-16
    ## alternative hypothesis: true p is not equal to 0.5
    ## 95 percent confidence interval:
    ##  0.7934447 0.8683642
    ## sample estimates:
    ##         p 
    ## 0.8341584

``` r
# separate race groups 
prop.test(df_location$Foot, df_location$Total)
```

    ## Warning in prop.test(df_location$Foot, df_location$Total): Chi-squared
    ## approximation may be incorrect

    ## 
    ##  6-sample test for equality of proportions without continuity
    ##  correction
    ## 
    ## data:  df_location$Foot out of df_location$Total
    ## X-squared = 11.155, df = 5, p-value = 0.0484
    ## alternative hypothesis: two.sided
    ## sample estimates:
    ##    prop 1    prop 2    prop 3    prop 4    prop 5    prop 6 
    ## 0.8328446 0.6666667 1.0000000 0.9000000 1.0000000 0.6666667

``` r
# White vs POC
POC_gender <- df_clean %>%
    group_by(White_or_POC,Location) %>% 
    summarise(counts=n()) %>% 
    group_by(White_or_POC) %>% 
    mutate(Total=sum(counts)) %>%
    filter(Location=='foot') %>%
    select(White_or_POC, counts, Total) %>%
    rename('Foot'=counts) %>%
    mutate(percentage = round(100*Foot/Total,2))
```

    ## `summarise()` regrouping output by 'White_or_POC' (override with `.groups` argument)

``` r
POC_gender 
```

    ## # A tibble: 3 x 4
    ## # Groups:   White_or_POC [3]
    ##   White_or_POC        Foot Total percentage
    ##   <fct>              <int> <int>      <dbl>
    ## 1 Non-Hispanic White   284   341       83.3
    ## 2 POC                   39    45       86.7
    ## 3 Unknown               14    21       66.7

``` r
# testing if there is a difference between white and POC in gender breakdown
prop.test(POC_gender$Foot, POC_gender$Total)
```

    ## Warning in prop.test(POC_gender$Foot, POC_gender$Total): Chi-squared
    ## approximation may be incorrect

    ## 
    ##  3-sample test for equality of proportions without continuity
    ##  correction
    ## 
    ## data:  POC_gender$Foot out of POC_gender$Total
    ## X-squared = 4.3668, df = 2, p-value = 0.1127
    ## alternative hypothesis: two.sided
    ## sample estimates:
    ##    prop 1    prop 2    prop 3 
    ## 0.8328446 0.8666667 0.6666667

**Location**

**1. Overall there are significantly more foot acral melanoma than hand.
See results above. **

**2. Comparing white and individual race groups. Here the null
hypothesis is that the proportion of foot melanoma is the same in each
race group: p1 = p2 = p3 = p4 = p5. The p-value here is 0.0484, which is
just below the 0.05 p-value threshold, so we can maybe reject the null
hypothesis. Simply put, at least one of the races had a different
percentage of melanoma location from the other races. **

**3. Comparing white and POC (combining individual race groups) and
Unknown. The p-value is 0.1127, not significant.**

``` r
df_laterality <- df_clean %>%
    group_by(Race_ethnicity,Laterality) %>% 
    summarise(counts=n()) %>% 
    group_by(Race_ethnicity) %>% 
    mutate(Total=sum(counts)) %>%
    filter(Laterality=='L') %>% 
    select(Race_ethnicity, counts, Total) %>%
    rename('L'=counts) %>%
    mutate(percentage = round(100*L/Total,2))
```

    ## `summarise()` regrouping output by 'Race_ethnicity' (override with `.groups` argument)

``` r
df_laterality
```

    ## # A tibble: 6 x 4
    ## # Groups:   Race_ethnicity [6]
    ##   Race_ethnicity         L Total percentage
    ##   <fct>              <int> <int>      <dbl>
    ## 1 Non-Hispanic White   176   341       51.6
    ## 2 Non-Hispanic Black     9    15       60  
    ## 3 Hispanic               9    17       52.9
    ## 4 Asian                  6    10       60  
    ## 5 Other                  2     3       66.7
    ## 6 Unknown               11    21       52.4

``` r
print(paste('total number of left acral melanoma:',sum(df_clean$Laterality == 'L')))
```

    ## [1] "total number of left acral melanoma: 213"

``` r
print(paste('total number of people with laterality labels:',sum(df_clean$Laterality!='na')))
```

    ## [1] "total number of people with laterality labels: 402"

``` r
prop.test(sum(df_clean$Laterality == 'L'), sum(df_clean$Laterality!='na'), p = 0.5)
```

    ## 
    ##  1-sample proportions test with continuity correction
    ## 
    ## data:  sum(df_clean$Laterality == "L") out of sum(df_clean$Laterality != "na"), null probability 0.5
    ## X-squared = 1.3159, df = 1, p-value = 0.2513
    ## alternative hypothesis: true p is not equal to 0.5
    ## 95 percent confidence interval:
    ##  0.4797699 0.5793521
    ## sample estimates:
    ##         p 
    ## 0.5298507

``` r
# separate race groups 
prop.test(df_laterality$L, df_laterality$Total)
```

    ## Warning in prop.test(df_laterality$L, df_laterality$Total): Chi-squared
    ## approximation may be incorrect

    ## 
    ##  6-sample test for equality of proportions without continuity
    ##  correction
    ## 
    ## data:  df_laterality$L out of df_laterality$Total
    ## X-squared = 0.90962, df = 5, p-value = 0.9695
    ## alternative hypothesis: two.sided
    ## sample estimates:
    ##    prop 1    prop 2    prop 3    prop 4    prop 5    prop 6 
    ## 0.5161290 0.6000000 0.5294118 0.6000000 0.6666667 0.5238095

``` r
# White vs POC
POC_laterality <- df_clean %>%
    group_by(White_or_POC,Laterality) %>% 
    summarise(counts=n()) %>% 
    group_by(White_or_POC) %>% 
    mutate(Total=sum(counts)) %>%
    filter(Laterality=='L') %>%
    select(White_or_POC, counts, Total) %>%
    rename('L'=counts) %>%
    mutate(percentage = round(100*L/Total,2))
```

    ## `summarise()` regrouping output by 'White_or_POC' (override with `.groups` argument)

``` r
POC_laterality
```

    ## # A tibble: 3 x 4
    ## # Groups:   White_or_POC [3]
    ##   White_or_POC           L Total percentage
    ##   <fct>              <int> <int>      <dbl>
    ## 1 Non-Hispanic White   176   341       51.6
    ## 2 POC                   26    45       57.8
    ## 3 Unknown               11    21       52.4

``` r
prop.test(POC_laterality$L, POC_laterality$Total)
```

    ## 
    ##  3-sample test for equality of proportions without continuity
    ##  correction
    ## 
    ## data:  POC_laterality$L out of POC_laterality$Total
    ## X-squared = 0.60569, df = 2, p-value = 0.7387
    ## alternative hypothesis: two.sided
    ## sample estimates:
    ##    prop 1    prop 2    prop 3 
    ## 0.5161290 0.5777778 0.5238095

**Laterality**

**Overall, there is no difference in laterality. Left and right occur
about the same amount of time.**

**2. Comparing white and individual race groups. Here the null
hypothesis is that the proportion of foot melanoma is the same in each
race group: p1 = p2 = p3 = p4 = p5. The p-value here is 0.7886., which
is not significant, so we cannot reject the null hypothesis. **

**3. Comparing white and POC (combining individual race groups) and
Unknown. The p-value is 0.7387, still not significant.**

**4. No difference in laterality across ethnicity groups.**

``` r
df_insurance <- df_clean %>%
    group_by(Race_ethnicity,insurance_type) %>% 
    summarise(counts=n()) %>% 
    group_by(Race_ethnicity) %>% 
    mutate(Total=sum(counts)) %>%
    filter(insurance_type=='Private_or_public') %>% 
    select(Race_ethnicity, counts, Total) %>%
    rename('Private_or_public'=counts) %>%
    mutate(percentage = round(100*Private_or_public/Total,2))
```

    ## `summarise()` regrouping output by 'Race_ethnicity' (override with `.groups` argument)

``` r
df_insurance
```

    ## # A tibble: 6 x 4
    ## # Groups:   Race_ethnicity [6]
    ##   Race_ethnicity     Private_or_public Total percentage
    ##   <fct>                          <int> <int>      <dbl>
    ## 1 Non-Hispanic White               330   341       96.8
    ## 2 Non-Hispanic Black                12    15       80  
    ## 3 Hispanic                          16    17       94.1
    ## 4 Asian                              8    10       80  
    ## 5 Other                              2     3       66.7
    ## 6 Unknown                           21    21      100

``` r
print(paste('total number of people with private or public insurance:',sum(df_clean$insurance_type == 'Private_or_public')))
```

    ## [1] "total number of people with private or public insurance: 389"

``` r
print(paste('total number of people with insurance labels:',sum(df_clean$insurance_type!='na')))
```

    ## [1] "total number of people with insurance labels: 407"

``` r
# separate race groups 
prop.test(df_insurance$Private_or_public, df_insurance$Total)
```

    ## Warning in prop.test(df_insurance$Private_or_public, df_insurance$Total): Chi-
    ## squared approximation may be incorrect

    ## 
    ##  6-sample test for equality of proportions without continuity
    ##  correction
    ## 
    ## data:  df_insurance$Private_or_public out of df_insurance$Total
    ## X-squared = 22.496, df = 5, p-value = 0.0004212
    ## alternative hypothesis: two.sided
    ## sample estimates:
    ##    prop 1    prop 2    prop 3    prop 4    prop 5    prop 6 
    ## 0.9677419 0.8000000 0.9411765 0.8000000 0.6666667 1.0000000

``` r
# White vs POC
POC_insurance <- df_clean %>%
    group_by(White_or_POC,insurance_type) %>% 
    summarise(counts=n()) %>% 
    group_by(White_or_POC) %>% 
    mutate(Total=sum(counts)) %>%
    filter(insurance_type=='Private_or_public') %>%
    select(White_or_POC, counts, Total) %>%
    rename('Private_or_public'=counts) %>%
    mutate(percentage = round(100*Private_or_public/Total,2))
```

    ## `summarise()` regrouping output by 'White_or_POC' (override with `.groups` argument)

``` r
POC_insurance
```

    ## # A tibble: 3 x 4
    ## # Groups:   White_or_POC [3]
    ##   White_or_POC       Private_or_public Total percentage
    ##   <fct>                          <int> <int>      <dbl>
    ## 1 Non-Hispanic White               330   341       96.8
    ## 2 POC                               38    45       84.4
    ## 3 Unknown                           21    21      100

``` r
prop.test(POC_insurance$Private_or_public, POC_insurance$Total)
```

    ## Warning in prop.test(POC_insurance$Private_or_public, POC_insurance$Total): Chi-
    ## squared approximation may be incorrect

    ## 
    ##  3-sample test for equality of proportions without continuity
    ##  correction
    ## 
    ## data:  POC_insurance$Private_or_public out of POC_insurance$Total
    ## X-squared = 15.322, df = 2, p-value = 0.0004709
    ## alternative hypothesis: two.sided
    ## sample estimates:
    ##    prop 1    prop 2    prop 3 
    ## 0.9677419 0.8444444 1.0000000

**Insurance**

**It seems that White people have higher rates of having insurance for
both analyses.**

``` r
library(survival)
library(survminer)
```

    ## Loading required package: ggpubr

``` r
df_survival <- df_clean %>%
  select(Gender, Race_ethnicity, Laterality, DOB, dx_date, follow_up_date, Alive, Final_Stage, immuno, chemo) %>%
  mutate(age_diagnosed = time_length(difftime(dx_date, DOB), "years")) %>% # difference between DOB and diagnosis date
  mutate(age_fu=time_length(difftime(follow_up_date, DOB), "years")) %>% #difference between DOB and followup date
  mutate(yrs_post_dx = time_length(difftime(follow_up_date, dx_date), "years")) %>% # difference between diagnosis date and followup date
  mutate(status = case_when(Alive=='0' ~ '2',
                            TRUE ~ Alive)) %>% # consistent with survfit documentation 
  select(-Alive)

df_survival$status = as.numeric(df_survival$status)
```

    ## Warning: NAs introduced by coercion

``` r
head(df_survival)
```

    ## # A tibble: 6 x 13
    ##   Gender Race_ethnicity Laterality DOB                 dx_date   
    ##   <fct>  <fct>          <chr>      <dttm>              <date>    
    ## 1 Female Non-Hispanic … R          1992-07-13 00:00:00 2004-09-29
    ## 2 Female Non-Hispanic … R          1962-10-17 00:00:00 2010-09-16
    ## 3 Female Non-Hispanic … R          1960-02-24 00:00:00 2016-08-19
    ## 4 Female Non-Hispanic … L          1934-07-25 00:00:00 2008-07-02
    ## 5 Female Non-Hispanic … L          1963-05-04 00:00:00 2013-10-24
    ## 6 Male   Unknown        L          1946-10-01 00:00:00 1978-01-01
    ## # … with 8 more variables: follow_up_date <dttm>, Final_Stage <fct>,
    ## #   immuno <fct>, chemo <fct>, age_diagnosed <dbl>, age_fu <dbl>,
    ## #   yrs_post_dx <dbl>, status <dbl>

``` r
# multivariate Cox regression to how race, diagnosis stage, and diagnosis date affect the survival time simultaneously 
res.cox <- coxph(Surv(yrs_post_dx, status) ~ Race_ethnicity + Laterality + Gender + Final_Stage + age_diagnosed + immuno + chemo, data =  df_survival)
summary(res.cox)
```

    ## Call:
    ## coxph(formula = Surv(yrs_post_dx, status) ~ Race_ethnicity + 
    ##     Laterality + Gender + Final_Stage + age_diagnosed + immuno + 
    ##     chemo, data = df_survival)
    ## 
    ##   n= 396, number of events= 127 
    ##    (11 observations deleted due to missingness)
    ## 
    ##                                       coef exp(coef)  se(coef)      z Pr(>|z|)
    ## Race_ethnicityNon-Hispanic Black -0.366660  0.693045  0.518205 -0.708 0.479219
    ## Race_ethnicityHispanic            0.495129  1.640710  0.529078  0.936 0.349359
    ## Race_ethnicityAsian              -1.061676  0.345876  1.021120 -1.040 0.298471
    ## Race_ethnicityOther               0.497632  1.644822  0.737012  0.675 0.499547
    ## Race_ethnicityUnknown            -0.422332  0.655516  1.016351 -0.416 0.677748
    ## Lateralityna                      2.711046 15.045006  0.762793  3.554 0.000379
    ## LateralityR                       0.189845  1.209062  0.195744  0.970 0.332115
    ## GenderFemale                     -0.672379  0.510493  0.191547 -3.510 0.000448
    ## Final_Stage1                      0.972996  2.645859  0.438954  2.217 0.026649
    ## Final_Stage2                      1.725999  5.618129  0.423154  4.079 4.53e-05
    ## Final_Stage3                      2.282871  9.804788  0.423601  5.389 7.08e-08
    ## Final_Stage4                      3.039889 20.902914  0.500279  6.076 1.23e-09
    ## Final_Stagena                    -0.011016  0.989044  0.686087 -0.016 0.987190
    ## age_diagnosed                     0.049925  1.051192  0.007617  6.555 5.58e-11
    ## immunoY                          -0.559892  0.571271  0.261372 -2.142 0.032184
    ## chemoY                            0.387778  1.473703  0.246736  1.572 0.116036
    ##                                     
    ## Race_ethnicityNon-Hispanic Black    
    ## Race_ethnicityHispanic              
    ## Race_ethnicityAsian                 
    ## Race_ethnicityOther                 
    ## Race_ethnicityUnknown               
    ## Lateralityna                     ***
    ## LateralityR                         
    ## GenderFemale                     ***
    ## Final_Stage1                     *  
    ## Final_Stage2                     ***
    ## Final_Stage3                     ***
    ## Final_Stage4                     ***
    ## Final_Stagena                       
    ## age_diagnosed                    ***
    ## immunoY                          *  
    ## chemoY                              
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##                                  exp(coef) exp(-coef) lower .95 upper .95
    ## Race_ethnicityNon-Hispanic Black    0.6930    1.44291   0.25099    1.9136
    ## Race_ethnicityHispanic              1.6407    0.60949   0.58167    4.6279
    ## Race_ethnicityAsian                 0.3459    2.89121   0.04675    2.5592
    ## Race_ethnicityOther                 1.6448    0.60797   0.38795    6.9737
    ## Race_ethnicityUnknown               0.6555    1.52552   0.08943    4.8051
    ## Lateralityna                       15.0450    0.06647   3.37366   67.0939
    ## LateralityR                         1.2091    0.82709   0.82382    1.7745
    ## GenderFemale                        0.5105    1.95889   0.35071    0.7431
    ## Final_Stage1                        2.6459    0.37795   1.11925    6.2547
    ## Final_Stage2                        5.6181    0.17800   2.45133   12.8760
    ## Final_Stage3                        9.8048    0.10199   4.27433   22.4910
    ## Final_Stage4                       20.9029    0.04784   7.84095   55.7243
    ## Final_Stagena                       0.9890    1.01108   0.25776    3.7950
    ## age_diagnosed                       1.0512    0.95130   1.03562    1.0670
    ## immunoY                             0.5713    1.75048   0.34226    0.9535
    ## chemoY                              1.4737    0.67856   0.90863    2.3902
    ## 
    ## Concordance= 0.817  (se = 0.017 )
    ## Likelihood ratio test= 176.4  on 16 df,   p=<2e-16
    ## Wald test            = 138  on 16 df,   p=<2e-16
    ## Score (logrank) test = 183.7  on 16 df,   p=<2e-16

**For interpretations of the Cox regression model, you can use this as a
reference:
<http://www.sthda.com/english/wiki/cox-proportional-hazards-model>.**

**The Pr (\>|z|) column indicates p-value, and exp(coef) column
indicates hazard ratio. We see, for example, stage 4 has a hazard ratio
of 22.76. This means, holding other variables the same, having stage 4
increases the hazard 23 fold compared to stage 0.**

**There doesn’t seem to be a difference between races in terms of
survival since initial diagnosis date**

**Strangely, if laterality is unknown, these patients die at a much
faster rate than if laterality is L or R - there could be some
confounders here**

**age diagnosis is correlated with higher chance of death, which makes
sense**

**immunotherapy is protective against dying, which also makes sense**

``` r
# for patients without immunotherapy, what are their characteristics? age, race, gender, stage, time of diagnosis (compared to 2011), insurance.
immuno_pt <- df_clean %>% filter(immuno == 'Y')

print(paste('There are', dim(immuno_pt)[1],'patients who received immunotherapy in total.'))
```

    ## [1] "There are 68 patients who received immunotherapy in total."

``` r
# birthyear 
summary(immuno_pt$DOB)
```

    ##                  Min.               1st Qu.                Median 
    ## "1924-12-27 00:00:00" "1942-11-10 00:00:00" "1950-04-02 00:00:00" 
    ##                  Mean               3rd Qu.                  Max. 
    ## "1951-10-24 04:56:29" "1960-12-17 18:00:00" "1984-12-08 00:00:00"

``` r
# laterality
print(paste(sum(immuno_pt$Laterality=='L'), 'are on the left side'))
```

    ## [1] "34 are on the left side"

``` r
prop.test(sum(immuno_pt$Laterality=='L'), dim(immuno_pt)[1]) # no difference in laterality
```

    ## 
    ##  1-sample proportions test without continuity correction
    ## 
    ## data:  sum(immuno_pt$Laterality == "L") out of dim(immuno_pt)[1], null probability 0.5
    ## X-squared = 0, df = 1, p-value = 1
    ## alternative hypothesis: true p is not equal to 0.5
    ## 95 percent confidence interval:
    ##  0.3843806 0.6156194
    ## sample estimates:
    ##   p 
    ## 0.5

``` r
# location
print(paste(sum(immuno_pt$Location=='foot'), 'are on the foot.'))
```

    ## [1] "58 are on the foot."

``` r
prop.test(sum(immuno_pt$Location=='foot'), dim(immuno_pt)[1]) # more on the foot than hand
```

    ## 
    ##  1-sample proportions test with continuity correction
    ## 
    ## data:  sum(immuno_pt$Location == "foot") out of dim(immuno_pt)[1], null probability 0.5
    ## X-squared = 32.485, df = 1, p-value = 1.201e-08
    ## alternative hypothesis: true p is not equal to 0.5
    ## 95 percent confidence interval:
    ##  0.7415337 0.9234773
    ## sample estimates:
    ##         p 
    ## 0.8529412

``` r
# race breakdown
immuno_race <- immuno_pt %>% 
  group_by(Race_ethnicity) %>%
  summarise(received_immuno = n())
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
immuno_race <- merge(immuno_race, race_counts, on='Race_ethnicity') %>%
  mutate(percentage = received_immuno/Total_Counts)

immuno_race
```

    ##       Race_ethnicity received_immuno Total_Counts percentage
    ## 1              Asian               3           10 0.30000000
    ## 2           Hispanic               4           17 0.23529412
    ## 3 Non-Hispanic Black               1           15 0.06666667
    ## 4 Non-Hispanic White              60          341 0.17595308

``` r
# gender
prop.test(sum(immuno_pt$Gender=='Male'), dim(immuno_pt)[1]) # males and females seem to get immunotherapy at the same rate
```

    ## 
    ##  1-sample proportions test without continuity correction
    ## 
    ## data:  sum(immuno_pt$Gender == "Male") out of dim(immuno_pt)[1], null probability 0.5
    ## X-squared = 0, df = 1, p-value = 1
    ## alternative hypothesis: true p is not equal to 0.5
    ## 95 percent confidence interval:
    ##  0.3843806 0.6156194
    ## sample estimates:
    ##   p 
    ## 0.5

``` r
# year of diagnosis
summary(immuno_pt$dx_date)
```

    ##         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
    ## "2000-06-08" "2012-06-21" "2015-01-07" "2014-06-09" "2017-06-08" "2020-07-13"

``` r
print(paste('Out of',dim(immuno_pt)[1],'patients who received immunotherapy,',sum(immuno_pt$dx_date>'2011-01-01'),'had their first diagnoses after 2011-01-01.'))
```

    ## [1] "Out of 68 patients who received immunotherapy, 55 had their first diagnoses after 2011-01-01."

``` r
# stage breakdown
immuno_stage <- immuno_pt %>% 
  group_by(Final_Stage) %>%
  summarise(pts_in_stage_w_immuno = n())
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
immuno_stage <- merge(immuno_stage, stage_counts, on='Final_Stage') %>%
  mutate(percentage = pts_in_stage_w_immuno/Total_Counts)

immuno_stage
```

    ##   Final_Stage pts_in_stage_w_immuno Total_Counts percentage
    ## 1           0                     2           82 0.02439024
    ## 2           1                     6          104 0.05769231
    ## 3           2                    12           79 0.15189873
    ## 4           3                    39           94 0.41489362
    ## 5           4                     8           18 0.44444444
    ## 6          na                     1           30 0.03333333

``` r
# insurance
summary(immuno_pt$insurance_type)
```

    ## Private_or_public          Self-pay          Medicaid     International 
    ##                62                 6                 0                 0

``` r
stage34_pt <- df_clean %>% filter(Final_Stage %in% c(3,4))

print(paste('There are', dim(stage34_pt)[1],'patients who were labeled as stage 3 or 4 in total.'))
```

    ## [1] "There are 112 patients who were labeled as stage 3 or 4 in total."

``` r
stage34_pt <- stage34_pt %>%
  select(Gender, Race_ethnicity, Laterality, DOB, dx_date, follow_up_date, Alive, Final_Stage, immuno, chemo) %>%
  mutate(age_diagnosed = time_length(difftime(dx_date, DOB), "years")) %>% # difference between DOB and diagnosis date
  mutate(age_fu=time_length(difftime(follow_up_date, DOB), "years")) %>% #difference between DOB and followup date
  mutate(yrs_post_dx = time_length(difftime(follow_up_date, dx_date), "years")) %>% # difference between diagnosis date and followup date
  mutate(status = case_when(Alive=='0' ~ '2',
                            TRUE ~ Alive)) %>% # consistent with survfit documentation 
  select(-Alive)

stage34_pt$status = as.numeric(stage34_pt$status)
```

    ## Warning: NAs introduced by coercion

``` r
# for patients with stage 3 or 4, what is the diff in mean survival between ppl with immunotherapy or not (unit in years post diagnosis)
fit_stage34_pt <- survfit(Surv(yrs_post_dx, status) ~ immuno, data = stage34_pt)
print(fit_stage34_pt)
```

    ## Call: survfit(formula = Surv(yrs_post_dx, status) ~ immuno, data = stage34_pt)
    ## 
    ##    3 observations deleted due to missingness 
    ##           n events median 0.95LCL 0.95UCL
    ## immuno=N 65     42   2.33    1.95    4.71
    ## immuno=Y 44     18   5.44    3.21      NA

``` r
ggsurvplot(fit_stage34_pt,
          pval = TRUE, conf.int = TRUE,
          risk.table = TRUE, # Add risk table
          risk.table.col = "strata", # Change risk table color by groups
          linetype = "strata", # Change line type by groups
          surv.median.line = "hv", # Specify median survival
          fontsize = 3,
          surv.plot.height = 2
          )
```

    ## Warning: Vectorized input to `element_text()` is not officially supported.
    ## Results may be unexpected or may change in future versions of ggplot2.

![](Acral_Melanoma_files/figure-gfm/sub%20analysis%20of%20patients%20with%20stage%203%20or%204%20cancer-1.png)<!-- -->

``` r
res.cox <- coxph(Surv(yrs_post_dx, status) ~ immuno, data =  stage34_pt)
summary(res.cox)
```

    ## Call:
    ## coxph(formula = Surv(yrs_post_dx, status) ~ immuno, data = stage34_pt)
    ## 
    ##   n= 109, number of events= 60 
    ##    (3 observations deleted due to missingness)
    ## 
    ##            coef exp(coef) se(coef)      z Pr(>|z|)  
    ## immunoY -0.6995    0.4968   0.2826 -2.475   0.0133 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##         exp(coef) exp(-coef) lower .95 upper .95
    ## immunoY    0.4968      2.013    0.2855    0.8645
    ## 
    ## Concordance= 0.606  (se = 0.031 )
    ## Likelihood ratio test= 6.61  on 1 df,   p=0.01
    ## Wald test            = 6.13  on 1 df,   p=0.01
    ## Score (logrank) test = 6.38  on 1 df,   p=0.01

**After subsetting the patients in stage 3 or 4, we still see that
immunotherapy is protective of patient survival.**
