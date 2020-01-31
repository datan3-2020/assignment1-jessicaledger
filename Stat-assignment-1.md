Statistical assignment 1
================
Jessica Ledger 660013603
29/1/2020

## Open data (10 points)

In this assignment you will work with the individual level data from
wave 8 of the Understanding Society survey. First, you need to open the
data set. Please complete the code
    below.

``` r
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.2.1     ✓ purrr   0.3.3
    ## ✓ tibble  2.1.3     ✓ dplyr   0.8.3
    ## ✓ tidyr   1.0.0     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.4.0

    ## ── Conflicts ─────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(dplyr)

Data <- read_tsv("/Users/jessicaledger/Desktop/WORK/THIRD YEAR/Data Analysis III/data2020/data/UKDA-6614-tab/tab/ukhls_w8/h_indresp.tab")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double()
    ## )

    ## See spec(...) for full column specifications.

Now you have got your data frame stored as Data.

## Select variables (10 points)

The data for Wave 8 of the Understanding Society were collected in
2016-18. Among other things, people were asked the following question:
“Should the United Kingdom remain a member of the European Union or
leave the European Union?” In this assignment, we will explore how
answers to this question depend on sex and age.

First, you need to select the variables for the analysis. You want to
keep the following variables: cross-wave individual identifier (*pidp*),
support for the UK remaining or leaving the EU (*h\_eumem*), sex
(*h\_sex\_dv*), age (*h\_age\_dv*), and sample origin (*h\_memorig*).

Complete the code below to select those variables from the data frame
and save the result.

``` r
Data <- Data %>%
        select(pidp, h_eumem, h_sex_dv, h_age_dv, h_memorig)
```

## Filter observations (10 points)

To make nationally representative estimates from the Understanding
Society data we would need to use weight coefficients. There are many
different types of weight coefficients that can be used depending on the
question and the level of analysis (see the User Guide, pp. 65-71). We
will not do this in this assignment. However, what we want to do is to
keep data from the original Understanding Society sample only (ukhls gb
2009-10), dropping data for Northern Ireland, the BHPS cohort members
and ethnic minority boost samples. This will make data closer to be
representative for Great Britain. You need to choose the observations
where *h\_memorig* has the value of 1.

``` r
Data <- Data %>%
        filter(h_memorig == 1)
```

## Recode data (20 points)

Let us tabulate the variables for EU support, sex, and age.

``` r
table(Data$h_eumem)
```

    ## 
    ##    -9    -8    -7    -2    -1     1     2 
    ##    33   482   879   354   753 11118  9338

``` r
table(Data$h_sex_dv)
```

    ## 
    ##     0     1     2 
    ##     1 10470 12486

``` r
table(Data$h_age_dv)
```

    ## 
    ##  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35 
    ## 284 309 290 291 278 295 268 326 287 257 243 234 229 249 274 278 278 293 314 332 
    ##  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55 
    ## 351 332 321 336 320 327 368 404 372 386 435 465 425 447 406 420 427 414 432 422 
    ##  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75 
    ## 408 413 416 434 369 398 358 399 354 412 345 358 412 434 431 334 326 293 275 251 
    ##  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95 
    ## 219 231 211 205 181 162 138 117 117 108  89  78  77  48  41  27  15  18  15   7 
    ##  96  97  98  99 101 102 
    ##   6   2   3   1   1   1

You will see that all these variables are numeric. You can learn what
the numeric codes mean by checking the codebook here:
<https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/datafile/indresp/wave/8>

We want to do the following:

1)  Recode the variable for EU support as binary (1 for Remain, 0 for
    Leave), coding all types of missing values (including refusals and
    “don’t know”) as NA.

2)  Recode sex into a character vector with the values “male” or
    “female”.

3)  Recode age into a variable with the following categories: 16 to 25,
    26 to 40, 41 to 55, 56 to 70, over 70.

In all cases, we want to create new variables.

``` r
Data <- Data %>%
        mutate(EU = ifelse(h_eumem == "1", "1",
                           ifelse(h_eumem == "2", "0", NA_real_))
      
        ) %>%
        mutate(sex = recode(h_sex_dv,
                            "1" = "male",
                            "2" = "female",
                            .default = NA_character_)) %>%
        mutate(agegr = case_when(
                between(h_age_dv, 16, 25) ~ "16 to 25",
                between(h_age_dv, 26, 40) ~ "26 to 40",
                between(h_age_dv, 41, 55) ~ "41 to 55",
                between(h_age_dv, 56, 70) ~ "56 to 70",
                h_age_dv >= 70 ~ "over 70"
        )
        )

class(Data$EU)
```

    ## [1] "character"

``` r
class(Data$sex)
```

    ## [1] "character"

``` r
class(Data$agegr)
```

    ## [1] "character"

## Summarise data (20 points)

Let us **dplyr** to calculate how many people in the sample supported
Remain and Leave, both as absolute numbers and percentages.

``` r
Data %>%
        count(EU) %>%
  mutate(perc = n / sum(n) * 100)
```

    ## # A tibble: 3 x 3
    ##   EU        n  perc
    ##   <chr> <int> <dbl>
    ## 1 0      9338  40.7
    ## 2 1     11118  48.4
    ## 3 <NA>   2501  10.9

Write a couple of sentences with the interpretation of this result. How
this compares with the result of the 2016 referendum? Why?

This is an interesting result as there is not an overall majority for
either Remain or EU (neither Remain or Leave recieved 50% of the vote).
The result of the sample shows that 48% voted to Remain in the EU and
only 41% voted to Leave the EU. This is very different in comparison to
what happened overall in the 2016 referendum as in 2016, 46.6% voted in
favour of Remain and 53.4% voted in favour of Leave. This given sample
would indicate that more people were in favour to Remain in the EU, yet
this is not what happened in 2016. The difference in result may be as a
result of a smaller sample group (approx. 11% of sample have been
removed as NA) or as this was a questionnaire taken before the referndum
vote took place and does not determine what people actually voted,
therefore people could have changed their votes closer to when they cast
their vote.

## Summarise data by sex and age (30 points)

Now let us look at the support for Leave and Remain by sex and age. Use
your newly created variables.

``` r
Data %>%
  group_by(sex) %>%
  count(EU) %>%
  mutate(percentage = n / sum(n) * 100)
```

    ## # A tibble: 7 x 4
    ## # Groups:   sex [3]
    ##   sex    EU        n percentage
    ##   <chr>  <chr> <int>      <dbl>
    ## 1 female 0      4859       38.9
    ## 2 female 1      6371       51.0
    ## 3 female <NA>   1256       10.1
    ## 4 male   0      4479       42.8
    ## 5 male   1      4746       45.3
    ## 6 male   <NA>   1245       11.9
    ## 7 <NA>   1         1      100

``` r
Data %>%
  group_by(agegr) %>%
  count(EU) %>%
  mutate(percentage = n/sum(n) * 100)
```

    ## # A tibble: 15 x 4
    ## # Groups:   agegr [5]
    ##    agegr    EU        n percentage
    ##    <chr>    <chr> <int>      <dbl>
    ##  1 16 to 25 0       763      26.4 
    ##  2 16 to 25 1      1749      60.6 
    ##  3 16 to 25 <NA>    373      12.9 
    ##  4 26 to 40 0      1508      34.4 
    ##  5 26 to 40 1      2440      55.7 
    ##  6 26 to 40 <NA>    436       9.95
    ##  7 41 to 55 0      2509      40.8 
    ##  8 41 to 55 1      3013      49.0 
    ##  9 41 to 55 <NA>    628      10.2 
    ## 10 56 to 70 0      2737      46.1 
    ## 11 56 to 70 1      2646      44.5 
    ## 12 56 to 70 <NA>    558       9.39
    ## 13 over 70  0      1821      50.6 
    ## 14 over 70  1      1270      35.3 
    ## 15 over 70  <NA>    506      14.1

``` r
Data %>%
  group_by(sex, agegr) %>%
  count(EU) %>%
  mutate(percentage = n/sum(n) * 100)
```

    ## # A tibble: 31 x 5
    ## # Groups:   sex, agegr [11]
    ##    sex    agegr    EU        n percentage
    ##    <chr>  <chr>    <chr> <int>      <dbl>
    ##  1 female 16 to 25 0       371      24.3 
    ##  2 female 16 to 25 1       985      64.6 
    ##  3 female 16 to 25 <NA>    169      11.1 
    ##  4 female 26 to 40 0       817      32.8 
    ##  5 female 26 to 40 1      1455      58.5 
    ##  6 female 26 to 40 <NA>    216       8.68
    ##  7 female 41 to 55 0      1313      39.4 
    ##  8 female 41 to 55 1      1733      51.9 
    ##  9 female 41 to 55 <NA>    290       8.69
    ## 10 female 56 to 70 0      1448      45.2 
    ## # … with 21 more rows

Write a couple of sentences interpreting your results.

Regarding the support for Leave and Remain by sex, a higher proportion
of women voted to remain (51%) in the EU rather than leave (40%).
Similarly, a higher proportion of men voted remain (45%) rather than
leave (43%). It is clear that there is a closer gap between the vote for
men (2%) and there is a wider gap with women (11%.)

Regarding the support for Leave and Remain by age, it can be interpreted
that a larger proportion of young people voted to remain in the EU (61%
16 to 25, and 56% 26 to 40) and the middle ages were closer in their
divide between leaving and remaining in the EU. Furthermore, older
people, most noteably over 70 were much more likely to be pro Leave
rather than remaining in the EU (50% voted Leave in over 70 category)
