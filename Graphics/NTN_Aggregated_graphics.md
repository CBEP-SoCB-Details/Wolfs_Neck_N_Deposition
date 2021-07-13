Reviewing Annual Aggregation of Atmospheric Deposition Data
================
Curtis C. Bohlen, Casco Bay Estuary Partnership.
04/06/2021

-   [Introduction](#introduction)
-   [Install Libraries](#install-libraries)
-   [Read Annual Data](#read-annual-data)
    -   [Add Data Quality Flag](#add-data-quality-flag)
    -   [Trend Graphic](#trend-graphic)
        -   [Consistent Units](#consistent-units)
    -   [Kendall’s Tau](#kendalls-tau)
        -   [All Years](#all-years)
        -   [Years that Meet QA
            Standards](#years-that-meet-qa-standards)
    -   [Median-Based Linear Model](#median-based-linear-model)
        -   [All Years](#all-years-1)
    -   [Years that Meet QA Standards](#years-that-meet-qa-standards-1)
-   [Atmospheric Nitrogen Loading](#atmospheric-nitrogen-loading)
    -   [Mean Loading Rates 2015 -
        2019](#mean-loading-rates-2015---2019)
    -   [Modify Graphic](#modify-graphic)
-   [Annual Nutrient Load Estimate](#annual-nutrient-load-estimate)
    -   [Areas](#areas)
    -   [Deposition to Surface of Bay](#deposition-to-surface-of-bay)
-   [Load Graphic](#load-graphic)

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

# Introduction

Data on atmospheric deposition is available at several different
temporal scales, including daily, weekly, monthly, seasonal, and annual
summaries. Data is also available in both concentration and
deposition-focused versions.

In this notebook, we focus on developing graphics and a table for value
in the *State of Casco Bay* report. All graphics are based on the annual
summary data

# Install Libraries

``` r
library(readr)
library(tidyverse)
#> Warning: package 'tidyverse' was built under R version 4.0.5
#> -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
#> v ggplot2 3.3.3     v dplyr   1.0.6
#> v tibble  3.1.2     v stringr 1.4.0
#> v tidyr   1.1.3     v forcats 0.5.1
#> v purrr   0.3.4
#> Warning: package 'tidyr' was built under R version 4.0.5
#> Warning: package 'dplyr' was built under R version 4.0.5
#> Warning: package 'forcats' was built under R version 4.0.5
#> -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()

library(mblm)

library(CBEPgraphics)
load_cbep_fonts()
theme_set(theme_cbep())
```

# Read Annual Data

``` r
sibfldnm <- 'Original_Data'
parent <- dirname(getwd())
sibling <- paste(parent,sibfldnm, sep = '/')

dir.create(file.path(getwd(), 'figures'), showWarnings = FALSE)
```

The data we are pulling provides information on annual LOADINGS per unit
area.

``` r
fn <- 'NTN-ME96-cydep.csv'
annual_data <- read_csv(file.path(sibling, fn)) %>%
  mutate_at (c('Ca','Mg','K', 'Na','totalN', 'Cl', 'SO4', 'Br'), ~na_if(., -9)) %>%
  select(-c('Ca','Mg','K', 'Na', 'Cl','SO4', 'Br')) %>%
  select(-seas, -siteID)
#> 
#> -- Column specification --------------------------------------------------------
#> cols(
#>   .default = col_double(),
#>   siteID = col_character(),
#>   seas = col_character(),
#>   startDate = col_datetime(format = ""),
#>   lastDate = col_datetime(format = "")
#> )
#> i Use `spec()` for the full column specifications.

long_data <- annual_data %>%
  pivot_longer(NH4:totalN, names_to = 'Parameter', values_to = 'Value')
```

## Add Data Quality Flag

The data validation metadata for these data includes the following
statement:

> Three Completeness Criteria form the basis for the decision to include
> the laboratory chemistry data from a site in the Annual Isopleth Maps.
> All three criteria must be met.

The specific criteria are as follows:

| Name      | Value | Meaning                                                                                                                              |
|-----------|-------|--------------------------------------------------------------------------------------------------------------------------------------|
| Criteria1 | 75    | Percentage of the summary period for which there are valid samples.                                                                  |
| Criteria2 | 90    | Percentage of the summary period for which precipitation amounts are available either from the rain gauge or from the sample volume. |
| Criteria3 | 75    | Percentage of the total measured precipitation associated with valid samples.                                                        |

We add a flag to mark if all data quality metrics were met for annual
totals or not:

``` r
annual_data <- annual_data %>%
  mutate(fully_valid = Criteria1 >= 75 & Criteria2 >= 90 & Criteria3 >= 75)
```

It is worth pointing out that for all years, Criteria2 (precipitation
coverage) is 100% or near 100%. Where these years fail to meet data
quality criteria, it is because of lack of valid samples. All years have
at least 70% of rainfall with valid samples.

## Trend Graphic

### Consistent Units

``` r
MW_NH4 <- 14.007 + (1.008 * 4)
MW_NO3 <- 14.007 +(15.999 * 3)
MW_N <- 14.007
tmp <- annual_data %>%
  mutate(NH4 = NH4 * MW_N/ MW_NH4,
         NO3 = NO3 * MW_N/ MW_NO3) %>%
  pivot_longer(NH4:totalN, names_to = 'Parameter', values_to = 'Value')
```

``` r
ggplot(tmp, aes(yr, Value, color = Parameter)) +
  geom_line() +
  ylab('Nitrogen Deposition (kg/ha/yr)') +
  scale_color_manual(name = '', values = cbep_colors(),
                     labels = c('Ammonium', 'Nitrate', 'Total N'))
```

<img src="NTN_Aggregated_graphics_files/figure-gfm/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

We a huge spike in ammonium in 2012. There is a possible downward trend
in nitrate. Any downward trend in TN may be masked by that spike in
2012, especially if we use least squares models.

``` r
annual_data %>%
  mutate(NH4 = NH4 * MW_N/ MW_NH4,
         NO3 = NO3 * MW_N/ MW_NO3) %>%
  select (-totalN) %>%
  pivot_longer(c(NH4, NO3),
               names_to = 'Parameter',
               values_to = 'Value') %>%

  ggplot(aes(yr, Value, fill = Parameter)) +
  geom_col() +
  scale_fill_manual(name = '', values = cbep_colors()[5:6],
                     labels = c('Ammonium', 'Nitrate', 'Total N')) +
  theme_cbep(base_size = 12) +
  theme(legend.position = c(0.25, 0.85)) +
  ylab('Concentration (mg/l as N)') +
  xlab('')
```

<img src="NTN_Aggregated_graphics_files/figure-gfm/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />
\# Exploring Correlations of TN and Year One obvious question is whether
there are long-term trends in atmospheric deposition for NH\_4, NO\_3 or
TN.

A downward trend in nitrate appears likely. A trend in TN, based in part
on that downward trend, appears possible.

There are significant data quality questions to contend with here, as

1.  Not all years include sufficient complete data to comply with data
    completeness criteria established for the atmospheric deposition
    data program.  
2.  The anomalous high NH\_4 levels in 2012 appear to reflect a local
    source – specifically the decommissioning of the manure management
    pit at Wolfe’s Neck Farm.

Given the outlier for NH\_4 and TN in 2012, we prefer to use a statistic
that is resistant to outliers. Here that can be either a rank
correlation or Kendall’s Tau. We calculate both.

## Kendall’s Tau

### All Years

``` r
cor.test(annual_data$yr, annual_data$totalN, method = 'spearman')
#> 
#>  Spearman's rank correlation rho
#> 
#> data:  annual_data$yr and annual_data$totalN
#> S = 2764, p-value = 0.007536
#> alternative hypothesis: true rho is not equal to 0
#> sample estimates:
#>        rho 
#> -0.5607002
cor.test(annual_data$yr, annual_data$totalN, method = 'kendall')
#> 
#>  Kendall's rank correlation tau
#> 
#> data:  annual_data$yr and annual_data$totalN
#> T = 66, p-value = 0.00476
#> alternative hypothesis: true tau is not equal to 0
#> sample estimates:
#>        tau 
#> -0.4285714
```

So we appear to have a significant decline in TN (and thus presumably
NO3). The data providers do not use data from years that do not meet
Criteria 1 through 3. We check if results are robust if we restrict to
years that meet their data QA criteria.

### Years that Meet QA Standards

If we remove several years with data that does not meet completeness
criteria, we find a

``` r
tmp1 <- annual_data %>%filter(fully_valid)
cor.test(tmp1$yr, tmp1$totalN, method = 'spearman')
#> 
#>  Spearman's rank correlation rho
#> 
#> data:  tmp1$yr and tmp1$totalN
#> S = 1120, p-value = 0.1415
#> alternative hypothesis: true rho is not equal to 0
#> sample estimates:
#>       rho 
#> -0.372549
cor.test(tmp1$yr, tmp1$totalN, method = 'kendall')
#> 
#>  Kendall's rank correlation tau
#> 
#> data:  tmp1$yr and tmp1$totalN
#> T = 50, p-value = 0.1513
#> alternative hypothesis: true tau is not equal to 0
#> sample estimates:
#>        tau 
#> -0.2647059
```

## Median-Based Linear Model

### All Years

``` r
tn_mblm <- mblm(totalN ~ yr, data = annual_data)
summary(tn_mblm)
#> Warning in wilcox.test.default(z$intercepts): cannot compute exact p-value with
#> ties
#> Warning in wilcox.test.default(z$slopes): cannot compute exact p-value with ties
#> Warning in wilcox.test.default(z$intercepts): cannot compute exact p-value with
#> ties
#> Warning in wilcox.test.default(z$slopes): cannot compute exact p-value with ties
#> 
#> Call:
#> mblm(formula = totalN ~ yr, dataframe = annual_data)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -0.9442 -0.4305 -0.1148  0.1004  2.6563 
#> 
#> Coefficients:
#>             Estimate      MAD V value Pr(>|V|)   
#> (Intercept) 89.42994 29.18895     228  0.00104 **
#> yr          -0.04294  0.01427      25  0.00104 **
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.7272 on 20 degrees of freedom
```

That projects a decline of 0.04 mg/l TN as N per year.

## Years that Meet QA Standards

``` r
tmp <- annual_data %>% filter(fully_valid)
tn_mblm_2 <- mblm(totalN ~ yr, data = tmp)
summary(tn_mblm_2)
#> 
#> Call:
#> mblm(formula = totalN ~ yr, dataframe = tmp)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -0.98085 -0.55484 -0.16201  0.07469  2.60562 
#> 
#> Coefficients:
#>             Estimate      MAD V value Pr(>|V|)   
#> (Intercept) 86.34507 53.36556     130  0.00934 **
#> yr          -0.04139  0.02653      26  0.01500 * 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.8197 on 15 degrees of freedom
```

That continues to identify a statistically significant decline in TN,
even omitting the data that does not meet all data completeness
criteria. The predicted slope is quite similar, and while the intercept
looks different, the model predictions are nearly identical.

``` r
newdat <- tibble(yr = 1998:2019)
preds = predict(tn_mblm, newdata = newdat)
preds2 = predict(tn_mblm_2, newdata = newdat)
newdat$pred = preds
newdat$pred2 = preds2
```

``` r
plt <- ggplot(annual_data, aes(yr, totalN)) +
  geom_point(aes(color = fully_valid),
             size = 2) +
  scale_color_manual(values = cbep_colors()[3:4],
                     name = '', labels = c('Incomplete Data', 'Complete Data')) +
  geom_line(data = newdat, mapping = aes(x = yr, y = pred), lwd = 1.5, color = 'gray15') +
  geom_line(data = newdat, mapping = aes(x = yr, y = preds), color = 'yellow') +
  xlab('') +
  ylab('Nitrogen Deposition (kg/ha)') +
  theme_cbep(base_size = 12) +
  theme(legend.position = c(.25, .8)) +
  xlim(1998, 2020)
plt
```

<img src="NTN_Aggregated_graphics_files/figure-gfm/unnamed-chunk-13-1.png" style="display: block; margin: auto;" />

We conclude that there is a meaningful decline in TN over time, and that
it makes little difference whether we include or do not include the
years that do not meet data completeness criteria. We chose to include
them.

# Atmospheric Nitrogen Loading

## Mean Loading Rates 2015 - 2019

These are in the original units, kg/ha. NOT kg/ha of N except for TN.

``` r
recent_loads <- annual_data %>%
  filter(yr > 2014) %>%
  select(NH4:totalN) %>%
  summarize(across( .fn = mean))
recent_loads
#> # A tibble: 1 x 3
#>     NH4   NO3 totalN
#>   <dbl> <dbl>  <dbl>
#> 1  1.61  5.64   2.52
tn_rate <- recent_loads$totalN
```

## Modify Graphic

``` r
dat <- tibble(yr = 2015:2019, val = tn_rate)

plt <- ggplot(annual_data, aes(yr, totalN)) +
  geom_point(color = cbep_colors()[4], size = 2) +
  scale_color_manual(values = cbep_colors()[3:4],
                     name = '', labels = c('Incomplete Data', 'Complete Data')) +
  geom_line(data = newdat, mapping = aes(x = yr, y = pred)) +
  
  xlab('') +
  ylab('Nitrogen Deposition (kg/ha)') +
  
  theme_cbep(base_size = 12) +
  theme(legend.position = c(.25, .8)) +
  xlim(1998, 2020)

plt2 <- plt + 
  geom_line(data = dat, mapping = aes(yr, val), color = cbep_colors()[5]) +
  annotate('text', x = 2012, y = 2.62, label = 'Avg Rate 2015-2019\n 2.52 kg/ha',
           size = 3, color = cbep_colors()[5], hjust = .5)
plt2
```

<img src="NTN_Aggregated_graphics_files/figure-gfm/unnamed-chunk-15-1.png" style="display: block; margin: auto;" />

``` r
ggsave('figures/tn_trend.pdf', device = cairo_pdf, width = 5, height = 4)
```

``` r
plt2 +
  annotate('text', x = 2012, y = 5.69, label = 'O',
           size = 6, color = cbep_colors()[1], hjust = .5) +
    annotate('text', x = 2012, y = 5.4, label = 'Influenced by\nlocal source',
           size = 3, color = cbep_colors()[1], hjust = .5) +
  
ggsave('figures/tn_trend_with.pdf', device = cairo_pdf, width = 5, height = 4)
```

<img src="NTN_Aggregated_graphics_files/figure-gfm/unnamed-chunk-16-1.png" style="display: block; margin: auto;" />

# Annual Nutrient Load Estimate

## Areas

Based on CBEP GIS data.

|                                        |           |
|----------------------------------------|-----------|
| Watershed Area, including islands      | 2554 km^2 |
| Area of the Bay (inside Half Way Rock) | 535 km^2  |

## Deposition to Surface of Bay

The other quantities we discuss in the section on Casco Bay nutrient
loads are expressed in terms of metric tons of nitrogen per year. We use
the same units here. To make a graphic that looks similar to the other
load estimates, we need to show couple of colors, and scale a single bar
chart.

``` r
# WE need to convert to units of N for consistency
MW_NH4 <- 14.007 + (1.008 * 4)
MW_NO3 <- 14.007 +(15.999 * 3)
MW_N <- 14.007


recent_direct_loads <- annual_data %>%
  filter(yr > 2014) %>%
  mutate(NH4 = NH4*(MW_N / MW_NH4),
         NO3 = NO3*(MW_N / MW_NO3)) %>%
  pivot_longer(NH4:totalN, names_to = 'Parameter', values_to = 'Value') %>%
  mutate(Value = Value * 100 * 535 / 1000) %>%
  mutate(Parameter = factor(Parameter, levels = c('NH4', 'NO3', 'totalN'),
                            labels = c('Ammonium', 'Nitrate', 'Total Nitrogen'))) %>%
  group_by(Parameter) %>%
  summarize(Mean = round(mean(Value), 2),
            SD = round(sd(Value), 3),
            `Sample Size` = sum(! is.na(Value)))
  
  knitr::kable(recent_direct_loads)
```

| Parameter      |   Mean |     SD | Sample Size |
|:---------------|-------:|-------:|------------:|
| Ammonium       |  66.68 | 12.899 |           5 |
| Nitrate        |  68.22 |  4.038 |           5 |
| Total Nitrogen | 134.95 | 14.327 |           5 |

# Load Graphic

``` r
total = sum(recent_direct_loads$Mean[[3]])

recent_direct_loads %>%
  filter(Parameter != 'Total Nitrogen') %>%
  
  ggplot(aes(1, Mean, fill = Parameter)) +
  geom_col() +
  
  ylab('Total Nitrogen\n(Metric Tons per Year)')  +
  xlab('') +
  
  scale_fill_manual(values = cbep_colors()[4:6], name = '') +
  theme_cbep(base_size = 12) +
  theme(axis.text.x= element_blank(),
        axis.ticks.x = element_blank(),
        legend.key.size = unit(0.2, 'in'),
        legend.text = element_text(size = 9)) +
  ylim(0, 500) +
  annotate('text', x = 1, y = 1.1 * total, 
           label = paste(round(total), 'MT'), 
           size = 3)
```

<img src="NTN_Aggregated_graphics_files/figure-gfm/unnamed-chunk-18-1.png" style="display: block; margin: auto;" />

``` r
ggsave('figures/tn_atmospheric.pdf', device = cairo_pdf, width = 2.75, height = 4)
```
