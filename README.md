# Estimating real-world COVID-19 vaccine effectiveness in Israel

The vaccination rollout of the COVID-19 vaccines in Israel has been highly successful compared to all other countries. 
By the end of January, a third of the population has already been administered at least one dose of the BNT162b2 vaccine. 
Efforts to estimate the true real-world effectiveness of the vaccine have been hampered by disease dynamics and social-economic discrepancies. 
Here, using counts of positive and hospitalized cases of vaccinated individuals, we provide sensitive estimations of the vaccine effectiveness by 
correcting to daily case incidence. We find high effectiveness of 66-85% in reducing SARS-CoV-2 positive cases and over 90% in reducing severe 
hospitalizations. As more granular data will be available more it will be possible to extract more exact estimates; however, 
there is little doubt that the vaccine is highly effective in reducing cases, hospitalizations and deaths.

## Introduction

Vaccination rollout in Israel of the COVID-19 vaccines started on December 20, 2020. By the end of January, 33% and 19% of the population had 
already received the first dose and second dose, respectively, by the BNT162b2 vaccine developed by BioNTech and Pfizer. The vaccination campaign 
coincided with the beginning of a 3rd wave, and by mid-January SARS-CoV2 positive cases and hospitalizations more than doubled. To mitigate this 
increase in cases, on January 8 strict lockdown was imposed. However, cases and hospitalizations did not drop as expected and as observed in previous waves. 
There was some frustration in the public and by government officials, and doubts were raised whether the vaccines are effective.
 
Estimating real-world effectiveness of vaccinations is complicated. Israel has seen significant discrepancies between socio-economic and demographics 
groups in vaccination uptake. This discrepancy entangles with socio-economic disparities in infection rates. While in randomized clinical trials, 
the disease dynamics and socio-economic differences are less of an issue because of the blinded randomization, in real-world, teasing out those 
confounding factors is impossible without individual-level clinical and demographic data. To date, this information has not been shared with the public.
 
Here, using publicly available data of COVID-19 dynamics and SARS-CoV2 positive and hospitalizations of those that were vaccinated, we provide 
estimates using different scenarios of the effectiveness of the vaccines in reducing cases and severe cases.

## Methods

Daily SARS-CoV2 positive cases and severe or critical hospitalization was downloaded from the Israeli Ministry of Health COVID-19 public database.
Number of cases and severe or critical hospitalization of vaccinated individuals stratified by time from vaccination and age was provided by the number 
Ministry of Health on February 2, for all cases up to January 31.


To calculate effectiveness, we first estimate the expected number of cases or hospitalizations. To achieve this, we count the number of the cumulative 
vaccinated individuals on each day that are eligible to each of four groups – between day 0 to 13 of the first dose (group 1), between day 14 to 21 of the 
first dose (group 2), between day 0 to 6 of the second dose (group 3), and from day 7 of the second dose (group 4). We then multiply that count by the daily 
incidence rate of the whole population. The incidence rate is calculated using the number of cases divided by the population size (9.2 million). Different population sizes affect only marginally the observed effect (see https://dviraran.shinyapps.io/VaccineEffectIsrael/ to adjust population sizes). Finally, since incidence rates of the vaccinated cohort are different from the general population, we use a sensitivity parameter 
to adjust for the incidence rates. The effectiveness can be formulated as below:

E(β)= ∑_(i=1)^N▒〖V_i∙D_i∙β〗

Where V_i is the cumulative number of vaccinated individuals in day i, D_i  is the general population incidence rate in day i, β is the sensitivity parameter to adjust the incidence rate, and N is the number of relevant days (7 days for group 1-3, and 15 for group 4).

Standard error is the standard error of proportion using the observed cases as a denominator.

## Results

Between December 20, 2020 and January 31, 2021, there were 3,082,190 individuals that were vaccinated in Israel by the first dose of the vaccine, of them, 
1,215,797 over the age of 60. By that date, 1,789,836 have already received their second dose of the vaccine. Of all those vaccinated, 31,810 
individuals have tested positive for SARS-CoV2, and 1,525 have been hospitalized with severe or critical conditions or died (Table 1).

**Table 1. Number of cases as reported by the Ministry of Health.**

|                     | Positive cases (>60y) | Positive cases (<60y) | Severe / critical / death cases |
|---------------------|-----------------------|-----------------------|---------------------------------|
| 1st dose, day 0-13  | 7,070                 | 13,610                | 960                             |
| 1st dose, day 14-21 | 4,606                 | 3,931                 | 458                             |
| 2nd dose, day 0-6   | 1,005                 | 799                   | 80                              |
| 2nd dose, day >6    | 531                   | 258                   | 27                              |


Based on daily numbers of vaccinations and rates of general incidence we estimated expected numbers of cases and severe hospitalizations. 
The analysis suggests that for individuals aged 60 and above, there was a 28% reduction in SARS-CoV2 positive cases by day 13 of the first dose, 
43% between days 14 to 21 of the first dose, 80% between the second dose and day 6 and 83% after day 7 of the second dose (Figure 1).

![alt text](https://github.com/dviraran/covid_analyses/blob/master/figure1.jpg)

However, an underlying assumption here is that the incidence rates of those that were vaccinated early are similar to the general population. 
Previous analyses have shown that this is not the case as older populations have lower incidence and lower socio-economic groups have higher 
incidence. Therefore, we perform a sensitivity analysis by adjusting incidence rates using different levels of beta values. 
For a conservative beta of 0.5, which implies that the vaccinated population are expected to have half the cases of the general population, 
there is no case reduction by day 21 of the first dose. However, after the second dose, we observe reduction of 59 in cases, and 66% reduction 
7 days after the second dose. For individuals aged below 60, we see a relatively similar effect with a shift of 0.25 in beta values. 
Critical and severe cases are most prominently of older individuals, and this group represents around 75% of severe cases; thus, we report the number for beta = 4. Our analysis suggests a strong impact of the vaccine in reducing severe cases already after the first dose (54-59%), and  after the second dose, those numbers go up to 94% 7 days after the second dose.

## Discussion

The randomized clinical trial (RCT) of BNT162b2 has suggested efficacy of 95% a week after the second dose and unclear efficacy earlier.5 It also suggested differences between the older and younger population, but with high levels standard errors. In addition, the clinical trial was performed on a relatively small population compared to Israel. By the end of January, Israel had vaccinated 140-fold more individuals. Therefore, real-world data effectiveness is of high interest and important for decision-makers and mobilizing individuals to get the vaccine. Our sensitivity analysis provides an estimate for the effectiveness of the vaccine in reducing positive and severe cases. While this estimate is lower than the efficacy of the RCT, it is still substantive and provides reassurance for the vaccine efficacy. 

Our sensitivity analysis provides lower and upper bound estimate of the effectiveness. Accurate real-world beta values are hard to calculate, however, it is well supported that this value is 0.5-1 for the older and early vaccinated population, around 1 for the younger population, and 2-6 for severe cases in the older population. Thus, we can estimate that the positive case effectiveness is 66-83% for 60 years and older, 76-85% for below 60 years old, and 87-96% effective in preventing severe cases.

Our analysis suffers from many limitations. First, all analyses are performed on aggregated counts, which limits the possibilities to make individual-level inferences. Second, there are delays in reporting of cases and especially hospitalizations, so it is expected that some of the observed counts may increase. Third, cases may be complicated several days after infection, and thus the severe counts are expected to increase in all four groups. Fourth, In Israel, many individuals may get tested without symptoms, and there is an incentive to get tested if you are required to be in isolation due to contact with an infected individual. However, this incentive is reduced 7 days after the second dose, as the law now exempts those from isolation. Thus, there is a difference in testing rates of asymptomatic individuals between groups. It is reassuring to see that there are relatively similar levels of effectiveness of those 7 days after the second dose to those before those 7 days, suggesting that this testing incentive has only a marginal effect. Fifth, the general population incidence is also affected by the vaccination rollout, as more individuals are vaccinated, the incidence is expected to be affected by the vaccination; thus the real effectiveness might be higher than our estimates.

Other attempts to identify the impact of the vaccination campaign in Israel are underway. Chodik et al. compared cases in vaccinated individuals on days 13-24 after the first dose with vaccinated individuals in days 0-12.6 Rossman et al. used a natural experiment approach to compare early and late vaccinated cities and differences in the prioritization for the vaccine between age groups.4 Our contribution here is the use of the general population as a control group to assess the effectiveness. 

In conclusion, this study provides the first attempt to estimate the effectiveness of the BNT162b2 vaccine on a population level compared to the general population. Our analysis provides strong reassurance that the vaccine is highly effective. With more data that will be shared with the public we believe that more accurate estimation can be calculated.

## Data and code availability

All data is public and can be downloaded from https://data.gov.il/dataset/covid-19 or Ministry of Health press releases. Data and code used in the analyses was deposited in https://github.com/dviraran/covid_analyses. In addition, we provide an interactive shiny app, which will be updated as more data is available by the Ministry of Health - https://dviraran.shinyapps.io/VaccineEffectIsrael/. 

## Acknowledgments

DA is supported by the Azrieli Faculty Fellowship and is an Andre Deloro Fellow. We thank Uri Shalit for his valuable comments.

## Funding

There was no specific funding for this study.


