Link to pre-print https://www.medrxiv.org/content/10.1101/2021.02.05.21251139v1

# Estimating real-world COVID-19 vaccine effectiveness in Israel

The vaccination roll-out of the COVID-19 vaccines in Israel has been highly successful. By February 5th, approximately 37% of the population has already been administered at least one dose of the BNT162b2 vaccine. Efforts to estimate the true real-world effectiveness of the vaccine have been hampered by disease dynamics and social-economic discrepancies. Here, using counts of positive and hospitalized cases of vaccinated individuals, we provide sensitive estimations of the vaccine effectiveness by correcting to daily case incidence. A week after the second dose we observe effectiveness of 71-86% in reducing SARS-CoV-2 positive cases, and 83-87% reduction of COVID-19 hospitalizations and severe cases. Our analysis suggests that effectiveness of the vaccine only start after three weeks, which coincides with the administration of the second dose. As more granular data will be available, it will be possible to extract more exact estimates; however, there is little doubt that the vaccine is highly effective in reducing cases, hospitalizations and deaths.

## Introduction

Vaccination rollout in Israel of the COVID-19 vaccines started on December 20, 2020. By February 5th, 37% and 22% of the population had already received the first dose and second dose, respectively, by the BNT162b2 vaccine developed by BioNTech and Pfizer. The vaccination campaign coincided with the beginning of a “3rd wave”, and by mid-January SARS-CoV2 positive cases and hospitalizations more than doubled. To mitigate this increase in cases, on January 8 strict lockdown was imposed. However, cases and hospitalizations did not drop as expected and as observed in previous waves. There was some frustration in the public and by government officials, and doubts were raised whether the vaccines are effective.1
 
Estimating real-world effectiveness of vaccinations is complicated. Israel has seen significant discrepancies between socio-economic and demographics groups in vaccination uptake.2 This discrepancy entangles with socio-economic disparities in infection rates. In addition, some have speculated that behavioral changes of those immunized may affect the number of encounters and chances of infection. While in double-blinded randomized controlled clinical trials the disease dynamics, socio-economic differences and behavioral aspects are less of an issue, in real-world, it is not possible to accurately tease out those confounding factors.
 
Here, using publicly available data of COVID-19 dynamics and SARS-CoV2 positive and hospitalizations of those that were vaccinated, we provide estimates using different scenarios of the effectiveness of the vaccines in reducing cases and severe cases.

## Methods

Daily SARS-CoV2 positive cases and severe or critical hospitalization was downloaded from the Israeli Ministry of Health COVID-19 public database.3 Number of cases and severe or critical hospitalization of vaccinated individuals stratified by time from vaccination and age was provided by the number Ministry of Health on February 7th, for all cases up to February 5th.
 
To calculate effectiveness, we first estimate the expected number of cases or hospitalizations (Supplementary Figure 1). To achieve this, we count the number of the cumulative vaccinated individuals on each day that are eligible to each of four groups – between day 0 to 13 of the first dose (group 1), between day 14 to 20 of the first dose (group 2), between day 0 to 6 of the second dose (group 3), and from day 7 of the second dose (group 4). We then multiply that count by the daily incidence rate of the whole population. The incidence rate is calculated using the number of cases divided by the population size (9.2 million). Different population sizes affect only marginally the observed effect (see https://dviraran.shinyapps.io/VaccineEffectIsrael/ to adjust population sizes). Finally, since incidence rates of the vaccinated cohort are different from the general population, we use a sensitivity parameter to adjust for the incidence rates. The effectiveness can be formulated as below:
E(β)= ∑_(i=1)^N▒〖V_i∙D_i∙β〗
Where V_i is the cumulative number of vaccinated individuals in day i, D_i  is the general population incidence rate in day i, β is the sensitivity parameter to adjust the incidence rate, and N is the number of relevant days (7 days for group 1-3, and 15 for group 4).

Supplementary Figure 1. 
![alt text](https://github.com/dviraran/covid_analyses/blob/master/supp_fig1.jpg)

Standard error is the standard error of proportion using the observed cases as a denominator.

## Results

Between December 20, 2020 and February 5th, 2021, there were 3,404,623 individuals that were vaccinated in Israel by the first dose of the BNT162b2 vaccine, of them, 1,240,423 over the age of 60. By that date, 1,993,349 have already received their second dose of the vaccine. Of all those vaccinated, 35,704 individuals have tested positive for SARS-CoV2, and 2,531 have been hospitalized due to COVID-19 and 1,244 were hospitalized with severe or critical conditions or have died (Table 1).

**Table 1. Number of cases as reported by the Ministry of Health.**

|                     | Positive cases (>60y) | Positive cases (<60y) | Hospitalization | Severe / critical / death cases |
|---------------------|--------------------|---------------------|-------------------|-------------------------|
| 1st dose, day 0-13  | 6,011                 | 16,620                | 1,467      | 684                       |
| 1st dose, day 14-20 | 4,312                 | 5,198                | 780         | 394                  |
| 2nd dose, day 0-6   | 991                   | 966                   | 154        | 98                    |
| 2nd dose, day >6    | 976                   | 630                   | 130        | 68                             |

Based on daily numbers of vaccinations and rates of general incidence we estimated expected numbers of SARS-CoV-2 positive cases, COVID-19 hospitalizations and severe cases. The analysis suggests that for individuals aged 60 and above, there was a 52% reduction in SARS-CoV2 positive cases up to day 13 of the first dose, 34% between days 14 to 20 of the first dose, 82% between the second dose and day 6 and 86% after day 7 of the second dose (Figure 1).

However, an underlying assumption here is that the incidence rates of those that were vaccinated early are similar to the general population. Previous analyses have shown that this is not the case as older populations have lower incidence and lower socio-economic groups have higher incidence.4 Therefore, we perform a sensitivity analysis by adjusting incidence rates using different levels of beta values (Figure 1). As a lower bound, we hypothesize that by day 13 of the first dose, there should not be an observed effect of the vaccine.5 This hypothesis may correct for demographic and socio-economic differences, however it does not correct for the behavioral aspect. It is assumed that on the days following the vaccination, there is increased caution to avoid social encounters. We estimate this effect by 25%, and below we report the effectiveness derived from this value as an upper value.

For positive cases of aged 60 years and above, we find the lower and upper bounds of beta to be 0.48 to 0.64, which implies that the vaccinated population are expected to have 48-64% the cases of the general population. Strikingly, the analysis suggests an increase in cases of 3-37% by day 20 of the first dose. However, after the second dose we observe a reduction of 63-72% in cases before day 7, and 71-78% reduction after a week. For individuals aged below 60 years, the empirical beta values are 0.88 to 1.17. Here we see reduction of 1-13% between days 14 and 20, 73-77% reduction after the second dose, and 81-83% reduction after day 7. 

Similarly, we perform the analysis for hospitalizations and severe cases. For hospitalizations our analysis suggests beta values of 3.82 to 5.1, which imply that those vaccinated have 3.82 to 5.1-fold more chance of being hospitalized. These values fit with the demographics of the vaccinated cohort. Again, we see a mild effect of -4% to 20% by day 20. After the second dose we find 76-82% reduction in hospitalizations, which increase to 83-87% reduction a week later. For severe case we estimate the beta values to be between 3.15 and 4.2. Up to day 20 we find -8% to 19% reduction in severe cases, and after the second dose we observe a reduction of 70-77% and 83-87% after day 7. 	

![alt text](https://github.com/dviraran/covid_analyses/blob/master/figure1.2.5.2021.jpg)

## Discussion

The randomized clinical trial (RCT) of BNT162b2 has suggested efficacy of 95% a week after the second dose and unclear efficacy earlier.6 It also suggested differences between the older and younger population, but with high levels standard errors. In addition, the clinical trial was performed on a relatively small population, as by February 5th, in Israel alone 155-fold more individuals have been vaccinated. Therefore, real-world data effectiveness is of high interest and important for decision-makers and mobilizing individuals to get the vaccine. Our sensitivity analysis provides an estimate for the effectiveness of the vaccine in reducing positive cases, hospitalizations and severe cases. While this estimates are lower than the efficacy of the RCT, it is still substantive and provides reassurance for the vaccine efficacy. 

Our sensitivity analysis provides estimate of the effectiveness of the vaccine under different scenarios. Our empirical approach to identify the beta values, which combines demographic, socio-economic and additional behavioral aspects, provides lower and upper bound estimate of the effectiveness. We report here estimate effectiveness 71-81% in reducing positive cases in individuals older than 60 years, effectiveness of 81-83% for individuals younger than 60 years, 83-87% reduction in hospitalizations, and similar effectiveness in preventing severe cases. 

Our analysis suggest that the vaccine does not provide protection in days 14-20 after the first dose, as we only observe substantive effectiveness in days 0-6 of the second dose, which is administered in Israel on the 21st day after the first dose of the vaccine. We cannot differentiate here between the possibility that the first dose is effective but only after three weeks, or that the vaccine is only protective following the second dose of the vaccine. However, there is some preliminary evidence to support that the single dose is effective after three weeks.7

In Israel, individuals may get tested for SARS-CoV-2 for any reason, not just with symptoms, thus, the positive cases come from both symptomatic and asymptomatic individuals. This is different from the clinical trial, where only symptomatic individuals with suspected COVID-19 where tested. It might explain some of the difference in effectiveness we observe in Israel regarding positive cases. However, the lower effectiveness we observe for hospitalization and severe cases is alarming. 

It is important to note that our estimates of effectiveness in reducing the disease should not be confused by effectiveness in reducing transmission. As noted, we cannot exclude the possibility that vaccinated individuals may still get infected by SARS-CoV-2 and stay asymptomatic or with mild symptoms and will therefore not get tested. However, other studies have shown reduction in Ct values of the PCR test due to the vaccination, suggesting lower viral load, and in turn reduced transmission.8

Our analysis suffers from many limitations. First, all analyses are performed on aggregated counts, which limits the possibilities to make individual-level inferences. Second, hospitalizations and severe case may accumulate with time, as some of the patients will deteriorate later on. The number of new vaccinated individuals have been relatively low in the last week of our study, therefore this issue should not have a major effect on our estimates. Third, in Israel there is an incentive to get tested if you are required to be in isolation due to contact with an infected individual, and as note above some asymptomatic individuals are identified. However, this incentive is reduced 7 days after the second dose, as the regulations now exempts those from isolation. Thus, there is a difference in testing rates of asymptomatic individuals between groups. It is reassuring to see that there are relatively similar levels of effectiveness of those 7 days after the second dose to those before those 7 days, suggesting that this testing incentive has only a marginal effect. Fourth, the general population incidence is also affected by the vaccination roll-out, as more individuals are vaccinated, the incidence is expected to be affected by the vaccination; thus the real effectiveness might be higher than our estimates.

Other attempts to identify the impact of the vaccination campaign in Israel are underway. Chodik et al. compared cases in vaccinated individuals on days 13-24 after the first dose with vaccinated individuals in days 0-12.5 Rossman et al. used a natural experiment approach to compare early and late vaccinated cities and differences in the prioritization for the vaccine between age groups.4 Our contribution here is the use of the general population as a control group to assess the effectiveness. 

In conclusion, this study provides the first attempt to estimate the effectiveness of the BNT162b2 vaccine on a population level compared to the general population. Our analysis provides strong reassurance that the vaccine is highly effective. With more data that will be shared with the public we believe that more accurate estimation can be calculated.


## Data and code availability

All data is public and can be downloaded from https://data.gov.il/dataset/covid-19 or Ministry of Health press releases. Data and code used in the analyses was deposited in https://github.com/dviraran/covid_analyses. In addition, we provide an interactive shiny app, which will be updated as more data is available by the Ministry of Health - https://dviraran.shinyapps.io/VaccineEffectIsrael/. 

## Acknowledgments

DA is supported by the Azrieli Faculty Fellowship and is an Andre Deloro Fellow. We thank Uri Shalit for his valuable comments.

## Funding

There was no specific funding for this study.


