# cultureobesity

## Overview of repository

The script `R/run_analysis.R` produces all results and figures from the article, sourcing the functions defined in `R/helper_functions.R`. Readily produced results are already stored in the results folder. The data loaded into the script are in `data/data_51countries.csv` (data for main analysis) and `data/data_190countries.csv` (data for missingness analysis and imputations).

## Definitions of variables

The following table explains all variables in the data file `data_51countries.csv`. Further details, summary statistics and sources concerning these variables can be found in the article.

| Column name                 | Explanation                                               
|-----------------------------|-----------------------------------------------------------
| country                     | Name of country     
| country_short               | ISO-3 country code     
| region_wb                   | World Region according to the World Bank  
| obesity_m                   | Obesity rate (BMI >= 30) among male population
| obesity_m                   | Obesity rate (BMI >= 30) among female population                                
| overw_m                     | Overweight rate (BMI >= 25) among male population                                
| overw_f                     | Overweight rate (BMI >= 25) among female population      
| BMI_m                       | Average BMI among male population
| BMI_f                       | Average BMI among female population
| flex                        | Flexibility (according to Minkov-Hofstede)
| indiv                       | Individualism (according to Minkov-Hofstede)
| yr_sch_m                    | Average years of schooling among male population
| yr_sch_f                    | Average years of schooling among female population
| globaliz                    | KOF Globalisation Index
| uneven_eco_dev              | E2: Uneven Economic Development Indicator (Fragile States Index)
| gdppc_ln                    | Natural logarithm of GDP per capita in USD, PPP
| undernourish                | Undernourished population share
| rice                        | Rice supply in kg/person/year
| mcdonalds                   | Number of McDonald's restaurants per 100k
| temp_avg                    | Average temperature 1961-1990 in degree Celcius
| wgi_avg                     | Average of all Worldwide Governance Indicators
| share_animal_prot           | Share of animal protein in % of calories
| vegies_fruits               | Vegetables/fruit supply in kg/person/year
| hf_efiscore                 | Economic Freedom Index (Heritage Foundation)
| wdi_emp_agr_m               | Employment share in agriculture among male population
| wdi_emp_ind_m               | Employment share in industry among male population
| wdi_emp_ser_m               | Employment share in services among male population
| wdi_emp_agr_f               | Employment share in agriculture among female population
| wdi_emp_ind_f               | Employment share in industry among female population
| wdi_emp_ser_f               | Employment share in services among female population
| healthexp_perc_gdp          | Health expenditure as % of GDP
| wdi_popurb_share            | Urban population share
| wdi_smok_m                  | Smoking prevalence among male population
| wdi_smok_f                  | Smoking prevalence among female population
| indiv_welzel                | Individualism (according to Beugelsdijk & Welzel)
| self_expr_val               | Self-expression values
| emancip_val                 | Emancipative Values Index
| autonomy                    | Autonomy values
| indiv_hofstd                | Individualism (according to Hofstede)
