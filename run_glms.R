library(data.table)
library(ggplot2)
library(survey)

## Look at differences across self-reported race by generational status.
cps_mapped <- readRDS('C:/Users/ngraetz/Documents/Penn/papers/cps_2018/cps_mapped.rds')
native_by_race <- copy(cps_mapped)
native_by_race[, hispan := as.character(hispan)]
native_by_race[, race := as.character(race)]
native_by_race[hispan %in% c('0','901','902') & race == '100', native_by_race := 'White']
native_by_race[hispan %in% c('0','901','902') & race == '200', native_by_race := 'Black']
native_by_race[hispan %in% c('0','901','902') & race %in% c('650','651'), native_by_race := 'Asian']
native_by_race[!(hispan %in% c('0','901','902')), native_by_race := 'Hispanic']
native_by_race[is.na(native_by_race), drop := 1]
native_by_race <- native_by_race[is.na(drop), ]
## Combine second-generation subgroups.
combine_second_gen <- function(dt) {
  dt[gen %in% c('half_second','mixed_second'), table_order := 5]
  dt[gen %in% c('half_second','mixed_second'), gen := 'second']
}
combine_second_gen(native_by_race)
## Make race binary variables for proportion collapse.
dummy_vars <- function(dt, var, prefix) {
  values <- unique(dt[, get(var)])
  for(v in values) dt[, (paste0(prefix,v)) := ifelse(get(var) == v, 1, 0)]
}
dummy_vars(native_by_race, 'native_by_race', 'race_prop_')
dummy_vars(native_by_race, 'gen_super_region', 'gen_super_region_prop_')
dummy_vars(native_by_race, 'gen', 'gen_prop_')

## Fit GLMs
native_by_race[, gen_f := factor(gen, levels = c('third+', 'first','second'))]
native_by_race[, sex := as.character(sex)]
native_by_race[sex=='1', sex := 'Male']
native_by_race[sex=='2', sex := 'Female']
native_by_race[, sex_f := factor(gen, levels = c('Male','Female'))]
native_by_race[, age := as.numeric(age)]
native_by_race[, health := as.numeric(health)]
native_by_race[, metro := as.character(metro)]
native_by_race[metro==0, metro := 'Not identifiable']	
native_by_race[metro==1, metro := 'Not in metro area']	
native_by_race[metro==2, metro := 'Metro, central city']	
native_by_race[metro==3, metro := 'Metro, outside central city']	
native_by_race[metro==4, metro := 'Metro, central status unknown']	
native_by_race <- native_by_race[metro != 'Not identifiable',]
native_by_race[, metro_f := factor(metro, levels = c('Metro, central city','Metro, outside central city','Metro, central status unknown',
                                                     'Not in metro area'))]
native_by_race[, gen_super_region_f := as.factor(gen_super_region)]
native_by_race[, region := as.character(region)]
native_by_race[region == '11', region := 'New England']
native_by_race[region == '12', region := 'Middle Atlantic']
native_by_race[region == '21', region := 'East North Central']
native_by_race[region == '22', region := 'West North Central']
native_by_race[region == '31', region := 'South Atlantic']
native_by_race[region == '32', region := 'East South Central']
native_by_race[region == '33', region := 'West South Central']
native_by_race[region == '41', region := 'Mountain']
native_by_race[region == '42', region := 'Pacific']
native_by_race[, region_f := as.factor(region)]

cps_model_input <- copy(native_by_race)
## Combine second-generation subgroups
combine_second_gen(cps_model_input)
## Subset to working ages
cps_model_input <- cps_model_input[age >= 15 & age <= 64, ]
cps_model_input <- cps_model_input[!(gen_super_region_f %in% c('Other North America','Oceania')),]
## Define indicators of interest
cps_model_input[, empstat_binary := ifelse(empstat %in% c(20,21,22),1,0)]
educ_cb <- fread('C:/Users/ngraetz/Documents/repos/cps_2018/educ_cb.csv')
cps_model_input <- merge(cps_model_input, educ_cb, by='educ')
cps_model_input[, college := ifelse(educ_cat %in% c('College','Graduate'), 1, 0)]
spm <- fread('C:/Users/ngraetz/Downloads/pub_spm_master_csv/pub_spm_master.csv')
spm[, age := a_age]
spm[, sex := a_sex]
spm[, spm_year := year]
spm[, year := NULL]
cps_model_input[, spm_year := year - 1]
cps_model_input <- merge(cps_model_input, spm, by = c('spm_year','serial','lineno','sex','age'))
cps_model_input[, health_binary := ifelse(health %in% c(5,4),1,0)]
cps_model_input[, poverty := SPMu_Poor_Metadj_anch_cen]
cps_model_input[, spm_year := NULL]

## Make Table 1


## Run models - indicators of interest: health, unemployment, college education, poverty (SPMu_Poor_Metadj_anch_cen, use SPMu_Weight)
cps.design <-
  svydesign(
    id = ~cpsid ,
    strata = ~year ,
    data = cps_model_input[native_by_race == 'Black', ],
    weights = ~asecwt,
    nest = TRUE
  )
mysvyglm <- svyglm(poverty ~ year + age + as.factor(sex) +
                     region_f + metro_f + gen_super_region_f + gen_f + college, cps.design, family='quasibinomial') ## Doc says this is a bug that "binomial" doesn't work, but this gives same results.
summary(mysvyglm)
## Make Table 2
