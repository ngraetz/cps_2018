library(data.table)
library(ggplot2)

## Look at differences across self-reported race by generational status.
cps_mapped <- readRDS('C:/Users/ngraetz/Dropbox/Penn/papers/cps_2018/cps_mapped.rds')
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

cps_model_input <- copy(native_by_race)
## Combine second-generation subgroups
combine_second_gen(cps_model_input)
## Subset to working ages
#cps_model_input <- cps_model_input[age >= 15 & age <= 64, ]
#cps_model_input <- cps_model_input[!(gen_super_region_f %in% c('Other North America','Oceania')),]
## Define indicators of interest
cps_model_input[, empstat_binary := ifelse(empstat %in% c(20,21,22),1,0)]
educ_cb <- fread('C:/Users/ngraetz/Documents/repos/cps_2018/educ_cb.csv')
cps_model_input <- merge(cps_model_input, educ_cb, by='educ')
cps_model_input[, college := ifelse(educ_cat %in% c('College','Graduate'), 1, 0)]
# spm <- fread('C:/Users/ngraetz/Downloads/pub_spm_master_csv/pub_spm_master.csv')
# spm[, age := a_age]
# spm[, sex := a_sex]
# spm[, spm_year := year]
# spm[, year := NULL]
# cps_model_input[, spm_year := year - 1]
# cps_model_input <- merge(cps_model_input, spm, by = c('spm_year','serial','lineno','sex','age'))
cps_model_input[, health_binary := ifelse(health %in% c(5,4),1,0)]
#cps_model_input[, poverty := SPMu_Poor_Metadj_anch_cen]
cps_model_input[, spm_year := NULL]

## Subset ages, index by birth cohort and count individuals by birth cohort, race, nativity, region of origin.
model_data <- cps_model_input[age >= 24 & age <= 45, ]
model_data[, birth_cohort := year - age]
for(d in seq(1920,1990,10)) model_data[birth_cohort>=d, birth_cohort_group := d]
model_data[, N := 1]
model_data <- model_data[!is.na(birth_cohort_group), ]
setnames(model_data, c('birth_cohort_group','native_by_race','gen_super_region'), c('cohort','racial_group','gen_super_region'))
#totals <- model_data[, list(N=sum(N),college=sum(college),health_binary=sum(health_binary),empstat_binary=sum(empstat_binary)), by=c('cohort','gen','racial_group')]
totals <- model_data[, lapply(.SD, weighted.mean, w=asecwt, na.rm=TRUE), by=c('cohort','gen','racial_group'), .SDcols=c('college','health_binary','empstat_binary') ]
total_n <- model_data[, list(N=sum(N)), by=c('cohort','gen','racial_group')]
totals <- merge(totals, total_n, by=c('cohort','gen','racial_group'))
setcolorder(totals, c('racial_group','cohort','gen','N','college','health_binary','empstat_binary'))
totals <- totals[order(racial_group,gen,cohort)]
totals <- totals[gen != 'first', ]

plot_data <- copy(totals)
plot_data[, race_gen := paste0(racial_group,'_',gen)]
ggplot() + 
  geom_line(data=plot_data,
            aes(x=cohort,
                y=empstat_binary,
                color=race_gen),
            size=3) + 
  theme_minimal()
