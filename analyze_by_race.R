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

## Examine at shifting racial composition by super-region of origin for first-/second-generation.
race_comp <- native_by_race[, lapply(.SD, weighted.mean, w=asecwt, na.rm=TRUE), by=c('year','gen','gen_super_region'), .SDcols=grep("race_prop_", names(native_by_race)) ]
race_comp = melt(race_comp,
                 id.vars = c('year','gen','gen_super_region'),
                 measure.vars = grep("race_prop_", names(race_comp)),
                 value.name = 'prop', variable.name = 'race')
color_list <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999')
race_comp[, race := gsub('race_prop_','',race)]
race_prop_gg <- ggplot() +
  geom_line(data = race_comp, 
            aes(x = year,
                y = prop,
                color = race),
            size = 1) +
  scale_colour_manual(values = color_list) + 
  facet_wrap(~gen_super_region + gen) + 
  theme_minimal() + 
  labs(y = 'Proportion',
       x = 'Year',
       color = 'Race',
       title = 'Self-identified race by generational status and super-region of origin\nAll-age/all-sex, 1994-2017.')

race_comp <- native_by_race[, lapply(.SD, weighted.mean, w=asecwt, na.rm=TRUE), by=c('year','gen','native_by_race'), .SDcols=grep("gen_super_region_prop_", names(native_by_race))]
race_comp = melt(race_comp,
                 id.vars = c('year','gen','native_by_race'),
                 measure.vars = grep("gen_super_region_prop_", names(race_comp)),
                 value.name = 'prop', variable.name = 'gen_super_region')
race_comp[, gen_super_region := gsub('gen_super_region_prop_','',gen_super_region)]
race_prop_gg2 <- ggplot() +
  geom_line(data = race_comp[gen != 'third+'], 
            aes(x = year,
                y = prop,
                color = gen_super_region),
            size = 1) +
  scale_colour_manual(values = color_list) + 
  facet_wrap(~native_by_race + gen, ncol=2) + 
  theme_minimal() + 
  labs(y = 'Proportion',
       x = 'Year',
       color = 'Race',
       title = 'Super-region of origin by generational status, self-identified race\nAll-age/all-sex, 1994-2017.')
pdf('C:/Users/ngraetz/Documents/Penn/papers/cps_2018/01a_racial_composition_by_generation_region.pdf', height=8, width=10)
print(race_prop_gg)
dev.off()
pdf('C:/Users/ngraetz/Documents/Penn/papers/cps_2018/01b_racial_composition_by_generation_region.pdf', height=8, width=10)
print(race_prop_gg2)
dev.off()

## Examine shifting country of origin composition by super-region of origin for first-/second-generation.
cps_mapped_country <- readRDS('C:/Users/ngraetz/Documents/Penn/papers/cps_2018/cps_mapped_by_country.rds')
cps_mapped_country[gen %in% c('half_second','mixed_second'), table_order := 5]
cps_mapped_country[gen %in% c('half_second','mixed_second'), gen := 'second']
var <- 'gen_country'
prefix <- 'gen_country_prop_'
values <- unique(cps_mapped_country[, get(var)])
for(v in values) {
  message(v)
  cps_mapped_country[, (paste0(prefix,v)) := ifelse(get(var) == v, 1, 0)]
}
country_comp <- cps_mapped_country[, lapply(.SD, weighted.mean, w=asecwt, na.rm=TRUE), by=c('year','gen','gen_super_region'), .SDcols=grep("gen_country_prop_", names(cps_mapped_country))]
country_comp = melt(country_comp,
                 id.vars = c('year','gen','gen_super_region'),
                 measure.vars = grep("gen_country_prop_", names(country_comp)),
                 value.name = 'prop', variable.name = 'gen_country_prop')
country_comp[, gen_country_prop := gsub('gen_country_prop_','',gen_country_prop)]
country_comp <- country_comp[prop != 0,]
## Check if a country is ever below 5% in a super-region/generation, and if it is then code that country to "other" for all years in that super-region/generation.
for(s in unique(country_comp[, gen_super_region])) {
  for(g in unique(country_comp[gen_super_region==s, gen])) {
    for(c in unique(country_comp[gen_super_region==s & gen==g, gen_country_prop]))
    if(min(country_comp[gen==g & gen_super_region==s & gen_country_prop==c, prop])<0.05) country_comp[gen==g & gen_super_region==s & gen_country_prop==c, gen_country_prop := 'Other']
  }
}
for(s in unique(country_comp[, gen_super_region])) message(length(unique(country_comp[gen_super_region==s, gen_country_prop])))
country_comp <- country_comp[, list(prop=sum(prop)), by=c('year','gen','gen_super_region','gen_country_prop')]
color_list <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999')
country_gg <- function(s) {
country_gg <- ggplot() +
  geom_line(data = country_comp[gen_super_region==s, ], # They stopped asking after 2013 for some reason...
            aes(x = year,
                y = prop,
                color = gen_country_prop),
            size = 1) +
  scale_colour_manual(values = color_list) + 
  theme_minimal() +
  facet_wrap(~gen) + 
  labs(y = 'Proportion',
       x = 'Year',
       color = 'Country of origin',
       title = paste0(s,'\nAll-age/all-sex, 1994-2017.'))
return(country_gg)
}
ggs <- lapply(unique(country_comp[, gen_super_region]), country_gg)
pdf('C:/Users/ngraetz/Documents/Penn/papers/cps_2018/country_composition_by_generation_region.pdf', height=8, width=10)
for(i in 2:length(ggs)) print(ggs[[i]])
dev.off()

## Composition of generational-status within self-reported race.
race_comp <- native_by_race[, lapply(.SD, weighted.mean, w=asecwt, na.rm=TRUE), by=c('year','native_by_race'), .SDcols=grep("gen_prop_", names(native_by_race)) ]
race_comp = melt(race_comp,
                 id.vars = c('year','native_by_race'),
                 measure.vars = grep("gen_prop_", names(race_comp)),
                 value.name = 'prop', variable.name = 'gen')
color_list <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999')
race_comp[, gen := gsub('gen_prop_','',gen)]
race_prop_gg <- ggplot() +
  geom_line(data = race_comp, 
            aes(x = year,
                y = prop,
                color = gen),
            size = 1) +
  scale_colour_manual(values = color_list) + 
  facet_wrap(~native_by_race) + 
  theme_minimal() + 
  labs(y = 'Proportion',
       x = 'Year',
       color = 'Generation',
       title = 'Generational composition for each self-identified race group\nAll-age/all-sex, 1994-2017.')

pdf('C:/Users/ngraetz/Documents/Penn/papers/cps_2018/gen_composition_by_race.pdf', height=8, width=10)
print(race_prop_gg)
dev.off()

## Education by generational status within race.
educ_cb <- fread('C:/Users/ngraetz/Documents/repos/cps_2018/educ_cb.csv')
cps_educ <- copy(native_by_race)
cps_educ <- cps_educ[age > 25, ]
cps_educ <- merge(cps_educ, educ_cb, by='educ')
cps_educ[educ_cat %in% c('College','Graduate'), college := 1]
cps_educ[is.na(college), college := 0]
combine_second_gen(cps_educ)
## Make education plot
educ_table <- cps_educ[, list(college=weighted.mean(college, asecwt)), by=c('year','gen','gen_super_region')]
educ_gg1 <- ggplot() +
  geom_line(data = educ_table, # They stopped asking after 2013 for some reason...
            aes(x = year,
                y = college,
                color = gen),
            size = 1) +
  scale_colour_manual(values = color_list) + 
  theme_minimal() +
  facet_wrap(~gen_super_region) + 
  labs(y = 'Percent with college degree',
       x = 'Year',
       color = 'Generation',
       title = 'Percent with college degree by generational status within super-region of origin\nAges 25+/all-sex, 1994-2017.')

educ_table <- cps_educ[, list(college=weighted.mean(college, asecwt)), by=c('year','gen','native_by_race')]
educ_gg2 <- ggplot() +
  geom_line(data = educ_table, # They stopped asking after 2013 for some reason...
            aes(x = year,
                y = college,
                color = gen),
            size = 1) +
  scale_colour_manual(values = color_list) + 
  theme_minimal() +
  facet_wrap(~native_by_race) + 
  labs(y = 'Percent with college degree',
       x = 'Year',
       color = 'Generation',
       title = 'Percent with college degree by generational status within self-identified race\nAges 25+/all-sex, 1994-2017.')
pdf('C:/Users/ngraetz/Documents/Penn/papers/cps_2018/03a_gen_composition_of_educ_by_race.pdf', height=8, width=10)
print(educ_gg1)
dev.off()
pdf('C:/Users/ngraetz/Documents/Penn/papers/cps_2018/03b_gen_composition_of_educ_by_race.pdf', height=8, width=10)
print(educ_gg2)
dev.off()



## Education by generational status within race.
cps_educ <- copy(native_by_race)
combine_second_gen(cps_educ)
## Make education plot
cps_educ[, marst_binary := ifelse(marst>2,0,1)]
cps_educ[, quitsick_binary := ifelse(quitsick==1,1,0)]
educ_table <- cps_educ[, list(marst=weighted.mean(quitsick_binary, asecwt)), by=c('year','gen','native_by_race')]
ggplot() +
  geom_line(data = educ_table, # They stopped asking after 2013 for some reason...
            aes(x = year,
                y = marst,
                color = gen),
            size = 1) +
  scale_colour_manual(values = color_list) + 
  theme_minimal() +
  facet_wrap(~native_by_race) + 
  labs(y = 'Percent with college degree',
       x = 'Year',
       color = 'Generation',
       title = 'Percent with college degree by generational status within self-identified race\nAges 25+/all-sex, 1994-2017.')

## Education by generational status within race.
cps_educ <- copy(native_by_race)
combine_second_gen(cps_educ)
cps_educ <- cps_educ[age > 25, ]
## Make education plot
cps_educ[, empstat_binary := ifelse(empstat %in% c(20,21,22),1,0)]
educ_table <- cps_educ[, list(empstat_binary=weighted.mean(empstat_binary, asecwt)), by=c('year','gen','native_by_race')]
ggplot() +
  geom_line(data = educ_table, # They stopped asking after 2013 for some reason...
            aes(x = year,
                y = empstat_binary,
                color = gen),
            size = 1) +
  scale_colour_manual(values = color_list) + 
  theme_minimal() +
  facet_wrap(~native_by_race) + 
  labs(y = 'Percent with college degree',
       x = 'Year',
       color = 'Generation',
       title = 'Percent with college degree by generational status within self-identified race\nAges 25+/all-sex, 1994-2017.')


library(INLA)
inla.setOption("enable.inla.argument.weights", TRUE)
native_by_race[, gen_f := factor(gen, levels = c('third+', 'first','second'))]
native_by_race[, age := as.numeric(age)]
native_by_race[, sex := as.numeric(sex)]
native_by_race[, health := as.numeric(health)]
inla_formula <- health ~ age + as.factor(sex) + gen_f
inla_input <- native_by_race[year==2015, c('health','age','sex','gen_f')]
inla_weights <- native_by_race[year==2015, asecwt]
inla_model <- inla(inla_formula,
                    family = "gaussian",
                    data = inla_input,
                    weights = inla_weights,
                    verbose = TRUE,
                    #control.compute=list(config = TRUE, dic = TRUE),
                    control.inla=list(int.strategy='eb'))


library(survey)
cps_model_input <- copy(native_by_race)
combine_second_gen(cps_model_input)
cps_model_input <- cps_model_input[age > 25, ]
cps_model_input[, empstat_binary := ifelse(empstat %in% c(20,21,22),1,0)]
cps.design <-
  svydesign(
    id = ~cpsid ,
    strata = ~year ,
    data = cps_model_input[!is.na(empstat_binary),] ,
    weights = ~asecwt,
    nest = TRUE
  )
mysvyglm <- svyglm(empstat_binary ~ age + sex + gen_f + year, cps.design, family='quasibinomial') ## Doc says this is a bug that "binomial" doesn't work, but this gives same results.
