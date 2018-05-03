library(data.table)
library(ggplot2)
library(foreign)
library(haven)
library(stringr)

## Load CPS IPUMS extract and codebooks.
cps <- as.data.table(read_dta("C:/Users/ngraetz/Downloads/cps_00007.dta/cps_00007.dta"))
bpl_cb <- fread("C:/Users/ngraetz/Documents/repos/cps_2018/birthplace_cb.csv")
bpl_cb[, birth_country := str_split_fixed(bpl_cb$BPL, "           ", 2)[,2]]
bpl_cb[, BPL := str_split_fixed(bpl_cb$BPL, "           ", 2)[,1]]
bpl_cb <- bpl_cb[birth_region!="", ]        
setnames(bpl_cb, 'BPL', 'bpl')   

## Merge respondent birthplace.
cps[, bpl := as.character(bpl)]
cps[, mbpl := as.character(mbpl)]
cps[, fbpl := as.character(fbpl)]
cps[bpl == '9900', bpl := '09900']
cps[mbpl == '9900', mbpl := '09900']
cps[fbpl == '9900', fbpl := '09900']
cps_mapped <- merge(cps, bpl_cb, by='bpl') # 100% merge

## Make first- and second-generation categories by merging mother/father birthplace.
mothers_bpl_cb <- copy(bpl_cb)
setnames(mothers_bpl_cb, 'bpl', 'mbpl')
setnames(mothers_bpl_cb, 'birth_region', 'm_birth_region')
setnames(mothers_bpl_cb, 'birth_country', 'm_birth_country')
setnames(mothers_bpl_cb, 'super_birth_region', 'm_super_birth_region')
fathers_bpl_cb <- copy(bpl_cb)
setnames(fathers_bpl_cb, 'bpl', 'fbpl')
setnames(fathers_bpl_cb, 'birth_region', 'f_birth_region')
setnames(fathers_bpl_cb, 'birth_country', 'f_birth_country')
setnames(fathers_bpl_cb, 'super_birth_region', 'f_super_birth_region')

cps_mapped <- merge(cps_mapped, mothers_bpl_cb, by='mbpl') # 100% merge
cps_mapped <- merge(cps_mapped, fathers_bpl_cb, by='fbpl') # 100% merge

## Tabulate counts of first- and second-generation immigrants by region of origin.
## NATIVES AND 1ST GEN
cps_mapped[birth_region == "UNITED STATES", gen := 'third+']
cps_mapped[birth_region != "UNITED STATES", gen := 'first']

## 2ND GEN: CHILDREN OF ONE IMMIGRANT PARENT
cps_mapped[m_birth_region != "UNITED STATES" & f_birth_region == "UNITED STATES" & birth_region == "UNITED STATES", gen := 'half_second']
cps_mapped[m_birth_region != "UNITED STATES" & f_birth_region == "UNITED STATES" & birth_region == "UNITED STATES", parent_birth_region := m_birth_region]
cps_mapped[m_birth_region != "UNITED STATES" & f_birth_region == "UNITED STATES" & birth_region == "UNITED STATES", parent_super_birth_region := m_super_birth_region]

cps_mapped[m_birth_region == "UNITED STATES" & f_birth_region != "UNITED STATES" & birth_region == "UNITED STATES", gen := 'half_second']
cps_mapped[m_birth_region == "UNITED STATES" & f_birth_region != "UNITED STATES" & birth_region == "UNITED STATES", parent_birth_region := f_birth_region]
cps_mapped[m_birth_region == "UNITED STATES" & f_birth_region != "UNITED STATES" & birth_region == "UNITED STATES", parent_super_birth_region := f_super_birth_region]

## 2ND GEN: CHILDREN OF MIXED-REGION IMMIGRANTS: For now, assign children of immigrant parents from different regions to the mother's region.
cps_mapped[m_birth_region != "UNITED STATES" & f_birth_region != "UNITED STATES" & birth_region == "UNITED STATES" & f_birth_region != m_birth_region, gen := 'mixed_second']
cps_mapped[m_birth_region != "UNITED STATES" & f_birth_region != "UNITED STATES" & birth_region == "UNITED STATES" & f_birth_region != m_birth_region, parent_birth_region := m_birth_region]
cps_mapped[m_birth_region != "UNITED STATES" & f_birth_region != "UNITED STATES" & birth_region == "UNITED STATES" & f_birth_region != m_birth_region, parent_super_birth_region := m_super_birth_region]

## 2ND GEN: CHILDREN OF SAME-REGION IMMIGRANTS
cps_mapped[m_birth_region != "UNITED STATES" & f_birth_region != "UNITED STATES" & birth_region == "UNITED STATES" & f_birth_region == m_birth_region, gen := 'second']
cps_mapped[m_birth_region != "UNITED STATES" & f_birth_region != "UNITED STATES" & birth_region == "UNITED STATES" & f_birth_region == m_birth_region, parent_birth_region := m_birth_region]
cps_mapped[m_birth_region != "UNITED STATES" & f_birth_region != "UNITED STATES" & birth_region == "UNITED STATES" & f_birth_region == m_birth_region, parent_super_birth_region := m_super_birth_region]

## Create single variable for region defining generation status.
cps_mapped[gen=='third+', gen_region := birth_region]
cps_mapped[gen=='first', gen_region := birth_region]
cps_mapped[gen=='half_second', gen_region := parent_birth_region]
cps_mapped[gen=='mixed_second', gen_region := parent_birth_region]
cps_mapped[gen=='second', gen_region := parent_birth_region]
## Create single variable for region defining second-generation status.
cps_mapped[gen=='third+', gen_super_region := super_birth_region]
cps_mapped[gen=='first', gen_super_region := super_birth_region]
cps_mapped[gen=='half_second', gen_super_region := parent_super_birth_region]
cps_mapped[gen=='mixed_second', gen_super_region := parent_super_birth_region]
cps_mapped[gen=='second', gen_super_region := parent_super_birth_region]

saveRDS(cps_mapped, 'C:/Users/ngraetz/Documents/Penn/papers/cps_2018/cps_mapped.rds')

## Make table of counts, wide by year.
pop_table <- copy(cps_mapped)
pop_table <- pop_table[, c('year','gen','gen_region','gen_super_region')]
pop_table[, n := 1]

gen_index <- data.table(gen=c('third+','first','half_second','mixed_second','second'),
                        table_order=c(1:5))
pop_table <- merge(pop_table, gen_index)

table_total <- pop_table[, list(n=sum(n)), by=c('table_order','year','gen')]
table_total <- dcast(table_total, table_order + gen ~ year, value.var = "n")
table_total <- table_total[order(table_order)]

table_region <- pop_table[, list(n=sum(n)), by=c('table_order','year','gen','gen_region')]
table_region <- dcast(table_region, table_order + gen + gen_region ~ year, value.var = "n")
table_region <- table_region[order(table_order,gen_region)]

combine_second_gen <- function(dt) {
  dt[gen %in% c('half_second','mixed_second'), table_order := 5]
  dt[gen %in% c('half_second','mixed_second'), gen := 'second']
}
table_summary <- copy(pop_table)
combine_second_gen(table_summary)
table_summary <- table_summary[, list(n=sum(n)), by=c('table_order','year','gen','gen_super_region')]
table_summary <- dcast(table_summary, table_order + gen + gen_super_region ~ year, value.var = "n")
table_summary <- table_summary[order(table_order,gen_super_region)]

## Load metro codes
# metro_codes <- fread('C:/Users/ngraetz/Downloads/FIPSmetroregion.csv')
# metro_codes[, fips := as.character(fips)]
# metro_codes[nchar(fips)==4, fips := paste0('0',fips)]
# metro_codes[metroname=="Nonmetro, adjacent", metroname := 'Nonmetro']
# metro_codes[metroname=="Nonmetro, nonadjacent", metroname := 'Nonmetro']
# cps_mapped[, fips := as.character(county)]
# cps_mapped[nchar(fips)==4, fips := paste0('0',fips)]
# cps_mapped <- merge(cps_mapped, metro_codes[, c('fips','metroname','regionname')], by=c('fips'))

## Make trends by generation-super-region
## hcovany = any public/private insurance coverage (1 = not covered, 2 = covered)
color_list <- c('#e41a1c','#4daf4a','#377eb8','#984ea3','#ff7f00','#a65628','#f781bf','#999999')
cps_mapped[hcovany == 2, new_hcovany := 1]
cps_mapped[hcovany == 1, new_hcovany := 0]
hcov_table <- copy(cps_mapped)
combine_second_gen(hcov_table)
## Make insurance plot
hcov_table <- hcov_table[, list(insurance=weighted.mean(new_hcovany, asecwt)), by=c('year','gen','gen_super_region')]
hcov_gg <- ggplot() +
  geom_line(data = hcov_table[year<=2013, ], # They stopped asking after 2013 for some reason...
            aes(x = year,
                y = insurance,
                color = gen),
            size = 1) +
  scale_colour_manual(values = color_list) + 
  theme_minimal() +
  facet_wrap(~gen_super_region) + 
  labs(y = 'Insurance coverage, any public/private',
       x = 'Year',
       color = 'Generation',
       title = 'Any public/private insurance coverage by generational status and super-region\nAll-age/all-sex, 1994-2013.')

## Poverty from SPM group 
spm <- fread('C:/Users/ngraetz/Downloads/pub_spm_master_csv/pub_spm_master.csv')
spm[, age := a_age]
spm[, sex := a_sex]
spm[, spm_year := year]
## Per SPM documentation, the years are different (SPM is year of the actual survey)
cps_mapped[, spm_year := year - 1]
cps_mapped[, age := as.numeric(age)]
cps_mapped[, sex := as.character(sex)]
cps_mapped[sex=='1', sex := 'Male']
cps_mapped[sex=='2', sex := 'Female']
cps_spm <- merge(cps_mapped, spm, by = c('spm_year','serial','lineno','sex','age'))
cps_spm[, fips := as.character(county)]
cps_spm[nchar(fips)==4, fips := paste0('0',fips)]
## Make SPM plot
combine_second_gen(cps_spm)
cps_spm <- cps_spm[, list(poverty=weighted.mean(SPMu_Poor_Metadj_anch_cen, SPMu_Weight)), by=c('spm_year','gen','gen_super_region')]
spm_gg <- ggplot() +
  geom_line(data = cps_spm, # They stopped asking after 2013 for some reason...
            aes(x = spm_year,
                y = poverty,
                color = gen),
            size = 1) +
  scale_colour_manual(values = color_list) + 
  theme_minimal() +
  facet_wrap(~gen_super_region) + 
  labs(y = 'Percent below SPM threshold',
       x = 'Year',
       color = 'Generation',
       title = 'Percent living under Supplemental Poverty Measure threshold\n(accounting for cost of living, taxes, transfers, etc.) by generational status and super-region\nAll-age/all-sex, 1993-2015.')

## Educational attainment
educ_cb <- fread('C:/Users/ngraetz/Documents/repos/cps_2018/educ_cb.csv')
cps_educ <- copy(cps_mapped)
cps_educ <- cps_educ[age >= 25, ]
cps_educ <- merge(cps_mapped, educ_cb, by='educ')
cps_educ[educ_cat %in% c('College','Graduate'), college := 1]
cps_educ[is.na(college), college := 0]
combine_second_gen(cps_educ)
## Make education plot
educ_table <- cps_educ[, list(college=weighted.mean(college, asecwt)), by=c('year','gen','gen_super_region')]
educ_gg <- ggplot() +
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
       title = 'Percent with college degree by generational status and super-region\nAges 25+/all-sex, 1994-2017.')

## Split out native population by race (Non-Hispanic White, Black, Hispanic, Asian)
## Not Hispanic: hispan %in% c(000,901,902)
## Race: 100 = White, 200 = Black, 650/651 = Asian
native_by_race <- copy(cps_mapped)
native_by_race[, hispan := as.character(hispan)]
native_by_race[, race := as.character(race)]
native_by_race[gen=='third+' & hispan %in% c('0','901','902') & race == '100', native_by_race := 'White']
native_by_race[gen=='third+' & hispan %in% c('0','901','902') & race == '200', native_by_race := 'Black']
native_by_race[gen=='third+' & hispan %in% c('0','901','902') & race %in% c('650','651'), native_by_race := 'Asian']
native_by_race[gen=='third+' & !(hispan %in% c('0','901','902')), native_by_race := 'Hispanic']
native_by_race[gen=='third+' & is.na(native_by_race), drop := 1]
native_by_race <- native_by_race[is.na(drop), ]
native_by_race[gen=='third+', gen := paste0(gen, ': ', native_by_race)]
combine_second_gen(native_by_race)
native_by_race[, gen := as.factor(gen)]
native_by_race[, gen := factor(gen, levels = c("first","second","third+: Asian","third+: Black","third+: Hispanic","third+: White"))]

cps_educ <- copy(native_by_race)
cps_educ <- cps_educ[age >= 25, ]
cps_educ <- merge(native_by_race, educ_cb, by='educ')
cps_educ[educ_cat %in% c('College','Graduate'), college := 1]
cps_educ[is.na(college), college := 0]
combine_second_gen(cps_educ)
## Make education plot
educ_table <- cps_educ[, list(college=weighted.mean(college, asecwt)), by=c('year','gen','gen_super_region')]
race_educ_gg <- ggplot() +
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
       title = 'Percent with college degree by generational status and super-region\nAges 25+/all-sex, 1994-2017.')

cps_spm <- merge(native_by_race, spm, by = c('spm_year','serial','lineno','sex','age'))
cps_spm[, fips := as.character(county)]
cps_spm[nchar(fips)==4, fips := paste0('0',fips)]
## Make SPM plot
combine_second_gen(cps_spm)
cps_spm <- cps_spm[, list(poverty=weighted.mean(SPMu_Poor_Metadj_anch_cen, SPMu_Weight)), by=c('spm_year','gen','gen_super_region')]
race_spm_gg <- ggplot() +
  geom_line(data = cps_spm, # They stopped asking after 2013 for some reason...
            aes(x = spm_year,
                y = poverty,
                color = gen),
            size = 1) +
  scale_colour_manual(values = color_list) + 
  theme_minimal() +
  facet_wrap(~gen_super_region) + 
  labs(y = 'Percent below SPM threshold',
       x = 'Year',
       color = 'Generation',
       title = 'Percent living under Supplemental Poverty Measure threshold\n(accounting for cost of living, taxes, transfers, etc.) by generational status and super-region\nAll-age/all-sex, 1993-2015.')

hcov_table <- copy(native_by_race)
combine_second_gen(hcov_table)
## Make insurance plot
hcov_table <- hcov_table[, list(insurance=weighted.mean(new_hcovany, asecwt)), by=c('year','gen','gen_super_region')]
race_hcov_gg <- ggplot() +
  geom_line(data = hcov_table[year<=2013, ], # They stopped asking after 2013 for some reason...
            aes(x = year,
                y = insurance,
                color = gen),
            size = 1) +
  scale_colour_manual(values = color_list) + 
  theme_minimal() +
  facet_wrap(~gen_super_region) + 
  labs(y = 'Insurance coverage, any public/private',
       x = 'Year',
       color = 'Generation',
       title = 'Any public/private insurance coverage by generational status and super-region\nAll-age/all-sex, 1994-2013.')

pdf('C:/Users/ngraetz/Documents/Penn/papers/cps_2018/trends_by_generational_status.pdf', height=8, width=10)
print(educ_gg)
print(spm_gg)
print(hcov_gg)
dev.off()
pdf('C:/Users/ngraetz/Documents/Penn/papers/cps_2018/trends_by_generational_status_race.pdf', height=8, width=10)
print(race_educ_gg)
print(race_spm_gg)
print(race_hcov_gg)
dev.off()
write.csv(table_region, 'C:/Users/ngraetz/Documents/Penn/papers/cps_2018/count_table_detailed.csv', row.names = FALSE)
write.csv(table_total, 'C:/Users/ngraetz/Documents/Penn/papers/cps_2018/count_table_gen_totals.csv', row.names = FALSE)
write.csv(table_summary, 'C:/Users/ngraetz/Documents/Penn/papers/cps_2018/count_table_region_totals.csv', row.names = FALSE)



cps_educ <- copy(native_by_race)
combine_second_gen(cps_educ)
## Make education plot
educ_table <- cps_educ[, list(health=weighted.mean(health, asecwt)), by=c('year','gen','native_by_race')]
race_educ_gg <- ggplot() +
  geom_line(data = educ_table, # They stopped asking after 2013 for some reason...
            aes(x = year,
                y = health,
                color = gen),
            size = 1) +
  scale_colour_manual(values = color_list) + 
  theme_minimal() +
  facet_wrap(~native_by_race) + 
  labs(y = 'Percent with college degree',
       x = 'Year',
       color = 'Generation',
       title = 'Percent with college degree by generational status and super-region\nAges 25+/all-sex, 1994-2017.')

