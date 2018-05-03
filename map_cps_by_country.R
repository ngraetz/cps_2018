library(data.table)
library(ggplot2)
library(foreign)
library(haven)
library(stringr)

## Load CPS IPUMS extract and codebooks.
cps <- as.data.table(read_dta("C:/Users/ngraetz/Downloads/cps_00006.dta/cps_00006.dta"))
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
cps_mapped[m_birth_region != "UNITED STATES" & f_birth_region == "UNITED STATES" & birth_region == "UNITED STATES", parent_birth_country := m_birth_country]
cps_mapped[m_birth_region != "UNITED STATES" & f_birth_region == "UNITED STATES" & birth_region == "UNITED STATES", parent_birth_region := m_birth_region]
cps_mapped[m_birth_region != "UNITED STATES" & f_birth_region == "UNITED STATES" & birth_region == "UNITED STATES", parent_super_birth_region := m_super_birth_region]

cps_mapped[m_birth_region == "UNITED STATES" & f_birth_region != "UNITED STATES" & birth_region == "UNITED STATES", gen := 'half_second']
cps_mapped[m_birth_region == "UNITED STATES" & f_birth_region != "UNITED STATES" & birth_region == "UNITED STATES", parent_birth_country := f_birth_country]
cps_mapped[m_birth_region == "UNITED STATES" & f_birth_region != "UNITED STATES" & birth_region == "UNITED STATES", parent_birth_region := f_birth_region]
cps_mapped[m_birth_region == "UNITED STATES" & f_birth_region != "UNITED STATES" & birth_region == "UNITED STATES", parent_super_birth_region := f_super_birth_region]

## 2ND GEN: CHILDREN OF MIXED-REGION IMMIGRANTS: For now, assign children of immigrant parents from different regions to the mother's region.
cps_mapped[m_birth_region != "UNITED STATES" & f_birth_region != "UNITED STATES" & birth_region == "UNITED STATES" & f_birth_region != m_birth_region, gen := 'mixed_second']
cps_mapped[m_birth_region != "UNITED STATES" & f_birth_region != "UNITED STATES" & birth_region == "UNITED STATES" & f_birth_region != m_birth_region, parent_birth_country := m_birth_country]
cps_mapped[m_birth_region != "UNITED STATES" & f_birth_region != "UNITED STATES" & birth_region == "UNITED STATES" & f_birth_region != m_birth_region, parent_birth_region := m_birth_region]
cps_mapped[m_birth_region != "UNITED STATES" & f_birth_region != "UNITED STATES" & birth_region == "UNITED STATES" & f_birth_region != m_birth_region, parent_super_birth_region := m_super_birth_region]

## 2ND GEN: CHILDREN OF SAME-REGION IMMIGRANTS
cps_mapped[m_birth_region != "UNITED STATES" & f_birth_region != "UNITED STATES" & birth_region == "UNITED STATES" & f_birth_region == m_birth_region, gen := 'second']
cps_mapped[m_birth_region != "UNITED STATES" & f_birth_region != "UNITED STATES" & birth_region == "UNITED STATES" & f_birth_region == m_birth_region, parent_birth_country := m_birth_country]
cps_mapped[m_birth_region != "UNITED STATES" & f_birth_region != "UNITED STATES" & birth_region == "UNITED STATES" & f_birth_region == m_birth_region, parent_birth_region := m_birth_region]
cps_mapped[m_birth_region != "UNITED STATES" & f_birth_region != "UNITED STATES" & birth_region == "UNITED STATES" & f_birth_region == m_birth_region, parent_super_birth_region := m_super_birth_region]

## Create single variable for region defining generation status.
cps_mapped[gen=='third+', gen_country := birth_country]
cps_mapped[gen=='first', gen_country := birth_country]
cps_mapped[gen=='half_second', gen_country := parent_birth_country]
cps_mapped[gen=='mixed_second', gen_country := parent_birth_country]
cps_mapped[gen=='second', gen_country := parent_birth_country]

## Create single variable for region defining second-generation status.
cps_mapped[gen=='third+', gen_super_region := super_birth_region]
cps_mapped[gen=='first', gen_super_region := super_birth_region]
cps_mapped[gen=='half_second', gen_super_region := parent_super_birth_region]
cps_mapped[gen=='mixed_second', gen_super_region := parent_super_birth_region]
cps_mapped[gen=='second', gen_super_region := parent_super_birth_region]

saveRDS(cps_mapped, 'C:/Users/ngraetz/Documents/Penn/papers/cps_2018/cps_mapped_by_country.rds')

