
# Zillow project 
# step1: EDA on all property
library(dplyr)
library(ggplot2)

# load data
train = read.csv('./data/train_2016_v2.csv')
prop = read.csv('./data/properties_2016.csv')
sample = read.csv('./data/sample_submission.csv')


#library(data.table)
#prop1 = fread('./data/properties_2016.csv')

# fireplaceflag' to 'propertyzoningdesc'

part = c('parcelid',
'fireplaceflag',
'fullbathcnt',
'garagecarcnt',
'garagetotalsqft',
'hashottuborspa',
'heatingorsystemtypeid',
'latitude',
'longitude',
'lotsizesquarefeet',
'numberofstories',
'poolcnt',
'poolsizesum',
'pooltypeid10',
'pooltypeid2',
'pooltypeid7',
'propertycountylandusecode',
'propertylandusetypeid',
'propertyzoningdesc')


prop_part = subset(prop[,part])

save(prop_part , file = './data/prop_part_gene.Rda')
# load('./data/prop_part_gene.Rda')


# convert blank to NA
prop_part[prop_part==''] = NA

# check the shape of dataset
dim(prop_part)
length(unique(prop_part$parcelid)) # there are some duplicate

# checking the missing using sapply()
sapply(prop_part, function(x) sum(is.na(x)))
colSums(is.na(prop_part)) 

# checking the unique value for each columns
sapply(prop_part, function(x) length(unique(x)))

# look at subset of data without missing data
names(prop_part)

### subset again

part2 = c('parcelid',
         'fullbathcnt',
         'lotsizesquarefeet',
         'heatingorsystemtypeid', 
         'propertycountylandusecode',
         'propertylandusetypeid',
         'propertyzoningdesc')
    
prop_part2 = subset(prop_part[,part2])

# some imputation
summary(prop_part2)
sapply(prop_part2, class)



# propertyzoningdesc NA to 'OTHER'
prop_part2$propertyzoningdesc = as.character(prop_part2$propertyzoningdesc)
prop_part2$propertyzoningdesc[is.na(prop_part2$propertyzoningdesc)] = 'OTHER'
prop_part2$propertyzoningdesc = as.factor(prop_part2$propertyzoningdesc)
# impute other columns

prop_part2[is.na(prop_part2)] = 0

summary(prop_part2)
save(prop_part2 , file = './data/prop_part_gene_imputed.Rda')




