# Zillow project with caret package

library(caret)

# df = read.csv(file.choose())
# reload the dataset
load('./data/train_sold.Rda')
df = train_sold
df[is.na(df)] = NA
# check the missing data, if more than 75% missing, remove th variable
str(df)
sum(is.na(df))
na.tab = sapply(df, function(x) sum(is.na(x)))

na.prop = sort(na.tab/nrow(train_sold))
barplot(rev(na.prop), ylim=c(0,1), main='Total missing value sorted plot')
abline(h= 0.75, col = "red")

# what are those variable have 80~95% missing, remove th variable
na.prop[na.prop>0.8 & na.prop <0.95]

# visualize the missing value: pooltype, 
library(VIM)
library(mice)
df_pooltype = df[,c('pooltypeid2', 'pooltypeid7' ,'pooltypeid10')]
names(df_pooltype) = c('pool2', 'pool7', 'pool10')
aggr(df_pooltype)
md.pattern(df_pooltype) 

# visualize the missing value: finishedsquarefeet
finishedsqt = c("finishedfloor1squarefeet","finishedsquarefeet6", "finishedsquarefeet12","finishedsquarefeet13","finishedsquarefeet15","finishedsquarefeet50") 
df_finishedsqt = df[, finishedsqt]
names(df_finishedsqt) = c('sqf', 'sqf6', 'sqf12','sqf13','sqf15','sqf50')
aggr(df_finishedsqt)
md.pattern(df_finishedsqt) 


# list all names by order
sort(names(df))


#                                       [1] "airconditioningtypeid"      dummy   as factor
#                                       [2] "architecturalstyletypeid"   dummy   as factor
df$assessmentyear = 2015              # [3] "assessmentyear"             impute 2015 


#                                       [4] "basementsqft"                
#                                       [5] "bathroomcnt"                good lm    *    
#                                       [6] "bedroomcnt"                 good lm    *       
df$buildingclasstypeid = NULL        #  [7] "buildingclasstypeid"        drop 

#                                       [8] "buildingqualitytypeid"      dummy  as factor
#                                       [9] "calculatedbathnbr"          dummy  as factor
#                                       [10] "calculatedfinishedsquarefeet"  good lm * impute
df$censustractandblock = NULL        #  [11] "censustractandblock"         drop
#                                       [12] "decktypeid"  only one value 66  dummy as factor    
df$finishedfloor1squarefeet =NULL #     [13] "finishedfloor1squarefeet"   drop same as [17]
#                                       [14] "finishedsquarefeet12"        dummy 
#                                       [15] "finishedsquarefeet13"        dummy
#                                       [16] "finishedsquarefeet15"        dummy
#                                       [17] "finishedsquarefeet50"        dummy
#                                       [18] "finishedsquarefeet6"         dummy
#                                       [19] "fips"                        dummy as factor
#                                       [20] "fireplacecnt"                dummy as factor          
#                                       [21] "fireplaceflag"               dummy as factor
#                                       [22] "fullbathcnt"                 good lm    *
#                                       [23] "garagecarcnt"                good lm    *
#                                       [24] "garagetotalsqft"             good lm    *
#                                       [25] "hashottuborspa"              dummy as factor
#                                       [26] "heatingorsystemtypeid"       dummy as factor
#                                       [27] "landtaxvaluedollarcnt"       response
df$latitude=NULL                    #   [28] "latitude"                    drop
#                                       [29] "logerror"                    response
df$longitude=NULL                   #   [30] "longitude"                   drop
#                                       [31] "lotsizesquarefeet"           good lm    *
#                                       [32] "numberofstories"             dummy as factor
# [33] "parcelid"                                                          drop later
#                                       [34] "poolcnt"                     dummy as factor
#                                       [35] "poolsizesum"                 good lm    *
#                                       [36] "pooltypeid10"                dummy
#                                       [37] "pooltypeid2"                 dummy
#                                       [38] "pooltypeid7"                 dummy
df$propertycountylandusecode = NULL   # [39] "propertycountylandusecode"   drop
#                                       [40] "propertylandusetypeid"       dummy as factor
df$propertyzoningdesc = NULL          # [41] "propertyzoningdesc"          drop
df$rawcensustractandblock = NULL      # [42] "rawcensustractandblock"      drop
df$regionidcity = NULL                # [43] "regionidcity"                drop
                                      # [44] "regionidcounty"              dummy as factor
df$regionidneighborhood = NULL        # [45] "regionidneighborhood"        drop
df$regionidzip = NULL                 # [46] "regionidzip"                 drop
#                                       [47] "roomcnt"                     good lm     *
#                                       [48] "storytypeid"                 dummy as factor
df = df[!is.na(df$structuretaxvaluedollarcnt), ]  # [49] "structuretaxvaluedollarcnt"  response
#                                       [50] "taxamount"                   response drop later          
#                                       [51] "taxdelinquencyflag"          dummy as factor
#                                       [52] "taxdelinquencyyear"          dummy as factor
#                                       [53] "taxvaluedollarcnt"           drop response
#                                       [54] "threequarterbathnbr"         dummy as factor
#                                       [55] "transactiondate"             drop later
#                                       [56] "typeconstructiontypeid"      dummy as factor
#                                       [57] "unitcnt"                     dummy as factor
#                                       [58] "yardbuildingsqft17"          good lm    *
#                                       [59] "yardbuildingsqft26"          good lm    *
df$yearbuilt = as.integer(df$yearbuilt) # [60] "yearbuilt"                 impute

# impute rest of the value into 0
df[is.na(df)]=0


# set up factors
df$airconditioningtypeid = as.factor(df$airconditioningtypeid)
df$architecturalstyletypeid = as.factor(df$architecturalstyletypeid)
df$buildingqualitytypeid = as.factor(df$buildingqualitytypeid)
df$decktypeid = as.factor(df$decktypeid)
df$decktypeid = ifelse(df$decktypeid == '66' , 1 , 0)
df$fips = as.factor(df$fips)
df$fireplacecnt = as.factor(df$fireplacecnt)
df$fireplaceflag = as.factor(df$fireplaceflag)
df$fireplaceflag = ifelse(is.na(df$fireplaceflag), 0 , TRUE)
df$hashottuborspa = as.factor(df$hashottuborspa)
df$hashottuborspa = ifelse(is.na(df$hashottuborspa), 0 , TRUE)
df$heatingorsystemtypeid = as.factor(df$heatingorsystemtypeid)
df$numberofstories = as.factor(df$numberofstories)
df$poolcnt = as.factor(df$poolcnt)
df$propertylandusetypeid = as.factor(df$propertylandusetypeid)
df$regionidcounty = as.factor(df$regionidcounty)
df$storytypeid = as.factor(df$storytypeid)
df$taxdelinquencyflag = as.factor(df$taxdelinquencyflag)
df$taxdelinquencyflag = ifelse(is.na(df$taxdelinquencyflag), 0 , 'Y')
df$taxdelinquencyflag = ifelse(df$taxdelinquencyflag == 'Y', 1 , 0)
df$taxdelinquencyyear = as.factor(df$taxdelinquencyyear)
df$threequarterbathnbr = as.factor(df$threequarterbathnbr)
df$typeconstructiontypeid = as.factor(df$typeconstructiontypeid)
df[df$yearbuilt==0, 'yearbuilt'] = median(df$yearbuilt)


# ==========================================================================
# drop those columns
# save(df, file='./data/df_caret.Rda')
load('./data/df_caret.Rda')

# ==========================================================================
# select the variable for structuretaxvaluedollarcnt model
dropnames = c( "landtaxvaluedollarcnt" ,
               #"structuretaxvaluedollarcnt",
               "taxvaluedollarcnt" ,
               "taxamount",
               "logerror" , 
               "parcelid" ,
               "taxamount",
               'transactiondate'
               )

# subset dataframe for tax model only
df_tax1 = df[ , !names(df) %in% dropnames] 
save(df_tax1, file='./data/df_tax1_caret.Rda')

# load the df
load('data/df_tax1_caret.Rda')
# ==========================================================================


# parition the data 
# parition function from caret
set.seed(0)
indexes = createDataPartition(df_tax1$structuretaxvaluedollarcnt,
                              times = 1,
                              p = 0.7,
                              list = FALSE)

df.train = df_tax1[indexes, ]
df.test  = df_tax1[-indexes, ]

# transform all feature to dummy variables
# dummy.vars = dummyVars( ~ . data = df_tax1[ , -40])
# df.dummy = predict(dummy.vars , fd_tax1[, -40])


# modeling

model.1 = lm(structuretaxvaluedollarcnt ~ . , data = df.train)


model.2 = lm(structuretaxvaluedollarcnt ~ . 
             - taxdelinquencyyear 
             - assessmentyear 
             - typeconstructiontypeid
             - storytypeid
             - regionidcounty
             - finishedsquarefeet6 
             - finishedsquarefeet15 
             - decktypeid
             - architecturalstyletypeid 
             - airconditioningtypeid
                 , data = df.train)

AIC(model.1, model.2)

# backward
library(car)
library(MASS) 

model.empty = lm(structuretaxvaluedollarcnt ~ 1, data = df.train) #The model with an intercept ONLY.

scope = list(lower = formula(model.empty), upper = formula(model.2)) 
# specific a list from lower to upper.

#The Modern Applied Statistics library.
#Stepwise regression using AIC as the criteria (the penalty k = 2).
forwardAIC = step(model.empty, scope, direction = "forward", k = 2) # AIC p
summary(forwardAIC)
plot(forwardAIC)
influencePlot(forwardAIC)
vif(forwardAIC)

forwardAIC = step(model.empty, scope, direction = "forward", k = 2) # AIC p
backwardAIC = step(model.2, scope, direction = "backward", k = 2)   # AIC p
AIC(model.1, model.2, forwardAIC, backwardAIC)

# Box-Cox transformation

bc = boxCox(backwardAIC) 
lambda = bc$x[which(bc$y == max(bc$y))] 
str.tax.bc = (df.train$structuretaxvaluedollarcnt^lambda - 1)/lambda


model.bc = lm(str.tax.bc  ~ .
              - heatingorsystemtypeid
              - pooltypeid2 
              - pooltypeid7
              - pooltypeid10
              - typeconstructiontypeid
              - finishedsquarefeet6
              - regionidcounty
              - assessmentyear
              - storytypeid 
              , data = df.train[, -40]) 

summary(model.bc)

AIC(model.1, model.2, forwardAIC, backwardAIC ,model.bc)


# Box-Cox transformation again

bc2 = boxCox(model.bc) 
lambda = bc2$x[which(bc2$y == max(bc2$y))] 
lambda
str.tax.bc2 = (df.train$structuretaxvaluedollarcnt^lambda - 1)/lambda

model.bc2 = lm(str.tax.bc2  ~ .
              - heatingorsystemtypeid
              - pooltypeid2 
              - pooltypeid7
              - pooltypeid10
              - typeconstructiontypeid
              - finishedsquarefeet6
              - regionidcounty
              - assessmentyear
              - storytypeid 
              , data = df.train[, -40]) 

summary(model.bc2)

AIC(model.1, model.2, forwardAIC, backwardAIC , model.bc, model.bc2)
# keep the first tranfomred model model.bc



# validate with test.test

predicted.bc = predict(model.bc, newdata = df.test[ c(1: 10) , -40 ])
observed.bc =  (df.test[ c(1:10) , 'structuretaxvaluedollarcnt']^lambda - 1)/lambda

diff = predicted.bc/observed.bc - 1
t.test(diff)

###### GBM
data = df_tax1
# brief check

str(data)
names(data)
dim(data)
sapply(data, function(x) sum(is.na(x)))
sapply(data, function(x) length(unique(x)))
sapply(data, class)


# drop some columns

data$assessmentyear = NULL

# data = subset(data[1:10000, ]) # test 1000 row before





# set factors
# data$a = as.factor(data$a)



# partition the data
set.seed(0)
indexes1 = createDataPartition(data$structuretaxvaluedollarcnt,
                               times = 1,
                               p = 0.6,
                               list = FALSE)
df.train = data[indexes1, ]
df2 = data[-indexes1, ]
indexes2 = createDataPartition(df2$structuretaxvaluedollarcnt,
                               times = 1,
                               p = 0.5,
                               list = FALSE)
df.valid = df2[indexes2, ]
df.test  = df2[-indexes2, ]

y.train = df.train[, 40]
y.valid = df.valid[,40]
y.test= df.test[, 40]

print(nrow(df.train)/nrow(data))
print(nrow(df.valid)/nrow(data))
print(nrow(df.test)/nrow(data))

# modeling_1: Regression Tree
# modeling_2: Random Forest/Bagging

# modeling_3: Boosting
library(gbm)

# step.1 generate an overfit model first
set.seed(0)
boost.model = gbm(structuretaxvaluedollarcnt ~ ., 
                  data = df.train,
                  distribution = "gaussian",
                  n.trees = 5000,
                  interaction.depth = 4) # also need set lambda

# par(mfrow = c(1, 1))
summary(boost.model)

# step.2 make a series of prediction based on different tree numbers
n.trees = seq(from = 100, to = 5000, by = 100)
predmat = predict(boost.model, 
                  newdata = df.valid, 
                  n.trees = n.trees,
                  shrinkage = 0.01)

#Produces 100 different predictions for each of the observations in our
#test set.
dim(predmat)

#Calculating the boosted errors.
par(mfrow = c(1, 1))
boo.err = with(df.valid, apply((predmat - y.valid)^2, 2, mean))
plot(n.trees, boo.err, pch = 16,
     ylab = "Mean Squared Error",
     xlab = "# Trees",
     main = "Boosting Test Error")

#Include the best OOB error from the random forest.
abline(h = min(boo.err), col = "red")
min(boo.err)


#Increasing the shrinkage parameter; a higher proportion of the errors are
#carried over.
set.seed(0)
boost.model.2 = gbm(structuretaxvaluedollarcnt ~ ., 
                    data = df.train,
                    distribution = "gaussian",
                    n.trees = 5000,
                    interaction.depth = 4,  #10
                    #cv.folds = 3,
                    shrinkage = 0.01) # make shrinkage big (default is 0.01)

predmat.2 = predict(boost.model.2, 
                    newdata = df.valid,
                    n.trees = n.trees)

boo.err.2 = with(df.valid, apply((predmat.2 - y.valid)^2, 2, mean))
min(boo.err.2)

plot(n.trees, boo.err.2, pch = 16,
     ylab = "Mean Squared Error",
     xlab = "# Trees",
     main = "Boosting Test Error")

gbm.perf(boost.model.2)


print(relative.influence(boost.model.2))
vars = summary(boost.model.2)

vars_selected = vars[vars[2]$rel.inf>0, 1]
right = paste(vars_selected, collapse = " + ")
form = paste('structuretaxvaluedollarcnt ~ ', right)

# train pruned model with train dateset =================
boost.model.pruned = gbm(structuretaxvaluedollarcnt ~ 
                             + finishedsquarefeet12 
                         + calculatedfinishedsquarefeet 
                         + buildingqualitytypeid 
                         + yearbuilt + bathroomcnt 
                         + lotsizesquarefeet 
                         + bedroomcnt + propertylandusetypeid 
                         + poolcnt + airconditioningtypeid 
                         + pooltypeid7 + taxdelinquencyyear 
                         + pooltypeid10 + hashottuborspa 
                         + heatingorsystemtypeid 
                         + finishedsquarefeet15 + unitcnt, 
                         
                         data = df.train,
                         distribution = "gaussian",
                         n.trees = 5000,
                         interaction.depth = 10, #4>5>6>7>8>9>*10>15>20
                         # cv.folds = 3,
                         shrinkage = 0.01) #>1,0.1,*0.01, >>0.001

# R-squred: 
#   n.trees
#   depth: 0.6277704>0.6314801> 0.6292806 > 0.6360735 > 0.6363856 > *0.6418633
#   cv.folds = 
#   shrinkage = 0.4719414 >...*0.6363856>0.5765003


# validate using valide dataset ========================

predmat.3 = predict(boost.model.pruned, 
                    newdata = df.valid,
                    n.trees = n.trees)

boo.err.valid = with(boost.model.pruned, apply((predmat.3 - y.valid)^2, 2, mean))
min(boo.err.valid)

plot(n.trees, boo.err.valid, pch = 16,
     ylab = "Mean Squared Error",
     xlab = "# Trees",
     main = "Boosting Test Error")
abline(h = min(boo.err.valid), col = "red")


y_bar = mean(y.valid)
ssr = with(df.valid, apply((predmat.3 - y.valid)^2, 2, sum))
sst = with(df.valid, apply((predmat.3 - y_bar)^2, 2, sum))
max(1 - (ssr/sst))


# save the model
model.best = boost.model.pruned
boost.model.pruned = model.best

save(boost.model.pruned, file='GBM_model_pruned.rds')


# predict using test set ================================

predmat.4 = predict(model.best, 
                    newdata = df.test,
                    n.trees = n.trees)

boo.err.test = with(model.best, apply((predmat.4 - y.test)^2, 2, mean))
min(boo.err.valid)

plot(n.trees, boo.err.test, pch = 16,
     ylab = "Mean Squared Error",
     xlab = "# Trees",
     main = "Boosting Test Error")

y_hat = mean(y.test)
ssr = with(df.test, apply((predmat.4 - y.test)^2, 2, sum))
sst = with(df.test, apply((predmat.4 - y_hat)^2, 2, sum))
max(1 - (ssr/sst))

