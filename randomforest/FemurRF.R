# LT 31/10/2020
#
# a first go at the femur using classificatio trees  and random forest

require(tidyverse)
require(tree)
require(randomForest)

femur_train = read_csv('data/ModernFemur.csv', skip=2)

# Meleagris gallopavo oscela, sub species of gallopavo
# re level as gallopavo
femur_train$Taxa = fct_collapse(femur_train$Taxa, gallopavo=c('Meleagris gallopavo', 'Meleagris gallopavo osceola'),
                                           ocellata=c('Meleagris ocellata'))

# remove unknown gender
ft = filter(femur_train, Sex != 'U')

# combine species and gender
ft$TaxaSex = fct_cross(ft$Taxa, ft$Sex)

# plot the data

#train a classification tree
predictors = names(ft)[9:20]
femur = ft[,c('TaxaSex', predictors)]

#fit random forest
rf.femur = randomForest(TaxaSex~.,data=femur)
importance(rf.femur)
varImpPlot(rf.femur)

# test on unsexed samples
unsex = filter(femur_train, Sex == 'U')

ft.predicted = predict(rf.femur, newdata = unsex, type='prob')
# correctly classifies unsexed samples

# load archeological data
femur_pred = read_csv('data/ArchaeoFemur.csv', skip=2, na='-')
# remove columns containing comments
femur_pred=femur_pred[,1:14]

predict(rf.femur, newdata = femur_pred, type='prob')
predict(rf.femur, newdata = na.roughfix(femur_pred[3:14]), type='prob')

# use randomForestSRC
require(randomForestSRC)

# train model
rf.femur = rfsrc(TaxaSex~.,data=as.data.frame(femur))


###
# gradient boosting

install.packages("xgboost")
require(xgboost)

m = xgboost(data = femur, label = femur$TaxaSex)