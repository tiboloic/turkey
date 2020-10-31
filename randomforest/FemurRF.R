# LT 31/10/2020
#
# a first go at the femur using classificatio trees  and random forest

require(tidyverse)
require(tree)

femur_train = read_csv('data/ModernFemur.csv', skip=2)

# Meleagris gallopavo oscela, sub species of gallopavo
# re level as gallapavo
femur_train = femur_train %>% 