############################
# PracticeDA.R
# Sean Connolly
# 10/28/2020
# -------------------------
# Code from Chapter 12.1 in MASS
############################

setwd("C:\\Users\\connollys\\Documents\\SRCwork\\turkeys")
library(MASS)

femur.mod.raw = read.csv("ModernFemur.csv")
# Top two rows deleted and the headers modified so as not to have
# spaces or special characters

femur.mod = data.frame(lapply(femur.mod.raw,function(x){gsub(" osceola","",x)}))
# converg M gallopavo osceola to M gallopavo

for (i in 9:20){
   femur.mod[,i] = as.numeric(as.character(femur.mod[,i]))
}
# Convert predictors to numeric

femur.mod.sex = subset(femur.mod,Sex=="M" | Sex=="F")
# Pull out the ones whose sex is known

femur.grp = rep(NA,nrow(femur.mod.sex))
femur.grp[femur.mod.sex$Sex=="M" & femur.mod.sex$Taxa=="Meleagris gallopavo"] = "G"
femur.grp[femur.mod.sex$Sex=="F" & femur.mod.sex$Taxa=="Meleagris gallopavo"] = "g"
femur.grp[femur.mod.sex$Sex=="F" & femur.mod.sex$Taxa=="Meleagris ocellata"] = "o"
femur.grp[femur.mod.sex$Sex=="M" & femur.mod.sex$Taxa=="Meleagris ocellata"] = "O"
femur.mod.sex$grp = femur.grp
# Create a column with the four species x sex groups

pairs(femur.mod.sex[femur.mod.sex$grp=="G",9:20])
pairs(femur.mod.sex[femur.mod.sex$grp=="g",9:20])
pairs(femur.mod.sex[femur.mod.sex$grp=="O",9:20])
pairs(femur.mod.sex[femur.mod.sex$grp=="o",9:20])
# theoretically check multivariate normality -- useless given the small sample sizes

# do the discriminant function analysis and the plot them. Pretty much following
# MASS's crabs example for DFA here
femur.lda = lda(grp ~ GLA+Lm+BpB+Dp+SC+D+BdF+DdG+C+E+H+J,femur.mod.sex)
femur.mod.pr = predict(femur.lda,dimen=2)
femur.mod.crds = femur.mod.pr$x
eqscplot(femur.mod.crds,type="n",xlab="FirstLD (all the variation)",
          ylab="SecondLD (for show)")
text(femur.mod.crds,labels=as.character(femur.mod.sex$grp))

femur.mod.nosex = subset(femur.mod,Sex="U")
femur.nosex.lab = rep(NA,nrow(femur.mod.nosex))
# started to try to plot these on the above graph but didn't finish today