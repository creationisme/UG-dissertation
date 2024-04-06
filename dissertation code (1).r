rm(list = ls())

source_data = read.csv("C:/Users/HP/Documents/dissertation/gender pay gap/PanelStudyIncomeDynamics.csv",header=T)
use_data = source_data[source_data$wave==2007,c(4,10,17:20,23,28,211,234:249,251:272)]

rownames(use_data) = c(1:nrow(use_data))
attach(use_data)
col = names(use_data)

#age,race distribution

m = use_data[sex == 1,3:6]
f = use_data[sex == 2,3:6]

m_race=apply(m,2,sum);m_race
f_race=apply(f,2,sum);f_race
m_race+f_race

hist(age, prob = T, xlim = c(23,65), col = "grey50")

length(hrwage[hrwage < 50])

#histograms

hist(hrwage, prob = T, main = "Histogram of Hourly Wages", xlab = "Wage",xlim = c(0,600), breaks = c(seq(0,100,10), seq(200,600,100)))

boxplot(hrwage, main = "Boxplot of Hourly Wages", ylab = "Wage")
 
w_w = use_data[white == 1,"hrwage"]
b_w = use_data[black == 1,"hrwage"]
h_w = use_data[hisp == 1,"hrwage"]
o_w = use_data[othrace == 1,"hrwage"]
nw_w=c(b_w,h_w,o_w)
m_w = use_data[sex == 1,"hrwage"]
f_w = use_data[sex == 2,"hrwage"]

wage = list(w_w,b_w,h_w,o_w,m_w,f_w)
names = c("White People", "Black People", "Hispanic People", "Other Races", "Males","Females")

par(mfrow=c(2,3))

for(i in 1:6)
  hist(wage[[i]], xlab = "Wage", main = sprintf("Hourly Wages of %s", names[i]), prob = T)


for(i in 1:6)
  boxplot(wage[[i]] ,ylab = "Wage", main = sprintf("Hourly Wages of %s", names[i]))

edu_w = use_data[Education == 1 | legaleduc == 1| postseceduc == 1,c(1,3:6,9)]
fin_w = use_data[finance == 1 | financialop == 1 | business == 1 | sales == 1,c(1,3:6,9)]
med_w = use_data[Medical == 1 | healthcare == 1 | healthsupport == 1,c(1,3:6,9)]
trade_w = use_data[retailtrade == 1 | wholesaletrade == 1,c(1,3:6,9)]
sc_w = use_data[scientist == 1 | architect == 1 | computer == 1,c(1,3:6,9)]
other_w = use_data[Education == 0 & legaleduc == 0 & postseceduc == 0 & finance == 0 & 
                     financialop == 0 & business == 0 & sales == 0 & Medical == 0 & 
                     healthcare == 0 & healthsupport == 0 & retailtrade == 0 & wholesaletrade == 0 
                   & scientist == 0 & architect == 0 & computer == 0,c(1,3:6,9)]


occ_wage = list(edu_w$hrwage,fin_w$hrwage,med_w$hrwage,trade_w$hrwage,sc_w$hrwage,other_w$hrwage)
occ = c("Education","Finance","Medicine","Trade","SCA","Other")

for(i in 1:6)
  hist(occ_wage[[i]], xlab = "Wage", main = sprintf("Hourly Wages : %s", occ[i]), prob = T)


for(i in 1:6)
  boxplot(occ_wage[[i]] ,ylab = "Wage", main = sprintf("Hourly Wages : %s", occ[i]))


#K-S tests

library(DescTools)

LillieTest(hrwage)[c(1,2)]

for(i in 1:12)
{
  print(c(names,occ)[i])
  print(LillieTest(c(wage,occ_wage)[[i]])[c(1,2)])
}




#u-tests

wilcox.test(m_w, f_w, paired = F, alternative = "two.sided",correct = F)
wilcox.test(m_w, f_w, paired = F, alternative = "greater",correct = F)
wilcox.test(m_w, f_w, paired = F, alternative = "less",correct = F)

wilcox.test(edu_w[edu_w$sex == 1,"hrwage"], edu_w[edu_w$sex == 2,"hrwage"], paired = F, alternative = "two.sided",correct = F)
wilcox.test(edu_w[edu_w$sex == 1,"hrwage"], edu_w[edu_w$sex == 2,"hrwage"], paired = F, alternative = "greater",correct = F)
wilcox.test(edu_w[edu_w$sex == 1,"hrwage"], edu_w[edu_w$sex == 2,"hrwage"], paired = F, alternative = "less",correct = F)

wilcox.test(fin_w[fin_w$sex == 1,"hrwage"], fin_w[fin_w$sex == 2,"hrwage"], paired = F, alternative = "two.sided",correct = F)
wilcox.test(fin_w[fin_w$sex == 1,"hrwage"], fin_w[fin_w$sex == 2,"hrwage"], paired = F, alternative = "greater",correct = F)
wilcox.test(fin_w[fin_w$sex == 1,"hrwage"], fin_w[fin_w$sex == 2,"hrwage"], paired = F, alternative = "less",correct = F)

wilcox.test(med_w[med_w$sex == 1,"hrwage"], med_w[med_w$sex == 2,"hrwage"], paired = F, alternative = "two.sided",correct = F)
wilcox.test(med_w[med_w$sex == 1,"hrwage"], med_w[med_w$sex == 2,"hrwage"], paired = F, alternative = "greater",correct = F)
wilcox.test(med_w[med_w$sex == 1,"hrwage"], med_w[med_w$sex == 2,"hrwage"], paired = F, alternative = "less",correct = F)

wilcox.test(trade_w[trade_w$sex == 1,"hrwage"], trade_w[trade_w$sex == 2,"hrwage"], paired = F, alternative = "two.sided",correct = F)
wilcox.test(trade_w[trade_w$sex == 1,"hrwage"], trade_w[trade_w$sex == 2,"hrwage"], paired = F, alternative = "greater",correct = F)
wilcox.test(trade_w[trade_w$sex == 1,"hrwage"], trade_w[trade_w$sex == 2,"hrwage"], paired = F, alternative = "less",correct = F)

wilcox.test(sc_w[sc_w$sex == 1,"hrwage"], sc_w[sc_w$sex == 2,"hrwage"], paired = F, alternative = "two.sided",correct = F)
wilcox.test(sc_w[sc_w$sex == 1,"hrwage"], sc_w[sc_w$sex == 2,"hrwage"], paired = F, alternative = "greater",correct = F)
wilcox.test(sc_w[sc_w$sex == 1,"hrwage"], sc_w[sc_w$sex == 2,"hrwage"], paired = F, alternative = "less",correct = F)

wilcox.test(other_w[other_w$sex == 1,"hrwage"], other_w[other_w$sex == 2,"hrwage"], paired = F, alternative = "two.sided",correct = F)
wilcox.test(other_w[other_w$sex == 1,"hrwage"], other_w[other_w$sex == 2,"hrwage"], paired = F, alternative = "greater",correct = F)
wilcox.test(other_w[other_w$sex == 1,"hrwage"], other_w[other_w$sex == 2,"hrwage"], paired = F, alternative = "less",correct = F)

wilcox.test(w_w, nw_w, paired = F, alternative = "two.sided",correct = F)
wilcox.test(w_w, nw_w, paired = F, alternative = "greater",correct = F)
wilcox.test(w_w, nw_w, paired = F, alternative = "less",correct = F)

wilcox.test(edu_w[edu_w$white == 1,"hrwage"], edu_w[edu_w$black == 1 | edu_w$hisp == 1 | edu_w$othrace == 1,"hrwage"], paired = F, alternative = "two.sided",correct = F)
wilcox.test(edu_w[edu_w$white == 1,"hrwage"], edu_w[edu_w$black == 1 | edu_w$hisp == 1 | edu_w$othrace == 1,"hrwage"], paired = F, alternative = "greater",correct = F)
wilcox.test(edu_w[edu_w$white == 1,"hrwage"], edu_w[edu_w$black == 1 | edu_w$hisp == 1 | edu_w$othrace == 1,"hrwage"], paired = F, alternative = "less",correct = F)

wilcox.test(fin_w[fin_w$white == 1,"hrwage"], fin_w[fin_w$black == 1 | fin_w$hisp == 1 | fin_w$othrace == 1,"hrwage"], paired = F, alternative = "two.sided",correct = F)
wilcox.test(fin_w[fin_w$white == 1,"hrwage"], fin_w[fin_w$black == 1 | fin_w$hisp == 1 | fin_w$othrace == 1,"hrwage"], paired = F, alternative = "greater",correct = F)
wilcox.test(fin_w[fin_w$white == 1,"hrwage"], fin_w[fin_w$black == 1 | fin_w$hisp == 1 | fin_w$othrace == 1,"hrwage"], paired = F, alternative = "less",correct = F)

wilcox.test(med_w[med_w$white == 1,"hrwage"], med_w[med_w$black == 1 | med_w$hisp == 1 | med_w$othrace == 1,"hrwage"], paired = F, alternative = "two.sided",correct = F)
wilcox.test(med_w[med_w$white == 1,"hrwage"], med_w[med_w$black == 1 | med_w$hisp == 1 | med_w$othrace == 1,"hrwage"], paired = F, alternative = "greater",correct = F)
wilcox.test(med_w[med_w$white == 1,"hrwage"], med_w[med_w$black == 1 | med_w$hisp == 1 | med_w$othrace == 1,"hrwage"], paired = F, alternative = "less",correct = F)

wilcox.test(trade_w[trade_w$white == 1,"hrwage"], trade_w[trade_w$black == 1 | trade_w$hisp == 1 | trade_w$othrace == 1,"hrwage"], paired = F, alternative = "two.sided",correct = F)
wilcox.test(trade_w[trade_w$white == 1,"hrwage"], trade_w[trade_w$black == 1 | trade_w$hisp == 1 | trade_w$othrace == 1,"hrwage"], paired = F, alternative = "greater",correct = F)
wilcox.test(trade_w[trade_w$white == 1,"hrwage"], trade_w[trade_w$black == 1 | trade_w$hisp == 1 | trade_w$othrace == 1,"hrwage"], paired = F, alternative = "less",correct = F)

wilcox.test(sc_w[sc_w$white == 1,"hrwage"], sc_w[sc_w$black == 1 | sc_w$hisp == 1 | sc_w$othrace == 1,"hrwage"], paired = F, alternative = "two.sided",correct = F)
wilcox.test(sc_w[sc_w$white == 1,"hrwage"], sc_w[sc_w$black == 1 | sc_w$hisp == 1 | sc_w$othrace == 1,"hrwage"], paired = F, alternative = "greater",correct = F)
wilcox.test(sc_w[sc_w$white == 1,"hrwage"], sc_w[sc_w$black == 1 | sc_w$hisp == 1 | sc_w$othrace == 1,"hrwage"], paired = F, alternative = "less",correct = F)

wilcox.test(other_w[other_w$white == 1,"hrwage"], other_w[other_w$black == 1 | other_w$hisp == 1 | other_w$othrace == 1,"hrwage"], paired = F, alternative = "two.sided",correct = F)
wilcox.test(other_w[other_w$white == 1,"hrwage"], other_w[other_w$black == 1 | other_w$hisp == 1 | other_w$othrace == 1,"hrwage"], paired = F, alternative = "greater",correct = F)
wilcox.test(other_w[other_w$white == 1,"hrwage"], other_w[other_w$black == 1 | other_w$hisp == 1 | other_w$othrace == 1,"hrwage"], paired = F, alternative = "less",correct = F)


#kruskal wallis test

comb = array(dim = 1)

comb[which(use_data$white == 1 & use_data$sex == 1)] = "WM" #white male
comb[which((use_data$black == 1 | use_data$hisp == 1 | use_data$othrace == 1) & use_data$sex == 1)] = "NWM"
comb[which(use_data$white == 1 & use_data$sex == 2)] = "WF" #white male
comb[which((use_data$black == 1 | use_data$hisp == 1 | use_data$othrace == 1) & use_data$sex == 2)] = "NWF"


comb_edu = array(dim = 1)

comb_edu[which(edu_w$white == 1 & edu_w$sex == 1)] = "WM"
comb_edu[which((edu_w$black == 1 | edu_w$hisp == 1 | edu_w$othrace == 1) & edu_w$sex == 1)] = "NWM"
comb_edu[which(edu_w$white == 1 & edu_w$sex == 2)] = "WF"
comb_edu[which((edu_w$black == 1 | edu_w$hisp == 1 | edu_w$othrace == 1) & edu_w$sex == 2)] = "NWF"

comb_fin = array(dim = 1)

comb_fin[which(fin_w$white == 1 & fin_w$sex == 1)] = "WM"
comb_fin[which((fin_w$black == 1 | fin_w$hisp == 1 | fin_w$othrace == 1) & fin_w$sex == 1)] = "NWM"
comb_fin[which(fin_w$white == 1 & fin_w$sex == 2)] = "WF"
comb_fin[which((fin_w$black == 1 | fin_w$hisp == 1 | fin_w$othrace == 1) & fin_w$sex == 2)] = "NWF"

comb_med = array(dim = 1)

comb_med[which(med_w$white == 1 & med_w$sex == 1)] = "WM"
comb_med[which((med_w$black == 1 | med_w$hisp == 1 | med_w$othrace == 1) & med_w$sex == 1)] = "NWM"
comb_med[which(med_w$white == 1 & med_w$sex == 2)] = "WF"
comb_med[which((med_w$black == 1 | med_w$hisp == 1 | med_w$othrace == 1) & med_w$sex == 2)] = "NWF"

comb_trade = array(dim = 1)

comb_trade[which(trade_w$white == 1 & trade_w$sex == 1)] = "WM"
comb_trade[which((trade_w$black == 1 | trade_w$hisp == 1 | trade_w$othrace == 1) & trade_w$sex == 1)] = "NWM"
comb_trade[which(trade_w$white == 1 & trade_w$sex == 2)] = "WF"
comb_trade[which((trade_w$black == 1 | trade_w$hisp == 1 | trade_w$othrace == 1) & trade_w$sex == 2)] = "NWF"

comb_sc = array(dim = 1)

comb_sc[which(sc_w$white == 1 & sc_w$sex == 1)] = "WM"
comb_sc[which((sc_w$black == 1 | sc_w$hisp == 1 | sc_w$othrace == 1) & sc_w$sex == 1)] = "NWM"
comb_sc[which(sc_w$white == 1 & sc_w$sex == 2)] = "WF"
comb_sc[which((sc_w$black == 1 | sc_w$hisp == 1 | sc_w$othrace == 1) & sc_w$sex == 2)] = "NWF"

comb_other = array(dim = 1)

comb_other[which(other_w$white == 1 & other_w$sex == 1)] = "WM"
comb_other[which((other_w$black == 1 | other_w$hisp == 1 | other_w$othrace == 1) & other_w$sex == 1)] = "NWM"
comb_other[which(other_w$white == 1 & other_w$sex == 2)] = "WF"
comb_other[which((other_w$black == 1 | other_w$hisp == 1 | other_w$othrace == 1) & other_w$sex == 2)] = "NWF"

kruskal.test(hrwage~as.factor(comb))
kruskal.test(edu_w$hrwage~as.factor(comb_edu))
kruskal.test(fin_w$hrwage~as.factor(comb_fin))
kruskal.test(med_w$hrwage~as.factor(comb_med))
kruskal.test(trade_w$hrwage~as.factor(comb_trade))
kruskal.test(sc_w$hrwage~as.factor(comb_sc))
kruskal.test(other_w$hrwage~as.factor(comb_other))

#pairwise mann-whitney

use_data = cbind(use_data,comb)
edu_w = cbind(edu_w,comb = comb_edu)
fin_w = cbind(fin_w,comb = comb_fin)
med_w = cbind(med_w,comb = comb_med)
trade_w = cbind(trade_w,comb = comb_trade)
sc_w = cbind(sc_w,comb = comb_sc)
other_w = cbind(other_w,comb = comb_other)

og = list(use_data,edu_w,fin_w,med_w,trade_w,sc_w,other_w)
rg = c("WM","WF","NWM","NWF")

wilcox.test(use_data[use_data$comb == "WM","hrwage"],use_data[use_data$comb == "NWF","hrwage"], paired = F, correct = F)
wilcox.test(use_data[use_data$comb == "WM","hrwage"],use_data[use_data$comb == "NWF","hrwage"], paired = F, correct = F, alternative = "greater")
wilcox.test(use_data[use_data$comb == "WM","hrwage"],use_data[use_data$comb == "NWF","hrwage"], paired = F, correct = F, alternative = "less")

wilcox.test(use_data[use_data$comb == "WF","hrwage"],use_data[use_data$comb == "NWM","hrwage"], paired = F, correct = F)
wilcox.test(use_data[use_data$comb == "WF","hrwage"],use_data[use_data$comb == "NWM","hrwage"], paired = F, correct = F, alternative = "greater")
wilcox.test(use_data[use_data$comb == "WF","hrwage"],use_data[use_data$comb == "NWM","hrwage"], paired = F, correct = F, alternative = "less")

wilcox.test(edu_w[edu_w$comb == "WM","hrwage"],edu_w[edu_w$comb == "NWF","hrwage"], paired = F, correct = F)
wilcox.test(edu_w[edu_w$comb == "WM","hrwage"],edu_w[edu_w$comb == "NWF","hrwage"], paired = F, correct = F, alternative = "greater")
wilcox.test(edu_w[edu_w$comb == "WM","hrwage"],edu_w[edu_w$comb == "NWF","hrwage"], paired = F, correct = F, alternative = "less")

wilcox.test(edu_w[edu_w$comb == "WF","hrwage"],edu_w[edu_w$comb == "NWM","hrwage"], paired = F, correct = F)
wilcox.test(edu_w[edu_w$comb == "WF","hrwage"],edu_w[edu_w$comb == "NWM","hrwage"], paired = F, correct = F, alternative = "greater")
wilcox.test(edu_w[edu_w$comb == "WF","hrwage"],edu_w[edu_w$comb == "NWM","hrwage"], paired = F, correct = F, alternative = "less")

wilcox.test(fin_w[fin_w$comb == "WM","hrwage"],fin_w[fin_w$comb == "NWF","hrwage"], paired = F, correct = F)
wilcox.test(fin_w[fin_w$comb == "WM","hrwage"],fin_w[fin_w$comb == "NWF","hrwage"], paired = F, correct = F, alternative = "greater")
wilcox.test(fin_w[fin_w$comb == "WM","hrwage"],fin_w[fin_w$comb == "NWF","hrwage"], paired = F, correct = F, alternative = "less")

wilcox.test(fin_w[fin_w$comb == "WF","hrwage"],fin_w[fin_w$comb == "NWM","hrwage"], paired = F, correct = F)
wilcox.test(fin_w[fin_w$comb == "WF","hrwage"],fin_w[fin_w$comb == "NWM","hrwage"], paired = F, correct = F, alternative = "greater")
wilcox.test(fin_w[fin_w$comb == "WF","hrwage"],fin_w[fin_w$comb == "NWM","hrwage"], paired = F, correct = F, alternative = "less")

wilcox.test(med_w[med_w$comb == "WM","hrwage"],med_w[med_w$comb == "NWF","hrwage"], paired = F, correct = F)
wilcox.test(med_w[med_w$comb == "WM","hrwage"],med_w[med_w$comb == "NWF","hrwage"], paired = F, correct = F, alternative = "greater")
wilcox.test(med_w[med_w$comb == "WM","hrwage"],med_w[med_w$comb == "NWF","hrwage"], paired = F, correct = F, alternative = "less")

wilcox.test(med_w[med_w$comb == "WF","hrwage"],med_w[med_w$comb == "NWM","hrwage"], paired = F, correct = F)
wilcox.test(med_w[med_w$comb == "WF","hrwage"],med_w[med_w$comb == "NWM","hrwage"], paired = F, correct = F, alternative = "greater")
wilcox.test(med_w[med_w$comb == "WF","hrwage"],med_w[med_w$comb == "NWM","hrwage"], paired = F, correct = F, alternative = "less")

wilcox.test(trade_w[trade_w$comb == "WM","hrwage"],trade_w[trade_w$comb == "NWF","hrwage"], paired = F, correct = F)
wilcox.test(trade_w[trade_w$comb == "WM","hrwage"],trade_w[trade_w$comb == "NWF","hrwage"], paired = F, correct = F, alternative = "greater")
wilcox.test(trade_w[trade_w$comb == "WM","hrwage"],trade_w[trade_w$comb == "NWF","hrwage"], paired = F, correct = F, alternative = "less")

wilcox.test(trade_w[trade_w$comb == "WF","hrwage"],trade_w[trade_w$comb == "NWM","hrwage"], paired = F, correct = F)
wilcox.test(trade_w[trade_w$comb == "WF","hrwage"],trade_w[trade_w$comb == "NWM","hrwage"], paired = F, correct = F, alternative = "greater")
wilcox.test(trade_w[trade_w$comb == "WF","hrwage"],trade_w[trade_w$comb == "NWM","hrwage"], paired = F, correct = F, alternative = "less")

wilcox.test(sc_w[sc_w$comb == "WM","hrwage"],sc_w[sc_w$comb == "NWF","hrwage"], paired = F, correct = F)
wilcox.test(sc_w[sc_w$comb == "WM","hrwage"],sc_w[sc_w$comb == "NWF","hrwage"], paired = F, correct = F, alternative = "greater")
wilcox.test(sc_w[sc_w$comb == "WM","hrwage"],sc_w[sc_w$comb == "NWF","hrwage"], paired = F, correct = F, alternative = "less")

wilcox.test(sc_w[sc_w$comb == "WF","hrwage"],sc_w[sc_w$comb == "NWM","hrwage"], paired = F, correct = F)
wilcox.test(sc_w[sc_w$comb == "WF","hrwage"],sc_w[sc_w$comb == "NWM","hrwage"], paired = F, correct = F, alternative = "greater")
wilcox.test(sc_w[sc_w$comb == "WF","hrwage"],sc_w[sc_w$comb == "NWM","hrwage"], paired = F, correct = F, alternative = "less")

wilcox.test(other_w[other_w$comb == "WM","hrwage"],other_w[other_w$comb == "NWF","hrwage"], paired = F, correct = F)
wilcox.test(other_w[other_w$comb == "WM","hrwage"],other_w[other_w$comb == "NWF","hrwage"], paired = F, correct = F, alternative = "greater")
wilcox.test(other_w[other_w$comb == "WM","hrwage"],other_w[other_w$comb == "NWF","hrwage"], paired = F, correct = F, alternative = "less")

wilcox.test(other_w[other_w$comb == "WF","hrwage"],other_w[other_w$comb == "NWM","hrwage"], paired = F, correct = F)
wilcox.test(other_w[other_w$comb == "WF","hrwage"],other_w[other_w$comb == "NWM","hrwage"], paired = F, correct = F, alternative = "greater")
wilcox.test(other_w[other_w$comb == "WF","hrwage"],other_w[other_w$comb == "NWM","hrwage"], paired = F, correct = F, alternative = "less")


#outliers 


quantile(hrwage)
iqr= 26.041666-12.019231;iqr
ul=26.041666+1.5*iqr;ul

length(hrwage[hrwage > ul])

remove = which(use_data$hrwage>ul)
outlier = use_data[remove,]

lm_data = use_data[-c(remove),]

detach(use_data)

each_occ = array(dim = 1)
in_outlier = array(dim = 1)

for(i in 1:38)
{
  each_occ[i] = nrow(use_data[use_data[i+10]==1,])
  in_outlier[i] = nrow(outlier[outlier[i+10]==1,])
}

proportion  = in_outlier/each_occ

outlier_occ = data.frame(occupation = col[c(11:48)],frequency_in_sample = each_occ, frequency_in_outlier = in_outlier, proportion)
outlier_occ[order(proportion,decreasing = T),]

median(outlier$schupd);median(lm_data$schupd)
median(outlier$yrsexp);median(lm_data$yrsexp)

hist(lm_data$hrwage,prob=T)
LillieTest(lm_data$hrwage) #rejected

mean(lm_data$hrwage);median(lm_data$hrwage)