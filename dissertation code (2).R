attach(lm_data)

edu_w = lm_data[Education == 1 | legaleduc == 1| postseceduc == 1,c(1,3:6,9)]
fin_w = lm_data[finance == 1 | financialop == 1 | business == 1 | sales == 1,c(1,3:6,9)]
med_w = lm_data[Medical == 1 | healthcare == 1 | healthsupport == 1,c(1,3:6,9)]
trade_w = lm_data[retailtrade == 1 | wholesaletrade == 1,c(1,3:6,9)]
sc_w = lm_data[scientist == 1 | architect == 1 | computer == 1,c(1,3:6,9)]
other_w = lm_data[Education == 0 & legaleduc == 0 & postseceduc == 0 & finance == 0 & 
                     financialop == 0 & business == 0 & sales == 0 & Medical == 0 & 
                     healthcare == 0 & healthsupport == 0 & retailtrade == 0 & wholesaletrade == 0 
                   & scientist == 0 & architect == 0 & computer == 0,c(1,3:6,9)]

#wilcox

wilcox.test(lm_data[sex == 1,"hrwage"], lm_data[sex == 2,"hrwage"], paired = F, alternative = "two.sided",correct = F)
wilcox.test(lm_data[sex == 1,"hrwage"], lm_data[sex == 2,"hrwage"], paired = F, alternative = "greater",correct = F)
wilcox.test(lm_data[sex == 1,"hrwage"], lm_data[sex == 2,"hrwage"], paired = F, alternative = "less",correct = F)

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

wilcox.test(lm_data[white == 1,"hrwage"], lm_data[black == 1 | hisp == 1 | othrace == 1,"hrwage"], paired = F, alternative = "two.sided",correct = F)
wilcox.test(lm_data[white == 1,"hrwage"], lm_data[black == 1 | hisp == 1 | othrace == 1,"hrwage"], paired = F, alternative = "greater",correct = F)
wilcox.test(lm_data[white == 1,"hrwage"], lm_data[black == 1 | hisp == 1 | othrace == 1,"hrwage"], paired = F, alternative = "less",correct = F)

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

