#Lab 7

load(url("http://www.openintro.org/stat/data/evals.RData"))

summary(evals$score)
hist(evals$score)
summary(evals$score < 3)

#check association between prof ethinicity and score
boxplot(evals$score ~ evals$ethnicity)

#check association between prof native language and score
boxplot(evals$score ~ evals$language)

#check association between prof gender and score
boxplot(evals$score ~ evals$gender)

#Question 4:
# is avg beauty score a statistically and pratically significant predictor?
new_avg <- jitter(evals$bty_avg) #adds noise to data
plot(evals$score ~ new_avg)

mbty <- lm(evals$score ~ evals$bty_avg)
summary(mbty)
plot(evals$score ~ evals$bty_avg)
abline(mbty)

#Question 5: check on residuals to evaluate conditions
plot(mbty$residuals ~ evals$bty_avg) #check for linearity and constant variability
abline(h = 0, lty = 3)

hist(mbty$residuals) #check for nearly normal residuals
qqnorm(mbty$residuals) #theoritical vs sample quantiles
qqline(mbty$residuals)

#Questions 6 and 7
plot(evals$bty_avg ~ evals$bty_f1lower)
cor(evals$bty_avg, evals$bty_f1lower)

plot(evals[,13:19]) #plots relationship between all beauty variables

m_bty_gen <- lm(score ~ bty_avg + gender, data = evals) #relation b/w bty_avg and gender
summary(m_bty_gen)

multiLines(m_bty_gen) #plots regression lines for each gender level

#Question 8
m_bty_rank <- lm(score ~ bty_avg + rank, data = evals) #relation b/w bty_avg and rank
summary(m_bty_rank)

multiLines(m_bty_rank) #plots regression lines for each rank level

#Question 9: search of parsimonious model
m_full <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(m_full)$adj.r.squared #full model

m_new <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval + cls_students + cls_level + cls_credits + bty_avg, data = evals)
summary(m_new) #model without cls_profs variable: not much change
summary(m_new)$adj.r.squared

m1 <- lm(score ~ ethnicity + gender + language + age + cls_perc_eval + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(m1)$adj.r.squared #full model without variable rank

m2 <- lm(score ~ rank + gender + language + age + cls_perc_eval + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(m2)$adj.r.squared #full model without variable ethnicity

m3 <- lm(score ~ rank + ethnicity + language + age + cls_perc_eval + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(m3)$adj.r.squared #full model without variable gender 

m4 <- lm(score ~ rank + ethnicity + gender + age + cls_perc_eval + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(m4)$adj.r.squared #full model without variable language

m5 <- lm(score ~ rank + ethnicity + gender + language + cls_perc_eval + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(m5)$adj.r.squared #full model without variable age

m6 <- lm(score ~ rank + ethnicity + gender + language + age + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(m6)$adj.r.squared #full model without variable cls_perc_eval

m7 <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(m7)$adj.r.squared #full model without variable cls_students

m8 <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval + cls_students + cls_level + cls_profs + cls_credits + bty_avg, data = evals)
summary(m8)$adj.r.squared #full model without variable cls_level

m9 <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval + cls_students + cls_level + cls_profs + bty_avg, data = evals)
summary(m9)$adj.r.squared #full model without variable cls_credits

m10 <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval + cls_students + cls_level + cls_profs + cls_credits, data = evals)
summary(m10)$adj.r.squared #full model without variable bty_avg

#Eliminating second variable (besides cls_profs)
m_new1 <- lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval + cls_students + cls_credits + bty_avg, data = evals)
summary(m_new1)$adj.r.squared #remove variable cls_level (after cls_profs)

m_final <- lm(score ~ ethnicity + gender + language + age + cls_perc_eval + cls_students + cls_credits + bty_avg, data = evals)
summary(m_final)$adj.r.squared #removed variables cls_profs, cls_level, and rank
summary(m_final)

