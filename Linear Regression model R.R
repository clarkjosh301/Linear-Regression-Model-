#world happiness UN data set kaggle Multi Linear Regression Model www.kaggle.com/unsdsn/world-happiness
happ2017<-read.csv(file.choose(), header=TRUE, stringsAsFactors = FALSE)
cor(happ2017[c("Happiness.Score","Economy..GDP.per.Capita.","Family","Health..Life.Expectancy.","Freedom","Generosity","Trust..Government.Corruption.")])
#high correlation between health life expectancy and GDP independent variables, expect interaction effect. Also, see lack of correlation between independent variables government corruption and generosity with respect to the dependent variable Happiness.Score
happ2017_model<-lm(Happiness.Score ~ Economy..GDP.per.Capita.+ Family + Health..Life.Expectancy. + Freedom + Generosity + Trust..Government.Corruption.,data=happ2017)
summary(happ2017_model)
#Original multiple R^2 value=.8124 from just the lm() function without any model refining. Dedide to look at scatter-plot matrix in order to look at ways to refine model
pairs(happ2017[c("Happiness.Score","Economy..GDP.per.Capita.","Family","Health..Life.Expectancy.","Freedom","Trust..Government.Corruption.","Generosity")])
#looking at scatter-plot matrix determine that government trust and generosity don't have much of a linear relationship in regards to the happiness score. Between the scatter-plot matrix and the variables government trust/generosity not being statistically significant in the happ2017_model I decide to do an ANOVA test
full<-lm(Happiness.Score ~ Economy..GDP.per.Capita. + Family + Health..Life.Expectancy. + Freedom + Generosity + Trust..Government.Corruption., data=happ2017)
reduced<-lm(Happiness.Score ~ Economy..GDP.per.Capita. + Family + Health..Life.Expectancy. + Freedom, data=happ2017)
anova(reduced,full)
#P-value is greater than .05 and thus I cannot reject the null hypothesis. Therefore I get rid of the generosity and trust..government.corruption variables in the model
summary(reduced)
#R^2 value increases as compared to original happ2017_model as expected. Still need to fix high interaction between GDP and life expectancy, decide to refine model even more. I noticed throughout the process that Health..Life.Expectancy is skewed and box-plot variable to determeine if there are outliers in the data
boxplot(happ2017$Health..Life.Expectancy., horizontal=TRUE)
#there are no outliers but determine since the variable is skewed significantly more than any other variable to assign a binary indicator
happ2017$Health..Life.Expectancy2<-ifelse(happ2017$Health..Life.Expectancy.>=.3699,1,0)
reduced<-lm(Happiness.Score ~ Economy..GDP.per.Capita.*Health..Life.Expectancy2 + Health..Life.Expectancy. + Family + Freedom, data=happ2017)
summary(reduced)
#R^2 value increases to .83 and residual error decreases as expected 