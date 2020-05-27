library(fareway)
library(xlsx)
#co2 emission time series data
class(co2)
plot(co2)
model.co2=lm(co2~time(co2), co2)
abline(model.co2)

#Applying the setpwise techniques to the ourist data set.
tour=readxl::read_xlsx("Tourism.xlsx")
tour1=data.frame(date=tour$`Year/Month`, arrival=tour$`Visitors Arrival`)
View(tour1)
class(tour1)

# converting the tourist data frame intp a time series data. 
tour1.ts=ts(data=tour1[,2], frequency = 12)

#applying linear time series regression 
model.tour1=lm(arrival~time(tour1.ts), data=tour1)
plot(tour1.ts)

# Adding the regression line to the data
abline(model.tour1)
model.tour1

#Analysing the residuals
hist(model.tour1$residuals)
qqnorm(model.tour1$residuals)
qqline(model.tour1$residuals)
model.tour1.residuals=resid(model.tour1)
plot(model.tour1.residuals~time(tour1.ts))
plot(model.tour1.residuals~time(tour1.ts), xlim=c(5,7))

