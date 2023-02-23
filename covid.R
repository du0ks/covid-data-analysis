data<-read.csv("~/data.csv") #reading the csv file and named as "data"
data$death_temp<- as.integer(data$death!=0) #cleaned the data (new column)
sum(data$death_temp) /nrow(data) #death rate

#let's find out what are the countries that reported COVID cases in this dataset.
countries<-c()#creating empty vector
for(i in data$country) countries <- c(countries, i) #appending the countries vector
countries<-countries[!duplicated(countries)] #removing the duplicates
countries
edit(data)

#let's claim that, older people die more.
dead=subset(data, death_temp==1)
alive=subset(data,death_temp==0)
mean(dead$age, na.rm=TRUE) # mean of ages from dead subset, there are NA values so na.rm is TRUE.
mean(alive$age, na.rm=TRUE) #mean of ages from alive subset.

#using t test
t.test(alive$age,dead$age,alternative="two.sided",conf.level = 0.95)
#p value is almost 0. so we reject the null hypothesis.


#This time, we claim that gender has no effect.
men=subset(data, gender=="male")
women=subset(data,gender=="female")
mean(men$death_temp, na.rm=TRUE) # men's death rate is %8
mean(women$death_temp, na.rm=TRUE) #women's death rate is %3

#test using t test
t.test(men$death_temp,women$death_temp,alternative="two.sided",conf.level = 0.99)
#p value=0.02<0.05. 
#men have higher death rate.


