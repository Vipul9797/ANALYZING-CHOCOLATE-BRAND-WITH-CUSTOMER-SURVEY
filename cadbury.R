data<- read.csv("cadbury.csv")
head(data)
#for frequency
data$frequency=factor(data$frequency,levels = c('Daily','Few times a week',
                                                'Few times a month','Rarely','Never'),
                      labels = c('1','2','3','4','5'))
data$frequency=as.numeric(data$frequency)
str(data$frequency)
hist(data$frequency,labels = c('Daily','Few times a week',
                               'Few times a month','Rarely','Never'))
library(moments)
kurtosis(data$frequency)
skewness(data$frequency)
summary(data$frequency)
sd(data$frequency)
var(data$frequency)
#for chocolate brand
barplot(table(data$chocolate.brand))
pie(table(data$chocolate.brand))
#for products
data$products=factor(data$products,levels = c('Cadbury Dairy Milk','Cadbury Silk','Cadbury Bournville',
                                              'Nestle Kitkat','Nestle Crunch','Nestle Aero'),
                     labels = c('1','2','3','4','5','6'))
data$products=as.numeric(data$products)
str(data$products)
barplot(table(data$products),col=c('black','green','red','yellow','gray','blue'))
labels=c('Cadbury Dairy Milk','Cadbury Silk','Cadbury Bournville',
           'Nestle Kitkat','Nestle Crunch','5 star')
pie(table(data$products),labels = labels)
# Rating for cadbury taste
barplot(table(data$cadbury.taste))
#rating for nestle taste
pie(table(data$nestle.taste),col=c('black','green','red','yellow','blue'))
#rating for cadbury texture
pie(table(data$cadbury.texture),col=c('black','green','red','yellow','blue'))
#rating for nestle texture
barplot(table(data$nestle.texture),col=c('black','green','red','yellow','blue'))
#cadbury packing
pie(table(data$cadbury.packaging),col=c('black','green'))
#nestle packing
pie(table(data$nestle.packaging),col=c('red','blue'))
#price of chocolates range
barplot(table(data$price.range))
#liked feature
barplot(table(data$liked.feature))
#recomendation to friends
pie(table(data$recommendation))

#Logistic Regression
#for frequency
data$frequency=factor(data$frequency,levels = c('Daily','Few times a week',
                                                'Few times a month','Rarely','Never'),
                      labels = c('1','2','3','4','5'))
data$frequency=as.numeric(data$frequency)
str(data$frequency)
data$Gender=factor(data$Gender,levels = c('Male','Female'),labels = c('1','2'))
data$Gender=as.numeric(data$Gender)
data$liked.feature=factor(data$liked.feature,levels = c('Taste',
                      'Texture','Mood enhancement','Package'),labels = c('1','2','3','4'))
data$liked.feature=as.numeric(data$liked.feature)
data$price.range=factor(data$price.range,levels = c('5-10 rs','20-40','50-90','100 & above'),
                        labels = c('1','2','3','4'))
data$price.range=as.numeric(data$price.range)
data$chocolate.brand=factor(data$chocolate.brand,levels = c('Cadbury','Nestle'),
                            labels = c('1','0'))
data$chocolate.brand=as.numeric(data$chocolate.brand)
str(data$chocolate.brand)
# Check unique values in chocolate.brand
unique(data$chocolate.brand)
# Recode chocolate.brand to binary (0 and 1)
data$chocolate.brand <- ifelse(data$chocolate.brand == 2, 0, 1)
model<- glm(data$chocolate.brand ~ data$frequency +
              data$Gender + data$liked.feature + data$price.range,family = binomial)
summary(model)
#Linear Regression with Selection method
library(MASS)
fullmodel<- lm(formula = data$chocolate.brand ~ data$frequency +
                 data$Gender + data$liked.feature + data$price.range)
stepmodel<- stepAIC(fullmodel,direction = 'both')
#for manova test
data$chocolate.brand=factor(data$chocolate.brand,levels = c('Cadbury','Nestle'),
                            labels = c('1','0'))
data$chocolate.brand=as.numeric(data$chocolate.brand)
str(data$chocolate.brand)
# Check unique values in chocolate.brand
unique(data$chocolate.brand)
# Recode chocolate.brand to binary (0 and 1)
data$chocolate.brand <- ifelse(data$chocolate.brand == 2, 0, 1)
data$liked.feature=factor(data$liked.feature,levels = c('Taste',
                                                        'Texture','Mood enhancement','Package'),labels = c('1','2','3','4'))
H0='chocolate brand means are equal'
H1='chocolate brand means are not equal'
model<- lm(cbind(data$cadbury.taste,data$nestle.taste,
                 data$cadbury.texture,data$nestle.texture,data$liked.feature)
           ~data$chocolate.brand)
model
a=manova(model)
summary(a,test='Pillai')
Fcal<- summary(a,test='Pillai')$stats['data$chocolate.brand','approx F']
Fcal
ftab=qf(0.95,5,128)
ftab
if(Fcal>ftab){
  print('we reject H0')
}else{
  print("Accept H1")
}
#contingency table
H0=' There is no association between Gender and Chocolate Brand preference.'
H1='There is an association between Gender and Chocolate Brand preference.'
# Create contingency table
contingency_table <- table(data$Gender, data$chocolate.brand)
# Display the contingency table
print(contingency_table)
# Perform chi-squared test
chi_squared <- chisq.test(contingency_table)
# Print the results
print(chi_squared)

#anova
# Perform multi-factor ANOVA
anova_result <- aov(data$chocolate.brand ~ data$cadbury.taste +data$nestle.taste+
                    data$cadbury.texture+ data$nestle.texture +data$liked.feature)

# Summary of ANOVA
summary(anova_result)
