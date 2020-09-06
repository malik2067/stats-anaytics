data <- read.csv(file = "18343261.csv")
#Finding number of patients in control group
control <- data$Group[data$Group == 'Control']
length(control)
#Finding number of patients in placebo group
placebo <- data$Group[data$Group == 'Placebo']
length(placebo)
#Plotting histogram of ages of group
hist(data$Age)
#Adding BMI column to dataframe and analysing it
data$BMI = data$Weight/(data$Height/100)^2
hist(data$BMI)
BMImale = data$BMI[data$Sex == 'Male']
mean(BMImale)
BMIfemale = data$BMI[data$Sex == 'Female']
mean(BMIfemale)
#Calculating mean weight for each county and printing result so maximum mean weight and
#minimum mean weight counties can be identified
counties <- data$County[!duplicated(data$County)]
aggregate(data$Weight ~ data$County, data = data, FUN = 'mean')
#Determining the number of females in the sample of blood group O
cond1 <- data$BloodGroup == 'O'
cond2 <- data$Sex == 'Female'
ofemales = data$Sex[cond1 & cond2]
length(ofemales)
#Determining the percentage of males of blood group B
males = data$Sex[data$Sex == 'Male']
cond3 <- data$BloodGroup == 'B'
cond4 <- data$Sex == 'Male'
bmales = data$Sex[cond3 & cond4]
(length(bmales)/length(males))*100
#Determining total cholestoral difference among all patients
data$choldiff = data$Cholesterol1 - data$Cholesterol2
sum(data$choldiff)
#Testing hypothesis that males or females are or are not more at risk to develop cardiovascular disease
tab1 <- table(data$Sex, data$CardioRisk)
tab1
chisq.test(tab1)
#Testing hypothesis that there is a difference in weight between control and placebo groups
weightcontrol <- data$Weight[data$Group == 'Control']
weightplacebo <- data$Weight[data$Group == 'Placebo']
t.test(weightcontrol, weightplacebo, alternative = 'two.sided', mu = 0, var.equal = TRUE)
#testing hypothesis that the new drug reduces cholesterol compared to placebo
controlchol <- data$choldiff[data$Group == 'Control']
placebochol <- data$choldiff[data$Group == 'Placebo']
t.test(controlchol, placebochol, alternative = 'less', mu = 0, var.equal = TRUE)
#Computing correlation coefficient between BMI and initial cholesterol level
cor(data$BMI, data$Cholesterol1)
#Plotting data with regression line
reg = lm(Cholesterol1 ~ BMI, data = data)
plot(x = data$BMI, y = data$Cholesterol1,xlab = 'BMI', ylab = 'Cholesterol')
data$choldiff=data$Cholesterol2-data$Cholesterol1
sum(data$choldiff)
abline(reg, col = 'red')
summary(reg)
aggregate(data$Height ~ data$BloodGroup, data = data, FUN = 'mean')
ANOVA <-aov(data$Height ~ data$BloodGroup, data = data)
summary(ANOVA)