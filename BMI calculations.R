#BMI Module
#
#

#Libraries to Load for BMI Module
library(dplyr)
library(data.table)

popn2 <- read.csv(file.choose(),header=T)#Open the file and name it popn variable
attach (popn2) #takes all the header names and the header names become the column variable that can be called
table <- data.table(popn) #takes all the cloumns of the table and converts to a data table

bmi_table <- data.table (height,weight)#Takes height and weight columns and puts into variable bmi_table
bmi_table$BMI <- (bmi_table$weight/(bmi_table$height/100)^2) #Creates BMI column in bmi_table

#BMI categories (From bmi_table, the BMI conditions are organzied)
BMI_underweight <- filter(bmi_table, BMI <18.5)
BMI_normalweight <- filter(bmi_table, BMI >= 18.5 & BMI <= 24.9 )
BMI_overweight <- filter(bmi_table, BMI >= 25 & BMI <= 29.9)
BMI_obese <- filter(bmi_table, BMI >= 30)

