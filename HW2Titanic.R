titanic = read.csv("train.csv")


#Predict that the people with a higher probability of survival included
#people who were younger, female, and of a higher class.


titanic = titanic[c(1,2,3,5,6)]
titanic = na.omit(titanic)


titanic_regression = glm(Survived~Pclass+Sex+Age, data = titanic, family = "binomial")
summary(titanic_regression)

titanic_regression_no_age = glm(Survived~Pclass, data = titanic, family = "binomial")
summary(titanic_regression_no_age)



#Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
#  (Intercept)  5.056006   0.502128  10.069  < 2e-16 ***
#  Pclass      -1.288545   0.139259  -9.253  < 2e-16 ***
#  Sexmale     -2.522131   0.207283 -12.168  < 2e-16 ***
#  Age         -0.036929   0.007628  -4.841 1.29e-06 ***

#Coefficients:
#                Estimate Std. Error z value Pr(>|z|)    
#  (Intercept)   3.3468     0.3232  10.356   <2e-16 ***
#  Pclass       -0.9910     0.1182  -8.383   <2e-16 ***
#  Sexmale      -2.5739     0.2030 -12.680   <2e-16 ***
  
  
titanic_test = read.csv("test.csv")


probability_survive = predict(titanic_regression, newdata = titanic_test, type = "response", se.fit = FALSE, dispersion = NULL, terms = NULL, na.action = na.pass)
print(probability_survive)

na_rows = is.na(probability_survive)

for(n in 1:418){
  if(na_rows[n] == "TRUE" && titanic_test$Sex[n] == "male"){
    probability_survive[n] = 0
  }else if(na_rows[n] == "TRUE" && titanic_test$Sex[n] == "female" && titanic_test$Pclass[n] != 3){
    probability_survive[n] = 1
  }else if(na_rows[n] == "TRUE" && titanic_test$Sex[n] == "female" && titanic_test$Pclass[n] == 3){
    probability_survive[n] = 0
  }else{ 
  }
}


survive_prediction = round(probability_survive)
print(survive_prediction)

write.table (survive_prediction, file = "TitanicTest.csv", append = TRUE, col.names = FALSE, row.names = FALSE) 

