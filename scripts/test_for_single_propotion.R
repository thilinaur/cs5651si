# This is to test whether does IT professionals do not drink enough water 
# p  = propotion of IT professionals who drink water
# H0 = p = 0.5
# Ha = p < 0.5

data_set = read.csv(paste(getwd() , "DataSet.csv", sep = "/"))
total_set = (data_set['Total_more_than_65'])

arr=total_set[['Total_more_than_65']]
arr_propotion = c()
arr_length = length(arr)
test_value = sum(arr)/length(arr)

for (i in 1: 10000){
  sample = rbinom(arr_length, 1, 0.5)
  propotion = sum(sample)/arr_length;
  arr_propotion = c(arr_propotion, propotion);
}

hist(arr_propotion)
meann<-mean(arr_propotion)
standatDeviation <- sd(arr_propotion)

pvalue = pnorm(test_value,mean=meann,sd=standatDeviation)