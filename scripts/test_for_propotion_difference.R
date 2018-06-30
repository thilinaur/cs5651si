# This is to test whether the propotion of male IT professionals who drink 
# enough water is greater than that of female IT profesionals who drink
# enough water.

# pm := propotion of male IT professionals who drink enough water
# pf := propotion of female IT professionals who drink enough water

# H0 := pm = pf (null hypothesis)
# Ha := pm > pf (alt hypothesis)

data_set = read.csv(paste(getwd() , "DataSet.csv", sep = "/"))

tot_set = (data_set['Total_more_than_65'])
arr_tot_set = tot_set[['Total_more_than_65']]

male_set = (data_set['Males_more_than_65'])
arr_male_set = male_set[['Males_more_than_65']]

female_set = (data_set['Females_more_than_65'])
arr_female_set = female_set[['Females_more_than_65']]

sample_prop_diff = sum(arr_male_set)/41 - sum(arr_female_set)/17

arr_prop_dif = c()

for (i in 1: 10000){
  sample1 = sample(arr_tot_set, 41, replace= FALSE)
  sample2 = sample(arr_tot_set, 17, replace= FALSE)
  
  p_true_1 = sum(sample1)/41
  p_true_2 = sum(sample2)/17
  
  p_dif = p_true_1 - p_true_2
  arr_prop_dif = c(arr_prop_dif, p_dif)
}

mean_diff<-mean(arr_prop_dif)
sd_diff <- sd(arr_prop_dif)

pvalue = pnorm(sample_prop_diff, mean=mean_diff, sd=sd_diff,lower.tail = FALSE)
print(pvalue)