#knapsack
#Small problem
library(GA)

item = c(
  'shirts',
  'shorts',
  'dress shirts',
  'pants',
  'nike jacket',
  'basketball shoes',
  'souveneirs',
  'dress shoes',
  'everyday shoes',
  'sandals'
  )
# ,
#   'wedding attire',
#   'backup wedding attire')
weight=c(6,3,2,7,1,9,8,8,3,3)
survival=c(10,10,12,10,7,5,12,10,15,12)
data=data.frame(item,weight,survival)
max_weight=25
#create the function that we want to optimize
fitness=function(x)
{
  current_survpoint=x%*%data$survival
  current_weight=x%*%data$weight
  if(current_weight>max_weight)
  {
    return(0)
  }
  else
  {
    return(current_survpoint)
  }
}

ans1 =ga(type='binary',
      fitness=fitness,
      nBits=nrow(data),
      maxiter=1000,
      popSize=50,
      seed=1234,
      keepBest=TRUE)
summary(ans1)
plot(ans1)
il = which(ans1@solution != 0) #item list
item_list = item[il]
item_list
missing_items = item[-il]
missing_items

#Wedding list
item_wedding = c(
  'shirts',
  'shorts',
  'dress shirts',
  'pants',
  'nike jacket',
  'basketball shoes',
  'souveneirs',
  'dress shoes',
  'everyday shoes',
  'sandals',
  'wedding attire',
  'backup wedding attire')
weight_wedding=c(6,3,2,7,1,9,8,8,3,3,5,5)
survival_wedding=c(10,10,12,10,7,5,12,10,15,12,100,100)
data_wedding=data.frame(item_wedding,weight_wedding,survival_wedding)
max_weight=25
#create the function that we want to optimize
fitness_wedding=function(x)
{
  current_survpoint=x%*%data_wedding$survival_wedding
  current_weight=x%*%data_wedding$weight_wedding
  if(current_weight>max_weight)
  {
    return(0)
  }
  else
  {
    return(current_survpoint)
  }
}

ans2=ga(type='binary',
      fitness=fitness_wedding,
      nBits=nrow(data_wedding),
      maxiter=1000,
      popSize=50,
      seed=1234,
      keepBest=TRUE)
summary(ans2)
plot(ans2)
il = which(ans2@solution[1,] != 0) #item list
item_list_wedding = item_wedding[il]
item_list_wedding



c(
  2,
  2,
  1,
  1,
  2,
  2,
  23,
  3,
  2,
  1,
  4,
  2,
  4,
  3,
  2,
  1,
  2,
  5,
  3,
  3,
  1000,
  1,
  1,
  1
)


# long item list
item_long = c(
  'socks',
  'underwear',
  'workout shirts',
  'hat',
  'shirts',
  'shorts',
  'dress shirts',
  'pants',
  'nike jacket',
  'basketball shoes',
  'bathing suit',
  'goggles',
  'souveneirs',
  'dress shoes',
  'everyday shoes',
  'sandals',
  'toothbrush',
  'toothpaste',
  'deodorant',
  'sunscreen',
  'cash',
  'passport',
  'wallet',
  'phone'
  
)

weight_long = c(
  rnorm(length(item), 30, 10),
  rnorm(length(item_long) - length(item), 10, 1)
)
#           
# survival_long= c(
#   rnorm(length(item), 30, 10),
#   rnorm(length(item_long) - length(item), 10, 1)
# )
weight_long = c(5,
                5,
                7.5,
                15,
                5,
                5,
                10,
                5,
                7.5,
                15,
                3,
                3,
                0.5,
                10,
                15,
                5,
                0.25,
                0.25,
                0.5,
                0.5,
                0.001,
                0.25,
                0.25,
                0.25)
survival_long = c(  75,
                     75,
                     40,
                     2,
                     50,
                     50,
                     60,
                     60,
                     25,
                     45,
                     70,
                     45,
                     80,
                     65,
                     75,
                     60,
                     100,
                     100,
                     100,
                     100,
                     100,
                     1000,
                     1000,
                     1000)
upper_bound = c(
  2,
  2,
  1,
  1,
  2,
  2,
  3,
  3,
  2,
  1,
  4,
  2,
  4,
  3,
  2,
  1,
  2,
  5,
  3,
  3,
  1,
  1,
  1,
  1
)
#rnorm(length(item_long), 20, 5)
data_long=data.frame(item_long,weight_long,survival_long)
max_weight=100

fitness_real=function(x)
{
  current_survpoint=x%*%data_long$survival_long
  current_weight=x%*%data_long$weight_long
  if(any(x[22:24] > 1)){
    return(0)
  }
  else if(current_weight>max_weight)
  {
    return(0)
  }
  else
  {
    return(current_survpoint)
  }
}

# fitness_real(t1)
# 


ga_long = ga(type='real',
             lower = rep(0, length(item_long)),
             upper = upper_bound,
             fitness=fitness_real,
             nBits=nrow(data_long),
             maxiter=1000,
             popSize=50,
             seed=1234,
             keepBest=TRUE)

summary(ga_long)
plot(ga_long)
il = which(round(ga_long@solution) > 0) #item list
ln = length(ga_long@solution)
colnames(ga_long@solution) = item_long
ga_long@solution = round(ga_long@solution)
View(t(ga_long@solution))
ga_long@solution[ga_long@solution != 0]
round(ga_long@solution)
round(ga_long@solution)[round(ga_long@solution) != 0]
item_list = item_long[il]
item_list



# Full grid solution 
full_list = rep(row.names(t(ga_long@solution)), t(ga_long@solution))
full_list = full_list[full_list != 'cash']
full_list = c(full_list, 'cash')
expand.grid(full_list)

x <- seq(0, 10, length.out = 100)
y <- seq(-1, 1, length.out = 20)
fg = list()
for(i in 1:length(item_long)){
  g = lower_bound[i]:1
  fg[[i]] = g 
}
t2 = list(x, y)
t3 = expand.grid(fg)
lower_bound = rep(0, length(item_long))
l = data.frame(lower_bound,
               upper_bound)
expand.grid(l)
