library(tictoc)
library(GA)

source('tsp_source.R')
source('brute_force.R')
source('nearest_neighbor.R')

# example
cities <- simCities(n = 10)
cities 

# example
plotCities(cities)

# example
route <- randomRoute(cities)
route

# example
plotRoute(cities, route)


# example
set.seed(1)
n = 10
cities <- simCities(n)
# tic()
# bf <- bruteForce(cities)
# toc()
# bf
# 
# plotRoute(cities, bf$route)


ga_tsp = function(xx){
  adjmat = compAdjMat(cities)
  # rownames(adjmat) = rownames(cities)
  r1 <- c(start,xx,start)
  best_route = distRoute(adjmat, r1)
  return(-best_route)
}
rownames(cities) = as.numeric(as.factor(rownames(cities)))
full = rownames(cities)
start = rownames(cities)[1]
rest = rownames(cities)[-1]
# 
# ans1 = ga(type = 'permutation',
#           fitness = ga_tsp,
#           lower = rep(2, n-1),
#           upper = rep(n, n-1),
#           maxiter = 1000,
#           pmutation = 0.3
# )

# plot(ans1)
# plotRoute(cities, c(1, ans1@solution, 1))

n = 50
cities <- simCities(n)
nn = nearestNeighbor(cities)


rownames(cities) = as.numeric(as.factor(rownames(cities)))
full = rownames(cities)
start = rownames(cities)[1]
rest = rownames(cities)[-1]
ga_tsp = function(xx){
  adjmat = compAdjMat(cities)
  # rownames(adjmat) = rownames(cities)
  r1 <- c(start,xx,start)
  best_route = distRoute(adjmat, r1)
  return(-best_route)
}
# ga_tsp(r2)
ans2 = ga(type = 'permutation',
   fitness = ga_tsp,
   lower = rep(2, n-1),
   upper = rep(n, n-1),
   maxiter = 30000,
   pmutation = 0.3
)
plot(ans2)
summary(ans2)
rs = c(start, ans2@solution, start)
plotRoute(cities, rs)

nn = nearestNeighbor(cities)
plotRoute(cities, nn)


cc = gaperm_Population(ans1)
for(i in 1:(n-25)){
  cc[i,] = as.numeric(nn$route[-c(1, n+1)])
}    

m = matrix(as.numeric(rep(cc[1,], n)), ncol = 49, byrow = T)
library(tictoc)
tic()
ans2 = ga(type = 'permutation',
          fitness = ga_tsp,
          lower = rep(2, n-1),
          upper = rep(n, n-1),
          maxiter = 100000,
          pmutation = 0.3,
          suggestions = cc
)
toc()
