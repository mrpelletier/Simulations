#Setting variables
dmean = 50000
dstdev = 12000
fixed = 16
variable = 0.2
operation = 0.4
price = 3.7
years = 10
capacity = seq(30000,60000,5000)
AvProfit = c()

#Creating loop
for(c in 1:length(capacity)){
  profs = c()
  for (i in 1:10000){
    demand = rnorm(n = years, mean = dmean, sd = dstdev)
    cost = (capacity[c] * variable) + (operation * years) + fixed
    revenue = min(demand,capacity[c]) * price
    profit = revenue - cost
    profs[i] <- profit
  }
  AvProfit[c] = mean(profs)
}
AvProfit
#Plotting Forecasts for each quantity
maximized=40000
plot(capacity, AvProfit)
abline(v=maximized, lty=2)

#95% confidence Interval
max = AvProfit[3]
max - 1.96*(dstdev/sqrt(years))
max + 1.96*(dstdev/sqrt(years))
