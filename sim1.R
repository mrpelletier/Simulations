#The annual demand for Wozac, a prescription drug manufactured and marketed by the NuFeel 
#Company, is normally distributed with mean 50,000 and standard deviation 12,000. We assume that 
#demand during each of the next 10 years is an independent random draw from this distribution. 
#NuFeel needs to determine how large they should build the Wozac plant to maximize its expected 
#profit over the next 10 years. If the company builds a plant that can produce x units of Wozac per year, 
#it will incur a one-time cost of $16 for each of these x units. NuFeel will produce only the amount 
#demanded each year, and each unit of Wozac produced will sell for $3.70. Each unit of Wozac 
#produced incurs a variable production cost of $0.20. It costs $0.40 per year to operate a unit of 
#capacity. Among the capacity levels of 30K, 35K, 40K, 45K, 50K, 55K, and 60K units per year, which level 
#maximizes expected profit?

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
