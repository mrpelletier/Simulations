#PMF  
#P(0 arrivals) .15 
#P(1 arrival) .25 
#P(2 arrivals) .30 
#P(3 arrivals) .20 
#P(4 arrivals) .10 
 
#For each customer, there is a 60% chance that the person will want to purchase one of the 12 sale-
#priced items. Of those who are interested in washers, WTF knows that 40% desire a top-loader, 25% 
#desire the extra-capacity front-loaders, and 35% desire a regular capacity front-loader. If the store is 
#sold out of a particular washer, the customer will leave without making a  purchase. 

runs = 1000
days = 0
InvCount = c()
topInv = 0
regInv = 0
frontInv = 0


for (i in 1:runs){
  inventory = topInv + regInv + frontInv
  arrivals <- sample(x=c(0,1,2,3,4), size = 1, prob = c(0.15,0.25,0.3,0.2,0.1), replace = T)
  purchase <- rbinom(1,arrivals,0.6)
  if (purchase > 0){
    for (p in purchase){
      buyType <- sample(x=c(1,2,3), size = 1, prob = c(0.4,0.25,0.35), replace=T)
      if(buyType == 1 && topInv < 5){
        topInv = topInv + 1
        InvCount[i] = inventory
        break
      } else if (buyType == 2 && regInv < 4){
        regInv = regInv + 1
        InvCount[i] = inventory
      } else if (frontInv < 3){
        frontInv = frontInv + 1
        InvCount[i] = inventory
        break
      } else {
        break
      }
    }
  }
  if (inventory == 12){
    days = i
    break
  }
}

plot(InvCount, xlab = "Days", ylab = "Inventory")
hist(InvCount, xlab = "Days", ylab = "Inventory Change")
