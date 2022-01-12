# Function to generate the Sales table
gen_profit <- function(no_of_recs) {

  # Generate transaction data randomly
  loc <- sample(StoreLocation$key, no_of_recs, 
                replace=T)
  time <- sample(Date$key, no_of_recs, replace=T)
  piz_size <- sample(PizzaSize$key, no_of_recs, replace=T, prob=c(1, 1, 2,6,4))
  Topp <- sample(Topping$key, no_of_recs, replace=T, prob=c(1, 1, 2,2,1))
  dou <- sample(Dough$key, no_of_recs, replace=T)
  chez_type <- sample(CheeseType$key, no_of_recs, replace=T, prob=c(1,2,10))
  quantity <- sample(c(1,2,3), no_of_recs, replace=T)
  profit <- (PizzaSize[piz_size,]$price+Topping[Topp,]$price+Dough[dou,]$price+CheeseType[chez_type,]$price)*quantity
  # +Topping$price+Dough$price+CheeseType$price
  orders <- data.frame(date=time,
                      pizza_size=piz_size,
                      store_location=loc,
                      Toppings=Topp,
                      dough=dou,
                      cheese_type=chez_type,
                      quantity=quantity,
                      profit = profit)

  # Sort the records by time order
  orders <- orders[order(orders$profit),]
  row.names(orders) <- NULL
  return(orders)
}

# Now create the sales fact table
orders_fact <- gen_profit(100)

# show subset of data
head(orders_fact)

write.csv(orders_fact, 'Orders_fact_table.csv')