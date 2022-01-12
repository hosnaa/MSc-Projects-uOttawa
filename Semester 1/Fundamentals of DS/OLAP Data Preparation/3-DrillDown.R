#annual and monthly revenue for each product and collapse the location dimension;

apply(revenue_cube, c("cheese_type", "pizza_size", "Toppings"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})

# Apparently from the numbers it seems that customers like the cheese type #3 more which is Mozzerlla and 
# the topping #3 which is meat so we may need to buy more of them