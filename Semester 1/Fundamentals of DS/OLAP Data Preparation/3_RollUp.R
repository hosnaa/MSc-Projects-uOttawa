# annual revenue for each product and collapse the location dimension; (year, prod)

# check the quantity of pizzas ordered with different sizes (Is the quantity of larger pizza higher or not ?)
apply(revenue_cube, c("pizza_size", "quantity"),
      FUN=function(x) {return(sum(x, na.rm=TRUE))})

# As you can see for large and x_large  pizza we've sum revenue/profit of (553 1298 1773 369  592 1254)