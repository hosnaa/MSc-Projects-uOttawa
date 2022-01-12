
# Build up a cube
revenue_cube <- 
    tapply(orders_fact$profit, 
           orders_fact[,c("date", "pizza_size", "store_location", "Toppings","dough","cheese_type","quantity")], 
           FUN=function(x){return(sum(x))})

# Showing the cells of the cube
revenue_cube
head(revenue_cube)
# slice
# revenue_cube[,'large',,,,,]
dimnames(revenue_cube)

