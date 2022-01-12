# Setup the dimension tables

# Region: classes of these places; C1: class 1
# 
StoreLocation <- 
  data.frame(key=1:5,
             city=c("California", "new York", "Washington", "Cairo", "Alexandria"),
             country=c("USA", "USA", "USA", "Egypt", "Egypt"),
             Region=c("C1", "C1", "C3", "C1", "C2"),
             Province=c("CA", "NY", "WS", "CR", "ALX"),
             Address= c("street_X", "street_Y", "street_Z", "street_A", "street_B"))
write.csv(StoreLocation, 'loc.csv')

Date <- 
  data.frame(key=1:12,
            Month=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
            year= 2009:2020,
            Day=13:24)
            
write.csv(StoreLocation, 'loc.csv')

PizzaSize <- 
  data.frame(key=1:5,
            size=c("personal","small", "medium", "large","xlarge"),
            price = c(12,2,3,5,10))
write.csv(PizzaSize, 'pizza_size.csv')

Topping <-
  data.frame(key=1:5,
             Tops = c("tomato","pepper", "meat", "chicken","onion"),
             price = c(2,2,20,10,2))
write.csv(Topping, 'Topping.csv')

Dough <-
  data.frame(key=1:3,
             dough_type = c("whole wheat thin", "white regular", "stuffed crust"),
             price = c(5,3,20))
write.csv(Dough, 'dough.csv')

CheeseType <-
  data.frame(key=1:3,
             chz_type = c("Swiss", "cheddar", "Mozzarella"),
             price = c(30,20,10))
write.csv(CheeseType, 'cheese_Type.csv')


