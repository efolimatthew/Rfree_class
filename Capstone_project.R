#Import the data
library(gsheet) #load the gsheet package
shop <- 'https://docs.google.com/spreadsheets/d/1FWBeZrcvLqktuhyw1rGoWle4fhaVS6ouDAbeecElozg/edit?usp=sharing'
shop <- read.csv(text=gsheet2text(shop, format='csv'), stringsAsFactors=FALSE)
View(shop)
shop <- shop[-1]
class(shop)
nrow(shop)
ncol(shop)
write.csv(shop, "shop.csv", row.names = F)
shop2 <- shop #duplicate the shop data
head(shop2)
dim(shop2)


##1.	An inbuilt function for admin to change the price of 
#any of the items.
head(shop2)


## I think a function should bring the old price and other info first.
old_price <- function(item) {
  result <- item %in% shop2$Item
  if (result == T){
    item_row <- which(shop2$Item == item)
    class(item_row)
    return(shop2[item_row, ])
  } else {
    print(paste(item, "is not available at Rosemary's shop."))
  }
}
price <- 400
shop2[assigned_row, 3] <- price


#The function below restricts us to change the price of the items singly
#---------------------------------------------------
change_price <- function(item, price){
  result <- item %in% shop2$Item
  if (result == T) {
    assigned_row <- which(shop2$Item == item)
    shop2[assigned_row, 3] <- price
    return(shop2[assigned_row, ])
  } else {
    cat(paste(item, "is not available at Rosemary's shop.\nWant to add?\nUse the 'AddItem' function"))
  }
}


change_price(item = "Sugar", price = 200)
shop2
change_price(item = "Egg", price = 100)
change_price(item = "Orange", price = 200) #Remove the conditional statement
#----------------------------------------------


#Function reset shop to original stock
#----------------------------------------------
reset_shop <- function() {
  shop2 <- shop
}
reset_shop()
shop2

#------------------------------------------------------


#------------------------------------------------------
AddItem <- function(item, quantity, price) {
  new_item <-data.frame(Item = item, 
                        Quantity = quantity,
                        Price = price,
                        stringsAsFactors = F, row.names = T)
  colnames(new_item) <- colnames(shop2)
  shop3 <- rbind.data.frame(shop2, new_item)
  return(tail(shop3)) #perhaps return the entire data set
}
#-------------------------------------------------------

#test the function
new_items = c("Boli", "Nuts")
quantities = c(20, 30)
price.list = c(500, 600)

AddItem(item = new_items, quantity = quantities, price = price.list)

## what if the item is already in the list?
