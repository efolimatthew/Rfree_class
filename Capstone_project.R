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




##3.	An inbuilt function to compute goods purchased per 
#customer and displays a receipt for printing. 

order_table <- data.frame(
  time = Sys.time(),
  item_bought = item,
  quantity_bought = quantity,
  price_sold = price, #price from shop list
  sub_total = (quantity*price)
)

# 3a.having an if condition of VAT of 20% when customers buy less than 5 items.
# < 5 items
if (item < 5) {
  print(paste("Items bought", items, "Quantity bought", quantity, "Total Price + 20% VAT", price))
} else {
  if (items > 10) {
    print(paste("Items bought", items, "Quantity bought", quantity, "Total Price + 20% VAT", price))
  } 
}
##a.	The function must be able to add 20% VAT for customers 
#buying less than 5 items. 

##b.	30% VAT for customers buying more than 10 items. 
##c.	N800 bonus goods for customers purchasing more than 
#10 items with the least amount being N100.

#Solution
#the code should be able to take the item and quantity needed
#then bring back the price
#but in the return of the price it should have an if statement
#for different calculation of items more than 5, 10

##4.	An inbuilt function that updates stock after an item 
#has been purchased. 
update_stock <- function() {
  
}


##5.	An inbuilt function that takes record of total gain per day. 


##b.	Write a test program, that communicates with the buyer, 
#taking note of the following: 
##i.	Displays the available items. 
ordered_item <- NULL #customers' order 
# Get customers' order from graphics output
available_order <- NULL #available items in the shop from customers' order.
#available_order is set once the item has been set to be available in the shop.

## Select your order at the shop
ordered_item <- select.list(choice = c(shop$Item),
                            preselect = NULL,
                            multiple = T,
                            title = 'Select your item of choice',
                            graphics = T)

for (item in ordered_item) {
  if (item %in% ordered_item) {
    available_order <- append(available_order, item)
  } else {
    print(paste(item, "is not available at Rosemary's shop"))
  }
}
#-----------------------------------------------------
# call the available_order
available_order


result <- available_order %in% shop3$Item
result
which(shop3$Item == available_order)

assigned_row <- which(rownames(shop4) == item)
shop4[assigned_row, 2] <- price
return(shop4[assigned_row, ])
##ii.	Their corresponding price. 
##iii.	Receives items customers want to buy, and their 
#corresponding quantities. 
##iv.	Computes their total payment. 
##v. Finally display to the customers, a receipt (with 
#the item purchased, quantity, corresponding prices and 
#total price). Also display discounts (if any)
