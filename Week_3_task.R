###########################################################
###########################################################

## Week 3 task

########################################################### 
###########################################################
#Copied and pasted the data to a google sheet to avoid typing
#then imported the data

#-----------------------------------------------------

#Import the data
library(gsheet) #load the gsheet package
shop <- 'https://docs.google.com/spreadsheets/d/1FWBeZrcvLqktuhyw1rGoWle4fhaVS6ouDAbeecElozg/edit?usp=sharing'

shop2 <- read.csv(text=gsheet2text(shop, format='csv'), stringsAsFactors=FALSE)
View(shop2)
Items <- shop2[ ,2] # Items in the Rosemary's Shop
Qty <- shop2[ ,3] #Quantity of items available
Price <- shop2[ ,4] #Price of item

ordered_item <- NULL #customers' order 
# Get customers' order from graphics output
available_order <- NULL #available items in the shop from customers' order.
#available_order is set once the item has been set to be available in the shop.

## Select your order at the shop
ordered_item <- select.list(choice = Items,
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

### A
# 1. Function to reassigning a value to a vector (in this case price)
# 'change_price' function - 
# 2. Function to add more rows and also add values to the vectors
# 'add_items' function - add items, quantity, price
# 3. Function containing arguments that sums up all items bought
# irrespective of the quantity bought adding a receipt (use paste() or print())
# vat is in the if condition parenthesis 

# 3a.having an if condition of VAT of 20% when customers buy less than 5 items.
# < 5 items
if (item < 5) {
  print(paste("Items bought", items, "Quantity bought", quantity, "Total Price + 20% VAT", price))
} else {
  if (items > 10) {
    print(paste("Items bought", items, "Quantity bought", quantity, "Total Price + 20% VAT", price))
  } 
}
#meaning that the item arguments would have been assigned to NULL
# else 30% VAT for customers buying more that 10 items (items > 10)

# else 800 bonus to customers purchasing more than 10 items with the least amount being 100
# (items > 10 | total_price > 1000) subtract 800
# function 'update' to calculate stock left after purchase has been made
# function 'total_gain_per_day' to calculate the gain made per day by shop
# how do we calculate gains when we don't have the idea of the cost price of purchase
# by the Rosemary shop.


# How will all the above functions update the masterlist.




### Argument for number of items
# Argument for quantity of items bought
# A function that reduces the number of item in the shop as the shopper buys
# 
