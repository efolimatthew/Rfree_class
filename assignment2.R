stock <-
  data.frame(
    "Item" = character(0),
    "AvailableQuantity" = numeric(0),
    "PricePerQuantity" = numeric(0),
    stringsAsFactors = FALSE
  )

Price <- function(price) {
  p = price
  return(p)
}

AddItem <- function(item, quantity, price) {
  stock =
    rbind(
      stock,
      data.frame(
        "Item" = item,
        "AvailableQuantity" = quantity,
        "PricePerQuantity" = price
      )
    )
}

Goods <- function(item, quantity) {
  
}
SubTotal <- function(item, quantity) {
  for (i in item) {
  }
}