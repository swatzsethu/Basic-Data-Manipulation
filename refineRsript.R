df <- read.csv("refine_original.csv", header = TRUE, stringsAsFactors = FALSE)
head(df)
tail(df)
dim(df)
df$company
correctcompanynames <- function(x)
{
  x <- tolower(x)
  n <- nchar(x)
  first_2 <- substr(x,1,2)
  last_2 <- substr(x, n-1, n)
  if(last_2=='ps'){
    return('philips')}
  else if(first_2=='ak'){
    return('akzo')}
  else if(first_2=='va'){
    return('van houten')}
  else {
    return('unilever')}
}
correctcompanynames(df$company[1])
df$company <- sapply(df$company, FUN = correctcompanynames)
df$company <- factor(df$company)
df$company


products_codes <- strsplit(df$Product.code...number, split = "-")
df$product_code <- sapply(products_codes, FUN = function(x) x[1])
df$product_number <- sapply(products_codes, FUN = function(x) x[2])
df$product_number <- as.integer(df$product_number)

e <- c('p' = 'Smartphone', 'v' = 'TV', 'x' = 'Laptop', 'q' = 'Tablet')
df$product_category <- factor(e[df$product_code])
df$product_category

df$full_address <- paste(df$address, df$city, df$country,sep = ', ')
names(df)

create_dummy <- function(vec, value){
  return(as.integer(vec == value))
}
company_names <- as.character(unique(df$company))

for(company in company_names){
  new_var_name <- paste0('company_',company)
  df[[new_var_name]] <- create_dummy(df$company, company)
}

product_names <- as.character(unique(df$product_category))
for(product in product_names){
  new_var_name <- paste0('product_',product)
  df[[new_var_name]] <- create_dummy(df$product_category, product)
}
names(df)
write.csv(df, file='refine_clean.csv')
