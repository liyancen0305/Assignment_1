# Importing Data
myDir <- "C:/Users/Lishuang Cen/Desktop/Intro to Data Science/refine.csv"
#origDataFile <- file.path(myDir, "refine_original.csv")
refine_Original <- read.table(myDir, header = TRUE, sep = ",")

# Clean up brand names

# a. set function for "philip" "akzo","van houten", and "unilever"
cleanCompName <- function(x) {
  x <- tolower(x)
  if (substring(x, 1, 1) == "p" | substring(x, 1, 1) == "f") {
    return("philips")
  }
  else if (substring(x,1,1)== "a"){
    return("akzo")}
  else if (substring(x,1,1)== "v"){
    return("van houten")
  }
  else if (substring(x,1,1)== "u"){
    return("unilever")
  }else{
    return(x)
  }
}


#apply to the fist column

refine_Original$company <- lapply(refine_Original$company, cleanCompName)

# separate product code and number
refine_Separate <- separate(refine_Original, Product.code...number, c("product_code", "product_number"), sep="-")

# add a column with the product category for each record
# a. add a new column to a table

data_Refine_category <- function(c){
  if (substring(c, 1, 1) == "p") {
    return("Smartphone")}
  else if (substring(c, 1, 1) == "v") {
    return("TV")
  } else if(substring(c, 1, 1) =="x") {
    return("Laptop")
  } else  {
    return("Tablet")
  }
}

#refine_Original$product_category <- ""
refine_Separate$product_category <- lapply(refine_Separate$product_code, data_Refine_category)

#create dummyvariables for company and product category

refine_Separate$company_philips <- ifelse(refine_Separate$company == "philips", 1, 0)
refine_Separate$company_akzo <- ifelse(refine_Separate$company == "akzo", 1, 0)
refine_Separate$company_van_houten <- ifelse(refine_Separate$company == "van houten", 1, 0)
refine_Separate$company_unilever <- ifelse(refine_Separate$company == "unilever", 1, 0)



#unite three columns for address
data_Clean <- unite(refine_Separate, "address", address, city, country, sep=",")


write.table(refine_Original, "C:/Users/Lishuang Cen/Desktop/Intro to Data Science/refine_original.csv")
write.table(data_Clean, "C:/Users/Lishuang Cen/Desktop/Intro to Data Science/refine_clean.csv")

#origDataFile < file.path("C:/Users/Lishuang Cen/Desktop/Intro to Data Science","refine_original.csv")
#cleanDataFile < file.path("C:/Users/Lishuang Cen/Desktop/Intro to Data Science","refine_clean.csv")
#refineOutFile_Original <- file.path(myDir, "refine_original.csv")
#refineOutFile_Original <- file.path(data_Clean, "refine_clean.csv")
#write.csv(data_Refine, refineOutFile)
#write.csv(data_Refine, "refine_clean.csv")
#export(data_Refine, "refine_original.csv")
#export(data_Clean, "refine_clean.csv")
