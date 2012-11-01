## Load the geographies dataset as an R dataframe and save the corresponding R
## objects

gsslookup <- read.csv(file = "gsslookup.csv", header = TRUE)

save(gsslookup, file = "gsslookup.Rdata")
