# Convert sturcture to xlsx
install.packages("writexl")
library(writexl)

# um diese csv Datei zu bekommen wurde structure in csv verändert am Dateiende
text <- read.csv("C:/Users/Gronefeld/Desktop/D&D_examples/Ariagona_example/populationsV6.str")


# create a dataframe with several columns
dataframetext <- data.frame()
for (i in 1:nrow(text)) {
  row <- strsplit(text[i,1], "\t")[[1]]
  dataframetext <- rbind(dataframetext, row)
}

# new colnames
column_names <- c("ind", paste0("L", 1:(length(strsplit(text[1, 1], "\t")[[1]]) - 1), "_1"))
colnames(dataframetext) <- column_names

# name of the second allele
add_column_names <- paste0("L", 1:(ncol(dataframetext) -1), "_2")

# add new empty columns
for (column_name in add_column_names) {
  dataframetext[[column_name]] <- NA
}

# transfer information of the second allele individual to new empty columns
for (y in seq(from =2, to = nrow(dataframetext), by =2)) { # rows with information about the second allele
  info_second_allele <- dataframetext[y, 2:((ncol(dataframetext)+1)/2)] # [ind (allele2), first Locus1 to last Locus1]
  dataframetext[y-1, (((ncol(dataframetext)+1)/2)+1):ncol(dataframetext)] <- info_second_allele # (((ncol(dataframetext)+1)/2)+1)  = Start of Locus 2, (((ncol(dataframetext)+1)/2)+1):ncol(dataframetext) = fist Locus 2 to last Locus 2
}

# delete every second row
dataframetext <- dataframetext[-seq(2, nrow(dataframetext), by = 2), ]

# sort dataframe
new_order_col <- c("ind")
for (i in 2:(((ncol(dataframetext)+1)/2))) {
  new_order_col <- c(new_order_col, colnames(dataframetext)[i], colnames(dataframetext)[i+ (((ncol(dataframetext)+1)/2)-1)])
}
dataframetext <- dataframetext[, new_order_col]

# if there is a second row for sites:
dataframetext <- dataframetext[, !(names(dataframetext) %in% "L1_1")]
names(dataframetext)[names(dataframetext) == "L1_2"] <- "site"

# for the program the first two columns must seem empty
names(dataframetext)[names(dataframetext) == "ind"] <- "...1"
names(dataframetext)[names(dataframetext) == "site"] <- "...2"
# and every second column too
two_chain <- seq(4, by = 2, length.out = sum(grepl("^L[0-9]+_2$", names(dataframetext))))
counter <- 1  
names(dataframetext) <- sapply(names(dataframetext), function(col) {
  if (grepl("^L[0-9]+_2$", col)) {  
    new_name <- paste0("...", two_chain[counter])  
    counter <<- counter + 1  
    return(new_name)
  }
  return(col)  
})

# save as xlsx. file
write_xlsx(dataframetext, "C:/Users/Gronefeld/Desktop/D&D_examples/Ariagona_example/populationsV6.xlsx")
