library("readxl")

# Daten einlesen
df <- read_xlsx("C:/Users/Gronefeld/Desktop/StructureD&D/Pinus_halepensis/Pinus_halepensis2/Pinus_halepensis.xlsx")
df <- data.frame(df)
colnames(df) <- gsub("\\.+", "_", colnames(df)) # Ersetzen von Punkten durch Unterstriche
# NA-Werte durch 0 ersetzen
# df[is.na(df)] <- 0  # Pinus
#df <- df[, -((ncol(df)-1):ncol(df))] # Panthera

# Erstellen der wiederholten Strings für die ersten beiden Spalten
individual_str <- rep(df[, 1], each = 2)
location_str <- rep(df[, 2], each = 2)
location_str <- gsub(" ", "_", location_str)

# Berechnen der kombinierten Werte für alle Spalten, die in Paaren vorliegen
combined_values <- lapply(seq(3, ncol(df), by = 2), function(i) {
  c(mapply(c, df[, i], df[, i + 1]))
})

# Erstellen des neuen DataFrames
result_df <- data.frame(
  individual = individual_str,
  site = location_str,
  do.call(cbind, combined_values)
)

write.table(result_df, file = "C:/Users/Gronefeld/Desktop/StructureD&D/Pinus_halepensis/Pinus_halepensis2/Pinus_halepensis.str", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
