Oncorhynchus_mykiss <- readLines("Oncorhynchus_mykiss.txt")
Oncorhynchus_mykiss_strsplit <- strsplit(Oncorhynchus_mykiss, "\t")
Oncorhynchus_mykiss <- as.data.frame(do.call(rbind, Oncorhynchus_mykiss_strsplit))


library(writexl)
write_xlsx(Oncorhynchus_mykiss, "Oncorhynchus_mykiss.xlsx")


Abies_alba <- readLines("Abies_alba.txt")
Abies_alba_strsplit <- strsplit(Abies_alba, "\t")
Abies_alba <- as.data.frame(do.call(rbind, Abies_alba_strsplit))



### Ergänzungen bei Abies alba

Abies_alba <- Abies_alba[-1, ]
Abies_alba <- Abies_alba[,-1]
Abies_alba <- Abies_alba[,-3]
Abies_alba <- Abies_alba[,-3]
Abies_alba$V2 <- 1:length(Abies_alba$V2)

spalten_namen <- colnames(Abies_alba)[3:ncol(Abies_alba)]
for (spalte in spalten_namen) {
  neue_spalte_name <- paste0(spalte, "_2")
  Abies_alba[, neue_spalte_name] <- Abies_alba[, spalte]
}

neue_reihenfolge <- c("V2", "V3")
for (i in 6:142) {
  neue_reihenfolge <- c(neue_reihenfolge, paste0("V", i), paste0("V", i, "_2"))
}
Abies_alba <- Abies_alba[, neue_reihenfolge]

library(stringr)
spalten_namen <- colnames(Abies_alba)[3:ncol(Abies_alba)]
for (spalten_name in spalten_namen) {
  if (endsWith(spalten_name , "_2")) {
     letzte_zahlen <- as.numeric(str_extract(Abies_alba[, spalten_name], "\\d+$"))
     Abies_alba[, spalten_name] <- letzte_zahlen
  } else {
    erste_zahlen <- erste_zahlen <- as.numeric(str_extract(Abies_alba[, spalten_name], "\\d+"))
    Abies_alba[, spalten_name] <- erste_zahlen
  }
}


write_xlsx(Abies_alba, "Abies_alba.xlsx")


# Carcinus
Carcinus_meanas <- readLines("Carcinus meanas.txt")
Carcinus_meanas <- strsplit(Carcinus_meanas, " ")
Carcinus_meanas <- unlist(Carcinus_meanas)[unlist(Carcinus_meanas) != ""]
Carcinus_meanas <- Carcinus_meanas[Carcinus_meanas != ","]

# Alles ist ein Vektor die Spalten sollen 12 sein. Schreib den Vektor in ein df
zeichen_pro_reihe <- 12
# Berechnung der Anzahl der benötigten Zeilen
anzahl_reihen <- length(Carcinus_meanas) %/% zeichen_pro_reihe
# Erstellung des DataFrames
df <- data.frame(matrix(NA, nrow = anzahl_reihen, ncol = zeichen_pro_reihe))
# Befüllen des DataFrames
for (i in 1:anzahl_reihen) {
  start_index <- (i - 1) * zeichen_pro_reihe + 1
  end_index <- i * zeichen_pro_reihe
  df[i, ] <- Carcinus_meanas[start_index:end_index]
}
# Benennung der Spalten
colnames(df) <- paste0("Zeichen", 1:zeichen_pro_reihe)
# Ausgabe des DataFrames
print(df)

spalten_namen <- colnames(df)[2:ncol(df)]
for (spalte in spalten_namen) {
  neue_spalte_name <- paste0(spalte, "_2")
  df[, neue_spalte_name] <- df[, spalte]
}
neue_reihenfolge <- c("Zeichen1")
for (i in 2:12) {
  neue_reihenfolge <- c(neue_reihenfolge, paste0("Zeichen", i), paste0("Zeichen", i, "_2"))
}
# DataFrame neu anordnen
df <- df[, neue_reihenfolge]

 pop_id <- df$Zeichen1

library(stringr)
spalten_namen <- colnames(df)
for (spalten_name in spalten_namen) {
  if (endsWith(spalten_name, "_2")) {
    # Für Spalten mit "_2" am Ende die letzten drei Ziffern extrahieren
    letzte_zahlen <- as.numeric(str_extract(df[, spalten_name], "\\d{3}$"))
    df[, spalten_name] <- letzte_zahlen
  } else {
    # Für alle anderen Spalten die ersten drei Ziffern extrahieren
    erste_zahlen <- as.numeric(str_extract(df[, spalten_name], "^\\d{3}"))
    df[, spalten_name] <- erste_zahlen
  }
}

df$Zeichen1 <- pop_id

# erste Spalteninformationen in zwei Spalten aufteilen 
neue_spalte <- 1:nrow(df)
# DataFrame aktualisieren
df <- cbind(neue_spalte, df)
head(df)
df$Zeichen1 <- str_replace_all(df$Zeichen1, "[0-9_]", "")

# 0 durch -9 ersetzten
df <- data.frame(lapply(df, function(x) ifelse(x == 0, -9, x)))

# alles nur fehlende Daten löschen
df <- df[apply(df[, 3:ncol(df)], 1, function(row) !all(row == -9)), ]

write_xlsx(df, "Carcinus_meanas.xlsx")