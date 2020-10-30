# install.packages("stringr")
# install.packages("xlsx")
library("stringr")
library("xlsx")

gospodarstwa = read.table(file="./storage/ROLN_3179.csv", header=TRUE, sep=";")

# Sprawdź czy wartości dla wiersza POLSKA są poprawne we wszystkich kolumnach. Odpowiedź zapisz do pliku pol.csv. Podpowiedź: ? colSums.

colNames = names(gospodarstwa)
polandRowIndex = which(gospodarstwa$Nazwa == "POLSKA")[1]
regions = gospodarstwa$Nazwa

valuesCols = which(colNames != "Nazwa" & colNames != "Kod" & colNames != "X")
firstCol = valuesCols[1]
lastCol = valuesCols[length(valuesCols)]

category = c()
categorySum = c()
categoryOriginalSum = c()
categoryIsEqual = c()

for(i in firstCol:lastCol) {
  colVector = gospodarstwa[,i]
  colSum = sum(colVector[-polandRowIndex])
  originalSum = colVector[polandRowIndex]

  category = c(category, colNames[i])
  categorySum = c(categorySum, colSum)
  categoryOriginalSum = c(categoryOriginalSum, originalSum)
  categoryIsEqual = c(categoryIsEqual, colSum == originalSum)
}

polandResult = data.frame(category, categorySum, categoryOriginalSum, categoryIsEqual)
write.table(polandResult, file="pol.csv")

# Oblicz średnią powierzchnię użytków rolnych na jedno gospodarstwo w podziale na lata i województwa.

areaCols = which(str_extract(colNames, "gospodarstwa.ogółem.powierzchnia.użytków.rolnych") != "")
unitCols = which(str_extract(colNames, "gospodarstwa.ogółem.gospodarstwa") != "")

for(i in 2:nrow(gospodarstwa)) {
  print(gospodarstwa$Nazwa[i])
  areaSum = 0
  unitSum = 0

  for(j in 1:length(areaCols)) {
    areaSum = areaSum + gospodarstwa[i,areaCols[j]]
  }

  for(j in 1:length(unitCols)) {
    unitSum = unitSum + gospodarstwa[i,unitCols[j]]
  }

  averageArea = areaSum / unitSum
  print(averageArea)
}

years = 2006:2018

for(i in years[1]:years[length(years)]) {
  print(sprintf("ROK %d", i))
  yearUnits = gospodarstwa[sprintf("gospodarstwa.ogółem.gospodarstwa.%d....", i)]
  yearArea = gospodarstwa[sprintf("gospodarstwa.ogółem.powierzchnia.użytków.rolnych.%d..ha.", i)]
  print(sum(yearArea[-1,1]) / sum(yearUnits[-1,1]))
}


# Wczytaj dane z pliku ROLN_3179.xlsx arkusz OPIS.

description = read.xlsx(file="./storage/ROLN_3179.xlsx", sheetName="OPIS")

# Rozdziel na słowa wartości z kolumny B. Wybierz unikalne wartości i zapisz je do pliku w formacie CSV (1 kolumna „słowa”) o nazwie słowa.txt

words = c()

for(i in 1:length(description$ROLNICTWO)) {
  words = c(words, strsplit(tolower(description$ROLNICTWO[i]), " ")[[1]])
}

wordsFrame = data.frame(words = unique(words))
write.table(wordsFrame, file="słowa.txt")

