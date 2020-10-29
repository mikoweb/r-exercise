gospodarstwa = read.table(file="./storage/ROLN_3179.csv", header=TRUE, sep=";")

# Sprawdź czy wartości dla wiersza POLSKA są poprawne we wszystkich kolumnach. Odpowiedź zapisz do pliku pol.csv. Podpowiedź: ? colSums.

names = names(gospodarstwa)
polandRowIndex = which(gospodarstwa$Nazwa == "POLSKA")[1]
regions = gospodarstwa$Nazwa

valuesCols = which(names != "Nazwa" & names != "Kod" & names != "X")
firstCol = valuesCols[1]
lastCol = valuesCols[length(valuesCol)]

category = c()
categorySum = c()
categoryOriginalSum = c()
categoryIsEqual = c()

for(i in firstCol:lastCol) {
  colVector = gospodarstwa[,i]
  colSum = sum(colVector[-polandRowIndex])
  originalSum = colVector[polandRowIndex]

  category = c(category, names[i])
  categorySum = c(categorySum, colSum)
  categoryOriginalSum = c(categoryOriginalSum, originalSum)
  categoryIsEqual = c(categoryIsEqual, colSum == originalSum)
}

polandResult = data.frame(category, categorySum, categoryOriginalSum, categoryIsEqual)
write.table(polandResult, file="pol.csv")


