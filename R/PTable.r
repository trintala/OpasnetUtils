# PTABLE ########################################
## PTable muuntaa arvioinnin todennäköisyystaulun sopivaan muotoon arviointia varten.
## Parametrit: P = todennäköisyystaulu Opasnet-kannasta kaivettuna.
##             n = iteraatioiden lukumäärä Monte Carlossa
## Todennäköisyystaulun sarakkeiden on oltava: Muuttuja, Selite, Lokaatio, P
## Tuotteena on Monte Carloa varten tehty taulu, jonka sarakkeina ovat
## n (iteraatio) ja kaikki todennäköisyystaulussa olleet selitteet, joiden riveille on arvottu
## lokaatiot niiden todennäköisyyksien mukaisesti, jotka todennäköisyystaulussa oli annettu.

PTable <- function(P, n) {
Pt <- unique(P[,c("Muuttuja", "Selite")])
Pt <- data.frame(Muuttuja = rep(Pt$Muuttuja, n), Selite = rep(Pt$Selite, n), obs = rep(1:n, each = nrow(Pt)), P = runif(n*nrow(Pt), 0, 1))
for(i in 2:nrow(P)){P$Result[i] <- P$Result[i] + ifelse(P$Muuttuja[i] == P$Muuttuja[i-1] & P$Selite[i] == P$Selite[i-1], P$Result[i-1], 0)}
P <- merge(P, Pt)
P <- P[P$P <= P$Result, ]
Pt <- as.data.frame(as.table(tapply(P$Result, as.list(P[, c("Muuttuja", "Selite", "obs")]), min)))
colnames(Pt) <- c("Muuttuja", "Selite", "obs", "Result")
Pt <- Pt[!is.na(P$Result), ]
P <- merge(P, Pt)
P <- P[, !colnames(P) %in% c("Result", "P", "Muuttuja")]
P <- reshape(P, idvar = "obs", timevar = "Selite", v.names = "Lokaatio", direction = "wide")
colnames(P) <- ifelse(substr(colnames(P), 1, 9) == "Lokaatio.", substr(colnames(P), 10,30), colnames(P))
return(P)
}