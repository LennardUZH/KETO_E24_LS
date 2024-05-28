## ==============================================================
#     Inhaltsverzeichnis:
#     1.1 Erstes Dendrogram (Zeile 13)
#     1.2 Zweites Dendrogram mit Experten Meinung (Zeile 61)
#     1.3 Drittes Diagramm (Varianz und Korrelation) (Zeile 191)
#     1.4 Viertes Diagramm (Kombination aus 2 und 3) (Zeile 280)
#     2.0 Anatz mit Binomial-Test (Zeile 393)
## ==============================================================


# Daten einlesen, die Spaltennamen müssen noch umbenannt werden, da R keine ö/ä/ü
# lesen kann
data <- read.csv(file.choose(), header=TRUE, sep =";")

# ===============================
#          1.1 Erstes Dendrogram
# ===============================
# Alle Variablen von der unseriösen Bedingung in ein data frame ohne Theorie
data_unserioes <- data[, 112:161]

hc_1 <- hclust(dist(data_unserioes, method = "euclidean"), method = "single")
plot(hc_1)
hc_2 <- hclust(dist(data_unserioes, method = "manhattan"), method = "single")
plot(hc_2)
hc_3 <- hclust(dist(data_unserioes, method = "euclidean"), method = "complete")
plot(hc_3)
hc_4 <- hclust(dist(data_unserioes, method = "manhattan"), method = "complete")
plot(hc_4)

# wenn mans hinkriegt, könnte man auch das versuchen zu analysieren

# ---------------------------------
#          Umrechnungen
# ---------------------------------
convert_to_seconds <- function(time_string) {
  time_parts <- unlist(strsplit(time_string, ":"))
  hours <- as.numeric(time_parts[1])
  minutes <- as.numeric(time_parts[2])
  seconds <- as.numeric(time_parts[3])
  total_seconds <- hours * 3600 + minutes * 60 + seconds
  return(total_seconds)
}

# Anwenden der Funktion auf die Daten S_Duration, S_Max_Duration, S_Sart und S_Total_Duration
S_Duration_converted_to_seconds <- sapply(data_unserioes$S_Duration, convert_to_seconds)
S_Max_Duration_converted_to_seconds <- sapply(data_unserioes$S_Max_Duration, convert_to_seconds)
S_Sart_converted_to_seconds <- sapply(data_unserioes$S_Start, convert_to_seconds)
S_Total_Duration_converted_to_seconds <- sapply(data_unserioes$S_Total_Duration, convert_to_seconds)

# Konvertierte Daten data_unseriös hinzufügen
data_unserioes <- cbind(data_unserioes, S_Duration_converted_to_seconds, 
                      S_Max_Duration_converted_to_seconds, 
                      S_Sart_converted_to_seconds, S_Total_Duration_converted_to_seconds)

# NA werte bei S_Nachberechnetes.Haus, S_Anzahl.Fehler.in.Bezug.auf.nachberechnetes.HAUS, S_Summer.der.quadrierten.Fehlerwerte
data_unserioes$S_Nachberechnetes.Haus[is.na(data_unserioes$S_Nachberechnetes.Haus)] <- -100
data_unserioes$S_Anzahl.Fehler.in.Bezug.auf.nachberechnetes.HAUS[is.na(data_unserioes$S_Anzahl.Fehler.in.Bezug.auf.nachberechnetes.HAUS)] <- -100
data_unserioes$S_Summer.der.quadrierten.Fehlerwerte[is.na(data_unserioes$S_Summer.der.quadrierten.Fehlerwerte)] <- -100

# =======================================================
#          1.2 Zweites Dendrogram mit Experten Meinung
# =======================================================
# Nur mit S_Nachberechnetes.Haus
# --------------------------------
data_V1 <- scale(data.frame(data_unserioes$S_Nachberechnetes.Haus))

# Dendrogram erstellen
hc_V1 <- hclust(dist(data_V1, method = "euclidean"), method = "complete")
plot(hc_V1)
hc_V1 <- hclust(dist(data_V1, method = "manhattan"), method = "complete")
plot(hc_V1)
hc_V1 <- hclust(dist(data_V1, method = "euclidean"), method = "single")
plot(hc_V1)
hc_V1 <- hclust(dist(data_V1, method = "manhattan"), method = "single")
plot(hc_V1)
hc_V1 <- hclust(dist(data_V1, method = "euclidean"), method = "average")
plot(hc_V1)
hc_V1 <- hclust(dist(data_V1, method = "manhattan"), method = "average")
plot(hc_V1)

# Mit S_Nachberechnetes.Haus, S_Anzahl.Fehler.in.Bezug.auf.nachberechnetes.HAUS 
# oder S_Summer.der.quadrierten.Fehlerwerte
#------------------------------------------------------------------------------
data_V2 <- scale(data.frame(data_unserioes$S_Anzahl.Fehler.in.Bezug.auf.nachberechnetes.HAUS))
data_V2 <- cbind(data_V1, data_V2)

# Dendrogram erstellen
hc_V2 <- hclust(dist(data_V2, method = "manhattan"), method = "average")
plot(hc_V2)

data_V2 <- scale(data.frame(data_unserioes$S_Summer.der.quadrierten.Fehlerwerte))
data_V2 <- cbind(data_V1, data_V2)

# Dendrogram erstellen
hc_V2 <- hclust(dist(data_V2, method = "manhattan"), method = "average")
plot(hc_V2)

# Mit S_Nachberechnetes.Haus, S_Summer.der.quadrierten.Fehlerwerte,  
# S_Sum_Cues
#-------------------------------------------------------------------
data_V3 <- data.frame(data_unserioes$S_Sum_Cues)
data_V3 <- scale(data_V3)
data_V3 <- cbind(data_V2, data_V3)

# Dendrogram erstellen
hc_V3 <- hclust(dist(data_V3, method = "manhattan"), method = "average")
plot(hc_V3)

# Mit S_Nachberechnetes.Haus, S_Summer.der.quadrierten.Fehlerwerte,  
# S_Sum_Cues, S_Anz_RA.
#-------------------------------------------------------------------
data_V4 <- data.frame(data_unserioes$S_Anz_RA.)
data_V4 <- scale(data_V4)
data_V4 <- cbind(data_V3, data_V4)

# Dendrogram erstellen
hc_V4 <- hclust(dist(data_V4, method = "manhattan"), method = "average")
plot(hc_V4)

# Mit S_Nachberechnetes.Haus, S_Summer.der.quadrierten.Fehlerwerte,  
# S_Sum_Cues, S_Anz_RA., S_Anz_NotChosenMaxOverallProb, S_Anz_ChosenOptionProb25withInfo
#----------------------------------------------------------------------------------------
data_V5 <- data.frame(data_unserioes[, c("S_Anz_NotChosenMaxOverallProb", "S_Anz_ChosenOptionProb25withInfo")])
data_V5 <- scale(data_V5)
data_V5 <- cbind(data_V4, data_V5)

# Dendrogram erstellen
hc_V5 <- hclust(dist(data_V5, method = "manhattan"), method = "average")
plot(hc_V5)

# Mit S_Nachberechnetes.Haus, S_Summer.der.quadrierten.Fehlerwerte,  
# S_Sum_Cues, S_Anz_RA., S_Anz_NotChosenMaxOverallProb, S_Anz_ChosenOptionProb25withInfo
# S_Duration_converted_to_seconds
#------------------------------------------------------------------------------------------
data_V6 <- scale(data.frame(data_unserioes$S_Duration_converted_to_seconds))
data_V6 <- cbind(data_V5, data_V6)
data_V6 <- data.frame(data_V6)

# Dendrogram erstellen
hc_V6 <- hclust(dist(data_V6, method = "manhattan"), method = "complete")
plot(hc_V6)

#          Diagramm hc_V6 schneiden (konformativ)
# ------------------------------------------------------
höhe <- 15  # Schwellenwert für die Höhe für 3 Cluster
cluster <- cutree(hc_V6, h = höhe)
print(cluster)

# Extrahiere die Cluster als Subdatensätze nicht standardisiert für interpretation
subdatensätze <- lapply(unique(cluster), function(cl) data_V6[cluster == cl, ])

# Überprüfe die Subdatensätze
subdata_2.1.0 <- (subdatensätze[[1]])
subdata_2.2.0 <- (subdatensätze[[2]])
subdata_2.3.0 <- (subdatensätze[[3]])

# centeroiden erstellen
C_2.1.0 <- colMeans(subdata_2.1.0)
C_2.2.0 <- colMeans(subdata_2.2.0)
C_2.3.0 <- colMeans(subdata_2.3.0)
centroids_2.1.0 <- data.frame(C_2.1.0, C_2.2.0, C_2.3.0)

#          Diagramm hc_V6 schneiden (explorativ)
# ----------------------------------------------------
höhe <- 8  # Schwellenwert für die Höhe für 5 Cluster
cluster <- cutree(hc_V6, h = höhe)
print(cluster)

# Extrahiere die Cluster als Subdatensätze nicht standardisiert für interpretation
subdatensätze <- lapply(unique(cluster), function(cl) data_V6[cluster == cl, ])

# Überprüfe die Subdatensätze
subdata_2.1.1 <- (subdatensätze[[1]])
subdata_2.2.1 <- (subdatensätze[[2]])
subdata_2.3.1 <- (subdatensätze[[3]])
subdata_2.4.1 <- (subdatensätze[[3]])
subdata_2.5.1 <- (subdatensätze[[3]])


# Centeroiden erstellen
C_2.1.1 <- colMeans(subdata_2.1.1)
C_2.2.1 <- colMeans(subdata_2.2.1)
C_2.3.1 <- colMeans(subdata_2.3.0)
C_2.4.1 <- colMeans(subdata_2.4.1)
C_2.5.1 <- colMeans(subdata_2.5.1)

centroids_2.1.0 <- data.frame(C_2.1.1, C_2.2.1, C_2.3.1, C_2.4.1, C_2.5.1)

print(colnames(data_V6))
# =======================================================
#          1.3 Drittes Diagramm (Varianz und Korrelation)
# =======================================================

# Variblen ausschliessen, bei denen keine Varianz gerechnet werden kann
print(colnames(data_unserioes))
data_unserioes_1.0 <- data_unserioes[, c(1:3, 9:10, 14:40, 42:53)]

# Varianzen berechnen
variances <- apply(data_unserioes_1.0, 2, var)
print(variances)

# Daten Satz mit nur Variablen mit höheren Varianzen (> 1)
vars_to_keep <- names(variances)[variances >= 1]
data_unserioes_1.1 <- data_unserioes_1.0[, vars_to_keep]

# Daten standardiesieren
data_unserioes_1.1_stadardized_1.0 <- as.data.frame(scale(data_unserioes_1.1))

# Korrelationsmatrix berechnen
cor(data_unserioes_1.1_stadardized_1.0)

# nach sehr Konservativer Grenzwerten >=.8 Werte Ausschliessen!!!! <-------------------- noch mal machen
print(colnames(data_serioes1.0_stadardized_1.0))
data_unserioes_1.1_stadardized_1.1 <- subset(data_unserioes_1.1_stadardized_1.0, 
                                             select = -c(S_Anzahl.Fehler.in.Bezug.auf.nachberechnetes.HAUS, S_Anz_MRDM, S_Anz_T5,
                                                         S_Diff_Range_zwischen.nachberechnetem.und.eingegrenztem.HAUS., S_Anz_RS.RA,
                                                         S_Anz_SAT, S_Anz_ChosenOptionProb25withInfo))
cor(data_unserioes_1.1_stadardized_1.1)

# Dendrogram erstellen
hc_data_unserioes_1.1_stadardized_1.1 <- hclust(dist(data_unserioes_1.1_stadardized_1.1, method = "euclidean"), method = "complete")
plot(hc_data_unserioes_1.1_stadardized_1.1)
hc_data_unserioes_1.1_stadardized_1.1 <- hclust(dist(data_unserioes_1.1_stadardized_1.1, method = "manhattan"), method = "complete")
plot(hc_data_unserioes_1.1_stadardized_1.1)
hc_data_unserioes_1.1_stadardized_1.1 <- hclust(dist(data_unserioes_1.1_stadardized_1.1, method = "euclidean"), method = "single")
plot(hc_data_unserioes_1.1_stadardized_1.1)
hc_data_unserioes_1.1_stadardized_1.1 <- hclust(dist(data_unserioes_1.1_stadardized_1.1, method = "manhattan"), method = "single")
plot(hc_data_unserioes_1.1_stadardized_1.1)
hc_data_unserioes_1.1_stadardized_1.1 <- hclust(dist(data_unserioes_1.1_stadardized_1.1, method = "euclidean"), method = "average")
plot(hc_data_unserioes_1.1_stadardized_1.1)
hc_data_unserioes_1.1_stadardized_1.1 <- hclust(dist(data_unserioes_1.1_stadardized_1.1, method = "manhattan"), method = "average")
plot(hc_data_unserioes_1.1_stadardized_1.1)

#          Diagram schneiden (konformativ)
# =======================================================
höhe <- 30  # Schwellenwert für die Höhe für 3 Cluster
cluster <- cutree(hc_data_unserioes_1.1_stadardized_1.1, h = höhe)
print(cluster)

# Extrahiere die Cluster als Subdatensätze nicht standardisiert für interpretation
subdatensätze <- lapply(unique(cluster), function(cl) data_unserioes_1.1_stadardized_1.1[cluster == cl, ])

# Überprüfe die Subdatensätze
subdata_3.1.0 <- (subdatensätze[[1]])
subdata_3.2.0 <- (subdatensätze[[2]])
subdata_3.3.0 <- (subdatensätze[[3]])

# centeroiden erstellen
C_3.1.0 <- colMeans(subdata_3.1.0)
C_3.2.0 <- colMeans(subdata_3.2.0)
C_3.3.0 <- colMeans(subdata_3.3.0)
centroids_3.1.0 <- data.frame(C_3.1.0, C_3.2.0, C_3.3.0)

#          Diagramm Schneiden (explorativ)
# =======================================================
höhe <- 26.7  # Schwellenwert für die Höhe für 4 Cluster
cluster <- cutree(hc_data_unserioes_1.1_stadardized_1.1, h = höhe)
print(cluster)

# Extrahiere die Cluster als Subdatensätze nicht standardisiert für interpretation
subdatensätze <- lapply(unique(cluster), function(cl) data_unserioes_1.1_stadardized_1.1[cluster == cl, ])

# Überprüfe die Subdatensätze
subdata_3.1.1 <- (subdatensätze[[1]])
subdata_3.2.1 <- (subdatensätze[[2]])
subdata_3.3.1 <- (subdatensätze[[3]])
subdata_3.4.1 <- (subdatensätze[[4]])
subdata_3.5.1 <- (subdatensätze[[5]])

# centeroiden erstellen
C_3.1.1 <- colMeans(subdata_3.1.1)
C_3.2.1 <- colMeans(subdata_3.2.1)
C_3.3.1 <- colMeans(subdata_3.3.1)
C_3.4.1 <- colMeans(subdata_3.1.1)
C_3.5.1 <- colMeans(subdata_3.2.1)
centroids_3.1.1 <- data.frame(C_3.1.1, C_3.2.1, C_3.3.1, C_3.4.1, C_3.5.1)

print(colnames(data_unserioes_1.1_stadardized_1.1))
# =======================================================
#          1.4 Viertes Diagramm (Kombination aus 2 und 3)
# =======================================================

# Daten von data_serioes1.1_stadardized_1.1
print(colnames(data_unserioes_1.1_stadardized_1.1))
data_unserioes_1.1_stadardized_1.2  <- subset(data_unserioes_1.1_stadardized_1.1, 
                                                select = -c(S_Age, S_Mean_Range..eingegrenztes.HAUS., 
                                                            S_Anz_NoWins, S_Winning_Amount, S_Teilnahmemotivation, 
                                                            S_Wichtigkeit.Spielgewinn, S_Verlustvermeidung, S_Finanzielle.Sicherheit,
                                                            S_Vertrauen.in.Haendler, S_Sart_converted_to_seconds))

# Dendrogramm erstellen
hc_data_unserioes_1.1_stadardized_1.2 <- hclust(dist(data_unserioes_1.1_stadardized_1.2, method = "euclidean"), method = "complete")
plot(hc_data_unserioes_1.1_stadardized_1.2)
hc_data_unserioes_1.1_stadardized_1.2 <- hclust(dist(data_unserioes_1.1_stadardized_1.2, method = "manhattan"), method = "complete")
plot(hc_data_unserioes_1.1_stadardized_1.2)
hc_data_unserioes_1.1_stadardized_1.2 <- hclust(dist(data_unserioes_1.1_stadardized_1.2, method = "euclidean"), method = "single")
plot(hc_data_unserioes_1.1_stadardized_1.2)
hc_data_unserioes_1.1_stadardized_1.2 <- hclust(dist(data_unserioes_1.1_stadardized_1.2, method = "manhattan"), method = "single")
plot(hc_data_unserioes_1.1_stadardized_1.2)
hc_data_unserioes_1.1_stadardized_1.2 <- hclust(dist(data_unserioes_1.1_stadardized_1.2, method = "euclidean"), method = "average")
plot(hc_data_unserioes_1.1_stadardized_1.2)
hc_data_unserioes_1.1_stadardized_1.2 <- hclust(dist(data_unserioes_1.1_stadardized_1.2, method = "manhattan"), method = "average")
plot(hc_data_unserioes_1.1_stadardized_1.2)

#          Diagramm Schneiden (konformativ)
# =======================================================
höhe <- 23  # Schwellenwert für die Höhe für 3 Cluster
cluster <- cutree(hc_data_unserioes_1.1_stadardized_1.2, h = höhe)
print(cluster)

# Extrahiere die Cluster als Subdatensätze nicht standardisiert für interpretation
subdatensätze <- lapply(unique(cluster), function(cl) data_unserioes_1.1_stadardized_1.2[cluster == cl, ])

# Überprüfe die Subdatensätze
subdata_4.1.0 <- (subdatensätze[[1]])
subdata_4.2.0 <- (subdatensätze[[2]])
subdata_4.3.0 <- (subdatensätze[[3]])

# centeroiden erstellen
C_4.1.0 <- colMeans(subdata_4.1.0)
C_4.2.0 <- colMeans(subdata_4.2.0)
C_4.3.0 <- colMeans(subdata_4.3.0)
centroids_4.1.0 <- data.frame(C_4.1.0, C_4.2.0, C_4.3.0)

#          Diagramm Schneiden (explorativ)
# =======================================================
höhe <- 18  # Schwellenwert für die Höhe für 6 Cluster
cluster <- cutree(hc_data_unserioes_1.1_stadardized_1.2, h = höhe)
print(cluster)

# Extrahiere die Cluster als Subdatensätze
subdatensätze <- lapply(unique(cluster), function(cl) data_unserioes_1.1_stadardized_1.2[cluster == cl, ])

# Überprüfe die Subdatensätze
subdata_4.1.1 <- (subdatensätze[[1]])
subdata_4.2.1 <- (subdatensätze[[2]])
subdata_4.3.1 <- (subdatensätze[[3]])
subdata_4.4.1 <- (subdatensätze[[4]])
subdata_4.5.1 <- (subdatensätze[[5]])
subdata_4.6.1 <- (subdatensätze[[6]])

# centeroiden erstellen
C_4.1.1 <- colMeans(subdata_4.1.1)
C_4.2.1 <- colMeans(subdata_4.2.1)
C_4.3.1 <- colMeans(subdata_4.3.1)
C_4.4.1 <- colMeans(subdata_4.4.1)
C_4.5.1 <- colMeans(subdata_4.5.1)
C_4.6.1 <- colMeans(subdata_4.6.1)
centroids_4.1.1 <- data.frame(C_4.1.1, C_4.2.1, C_4.3.1, C_4.4.1, C_4.5.1, C_4.6.1)

# Extrahiere die Cluster als Subdatensätze für Typen einteilung
subdatensätze <- lapply(unique(cluster), function(cl) data_unserioes[cluster == cl, ])

# Überprüfe die Subdatensätze
subdata_4.1.2 <- (subdatensätze[[1]])
subdata_4.2.2 <- (subdatensätze[[2]])
subdata_4.3.2 <- (subdatensätze[[3]])
subdata_4.4.2 <- (subdatensätze[[4]])
subdata_4.5.2 <- (subdatensätze[[5]])
subdata_4.6.2 <- (subdatensätze[[6]])

print(colnames(data_unserioes_1.1_stadardized_1.2))
colMeans(Lazy_Clickers$S_Anz_RS.)
#         Typen bestimmen
# -----------------------------------
# Hohe Durchscnittswerte der Nachberechnete HAUS, ORDM und summer der quadriterten Fehlerwerte
# Niedirge Durchschnittswerte im Risk Seeking, ChosenShip1 und in der Duration
Analytical_Perfectionists <- subdata_4.1.2

# Hohe Durchscnittswerte der ChosenShip1, Risk Seeking und Risk Avoidance
# Niedirge Durchschnittswerte beim ChosenShip3, ChosenShip2, ChosenShip4, 
# nachberechnete HAUS, S_Max_Cues
Lazy_Ship1_Clickers <- subdata_4.2.2

# Hohe Durchscnittswerte der Risk Seeking, Chosenship3, Chosenship2, Chosenship4
# Niedirge Durchschnittswerte bei der Nachberechneten HAUS, ORDM, Sum_Cues, MAX_Cues
Lazy_Clickers <- subdata_4.3.2

# Hohe Durchscnittswerte der Nachberechnetes HAUS, NotChosenMAxOverallProb, Max_Cues
# Niedirge Durchschnittswerte bei T2, RS, Ship1
False_Integrationers <- subdata_4.4.2

# Hohe Durchscnittswerte T2, ORDM, Nachberechnetes HAUS, Duration
# Niedirge Durchschnittswerte bei Risk Seeking, Ship1, Risk Avoiding, NotChosenMAxOverallProb
Conservative_Decision_Makers <- subdata_4.5.2

# Hohe Durchscnittswerte Sum_Cues, NotChosenMaxOverallProb, Duration, Max_Cues, RA, 
# Niedirge Durchschnittswerte bei Risk Seeking, ORDM, Ship1
Information_Enthusiasts <- subdata_4.6.2

# =======================================================
#          2.0 Anatz mit Binomial-Test
# =======================================================
#RS-Typ
p <- c(2/3)
binom.test(10, 22, p)
binom.test(11, 22, p)
binom.test(19, 22, p)
binom.test(20, 22, p)
#number of successes = 10, number of trials = 22, p-value = 0.04203
#number of successes = 11, number of trials = 22, p-value = 0.1138
#-> sogenante Risiko aversieven haben 10 oder weniger Durchgäge
#number of successes = 19, number of trials = 22, p-value = 0.06776
#number of successes = 20, number of trials = 22, p-value = 0.01281
#also müssen 20 oder mehr mal direkt ein Schiff ausgewählt werden, 
#damit es nicht-zufällig ist

#Für Lokal Verkaufen
p <- c(1/6)
binom.test(0, 22, p)
binom.test(1, 22, p)
binom.test(7, 22, p,)
binom.test(8, 22, p,)
#number of successes = 0, number of trials = 22, p-value = 0.03907
#number of successes = 1, number of trials = 22, p-value = 0.1583
#-> 0 = Risk Avoidant Lokal aversiv
#number of successes = 7, number of trials = 22, p-value = 0.07862
#number of successes = 8, number of trials = 22, p-value = 0.02096
# mehr als 8 Mal auf  zb Risk avoidant Lokalmakt verkauft wird als R_Lokal-Typ gezählt

#Für ORDM Verkaufen 1/6 * 5/6 = 5/30 = 1/6
p <- c(1/6)
binom.test(0, 22, p)
binom.test(1, 22, p)
binom.test(7, 22, p,)
binom.test(8, 22, p,)
#number of successes = 0, number of trials = 22, p-value = 0.03907
#number of successes = 1, number of trials = 22, p-value = 0.1583
#-> 0 = Risk Avoidant ORDM aversiv
#number of successes = 7, number of trials = 22, p-value = 0.07862
#number of successes = 8, number of trials = 22, p-value = 0.02096
# mehr als 8 Mal ORDM wird als ORDM-Typ gezählt

#für ChoosenOptionProb25withInfo wäre es min. 1/6 * 1/3 = 1/18
# 1/6 = Händler information 
# 1/3, da drei Schiffe ohne wahrscheinlichkeit gibt und ich eins davon ausgewählt wird
p <- c(1/18)
binom.test(0, 22, p)
binom.test(1, 22, p)
binom.test(2, 22, p,)
binom.test(3, 22, p,)
binom.test(4, 22, p,)
#number of successes = 0, number of trials = 22, p-value = 0.632
#number of successes = 3, number of trials = 22, p-value = 0.1203
#number of successes = 4, number of trials = 22, p-value = 0.03119
#alles gleich oder über 4 währe als nicht zufällig und würde als abgrenzung gelten

#für MRDM wäre es 1/6 * 1/6 = 1/36 das stimmt glaub nicht ganz
p <- c(1/36)
binom.test(0, 22, p)
binom.test(1, 22, p)
binom.test(2, 22, p,)
#number of successes = 3, number of trials = 22, p-value = 0.02224
#alles gleich oder über drei währe als nicht zufällig und würde als abgrenzung gelten

# alle fünf aufzudecken: (1/6)^5 = 1/7776
p <- c(1/7776)
binom.test(0, 22, p)
binom.test(1, 22, p)
#number of successes = 1, number of trials = 22, p-value = 0.002825
# alles aufzudecken wäre also schon bei einem Durchgang nicht random

#Bestimmtes Schiff wählen: 1/5 unabhänigg von Durchgängen
p <- c(1/5)
binom.test(0, 22, p)
binom.test(1, 22, p)
binom.test(2, 22, p,)
binom.test(3, 22, p,)
binom.test(4, 22, p,)
binom.test(5, 22, p)
binom.test(6, 22, p,)
binom.test(7, 22, p,)
binom.test(8, 22, p,)
binom.test(9, 22, p)
binom.test(10, 22, p,)
binom.test(11, 22, p,)
binom.test(4, 22, p,)
#number of successes = 0, number of trials = 22, p-value = 0.01352
#number of successes = 1, number of trials = 22, p-value = 0.1041
#-> wenn 0 Mal ausgewählt, nicht zufällig

#number of successes = 8, number of trials = 22, p-value = 0.06352
#number of successes = 9, number of trials = 22, p-value = 0.02752
#-> 9 oder mehr nicht zufällig
# alles aufzudecken wäre also schon bei einem Durchgang nicht random

summary(data_cluster6)
Typen5 <-cbind(data_cluster6, data_cluster[,c(29:32)])

Typen5$RS_Typ <- ifelse(Typen5$S_Anz_RS. >= 20, (Typen5$S_Anz_RS.-19)^2, 
                        ifelse(Typen5$S_Anz_RS. <= 10, (-1)*(Typen5$S_Anz_RS.-11)^2, 0))

Typen5$RA_Lokal_Typ <- ifelse(Typen5$S_Anz_RS.RA >= 8, (Typen5$S_Anz_RS.RA-7)^2, 
                              ifelse(Typen5$S_Anz_RS.RA <= 0, (-1)*(Typen5$S_Anz_RS.RA-1)^2, 0))

Typen5$T5_Lokal_Typ <- ifelse(Typen5$S_Anz_T5.RA >= 1, (Typen5$S_Anz_T5.RA-0)^2, 0)

Typen5$ORDM_Typ <- ifelse(Typen5$S_Anz_ORDM >= 8, (Typen5$S_Anz_ORDM-7)^2, 
                          ifelse(Typen5$S_Anz_ORDM <= 0, (-1)*(Typen5$S_Anz_ORDM-1)^2, 0))

Typen5$MRDM_Typ <- ifelse(Typen5$S_Anz_MRDM >= 1, (Typen5$S_Anz_MRDM-2)^2, 0)

Typen5$MRDM_Typ <- ifelse(Typen5$S_Anz_MRDM >= 1, (Typen5$S_Anz_MRDM-2)^2, 0)

Typen5$T5_Typ <- ifelse(Typen5$S_Anz_T5 >= 1, (Typen5$S_Anz_T5-0)^2, 0)

Typen5$Ship1_Typ <- ifelse(Typen5$S_Anz_ChosenShip1 >= 9, (Typen5$S_Anz_ChosenShip1-7)^2, 
                           ifelse(Typen5$S_Anz_ChosenShip1 <= 0, (-1)*(Typen5$S_Anz_ChosenShip1-1)^2, 0))
Typen5$Ship2_Typ <- ifelse(Typen5$S_Anz_ChosenShip2 >= 9, (Typen5$S_Anz_ChosenShip2-7)^2, 
                           ifelse(Typen5$S_Anz_ChosenShip2 <= 0, (-1)*(Typen5$S_Anz_ChosenShip2-1)^2, 0))
Typen5$Ship3_Typ <- ifelse(Typen5$S_Anz_ChosenShip3 >= 9, (Typen5$S_Anz_ChosenShip3-7)^2, 
                           ifelse(Typen5$S_Anz_ChosenShip3 <= 0, (-1)*(Typen5$S_Anz_ChosenShip3-1)^2, 0))
Typen5$Ship4_Typ <- ifelse(Typen5$S_Anz_ChosenShip4 >= 9, (Typen5$S_Anz_ChosenShip4-7)^2, 
                           ifelse(Typen5$S_Anz_ChosenShip4 <= 0, (-1)*(Typen5$S_Anz_ChosenShip4-1)^2, 0))


Typen5.1 <-subset(Typen5, select = c("RS_Typ", "RA_Lokal_Typ","T5_Lokal_Typ", "ORDM_Typ", "T5_Typ", "MRDM_Typ", "Ship1_Typ", "Ship2_Typ", "Ship3_Typ", "Ship4_Typ"))

hc_15 <- hclust(dist(Typen5.1, method = "manhattan"), method = "average")
plot(hc_15)

hc_1 <- hclust(dist(Typen5.1, method = "euclidean"), method = "single")
plot(hc_1)

hc_2 <- hclust(dist(Typen5.1, method = "manhattan"), method = "single")
plot(hc_2)

hc_3 <- hclust(dist(Typen5.1, method = "euclidean"), method = "complete")
plot(hc_3)

hc_4 <- hclust(dist(Typen5.1, method = "manhattan"), method = "complete")
plot(hc_4)

#nur ChoosenShip interessant
choosenShip <-subset(Typen5, select = c("Ship1_Typ", "Ship2_Typ", "Ship3_Typ", "Ship4_Typ"))

hc_4 <- hclust(dist(choosenShip, method = "manhattan"), method = "complete")
plot(hc_4)

Ship1_Group <- data[c(4, 9, 24, 3, 41, 27, 48, 40, 5, 39),]



Typen <- data.frame(
  
  RSTyp = 
    # 1  2  3  4  5  6  7  8  9  10
    c(0, 0, 1, 1, 1, 1, 0, 1, 0, 1, #01 - 10 
      1, 0, 0, 0, 0, 0, 0, 0, 1, 0, #11 - 20
      1, 0, 0, 0, 0, 1, 1, 1, 0, 0, #21 - 30
      0, 1, 0, 1, 1, 0, 0, 1, 1, 1, #31 - 40
      0, 0, 0, 0, 0, 0, 0, 1, 0, 0, #41 - 50
      1, 1, 0, 0, 1),
  
  LokalTyp = 
    # 1  2  3  4  5  6  7  8  9  10
    c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, #01 - 10 
      0, 1, 0, 0, 0, 0, 0, 1, 0, 0, #11 - 20
      0, 0, 0, 0, 0, 0, 0, 0, 0, 1, #21 - 30
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, #31 - 40
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, #41 - 50
      0, 0, 0, 0, 0),
  # Hier werden alle nicht zufälligen Lokalverkäufe gezahlt also RS.RA und T5.RA
  
  ORDMTyp =
    # 1  2  3  4  5  6  7  8  9  10
    c(0, 1, 0, 0, 0, 0, 1, 0, 0, 0, #01 - 10 
      0, 0, 1, 0, 1, 1, 1, 1, 0, 1, #11 - 20
      0, 1, 1, 0, 1, 0, 0, 0, 1, 0, #21 - 30
      0, 0, 1, 0, 0, 1, 1, 0, 0, 0, #31 - 40
      0, 1, 0, 0, 1, 1, 0, 0, 1, 0, #41 - 50
      0, 0, 1, 1, 0),
  
  AllesoeffenTyp = 
    # 1  2  3  4  5  6  7  8  9  10
    c(1, 1, 0, 0, 0, 1, 0, 0, 0, 0, #01 - 10
      0, 0, 0, 1, 0, 0, 0, 0, 0, 1, #11 - 20
      0, 0, 0, 0, 0, 0, 0, 0, 0, 1, #21 - 30
      1, 0, 1, 0, 0, 0, 0, 1, 0, 0, #31 - 40
      1, 0, 0, 1, 0, 0, 0, 0, 0, 0, #41 - 50
      0, 0, 0, 0, 0),
  
  withInfoTyp = 
    # 1  2  3  4  5  6  7  8  9  10
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, #01 - 10 
      0, 0, 0, 0, 0, 0, 1, 0, 0, 1, #11 - 20
      0, 0, 0, 0, 0, 0, 0, 1, 1, 1, #21 - 30
      0, 0, 1, 0, 0, 0, 0, 0, 0, 0, #31 - 40
      1, 0, 1, 1, 0, 0, 1, 0, 0, 1, #41 - 50
      0, 0, 0, 0, 0)
)
hc_15 <- hclust(dist(Typen, method = "manhattan"), method = "average")
plot(hc_15)

#Anzahl Durchgänge minus schwellen wert + 1 (1 wenn wert genau auf Schwelle)
Typen2 <- data.frame(
  
  RSTyp = 
    # 1  2  3  4  5  6  7  8  9  10
    c(0, 0, 4, 4, 4, 3, 0, 4, 0, 3, #01 - 10 
      2, 0, 0, 0, 0, 0, 0, 0, 4, 0, #11 - 20
      4, 0, 0, 0, 0, 2, 4, 4, 0, 0, #21 - 30
      0, 4, 0, 4, 2, 0, 0, 4, 4, 4, #31 - 40
      0, 0, 0, 0, 0, 0, 0, 4, 0, 0, #41 - 50
      3, 3, 0, 0, 4),
  
  LokalTyp = 
    # 1  2  3  4  5  6  7  8  9  10
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, #01 - 10 
      0, 1, 0, 0, 0, 0, 0, 0, 0, 0, #11 - 20
      0, 0, 0, 0, 0, 0, 0, 3, 0, 1, #21 - 30
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, #31 - 40
      0, 0, 0, 4, 0, 0, 0, 0, 0, 0, #41 - 50
      0, 0, 0, 0, 0),
  # Hier werden alle nicht zufälligen Lokalverkäufe gezahlt also RS.RA und T5.RA
  
  ORDMTyp =
    # 01  02  03  04  05  06  07  08  09  10
    c(00, 03, 00, 00, 00, 00, 03, 00, 00, 00, #01 - 10 
      00, 00, 04, 00, 03, 06, 13, 01, 00, 09, #11 - 20
      00, 09, 00, 00, 15, 00, 00, 00, 03, 00, #21 - 30
      00, 00, 05, 00, 00, 04, 09, 00, 00, 00, #31 - 40
      00, 11, 00, 00, 10, 14, 00, 00, 15, 00, #41 - 50
      00, 00, 02, 12, 00),
  
  AllesoeffenTyp = 
    # 01  02  03  04  05  06  07  08  09  10
    c(03, 02, 00, 00, 00, 02, 00, 00, 00, 00, #01 - 10
      00, 00, 00, 06, 00, 00, 00, 00, 00, 03, #11 - 20
      00, 00, 00, 00, 00, 00, 00, 23, 00, 09, #21 - 30
      03, 00, 00, 00, 00, 00, 00, 00, 00, 00, #31 - 40
      02, 00, 00, 21, 00, 00, 00, 00, 00, 00, #41 - 50
      00, 00, 00, 00, 00),
  
  withInfoTyp = 
    # 01  02  03  04  05  06  07  08  09  10
    c(00, 00, 00, 00, 00, 00, 00, 00, 00, 00, #01 - 10 
      00, 00, 00, 00, 00, 00, 00, 01, 00, 09, #11 - 20
      00, 00, 00, 00, 00, 00, 00, 14, 01, 03, #21 - 30
      00, 00, 04, 00, 00, 00, 00, 00, 00, 00, #31 - 40
      01, 00, 00, 12, 00, 00, 02, 00, 00, 02, #41 - 50
      00, 00, 00, 00, 00)
)
hc_15 <- hclust(dist(Typen2, method = "manhattan"), method = "average")
plot(hc_15)

#wie Typen2 aber ohne withininfo, weil eh nur alles auf decker dort viel zeigen
Typen3 <- data.frame(
  
  RSTyp = 
    # 1  2  3  4  5  6  7  8  9  10
    c(0, 0, 4, 4, 4, 3, 0, 4, 0, 3, #01 - 10 
      2, 0, 0, 0, 0, 0, 0, 0, 4, 0, #11 - 20
      4, 0, 0, 0, 0, 2, 4, 0, 0, 0, #21 - 30
      0, 4, 0, 4, 2, 0, 0, 4, 4, 4, #31 - 40
      0, 0, 0, 0, 0, 0, 0, 4, 0, 0, #41 - 50
      3, 3, 0, 0, 4),
  
  LokalTyp = 
    # 1  2  3  4  5  6  7  8  9  10
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, #01 - 10 
      0, 1, 0, 0, 0, 0, 0, 0, 0, 0, #11 - 20
      0, 0, 0, 0, 0, 0, 0, 3, 0, 1, #21 - 30
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, #31 - 40
      0, 0, 0, 4, 0, 0, 0, 0, 0, 0, #41 - 50
      0, 0, 0, 0, 0),
  # Hier werden alle nicht zufälligen Lokalverkäufe gezahlt also RS.RA und T5.RA
  
  ORDMTyp =
    # 01  02  03  04  05  06  07  08  09  10
    c(00, 03, 00, 00, 00, 00, 03, 00, 00, 00, #01 - 10 
      00, 00, 04, 00, 03, 06, 13, 01, 00, 09, #11 - 20
      00, 09, 00, 00, 15, 00, 00, 00, 03, 00, #21 - 30
      00, 00, 05, 00, 00, 04, 09, 00, 00, 00, #31 - 40
      00, 11, 00, 00, 10, 14, 00, 00, 15, 00, #41 - 50
      00, 00, 02, 12, 00),
  
  AllesoeffenTyp = 
    # 01  02  03  04  05  06  07  08  09  10
    c(03, 02, 00, 00, 00, 02, 00, 00, 00, 00, #01 - 10
      00, 00, 00, 06, 00, 00, 00, 00, 00, 03, #11 - 20
      00, 00, 00, 00, 00, 00, 00, 23, 00, 09, #21 - 30
      03, 00, 00, 00, 00, 00, 00, 00, 00, 00, #31 - 40
      02, 00, 00, 21, 00, 00, 00, 00, 00, 00, #41 - 50
      00, 00, 00, 00, 00)
)
hc_15 <- hclust(dist(Typen3, method = "manhattan"), method = "average")
plot(hc_15)

#wie Typen3 aber quadriert
Typen4 <- data.frame(
  
  RSTyp = 
    # 01  02  03  04  05  06  07  08  09  10
    c(00, 00, 16, 16, 16, 09, 00, 16, 00, 09, #01 - 10 
      04, 00, 00, 00, 00, 00, 00, 00, 16, 00, #11 - 20
      16, 00, 00, 00, 00, 04, 16, 00, 00, 00, #21 - 30
      00, 16, 00, 16, 04, 00, 00, 16, 16, 16, #31 - 40
      00, 00, 00, 00, 00, 00, 00, 16, 00, 00, #41 - 50
      09, 09, 00, 00, 16),
  
  LokalTyp = 
    # 1  2  3  4  5  6  7  8  9  10
    c(00, 00, 00, 00, 00, 00, 00, 00, 00, 00, #01 - 10 
      00, 01, 00, 00, 00, 00, 00, 00, 00, 00, #11 - 20
      00, 00, 00, 00, 00, 00, 00, 09, 00, 01, #21 - 30
      00, 00, 00, 00, 00, 00, 00, 00, 00, 00, #31 - 40
      00, 00, 00, 16, 00, 00, 00, 00, 00, 00, #41 - 50
      00, 00, 00, 00, 00),
  # Hier werden alle nicht zufälligen Lokalverkäufe gezahlt also RS.RA und T5.RA
  
  ORDMTyp =
    # 001  002  003  004  005  006  007  008  009  010
    c(000, 009, 000, 000, 000, 000, 009, 000, 000, 000, #01 - 10 
      000, 000, 016, 000, 009, 036, 169, 001, 000, 081, #11 - 20
      000, 081, 000, 000, 225, 000, 000, 000, 009, 000, #21 - 30
      000, 000, 025, 000, 000, 016, 081, 000, 000, 000, #31 - 40
      000, 121, 000, 000, 100, 198, 000, 000, 225, 000, #41 - 50
      000, 000, 004, 144, 000),
  
  AllesoeffenTyp = 
    # 001  002  003  004  005  006  007  008  009  010
    c(009, 004, 000, 000, 000, 004, 000, 000, 000, 000, #01 - 10
      000, 000, 000, 036, 000, 000, 000, 000, 000, 009, #11 - 20
      000, 000, 000, 000, 000, 000, 000, 529, 000, 081, #21 - 30
      009, 000, 000, 000, 000, 000, 000, 000, 000, 000, #31 - 40
      004, 000, 000, 441, 000, 000, 000, 000, 000, 000, #41 - 50
      000, 000, 000, 000, 000)
)
hc_15 <- hclust(dist(Typen4, method = "euclidean"), method = "complete")
plot(hc_15)

heights <- hc_15$height
plot(heights, type = "b", pch = 19, xlab = "Cluster-Anzahl", ylab = "Höhe des Dendrogramms")

steigungen <- c(diff(heights), NA) / c(NA, diff(heights))

# Finde den Index des Knickpunkts (wo die relative Steigung am stärksten abfällt)
knick_index <- which.max(steigungen)

# Höhe des Knickpunkts
knick_höhe <- heights[knick_index]

# Erstelle ein Elbow-Diagramm
plot(heights, type = "b", pch = 19, xlab = "Cluster-Anzahl", ylab = "Höhe des Dendrogramms")

# Füge einen Knick-Indikator hinzu (z.B. ein vertikales Linie)
abline(v = knick_index, col = "green", lty = 2)  # Vertikale Linie am Knickpunkt

# Füge einen Punkt hinzu, um den Knickpunkt zu markieren
points(knick_index, knick_höhe, col = "red", pch = 19)

# Füge einen Punkt hinzu, um den Ellenbogen zu markieren
points(ellenbogen_index, ellenbogen_höhe, col = "blue", pch = 19)


mean(Typen4$AllesoeffenTyp[Typen4$AllesoeffenTyp>0])
#112.6
mean(Typen4$RSTyp[Typen4$RSTyp>0])
#12.95238
mean(Typen4$LokalTyp[Typen4$LokalTyp>0])
#6.75
mean(Typen4$ORDMTyp[Typen4$ORDMTyp>0])
#77.95

#Ich könnte zwei oder drei Gruppen machen, ich werde beides Testen und schauen
#ob es homogenität innerhalb der Gruppen in der Wettbwerbssituation gibt etc.

#Zwei Gruppen nach Klusterdiagramm
allesAufdecker_innen <- data[c(28, 44),]
Risiko <- data[c(1:27, 29:43, 45:55),]

#Durschnitt Nachberechnete HAUS in Wettbewerbsbedingung
mean(allesAufdecker_innen$W_Nachberechnetes.Haus)
#88.5
mean(Risiko$W_Nachberechnetes.Haus)
#76.5
wilcox.test(allesAufdecker_innen$W_Nachberechnetes.Haus, Risiko$W_Nachberechnetes.Haus)
t.test(allesAufdecker_innen$W_Nachberechnetes.Haus, Risiko$W_Nachberechnetes.Haus)

#Durschnitt Winning Amount in Wettbewerbsbedingung
mean(allesAufdecker_innen$W_Winning_Amount)
#18495
mean(Risiko$W_Winning_Amount)
#21849.06
wilcox.test(allesAufdecker_innen$W_Winning_Amount, Risiko$W_Winning_Amount)
t.test(allesAufdecker_innen$W_Winning_Amount, Risiko$W_Winning_Amount)


#Drei Gruppen, zuteilung mit hilfe des Mittelwerts der Einzelnen Typen
#für eine Klarere Abgrenzung somit entsteht eine Random Restkategorie -> Anova?
allesAufdecker_innen <- data[c(28, 44),]
ORDM <- data[c(17, 25, 37, 42, 45, 46, 49, 54),]
Wagnis <- data[c(3, 4, 5, 8, 19, 21, 27, 32, 34, 38, 39, 40, 48, 55),]
Random <- data[c(1, 2, 6, 7, 9:16, 18, 20, 22:24, 26, 29:31, 33, 35, 36, 41, 43, 47, 50:53),] 

shapiro.test(allesAufdecker_innen$W_Nachberechnetes.Haus)
shapiro.test((ORDM$W_Nachberechnetes.Haus))
shapiro.test(Wagnis$W_Nachberechnetes.Haus)

#Nachberechnete HAUS in Wettbewerbsbedingung
mean(data$W_Nachberechnetes.Haus)
#77.32727
mean(allesAufdecker_innen$W_Nachberechnetes.Haus)
#88.5
mean(ORDM$W_Nachberechnetes.Haus)
#77.875
mean(Wagnis$W_Nachberechnetes.Haus)
#77.78571
mean(Random$W_Nachberechnetes.Haus)
#76.25806

#Winningamount in Wettbewerbsbedingung
mean(data$W_Winning_Amount)
#21727.09
mean(allesAufdecker_innen$W_Winning_Amount)
#18495
mean(ORDM$W_Winning_Amount)
#18495
mean(Wagnis$W_Winning_Amount)
#23587.5
mean(Random$W_Winning_Amount)
#21690

#Nachberechnete HAUS in Basisbedingung
mean(data$B_Nachberechnetes.Haus)
#80.09091
mean(allesAufdecker_innen$B_Nachberechnetes.Haus)
#82
mean(ORDM$B_Nachberechnetes.Haus)
#78.75
mean(Wagnis$B_Nachberechnetes.Haus)
#80.14286
mean(Random$B_Nachberechnetes.Haus)
#80.29032

#Wie oben aber einfach alle die überzufällig gewisses Verhalten gezeigt haben, Personen werden mehrere Typen zugeschrieben
#Somit entstehen gewisse Mischtypen
allesAufdecker_innen2 <- data[c(14, 1, 31, 41),]
ORDM2 <- data[c(25, 49, 46, 17, 54, 42, 45, 22, 37, 16, 33, 13, 36, 7, 15, 29, 53, 18),]
Wagnis2 <- data[c(3, 4, 5, 8, 19, 21, 27, 32, 34, 38, 39, 40, 48, 55, 10, 51, 52, 11, 26, 35),]
allesAufdecker_innenLokal <- data[c(44, 28, 30),]
allesAufdecker_innenORDM <- data[c(20, 2),]
allesAufdecker_innenWagnis <- data[c(6),]
Random2 <- data[c(9, 12, 23, 24, 43, 47, 50),] 

shapiro.test(allesAufdecker_innen$W_Nachberechnetes.Haus)
shapiro.test((ORDM$W_Nachberechnetes.Haus))
shapiro.test(Wagnis$W_Nachberechnetes.Haus)

#Nachberechnete HAUS in Wettbewerbsbedingung
mean(data$W_Nachberechnetes.Haus)
#76
mean(allesAufdecker_innen2$W_Nachberechnetes.Haus)
#76
mean(ORDM2$W_Nachberechnetes.Haus)
#77.83333
mean(Wagnis2$W_Nachberechnetes.Haus)
#77.45
mean(Random2$W_Nachberechnetes.Haus)
#72.71429
mean(allesAufdecker_innenLokal$W_Nachberechnetes.Haus)
#84.66667
mean(allesAufdecker_innenORDM$W_Nachberechnetes.Haus)
#79
mean(allesAufdecker_innenWagnis$W_Nachberechnetes.Haus)
#78


#Winningamount in Wettbewerbsbedingung
mean(data$W_Winning_Amount)
mean(allesAufdecker_innen2$W_Winning_Amount)
#20287.5
mean(ORDM2$W_Winning_Amount)
#22638.33
mean(Wagnis2$W_Winning_Amount)
#21663
mean(Random2$W_Winning_Amount)
#21792.86
mean(allesAufdecker_innenLokal$W_Winning_Amount)
#19580
mean(allesAufdecker_innenORDM$W_Winning_Amount)
#18600
mean(allesAufdecker_innenWagnis$W_Winning_Amount)
#24600

wilcox.test(allesAufdecker_innenWagnis$W_Winning_Amount, allesAufdecker_innenORDM$W_Winning_Amount)
#nicht genügend Daten

