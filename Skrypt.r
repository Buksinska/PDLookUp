library(readr)
library(dplyr)
library(corrplot)
library(MASS)
library("car")
library(tidyverse) 
library(caret)     
library(pROC) 
library(RColorBrewer)
library(ineq)
library(caTools)
library(pscl) 

#wczytanie danych
data <- read_csv("C:/Users/48725/Desktop/pwc/13.12/final_data.csv")
View(data)
attach(data)

#sprawdzenie duplikatów dla kolumny Application_ID
if (anyDuplicated(data$Application_ID) == 0) {
  print("Brak duplikatów.")
} else {
  print("Znaleziono duplikaty.")
}


#analiza eksploracyjna danych 

#podstawowe statystyki 
summary(data)

#sprawdzenie braków danych 

is.na(data$IntoDefFlag)
sum(is.na(data$IntoDefFlag))/nrow(data)*100
nrow(data)


#kategoryzacja zmiennej IntoDefFlag 
data$IntoDefFlag <- ifelse(is.na(data$IntoDefFlag), "brak_danych",
                                    ifelse(data$IntoDefFlag == 0, "0", "1"))

sum(is.na.data.frame(data))

#analiza wartości odstających 
#za pomocą Z-score

find_outliers_zscore <- function(x) {
  z_scores <- scale(x)
  return(sum(abs(z_scores) > 3, na.rm = TRUE))
}
numeric_columns <- data %>% select_if(is.numeric)

outliers_zscore <- numeric_columns %>% summarise_all(find_outliers_zscore)
print(outliers_zscore)
data.frame(outliers_zscore)

#macierz korelacji
#dla zmiennych numerycznych z kategorii application data 
data$ap_data <- data[c("Age", "Household_children", "Monthly_Income", "Monthly_Spendings", "Credit_amount", "Number_of_installments")]
cor_matrix_numeric_ap <- cor(data$ap_data)
#heat map 
corrplot(cor_matrix_numeric_ap, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         col = brewer.pal(n = 8, name = "RdYlBu"),
         diag = FALSE)
data.frame(cor_matrix_numeric_ap)
#silna korelacja dla zmiennych miesięczne wydatki-miesięczne dochody oraz liczba rat-kwota kredytu 


#dla numerycznych zmiennych geolokalizacyjnych

data$Divorce_number_1000 <- gsub(",", ".", data$Divorce_number_1000) # Zamiana przecinków na kropki
data$Divorce_number_1000 <- as.numeric(data$Divorce_number_1000)

geolocation_data <- data[c("Divorce_number_1000", "Personal_car_number", "Truck_number", 
                           "Tractor_number", "Agricultural_tractor_number", "Building_permit_number",
                           "Building_permit_individual_number", "Building_project_subbmission_number",
                           "Apartment_project_subbmission_number", "Apartment_project_subbmission_area",
                           "Employed_number_total", "Employed_number_men", "Employed_number_women",
                           "Employed_agricultural_number", "Employed_industry_number", 
                           "Emplyed_trade_transport_number", "Employed_finance_number", 
                           "Employed_other_number", "Average_income", "Total_population_age_0_14_years",
                           "Total_population_age_15_29_years", "Total_population_age_30_44_years",
                           "Total_population_age_45_59_years", "Total_population_age_60_years_or_older",
                           "Spending_food", "Spending_clothing", "Spending_footwear", "Spending_household",
                           "Spending_glassware", "Spending_personal_care", "Spending_catering",
                           "Spending_electronics", "Spending_recreational_and_cultural",
                           "Total_population", "Working_age_population", "Unemployed_total",
                           "Unemployed_vocational_number", "Unemployed_highschool_and_lower_number")]

cor_matrix_geo <- cor(geolocation_data)

corrplot(cor_matrix_geo, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 90, 
         col = brewer.pal(n = 8, name = "RdYlBu"), diag = FALSE,
         tl.cex = 0.6)

#dla numerycznych zmiennych behawioralnych 
behavioral_data <- data[c("NotionalValue_t0", "NotionalValue_lag1", "NotionalValue_lag2", 
                          "NotionalValue_lag3", "NotionalValue_lag4", "NotionalValue_lag5", 
                          "NotionalValue_lag6", "NotionalValue_lag7", "NotionalValue_lag8", 
                          "NotionalValue_lag9", "NotionalValue_lag10", "NotionalValue_lag11", 
                          "NotionalValue_lag12", "DPD_t0", "DPD_lag1", "DPD_lag2", 
                          "DPD_lag3", "DPD_lag4", "DPD_lag5", "DPD_lag6", 
                          "DPD_lag7", "DPD_lag8", "DPD_lag9", "DPD_lag10", 
                          "DPD_lag11", "DPD_lag12", "NotionalOverdue_t0", "NotionalOverdue_lag1", 
                          "NotionalOverdue_lag2", "NotionalOverdue_lag3", "NotionalOverdue_lag4", 
                          "NotionalOverdue_lag5", "NotionalOverdue_lag6", "NotionalOverdue_lag7", 
                          "NotionalOverdue_lag8", "NotionalOverdue_lag9", "NotionalOverdue_lag10", 
                          "NotionalOverdue_lag11", "NotionalOverdue_lag12")]

cor_matrix_beh <- cor(behavioral_data)

corrplot(cor_matrix_beh, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 90, 
         col = brewer.pal(n = 8, name = "RdYlBu"), diag = FALSE,
         tl.cex = 0.6)


#transformacja zmiennych i wykresy
#Zmienna age
zakres_wiekowy <- c(17, 25, 35, 50, 60, 70, 78)
data$zakres_wiekowy <- cut(data$Age, breaks = zakres_wiekowy, labels = c("17-25", "26-35", "36-50", "51-60", "61-70", "71-78"), include.lowest = TRUE)
hist(zakres_wiekowy, breaks = c(17, 25, 35, 50, 60, 70, 78))

#Zmienna Household_children
liczba_dzieci <- c(0,1,2,3)
data$liczba_dzieci  <- cut(data$Household_children, breaks = liczba_dzieci , labels = c("0","1", "2 i więcej"), include.lowest = TRUE)
hist(liczba_dzieci, breaks = c(0,1,2,3), freq = T)

#nowe zmienne
data$Installment_amount <- data$Credit_amount / data$Number_of_installments
data$IRR <- data$Installment_amount / data$Monthly_Income * 100
data$SIR <- data$Monthly_Spendings / data$Monthly_Income * 100
data$Available_income_amount <-	data$Monthly_Income - data$Monthly_Spendings	
data$unemployed_to_working_age <- data$Unemployed_total / data$Working_age_population *100

#dummy variables - kodowanie zmiennych kategorycznych metodą one-hot encoder 
data$liczba_dzieci <- as.factor(data$liczba_dzieci)
data$zakres_wiekowy <- as.factor(data$zakres_wiekowy)
data$Job_type <- as.factor(data$Job_type)
data$Marital_status <- as.factor(data$Marital_status)
data$Home_status <- as.factor(data$Home_status)
data$Car_status <- as.factor(data$Car_status)
data$Credit_purpose <- as.factor(data$Credit_purpose)
one_hot <- model.matrix(~ liczba_dzieci + zakres_wiekowy + Job_type + 
                          Marital_status + Home_status + Car_status + Credit_purpose + zakres_wiekowy + liczba_dzieci - 1, data=data)
data$one_hot_df <- as.data.frame(one_hot)
head(data$one_hot_df)

#wykresy
#monthly_income ~ age
boxplot(data$Monthly_Income ~ data$zakres_wiekowy,
        main = "Zależność Monthly Income od Age",
        xlab = "Age",
        ylab = "Monthly Income",
        col = "lightblue")

#Installment_amount ~ age
boxplot(data$Installment_amount ~ data$zakres_wiekowy,
        main = "Zależność wysokości raty od wieku",
        xlab = "Age",
        ylab = "Installment amount",
        col = "lightblue")

#Installment_amount ~ Credit_purpose
boxplot(data$Installment_amount ~ data$Credit_purpose,
        main = "Zależność wysokości i raty od celu kredytowania",
        xlab = "Credit purpose",
        ylab = "Installment amount",
        col = "lightblue")


#analiza wielowymiarowa - analiza głownych składowych

#standaryzacja i normalizacja wybranych zmiennych numerycznych 
zmienne_standaryzacja <- data[c("Age", "Household_children", "Monthly_Income", "Monthly_Spendings", 
                                "Credit_amount", "Number_of_installments", "Divorce_number_1000",
                                "Personal_car_number", "Truck_number", "Tractor_number", 
                                "Agricultural_tractor_number", "Building_permit_number",
                                "Building_permit_individual_number", "Building_project_subbmission_number",
                                "Apartment_project_subbmission_number", "Apartment_project_subbmission_area",
                                "Employed_number_total", "Employed_number_men", "Employed_number_women",
                                "Employed_agricultural_number", "Employed_industry_number",
                                "Emplyed_trade_transport_number", "Employed_finance_number",
                                "Employed_other_number", "Average_income", "Total_population_age_0_14_years",
                                "Total_population_age_15_29_years", "Total_population_age_30_44_years",
                                "Total_population_age_45_59_years", "Total_population_age_60_years_or_older",
                                "Spending_food", "Spending_clothing", "Spending_footwear",
                                "Spending_household", "Spending_glassware", "Spending_personal_care",
                                "Spending_catering", "Spending_electronics", "Spending_recreational_and_cultural",
                                "Total_population", "Working_age_population", "Unemployed_total",
                                "Unemployed_vocational_number", "Unemployed_highschool_and_lower_number",
                                "NotionalValue_t0", "NotionalValue_lag1", "NotionalValue_lag2",
                                "NotionalValue_lag3", "NotionalValue_lag4", "NotionalValue_lag5",
                                "NotionalValue_lag6", "NotionalValue_lag7", "NotionalValue_lag8",
                                "NotionalValue_lag9", "NotionalValue_lag10", "NotionalValue_lag11",
                                "NotionalValue_lag12", "DPD_t0", "DPD_lag1", "DPD_lag2", "DPD_lag3",
                                "DPD_lag4", "DPD_lag5", "DPD_lag6", "DPD_lag7", "DPD_lag8", "DPD_lag9",
                                "DPD_lag10", "DPD_lag11", "DPD_lag12", "NotionalOverdue_t0",
                                "NotionalOverdue_lag1", "NotionalOverdue_lag2", "NotionalOverdue_lag3",
                                "NotionalOverdue_lag4", "NotionalOverdue_lag5", "NotionalOverdue_lag6",
                                "NotionalOverdue_lag7", "NotionalOverdue_lag8", "NotionalOverdue_lag9",
                                "NotionalOverdue_lag10", "NotionalOverdue_lag11", "NotionalOverdue_lag12", 
                                "Installment_amount", "IRR", "SIR", "Available_income_amount", "unemployed_to_working_age")]

#Standaryzacja
preProcValues <- preProcess(zmienne_standaryzacja, method = c("center", "scale"))
data_scaled <- predict(preProcValues, zmienne_standaryzacja)

#Normalizacja
preProcValues_norm <- preProcess(zmienne_standaryzacja, method = c("range"))
data_norm <- predict(preProcValues_norm, zmienne_standaryzacja)
names(data_norm) <- paste0(names(data_norm), "_norm")

#analiza głownych składowych
wyniki_pca <- prcomp(data_norm)
wyniki_pca
plot(wyniki_pca$sdev^2, type = "bar", main = "Wartości własne")
plot(cumsum(wyniki_pca$sdev^2) / sum(wyniki_pca$sdev^2), 
     type = "b", 
     main = "Skumulowana Wariancja",
     xlab = "Liczba składowych głównych",
     ylab = "Skumulowana wariancja wyjaśniona")

#Z analizy pca wynika, że 10 pierwszych składkowych niesie za sobą istotną informację, więc dodano zbiór "pca_data" z tymi składowymi
pca_data <- as.data.frame(wyniki_pca$x[, 1:10])

#nowy zbiór
data1 <- cbind(data_norm, data$one_hot_df, data, pca_data)
data1 <- subset(data1, IntoDefFlag != "brak_danych")

#analiza zmiennej celu 

#zmienna IntoDefFlag
table(data1$IntoDefFlag)
barplot(table(data1$IntoDefFlag), main = "IntoDefFlag")

#wybór zmiennych do modelu 
#zmienne numeryczne 
# Installment_amount
# IIR
# SIR
# Available_income_amount
# unemployed_to_working_age
# Average_income
# Divorce_number_1000
# Building_permit_individual_number
# DPD_t0
# DPD_lag_2
# NotionalValue_t0
# NotionalOverdue_t0
# NotionalOverdue_lag12

#dummy variables
# zakres_wiekowy
# liczba_dzieci
# Job_type
# Marital_status
# Home_status
# Car_status
# Credit_purpose

#potencjalne interakcje 
# Age~Maritial_status
# Age~Credit_purpose
# Age~Household_children
# Job_type~Credit_amount
# Monthly_income~Credit_purpose
# Maritial_status~Household_children
# Monthly_income~Monthly_spendings
# Monthly_income~Credit_amount
# Monthly_spendings~Credit_amount
# Job_type~Employed_number_total
# Car_status~Personal_car_number
# Age~NotionalValue_lag12
# Age~DPD_lag12
# Age~NotionalOverdue_lag12
# Job_type~NotionalValue_lag12
# Job_type~DPD_lag12
# Job_type~NotionalOverdue_lag12
# SIR~NotionalValue_lag12

zmienne_numeryczne <- c("data1$Installment_amount", "data1$IRR", "data1$SIR", 
                        "data1$Available_income_amount", "data1$unemployed_to_working_age", 
                        "data1$Average_income", "data1$Divorce_number_1000", 
                        "data1$Building_permit_individual_number", "data1$DPD_t0", 
                        "data1$DPD_lag_2", "data1$NotionalValue_t0", 
                        "data1$NotionalOverdue_t0", "data1$NotionalOverdue_lag12")


dummy <- c("zakres_wiekowy", "liczba_dzieci", "Job_type", "Marital_status", "Home_status", "Car_status", "Credit_purpose")


#testy statystyczne 
#badanie współliniowości zmiennych numerycznych za pomocą VIF
model_lm <- lm(data1$IntoDefFlag ~ data1$Installment_amount + data1$IRR + data1$SIR + 
     data1$Available_income_amount + data1$unemployed_to_working_age + 
     data1$Average_income + data1$Divorce_number_1000 + 
     data1$Building_permit_individual_number + data1$DPD_t0 + 
     data1$DPD_lag12 + data1$NotionalValue_t0 + 
     data1$NotionalOverdue_t0 + data1$NotionalOverdue_lag12, data = data1)

vif_values <- vif(model_lm)
data.frame(vif_values)

# Podział zbioru danych na zestaw treningowy i testowy
set.seed(123)
indeksy <- sample(1:2, size = nrow(data1), replace = TRUE, prob = c(0.7, 0.3))
dane_uczacy <- data1[indeksy == 1,]
dane_testowy <- data1[indeksy == 2,]


# Budowa modelu regresji logistycznej
#model podstawowy
dane_uczacy$IntoDefFlag <- as.numeric(dane_uczacy$IntoDefFlag)
logit_1 <- glm(IntoDefFlag ~ Installment_amount + IRR + SIR + 
                 Available_income_amount + unemployed_to_working_age + 
                 Average_income_norm + Divorce_number_1000_norm + 
                 Building_permit_individual_number_norm + DPD_t0_norm + 
                 DPD_lag12_norm + NotionalValue_t0_norm + 
                 NotionalOverdue_t0_norm + NotionalOverdue_lag12_norm +
                 zakres_wiekowy + liczba_dzieci +
                 Job_type + Marital_status + Home_status + Car_statusOwner + Credit_purpose, 
               data = dane_uczacy, family = binomial)

#za pomocą metody krokowej wybrano model najlepiej dopasowany do danych z jak najmniejszą liczbą zmiennych
#Zaczynając od początkowego modelu, funkcja step() iteracyjnie testuje modele z usuniętymi lub dodanymi 
#zmiennymi i wybiera model z najniższym AIC.
logit_step <- step(logit_1, direction = "both")
#z modelu usunięta została również zmienna "Divorce_number_1000_norm", ponieważ interpretacja jej współczynnika była kontrintuicyjna
logit_basic_final <- glm(formula = IntoDefFlag ~ Installment_amount + IRR + SIR + 
                 Available_income_amount + unemployed_to_working_age + Average_income_norm + 
                Building_permit_individual_number_norm + DPD_t0_norm + DPD_lag12_norm + NotionalValue_t0_norm + NotionalOverdue_lag12_norm + 
                 Job_type + Marital_status + Home_status + Credit_purpose, 
               family = binomial, data = dane_uczacy)

#model z uwzględnienim interakcji 
logit_interactions <- glm(IntoDefFlag ~ Installment_amount + IRR + SIR + 
                            Available_income_amount + unemployed_to_working_age + Average_income_norm + 
                            Divorce_number_1000_norm + Building_permit_individual_number_norm + 
                            DPD_t0_norm + DPD_lag12_norm + NotionalValue_t0_norm + NotionalOverdue_t0_norm +
                            NotionalOverdue_lag12_norm + zakres_wiekowy + liczba_dzieci + 
                            Job_type + Marital_status + Home_status + Car_statusOwner + 
                            Credit_purpose + Age_norm:Marital_status + Age_norm:Credit_purpose + 
                            Age_norm:Household_children_norm + Job_type:Credit_amount_norm + 
                            Monthly_Income_norm:Credit_purpose + Marital_status:Household_children_norm + 
                            Monthly_Income_norm:Monthly_Spendings_norm + 
                            Monthly_Income_norm:Credit_amount_norm + Monthly_Spendings_norm:Credit_amount_norm + 
                            Job_type:Employed_number_total_norm + Car_status:Personal_car_number_norm + 
                            Age_norm:NotionalValue_lag12_norm + Age_norm:DPD_lag12_norm + 
                            Age_norm:NotionalOverdue_lag12_norm + Job_type:NotionalValue_lag12_norm + 
                            Job_type:DPD_lag12_norm + Job_type:NotionalOverdue_lag12_norm + 
                            SIR_norm:NotionalValue_lag12_norm,
                          data = dane_uczacy, family = binomial)

#za pomocą metody krokowej wybrano model najlepiej dopasowany do danych z jak najmniejszą liczbą zmiennych
logit_step_interactions <- step(logit_interactions, direction = "both")
#z modelu usunięta została również zmienna "Divorce_number_1000_norm", ponieważ interpretacja jej współczynnika była kontrintuicyjna
logit_interactions_final <- glm(formula = IntoDefFlag ~ Installment_amount + IRR + SIR + 
                              Available_income_amount + unemployed_to_working_age + 
                              Building_permit_individual_number_norm + DPD_t0_norm + DPD_lag12_norm + 
                              NotionalValue_t0_norm + NotionalOverdue_lag12_norm + zakres_wiekowy + 
                              Job_type + Marital_status + Home_status + Credit_purpose + 
                              Marital_status:Age_norm + Age_norm:Household_children_norm + 
                              Job_type:Employed_number_total_norm + Age_norm:NotionalValue_lag12_norm + 
                              NotionalValue_lag12_norm:SIR_norm + NotionalOverdue_lag12_norm:Job_type, 
                            family = binomial, data = dane_uczacy)

#model, który za zmienne objaśniające przyjmuje dziesięć głównych składowych wyznaczonych wcześniej w analizie pca
logit_pca <- glm(IntoDefFlag ~ PC1 + PC2 + PC3 + PC4+ PC5 +
                   PC6 + PC7 + PC8 + PC9 + PC10, 
                 data = dane_uczacy, family = binomial)
logit_step_pca <- step(logit_pca, direction = "both")
logit_pca_final <- glm(formula = IntoDefFlag ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + 
                         PC7 + PC10, family = binomial, data = dane_uczacy)

summary(logit_basic_final)
summary(logit_interactions_final)
summary(logit_pca_final)

# Ocena modelu na zestawie testowym

#Definiowanie funkcji do oceny dobroci dopasowania modeli dwumianowych
ocena_modelu_dwum <- function(model) {
  kryterium_AIC <- c(model$aic)
  McFadden<-pR2(model)[4]
  Cragg_Uhler<-pR2(model)[6]
  ocena <- data.frame(kryterium_AIC, McFadden, Cragg_Uhler)
  return(ocena)
}

#Ocena
wyniki_oceny_logit <- rbind(
  logit_basic_final=ocena_modelu_dwum(logit_basic_final),
  logit_interactions_final=ocena_modelu_dwum(logit_interactions_final),
  logit_pca_final=ocena_modelu_dwum(logit_pca_final))
wyniki_oceny_logit

#Dla trzech finalnych modeli najlepsze wyniki ma model, który uwzględnia główne składowe zamiast zwykłych zmiennych. 
#Model z PCA jest przydatny, gdy chcemy zredukować liczbę zmiennych i usunąć współliniowość. Jest to szczególnie pomocne w dużych zbiorach 
#danych z wieloma zmiennymi, gdzie tradycyjne modele regresji mogą być przeciążone lub zawierać wiele skorelowanych predyktorów. 
#Jak pokazała analiza VIF zmienne użyte w modelu nie są współliniowe, a dodatkowo ich ilość pozwala na swobodną interpretację. 
#Interpretacja modelu PCA wymaga uwzględnienia informacji, że każda główna składowa to kombinacja wielu oryginalnych zmiennych, co zdecydowanie
#komplikuje jego stosowanie, zwłaszcza w kontekście modelu scoringowego. Dlatego do dalszego porównania zdecydowano wybrać się dwa pozostałe modele.

#Weyfikacja finalnych modeli na zbiorze testowym. 
#Tablice trafności

#Wyznaczenie punktu odcięcia
p <- 0.5

cat("Tablica trafności dla modelu bez interakcji\n")
tab_traf <- data.frame(obserwowane=logit_basic_final$y, 
                       przewidywane=ifelse(logit_basic_final$fitted.values>p, 1, 0))
table(tab_traf)

cat("Tablica trafności dla modelu z interakcjami\n")
tab_traf <- data.frame(obserwowane=logit_interactions_final$y, 
                       przewidywane=ifelse(logit_interactions_final$fitted.values>p, 1, 0))
table(tab_traf)

#Miary jakości predykcji oparte na tablicy trafności
miary_pred <- function(model, dane, Y, p = 0.5) {
  tab <- table(obserwowane = Y, przewidywane = ifelse(predict(model, dane, type = "response") > p, 1, 0))
  ACC <- (tab[1,1]+tab[2,2])/sum(tab)
  ER <- (tab[1,2]+tab[2,1])/sum(tab)
  SENS <- (tab[2,2])/sum(tab[2,2]+tab[2,1])
  SPEC <- (tab[1,1])/sum(tab[1,1]+tab[1,2])
  PPV <- (tab[1,1])/sum(tab[2,2]+tab[2,1])
  NPV <- (tab[2,2])/sum(tab[1,1]+tab[1,2])
  miary <- data.frame(ACC, ER, SENS, SPEC, PPV, NPV)
  return(miary)
}
wyniki_miary_pred <- rbind(
  logit_basic_final = miary_pred(logit_basic_final, dane_testowy, dane_testowy$IntoDefFlag, p), 
  logit_interactions_final = miary_pred(logit_interactions_final, dane_testowy, dane_testowy$IntoDefFlag, p))
wyniki_miary_pred

#Przy obranym punkcie odcięcia p, miary jakości predykcji są takie same dla obu modeli, z tego też powodu przeanalizowana zostanie krzywa ROC i miary na jej podstawie. 

#Krzywa ROC

#Zdefiniowanie obiektów klasy roc
rocobj1 <- roc(logit_basic_final$y, logit_basic_final$fitted.values)
rocobj2 <- roc(logit_interactions_final$y, logit_interactions_final$fitted.values)

#Wykresy `plot()` obiektów klasy roc
plot(rocobj1)
plot(rocobj2)

#Wykresy `ggroc()` obiektów klasy roc
ggroc(rocobj1, legacy.axes = TRUE)+
  ggtitle("Krzywa ROC dla modelu bez interakcji") +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="red")+
  geom_hline(aes(yintercept=1), lty=2, color="grey")+
  geom_hline(aes(yintercept=0), lty=2, color="grey")+
  geom_vline(aes(xintercept=1), lty=2, color="grey")+
  geom_vline(aes(xintercept=0), lty=2, color="grey")+
  theme_classic()

ggroc(rocobj2, legacy.axes = TRUE)+
  ggtitle("Krzywa ROC dla modelu z interakcjami") +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="red")+
  geom_hline(aes(yintercept=1), lty=2, color="grey")+
  geom_hline(aes(yintercept=0), lty=2, color="grey")+
  geom_vline(aes(xintercept=1), lty=2, color="grey")+
  geom_vline(aes(xintercept=0), lty=2, color="grey")+
  theme_classic()

#Oba wykresy wskazują, że krzywa ROC jest powyżej linii losowości, co wskazuje na to, 
#że modele mają dobrą zdolność rozróżniania między zdarzeniami pozytywnymi a negatywnymi.
#W celu weryfikacji który jest lepszy zastosowana zostanie miara wsp. Giniego.

#Pole powierzchni pod krzywą ROC
pole_AUC_logit_final<-as.numeric(auc(logit_basic_final$y, logit_basic_final$fitted.values))
pole_AUC_logit_interactions_final<-as.numeric(auc(logit_interactions_final$y, logit_interactions_final$fitted.values))
pole_AUC <- rbind(pole_AUC_logit_final, pole_AUC_logit_interactions_final)
pole_AUC

#Podsumowanie

#Miara AUC zawiera się między 0,5 a 1. Nie ma jednoznacznego poziomu AUC, który określałby, że predykcja modelu jest na odpowiednim poziomie. 
#Jest to zależne często od kontekstu analizowanego problemu. W tym przypadku zostanie przyjęte, że poziom 0,75, 
#oznacza odpowiednie dopasowanie modelu, ale zostanie też policzony wskaźnik Ginniego, który również jest miarą predykcji modelu. 

#Wskaźnik Ginniego
Wsp_Ginniego_logit_basic_final <-(2*(as.numeric(auc(logit_basic_final$y, logit_basic_final$fitted.values)))-1)
Wsp_Ginniego_logit_interactions_final <-(2*(as.numeric(auc(logit_interactions_final$y, logit_interactions_final$fitted.values)))-1)
Wsp_Ginniego <- rbind(Wsp_Ginniego_logit_basic_final, Wsp_Ginniego_logit_interactions_final)
Wsp_Ginniego
#Wskaźnik Giniego na poziomie ok. 47% oznacza że model ma pewną zdolność do rozróżniania między dobrymi a złymi kredytobiorcami, ale nie jest to zdolność wyjątkowo wysoka.
#Żeby model lepiej przewidywał "dobrych" i "złych" klientów należałoby spróbować go zmodyfikować. 
#Oto możliwe ulepszenia:
#Dodanie nowych zmiennych,
#regularyacja
#zbalansowanie zmiennej celu, 
#dodatkowe analizy/metody

#Kryterium AIC wskazało na to, że model bez interakcji jest lepszy, tak samo jak miary pseudo R-kwadrat, a dodatkowo pozwala on na dużo łatwiejszą interpretację zmiennych. 
#Zatem jako model finalny wybrany został model "logit_basic_final", ponieważ weryfikacja na zbiorze testowym nie wykazała isototnych różnic między dwoma modelami. 


#Budowa karty scoringowej
# Współczynniki modelu
data1$IntoDefFlag <- as.numeric(data1$IntoDefFlag)
PD_LOGIT <- glm(formula = IntoDefFlag ~ Installment_amount + IRR + SIR + 
                           Available_income_amount + unemployed_to_working_age + Average_income_norm + 
                           + Building_permit_individual_number_norm + 
                           DPD_t0_norm + DPD_lag12_norm + NotionalValue_t0_norm + NotionalOverdue_lag12_norm + 
                           Job_type + Marital_status + Home_status + Credit_purpose, 
                         family = binomial, data = data1)
coefficients <- coef(PD_LOGIT)

#Punkty odniesienia mogą być różne - w tym modelu zostały wykonane bazując na klasach ryzyka przedstawionych przez M. Iwanicz-Drozdowską ("Zarządzanie fiansowe bankiem")
#PD = 3% dla akceptowalnego ryzyka
pd_reference <- 0.03
log_odds_reference <- log(pd_reference / (1 - pd_reference))
score_reference <- 600

#PD = 100% (najwyższe ryzyko) - 0 punktów
log_odds_highest_risk <- log(1 / (1 - 1)) 
score_highest_risk <- 0

#Funkcja do przekształcania log-odds(które są wynikiem regresji liniowej) na punkty
log_odds_to_score <- function(log_odds) {
  score <- (log_odds - log_odds_highest_risk) / (log_odds_reference - log_odds_highest_risk) * (score_reference - score_highest_risk) + score_highest_risk
  return(score)
}
# Funkcja do przypisania klasy ryzyka na podstawie wyniku scoringowego
przypisz_klase_ryzyka <- function(score, granice, klasy) {
  index <- findInterval(score, granice)
  return(klasy[index])
}

# Lista klas ryzyka (9 klas ryzyka)
klasy <- c("praktycznie brak ryzyka", "niskie ryzyko", "umiarkowane ryzyko", "przeciętne ryzyko",
           "akceptowalne ryzyko", "graniczne ryzyko", "pod obserwacją", "poniżej standardu", "wątpliwe")

# Granice scoringowe dla klas ryzyka (8 granic)
granice <- c(650, 600, 550, 500, 450, 400, 350, 300) # Przykładowe granice, dostosuj do swoich potrzeb

# Sprawdzenie, czy liczba granic jest o jeden mniejsza niż liczba klas ryzyka
if(length(klasy) != length(granice) + 1) {
  stop("Liczba granic musi być o jeden mniejsza niż liczba klas ryzyka.")
}

# Funkcja do przypisania klasy ryzyka
przypisz_klase_ryzyka <- function(score, granice, klasy) {
  index <- findInterval(score, granice)
  return(klasy[index + 1]) # Dodajemy 1, ponieważ findInterval zwraca indeks przedziału poniżej punktu
}

# Wyświetlenie pierwszych kilku klas ryzyka
head(data1$klasa_ryzyka)
