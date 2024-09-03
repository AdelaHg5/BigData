library(tidyverse)
library(modelr)
library(caret)
library(rsample)
library(readr)
library(corrplot)
library(e1071)

# Citire set date (CSV)
ClinicalRecords <- read_csv("C:/Users/Adela/Desktop/proiectbig/proiect/heart_failure_clinical_records_dataset.csv")

# Generare grafice cu atribute numerice
ClinicalRecords %>%
  select_if(is.numeric) %>%
  gather(metric, value) %>%
  ggplot(aes(value, fill=metric)) +
  geom_density(show.legend = FALSE) +
  facet_wrap(~metric, scales = "free")

# Transformare atribute categorice -> atribute nominale
names(ClinicalRecords)
ClinicalRecords <- ClinicalRecords %>% 
  mutate(
    anaemia = ifelse(anaemia == 0, "No", "Yes"),
    diabetes = ifelse(diabetes == 0, "No", "Yes"),
    high_blood_pressure = ifelse(high_blood_pressure == 0, "No", "Yes"),
    sex = ifelse(sex == 0, "F", "M"),
    smoking = ifelse(smoking == 0, "No", "Yes"),
    DEATH_EVENT = ifelse(DEATH_EVENT == 0, "No", "Yes"))


# Transformare atribute nominale in factori nominali
ClinicalRecords <- ClinicalRecords %>% 
  mutate(
    anaemia = factor(anaemia),
    diabetes = factor(diabetes),
    high_blood_pressure = factor(high_blood_pressure),
    sex = factor(sex),
    smoking = factor(smoking),
    DEATH_EVENT = factor(DEATH_EVENT))


# Stergere atribut time
ClinicalRecords <- ClinicalRecords %>%
  relocate(time, .after = DEATH_EVENT) %>%
  select(-time)

### REGRESIE LOGISTICA ###

# Comparare importanta factori de risc

# age-creatinine_phosphokinase
ggplot(ClinicalRecords) +
  geom_point(aes(x = age, y = creatinine_phosphokinase, color=DEATH_EVENT, shape = DEATH_EVENT)) + 
  scale_shape_manual(values = c(1, 4)) +
  theme(text = element_text(size=10))

# ejection_fraction - platelets
ggplot(ClinicalRecords) +
  geom_point(aes(x = ejection_fraction, y = platelets, color=DEATH_EVENT, shape = DEATH_EVENT)) + 
  scale_shape_manual(values = c(1, 4)) +
  theme(text = element_text(size=10))

#serum_creatinine - serum_sodium
ggplot(ClinicalRecords) +
  geom_point(aes(x = serum_creatinine, y = serum_sodium, color=DEATH_EVENT, shape = DEATH_EVENT)) + 
  scale_shape_manual(values = c(1, 4)) +
  theme(text = element_text(size=10))

#serum_creatinine - ejection_fraction
ggplot(ClinicalRecords) +
  geom_point(aes(x = serum_creatinine, y = ejection_fraction, color=DEATH_EVENT, shape = DEATH_EVENT)) + 
  scale_shape_manual(values = c(1, 4)) +
  theme(text = element_text(size=10))

#platelets-serum_sodium
ggplot(ClinicalRecords) +
  geom_point(aes(x = platelets, y = serum_sodium, color=DEATH_EVENT, shape = DEATH_EVENT)) + 
  scale_shape_manual(values = c(1, 4)) +
  theme(text = element_text(size=10))

#age
ggplot(ClinicalRecords) +
  geom_boxplot(aes(x = DEATH_EVENT, y =age,  fill = DEATH_EVENT)) +
  theme(text = element_text(size=10))

#creatinine_phosphokinase
ggplot(ClinicalRecords) +
  geom_boxplot(aes(x = DEATH_EVENT, y =creatinine_phosphokinase,  fill = DEATH_EVENT)) +
  theme(text = element_text(size=10))

#ejection_fraction
ggplot(ClinicalRecords) +
  geom_boxplot(aes(x = DEATH_EVENT, y =ejection_fraction,  fill = DEATH_EVENT)) +
  theme(text = element_text(size=10))

#platelets
ggplot(ClinicalRecords) +
  geom_boxplot(aes(x = DEATH_EVENT, y =platelets,  fill = DEATH_EVENT)) +
  theme(text = element_text(size=10))

#serum_creatinine
ggplot(ClinicalRecords) +
  geom_boxplot(aes(x = DEATH_EVENT, y =serum_creatinine,  fill = DEATH_EVENT)) +
  theme(text = element_text(size=10))

#serum_sodium
ggplot(ClinicalRecords) +
  geom_boxplot(aes(x = DEATH_EVENT, y =serum_sodium,  fill = DEATH_EVENT)) +
  theme(text = element_text(size=10))

# Numar total de decese
by_default <- group_by(ClinicalRecords, DEATH_EVENT)
summarize(by_default, count = n())


# Regresie logistica: DEATH_EVENT in fct de serum_creatinine
mod <- glm(data = ClinicalRecords, DEATH_EVENT ~ serum_creatinine, family = binomial)
summary(mod)

grid <- ClinicalRecords %>%
  data_grid(serum_creatinine = seq_range(serum_creatinine, 50)) %>%
  add_predictions(mod, "prob_default", type="response")

ggplot() +
  geom_line(data = grid, aes(serum_creatinine, prob_default), color = "red", size = 2) 

nd <- tribble(~serum_creatinine, 0.5,  5)
predicted <- predict(mod, newdata = nd, type = "response")

# Regresie logistica: DEATH_EVENT in fct de ejection_fraction
mod <- glm(data = ClinicalRecords, DEATH_EVENT ~ ejection_fraction , family = binomial)
summary(mod)

grid <- ClinicalRecords %>%
  data_grid(ejection_fraction = seq_range(ejection_fraction, 100)) %>%
  add_predictions(mod, "prob_default", type="response")

ggplot() +
  geom_line(data = grid, aes(ejection_fraction, prob_default), color = "red", size = 2)

nd <- tribble(~ejection_fraction, 24, 46)
predicted <- predict(mod, newdata = nd, type = "response")


# Coeficinti anemie 

mod_anaemia <- glm(data = ClinicalRecords, DEATH_EVENT ~ anaemia, family = binomial)
summary(mod_anaemia)

nd <- tribble(~anaemia, "Yes", "No")
predicted <- predict(mod_anaemia, newdata = nd, type = "response")

# Coeficientii pentru toate variabilele
mod_all <- glm(data = ClinicalRecords, DEATH_EVENT ~ age + anaemia + creatinine_phosphokinase + diabetes + ejection_fraction + high_blood_pressure + platelets + serum_creatinine + serum_sodium + sex + smoking, family = binomial)
summary(mod_all)


# serum_creatinine / anaemiaYes         
mod_serum_creatinine_anaemia <- glm(data = ClinicalRecords, DEATH_EVENT ~  serum_creatinine + anaemia, family = binomial)
summary(mod_serum_creatinine_anaemia)

grid_anaemia_yes <- ClinicalRecords %>%
  data_grid(serum_creatinine = seq_range(serum_creatinine, 100)) %>%
  mutate(anaemia = "Yes") %>%
  add_predictions(mod_serum_creatinine_anaemia, "prob_default", type = "response")

grid_anaemia_no <- ClinicalRecords %>%
  data_grid(serum_creatinine = seq_range(serum_creatinine, 100)) %>%
  mutate(anaemia = "No") %>%
  add_predictions(mod_serum_creatinine_anaemia, "prob_default", type = "response")

ggplot(grid_anaemia_yes, aes(serum_creatinine, prob_default)) +
  geom_line(color = "blue", size = 2) +
  geom_line(data = grid_anaemia_no, color = "orange", size = 2)

ggplot(ClinicalRecords, aes(x=anaemia, y=serum_creatinine, fill=anaemia)) +
  geom_boxplot()
#creare model ejection_fraction high_blood_pressure

mod_ejection_fraction_high_blood_pressure <- glm(data = ClinicalRecords, DEATH_EVENT ~ ejection_fraction + high_blood_pressure, family = binomial)
summary(mod_ejection_fraction_high_blood_pressure)

grid_high_blood_pressure_yes <- ClinicalRecords %>%
  data_grid(ejection_fraction = seq_range(ejection_fraction, 100)) %>%
  mutate(high_blood_pressure = "Yes") %>%
  add_predictions(mod_ejection_fraction_high_blood_pressure, "prob_default", type = "response")

grid_high_blood_pressure_no <- ClinicalRecords %>%
  data_grid(ejection_fraction = seq_range(ejection_fraction, 100)) %>%
  mutate(high_blood_pressure = "No") %>%
  add_predictions(mod_ejection_fraction_high_blood_pressure, "prob_default", type = "response")

ggplot(grid_high_blood_pressure_yes, aes(ejection_fraction, prob_default)) +
  geom_line(color = "blue", size = 2) +
  geom_line(data = grid_high_blood_pressure_no, color = "orange", size = 2)

ggplot(ClinicalRecords, aes(x=high_blood_pressure, y=ejection_fraction, fill=high_blood_pressure)) +
  geom_boxplot()

# Rezolvarea problemei de clasificare
# pe baza setului de date disponibil

set.seed(123)
split <- initial_split(ClinicalRecords, prop = 0.7, strata = "DEATH_EVENT")
train <- training(split)
test <- testing(split)

mod_ejection_fraction_high_blood_pressure_train <- glm(data = train, DEATH_EVENT ~ ejection_fraction  + high_blood_pressure, family = binomial)
summary(mod_ejection_fraction_high_blood_pressure_train)

pred_test <- predict(mod_ejection_fraction_high_blood_pressure_train, newdata = test, type = "response")
table(pred_test > 0.3, test$DEATH_EVENT)  

#perform classification analysis with caret
set.seed(123)
split <- initial_split(ClinicalRecords, prop = 0.7, strata = "DEATH_EVENT")
train <- training(split)
test <- testing(split)
table(test$DEATH_EVENT)
table(train$DEATH_EVENT)

features <- setdiff(names(ClinicalRecords),  "DEATH_EVENT")
x <- train[, features]
y <- train$DEATH_EVENT

fitControl <- trainControl(
  method = "cv",
  number = 5
)
modGLM_all <- train(
  x=x,
  y=y,
  method = "glm",
  family = "binomial",
  trControl = fitControl
)
modGLM_all

confusionMatrix(modGLM_all)
pred_all = predict(modGLM_all, newdata = test, type = "raw")
confusionMatrix(pred_all, test$DEATH_EVENT)

summary(modGLM_all)

xNoejection_fraction <- x %>% select(-creatinine_phosphokinase,-platelets,-sex,-smoking,-diabetes,-anaemia,-high_blood_pressure,-serum_sodium  )

modGLM_Noejection_fraction <- train(
  x=xNoejection_fraction,
  y=y,
  method = "glm",
  family = "binomial",
  trControl = fitControl
)
confusionMatrix(modGLM_Noejection_fraction)

summary(modGLM_Noejection_fraction)
#names(modGLM_Noejection_fraction)

pred_noejection_fraction <- predict(modGLM_Noejection_fraction, test)
predProb <- predict(modGLM_Noejection_fraction, test, type = "prob")
confusionMatrix(pred_noejection_fraction, test$DEATH_EVENT)


# ROC
library(pROC)

dataset <- data.frame(
  actual.class <- test$DEATH_EVENT,
  probability <- predProb[,1])

roc.val <- roc(actual.class ~ probability, dataset)
adf <- data.frame(  # creem un data frame nou pentru a putea face grafic cu ggplot
  specificity <- roc.val$specificities, 
  sensitivity <- roc.val$sensitivities)

ggplot() + 
  geom_line(data=adf, aes(specificity, sensitivity), color = 'blue') + 
  scale_x_reverse() + 
  theme(text = element_text(size=20))


### FINAL REGRESIE LOGISTICA ###



### ARBORI DE DECIZIE ####

library(rpart)       
library(rpart.plot)  
library(partykit)

ClinicalRecords <- read_csv("C:/Users/Adela/Desktop/proiectbig/proiect/heart_failure_clinical_records_dataset.csv")

ClinicalRecords <- ClinicalRecords %>% 
  mutate(anaemia = ifelse(anaemia == 0, "No", "Yes"),
         diabetes = ifelse(diabetes == 0, "No", "Yes"),
         high_blood_pressure = ifelse(high_blood_pressure == 0, "No", "Yes"),
         sex = ifelse(sex == 0, "F", "M"),
         smoking = ifelse(smoking == 0, "No", "Yes"),
         DEATH_EVENT = ifelse(DEATH_EVENT == 0, "No", "Yes"))

ClinicalRecords <- ClinicalRecords %>% 
  mutate(
    anaemia = factor(anaemia),
    diabetes = factor(diabetes),
    high_blood_pressure = factor(high_blood_pressure),
    sex = factor(sex),
    smoking = factor(smoking),
    DEATH_EVENT = factor(DEATH_EVENT))

for(i in 1:nrow(ClinicalRecords)) {
  if(ClinicalRecords$age[i] >= 40 && ClinicalRecords$age[i] <=49) {
    ClinicalRecords$age[i] = 40
  }
  else if (ClinicalRecords$age[i] >= 50 && ClinicalRecords$age[i] <=59) {
    ClinicalRecords$age[i] = 50
  }
  else if (ClinicalRecords$age[i] >= 60 && ClinicalRecords$age[i] <=69) {
    ClinicalRecords$age[i] = 60
  }
  else if (ClinicalRecords$age[i] >= 70 && ClinicalRecords$age[i] <=79) {
    ClinicalRecords$age[i] = 70
  }
  else {
    ClinicalRecords$age[i] = 80
  }
}

ClinicalRecords <- ClinicalRecords %>% 
  mutate(age = factor(age))

set.seed(123)
records_split <- initial_split(ClinicalRecords, prop = 0.7, strata = "DEATH_EVENT")
records_train <- training(records_split)
records_test <- testing(records_split)
table(records_train$DEATH_EVENT)
table(records_test$DEATH_EVENT)

set.seed(123)

m1 = rpart(
  formula = DEATH_EVENT ~. - time,
  data = records_train,
  method = "class"
)
m1
summary(m1)
rpart.plot(m1)

pred_m1 <- predict(m1, newdata = records_test, target ="class")
pred_m1 <- as_tibble(pred_m1) %>% mutate(class = ifelse(No >= Yes, "No", "Yes"))
table(pred_m1$class, records_test$DEATH_EVENT)
confusionMatrix(factor(pred_m1$class), factor(records_test$DEATH_EVENT))
plotcp(m1)

set.seed(123)
m1_pruned <- prune(m1, cp=0.017)
m1_pruned

pred_m1_pruned <- predict(m1_pruned, newdata = records_test, target = "class")
pred_m1_pruned <- as_tibble(pred_m1_pruned) %>% mutate(class = ifelse(No >= Yes, "No", "Yes"))
confusionMatrix(factor(pred_m1_pruned$class), factor(records_test$DEATH_EVENT))



library(pROC)
#curba roc pt m1
pred_m1 <- predict(m1, newdata = records_test, target ="class")
datasetM1 <- data.frame(
  actual.class <- records_test$DEATH_EVENT,
  probability <- pred_m1[,1] #
)
roc.val <- roc(actual.class ~ probability, datasetM1)
adf <- data.frame(  # creem un data frame nou pentru a putea face grafic cu ggplot
  specificity <- roc.val$specificities, 
  sensitivity <- roc.val$sensitivities)
ggplot() + 
  geom_line(data=adf, aes(specificity, sensitivity), color = 'blue') + 
  scale_x_reverse() + 
  theme(text = element_text(size=14))



#bagging 
library(ipred)
set.seed(123)
bagged_m1 <- bagging(DEATH_EVENT ~ . - time,
                     data = records_train, 
                     coob = TRUE)
bagged_m1
summary(bagged_m1)
pred_bagged_m1 <- predict(bagged_m1, newdata = records_test, target = "class")
confusionMatrix(pred_bagged_m1, factor(records_test$DEATH_EVENT))

#
ntree <- seq(10, 70, by = 1)
misclassification <- vector(mode = "numeric", length = length(ntree))
for (i in seq_along(ntree)) {
  set.seed(123)
  model <- bagging( 
    DEATH_EVENT ~. - time,
    data = records_train,
    coob = TRUE,
    nbag = ntree[i])
  misclassification[i] = model$err
}
plot(ntree, misclassification, type="l", lwd="2")
abline(v = 56, col = "red", lty = "dashed")

#ceva mai mult de 50 bags sunt necesare pentru a stabiliza rata de eroare

bagged_m1_56 <- bagging(DEATH_EVENT ~ .- time,
                        data = records_train, coob = TRUE, nbag = 56)
bagged_m1_56
summary(bagged_m1_56)
pred_bagged_m1_56 <- predict(bagged_m1_56, newdata = records_test, target = "class")
confusionMatrix(pred_bagged_m1_56, factor(records_test$DEATH_EVENT))

