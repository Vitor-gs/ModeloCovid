##################################################################################
#                  INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS              #
##################################################################################
#Pacotes utilizados
pacotes <- c("plotly","tidyverse","knitr","kableExtra","fastDummies","rgl","car",
             "reshape2","jtools","stargazer","lmtest","caret","pROC","ROCR","nnet",
             "magick","cowplot","globals","haven", "data.table")


options(rgl.debug = TRUE)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

##############################################################################
#                   REGRESSÃO LOGÍSTICA BINÁRIA - OBITO                      #
##############################################################################
load(file = "covid_data.Rdata")
glimpse(df_analise)
summary(df_analise)


# Dummizando todas as variáveis
df_dummies <- df_analise %>% dummy_columns(select_columns = c(
  "CS_SEXO", "CS_RACA", "SG_UF", "IS_GESTANT", "IS_NOSOCOMIAL", "IS_PUERPERA",
  "IS_CARDIOPATI", "IS_HEMATOLOGIL", "IS_SIND_DOWN", "IS_HEPATICA", "IS_ASMA",
  "IS_DIABETES", "IS_NEUROLOGIC", "IS_PNEUMOPATI", "IS_IMUNODEPRE", "IS_RENAL",
  "IS_OBESIDADE", "VACINA_DOSES", "IDADE"),
  remove_selected_columns = T,
  remove_most_frequent_dummy = T
)

# Modelo - Óbito
modelo_obito <- glm(formula = OBITO ~ . -VENT_NAO_INV - VENT_INV,
                    data = df_dummies, 
                    family = "binomial")
summary(modelo_obito)
logLik(modelo_obito)

## Procedimento Stepwise
step_obito <- step(object = modelo_obito,
                   k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))
summary(step_obito)
logLik(step_obito)
summ(step_obito, confint = T, digits = 3, ci.width = .95)

## Comparando os modelos
lrtest(modelo_obito, step_obito)

## Matriz de confusão
confusionMatrix(table(predict(step_obito, type = "response") >= 0.5, 
                      df_analise$OBITO == 1)[2:1, 2:1])

df_predictions <- df_analise %>% mutate(
  PRED_OBITO_MODEL = modelo_obito$fitted.values,
  PRED_OBITO_STEP = step_obito$fitted.values,
)

## Construção curva ROC
predicoes <- prediction(predictions = step_obito$fitted.values, 
                        labels = df_analise$OBITO)
dados_curva_roc <- performance(predicoes, measure = "sens")
sensitividade <- dados_curva_roc@y.values[[1]]
especificidade <- performance(predicoes, measure = "spec") 
especificidade <- especificidade@y.values[[1]]
cutoffs <- dados_curva_roc@x.values[[1]]
dados_plotagem <- cbind.data.frame(cutoffs, especificidade, sensitividade)
ggplotly(dados_plotagem %>%
           ggplot(aes(x = cutoffs, y = especificidade)) +
           geom_line(aes(color = "Especificidade"),
                     size = 1) +
           geom_point(color = "#95D840FF",
                      size = 1.9) +
           geom_line(aes(x = cutoffs, y = sensitividade, color = "Sensitividade"),
                     size = 1) +
           geom_point(aes(x = cutoffs, y = sensitividade),
                      color = "#440154FF",
                      size = 1.9) +
           labs(x = "Cutoff",
                y = "Sensitividade/Especificidade") +
           scale_color_manual("Legenda:",
                              values = c("#95D840FF", "#440154FF")) +
           theme_bw())

## Plotagem ROC
ROC <- roc(response = df_analise$OBITO, 
           predictor = step_obito$fitted.values)
ggplot() +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
               color = "grey40", linewidth = 0.2) +
  geom_line(aes(x = 1 - especificidade, y = sensitividade),
            color = "darkorchid", linewidth = 2) +
  labs(x = "1 - Especificidade",
       y = "Sensitividade",
       title = paste("Área abaixo da curva:",
                     round(ROC$auc, 4),
                     "|",
                     "Coeficiente de Gini:",
                     round((ROC$auc[1] - 0.5) / 0.5, 4))) +
  theme(panel.background = element_rect(NA),
        panel.border = element_rect(color = "black", fill = NA),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)
  )
