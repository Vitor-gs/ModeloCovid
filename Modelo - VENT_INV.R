##################################################################################
#                  INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS              #
##################################################################################
# Pacotes utilizados
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
#                   REGRESSÃO LOGÍSTICA BINÁRIA - VENT_INV                   #
##############################################################################
load(file = "covid_data.Rdata")
glimpse(df_analise)
summary(df_analise)


# Dummizando todas as variáveis
df_dummies <- df_analise %>% dummy_columns(select_columns = c(
  "SEXO", "RACA", "UF", "IS_GESTANT", "IS_NOSOCOMIAL", "IS_PUERPERA",
  "IS_CARDIOPATI", "IS_HEMATOLOGIL", "IS_SIND_DOWN", "IS_HEPATICA", "IS_ASMA",
  "IS_DIABETES", "IS_NEUROLOGIC", "IS_PNEUMOPATI", "IS_IMUNODEPRE", "IS_RENAL",
  "IS_OBESIDADE", "VACINA_DOSES", "IDADE"),
  remove_selected_columns = T,
  remove_most_frequent_dummy = T
)

# Modelo - Suporte ventilatório invasivo
modelo_vent_inv <- glm(formula = VENT_INV ~ . -VENT_NAO_INV - OBITO, 
                       data = df_dummies, 
                       family = "binomial")
summary(modelo_vent_inv)
logLik(modelo_vent_inv)
summ(modelo_vent_inv, confint = T, digits = 3, ci.width = .95)

## Procedimento Stepwise
step_vent_inv <- step(object = modelo_vent_inv,
                      k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))
summary(step_vent_inv)
logLik(step_vent_inv)
summ(step_vent_inv, confint = T, digits = 3, ci.width = .95)

## Comparando os modelos
lrtest(modelo_vent_inv, step_vent_inv)

## Matriz de confusão
confusionMatrix(table(predict(step_vent_inv, type = "response") >= 0.5, 
                      df_analise$VENT_INV == 1)[2:1, 2:1])

df_predictions <- df_analise %>% mutate(
  PRED_VENT_INV_MODEL = modelo_vent_inv$fitted.values,
  PRED_VENT_INV_STEP = step_vent_inv$fitted.values,
)

## Construção curva ROC
predicoes <- prediction(predictions = step_vent_inv$fitted.values, 
                        labels = df_analise$VENT_INV)
dados_curva_roc <- performance(predicoes, measure = "sens")
sensitividade <- dados_curva_roc@y.values[[1]]
especificidade <- performance(predicoes, measure = "spec") 
especificidade <- especificidade@y.values[[1]]
cutoffs <- dados_curva_roc@x.values[[1]]
dados_plotagem <- cbind.data.frame(cutoffs, especificidade, sensitividade)
ggplotly(dados_plotagem %>%
           ggplot(aes(x = cutoffs, y = especificidade)) +
           geom_line(aes(color = "Especificidade"),
                     linewidth = 1) +
           geom_point(color = "#95D840FF",
                      size = 1.9) +
           geom_line(aes(x = cutoffs, y = sensitividade, color = "Sensitividade"),
                     linewidth = 1) +
           geom_point(aes(x = cutoffs, y = sensitividade),
                      color = "#440154FF",
                      size = 1.9) +
           labs(x = "Cutoff",
                y = "Sensitividade/Especificidade") +
           scale_color_manual("Legenda:",
                              values = c("#95D840FF", "#440154FF")) +
           theme_bw())

## Plotagem ROC
ROC <- roc(response = df_analise$VENT_INV, 
           predictor = step_vent_inv$fitted.values)
ggplot() +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
               color = "grey40", linewidth = 0.2) +
  geom_line(aes(x = 1 - especificidade, y = sensitividade),
            color = "darkorchid", linewidth = 2) +
  labs(x = "1 - Especificidade",
       y = "Sensitividade",
       title = paste("VENT_INV - Área abaixo da curva ROC:",
                     round(ROC$auc, 4))) +
  theme(panel.background = element_rect(NA),
        panel.border = element_rect(color = "black", fill = NA),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)
  )
