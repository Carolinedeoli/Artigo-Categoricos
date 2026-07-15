###############################################################################

### APLICAÇÃO DE TESTES PARA DADOS CATEGÓRICOS ORDINAIS

###############################################################################

#===============================================================================
#                 BIBLIOTECAS
#===============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(nortest)
library(scales)
library(ggplot2)
library(knitr)
library(kableExtra)
library(patchwork)
library(vcdExtra)
library(DescTools)
library(vcd)
#===============================================================================
#                 DADOS
#===============================================================================


dados_concluintes <- read_csv("Artigo-Categoricos/CONCLUINTES_2023.csv")
# 1. Calculamos os pontos de corte (vetor com Mínimo, Q1, Mediana, Q3 e Máximo)
cortes <- quantile(dados_concluintes$desempenho,
                   probs = c(0, 0.25, 0.5, 0.75, 1),
                   na.rm = TRUE)

# 2. Criamos a variável categorizando os alunos nesses intervalos
dados_concluintes <- dados_concluintes %>%
  mutate(setor_desempenho = cut(desempenho,
                                breaks = cortes,
                                include.lowest = TRUE,
                                right = FALSE,
                                labels = c("Baixo (até Q1)",
                                           "Médio-Baixo (Q1 a Mediana)",
                                           "Médio-Alto (Mediana a Q3)",
                                           "Alto (acima de Q3)")))


tabela_cruzada <- read_csv("Artigo-Categoricos/tabela_cruzada.csv")

# 1. Garantir a ordem lógica/ordinal das categorias
dados_concluintes$escolaridade_mae <- factor(
  dados_concluintes$escolaridade_mae, 
  levels = c("Nunca estudou","Não completou EF (até 5º ano)", "EF incompleto (até 9º ano)", "EF completo", "Médio completo", "Superior completo","Pós-graduação")
)

dados_concluintes$setor_desempenho <- factor(
  dados_concluintes$setor_desempenho, 
  levels = c("Baixo (até Q1)", "Médio-Baixo (Q1 a Mediana)", "Médio-Alto (Mediana a Q3)", "Alto (acima de Q3)"), 
  ordered = TRUE
)

# 2. Criar a tabela (o seu código)
tab <- xtabs(
  n ~ escolaridade_mae + setor_desempenho,
  data = tabela_cruzada
)

# 3. Visualizar para conferir
print(tab)

#===============================================================================
#  TESTE DE INDEPENDÊNCIA QUI-QUADRADO DE PEARSON (GERAL/NOMINAL)
#===============================================================================
# H0: A escolaridade da mãe e o setor de desempenho são independentes.
# H1: A escolaridade da mãe e o setor de desempenho são dependentes.

teste_independencia <- chisq.test(tab)
print(teste_independencia)

# CONCLUSÃO: p-valor < 2.2e-16 (p < 0.05). Rejeita-se H0.
# As variáveis são estatisticamente dependentes; o perfil de desempenho varia 
# de acordo com a escolaridade da mãe.


#===============================================================================
#                 Mantel - Haenszel 
#===============================================================================
# H0: NÃO existe uma tendência linear entre a escolaridade da mãe e o desempenho.
# H1: EXISTE uma tendência linear entre a escolaridade da mãe e o desempenho.

library(vcdExtra)
lbl_teste <- CMHtest(tab)
print(lbl_teste) # Focar na linha 'cor' (Nonzero correlation)

# Extração do p-valor exato da tendência linear
p_valor_linear <- lbl_teste$table["cor", "Prob"]
cat("P-valor exato da tendência linear:", format(p_valor_linear, scientific = TRUE), "\n")

# CONCLUSÃO: Chisq = 109914, Df = 1, p-valor < 0.0001 (indicado como 0e+00). 
# Rejeita-se H0. Há uma tendência linear altamente significativa. À medida que 
# o nível de instrução da mãe se eleva, o desempenho do aluno no ENEM tende 
# a aumentar sistematicamente.


#===============================================================================
#                 MEDIDA DE ASSOCIAÇÃO ORDINAL: GAMA DE GOODMAN E KRUSKAL
#===============================================================================
# H0: A associação ordinal entre as variáveis na população é igual a zero (Gama = 0).
# H1: A associação ordinal na população é diferente de zero (Gama != 0).

gama_res <- GKgamma(tab)
print(gama_res)

GoodmanKruskalGamma(tab)
assocstats(tab)


# 1. Pega os valores que a função gerou
gama_est <- gama_res$gamma
gama_erro <- gama_res$sigma # O erro padrão fica salvo aqui

# 2. Calcula a Estatística Z (Estimativa dividida pelo Erro Padrão)
z_score <- gama_est / gama_erro

# 3. Calcula o p-valor bi-caudal
p_valor_gamma <- 2 * pnorm(-abs(z_score))

# 4. Mostra o resultado formatado
cat("O p-valor exato do Gamma é:", format(p_valor_gamma, scientific = TRUE), "\n")
# CONCLUSÃO: Gama = 0.376 (IC95%: 0.374 a 0.378). Rejeita-se H0 (IC não inclui o 0).
# Indica uma associação positiva moderada. Estudantes com mães de maior 
# escolaridade têm probabilidade substancialmente maior de atingir quartis 
# mais altos de desempenho.

#===============================================================================
#                 Tau-b de Kendall
#===============================================================================

# H0: O coeficiente de correlação de postos Tau-b na população é igual a zero (Tau-b = 0).
# H1: O coeficiente de correlação de postos Tau-b é diferente de zero (Tau-b != 0).

tau_b_res <- KendallTauB(tab, conf.level = 0.95)
print(tau_b_res)

# O cor.test exige os dados brutos como números
teste_kendall_exato <- cor.test(
  as.numeric(dados_concluintes$escolaridade_mae), 
  as.numeric(dados_concluintes$desempenho), 
  method = "kendall",
  exact = FALSE # Usamos FALSE porque a base do ENEM é gigante
)

print(teste_kendall_exato)

# CONCLUSÃO: Tau-b = 0.287 (IC95%: 0.286 a 0.289). Rejeita-se H0 (IC não inclui o 0).
# Confirma a associação positiva. O valor é numericamente menor que o Gama porque
# o Tau-b ajusta e penaliza a métrica pela grande quantidade de empates (ties) 
# nas categorias, sendo uma estimativa mais conservadora e robusta para este banco.


# ==============================================================================
# SÍNTESE GERAL DOS ACHADOS:
# Todos os testes convergem. A hipótese de que o background educacional materno
# não interfere no resultado do aluno é rejeitada. Há um efeito direto, positivo
# e de magnitude moderada da escolaridade da mãe sobre o desempenho no ENEM.
# ==============================================================================



# 1. Construindo o banco de dados com os resultados da sua saída
tabela_associacao <- data.frame(
  Teste = c(
    "Qui-quadrado de Pearson",
    "Mantel-Haenszel (Tendência Linear)",
    "V de Cramér",
    "Gamma de Goodman-Kruskal",
    "Tau-b de Kendall"
  ),
  Objetivo = c(
    "Independência Geral",
    "Correlação Linear",
    "Força da Associação",
    "Associação Ordinal",
    "Associação Ordinal"
  ),
  Estatistica = c("138.027", "117.317", "0,214", "0,380", "0,291"),
  Graus_Liberdade = c("18", "1", "-", "-", "-"),
  Significancia_IC = c("< 0,001", "< 0,001", "-", "[0,378; 0,382]", "[0,289; 0,292]")
)

# 2. Gerando o código LaTeX da tabela
tabela_associacao %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    align = "llccc",
    caption = "Testes de Independência e Associação entre Escolaridade da Mãe e Desempenho",
    col.names = c("Teste Estatístico", "Objetivo", "Valor da Estatística", "gl", "Valor-p / IC 95%")
  ) %>%
  kable_styling(
    latex_options = c("striped", "hold_position"),
    full_width = FALSE
  )
################################################################################

##          GRÁFICOS

################################################################################

#===============================================================================
#  GRÁFICO DE BARRAS
#===============================================================================

# Convertendo sua tabela xtabs de volta para data.frame para o ggplot
df_plot <- as.data.frame(tab)

ggplot(df_plot, aes(x = escolaridade_mae, y = Freq, fill = setor_desempenho)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Blues", direction = 1) + # Tons de azul para o desempenho
  theme_minimal() +
  labs(
    title = "Tendência de Desempenho no ENEM por Escolaridade Materna",
    x = "Escolaridade da Mãe",
    y = "Percentual (%)",
    fill = "Setor de Desempenho"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotaciona o texto para não encavalar



#===============================================================================
#  GRÁFICO DE MOSAICO
#===============================================================================

# Gráfico de mosaico simples e elegante
mosaicplot(tab, 
           main = "Gráfico de Mosaico: Escolaridade vs Desempenho",
           shade = TRUE, # Colore de acordo com a força da associação (Azul/Vermelho)
           las = 2,  
           
           color = TRUE,
           xlab = "Escolaridade da Mãe",
           ylab = "Desempenho"
           )






# O gráfico de mosaico confirma visualmente a tendência linear identificada no teste CMH. 
# Observa-se uma clara transição diagonal de resíduos: nos níveis de menor escolaridade materna 
# (Nunca estudou e EF incompleto), há um excesso significativo de estudantes no quartil de desempenho Baixo 
# (resíduos azuis > 4). À medida que a escolaridade da mãe avança para o nível Superior e Pós-graduação,
# a concentração de estudantes desloca-se de forma acentuada para os quartis Médio-Alto e Alto,
# onde os resíduos positivos indicam uma frequência observada drasticamente maior do que a esperada 
# sob a hipótese de independência.

# Interpretação:Filhos de mães que "Nunca estudaram" ou têm "EF incompleto" estão massivamente concentrados
# no desempenho Baixo (muito acima do esperado) e quase não aparecem no desempenho 
# Alto (muito abaixo do esperado).

# Interpretação: Filhos de mães com Superior Completo e Pós-Graduação têm uma presença esmagadora 
# nos níveis mais altos de nota (muito acima do esperado) e raramente ficam no quartil Baixo
# (muito abaixo do esperado).
#===============================================================================
#  GRÁFICO DE TENDÊNCIA
#===============================================================================

# 1. Converter sua tabela xtabs em um data.frame formatado
df_linhas <- as.data.frame(tab)

# 2. Atribuir valores numéricos (scores/ranks) para o desempenho
# Garantindo que a conversão siga a ordem correta das categorias
df_linhas <- df_linhas %>%
  mutate(
    score_desempenho = as.numeric(setor_desempenho) # Baixo=1, Médio-Baixo=2, etc.
  )

# 3. Calcular a média ponderada do desempenho para cada escolaridade da mãe
df_tendencia <- df_linhas %>%
  group_by(escolaridade_mae) %>%
  summarise(
    desempenho_medio = sum(score_desempenho * Freq) / sum(Freq)
  )

# 4. Plotar o Gráfico de Linha de Tendência
ggplot(df_tendencia, aes(x = escolaridade_mae, y = desempenho_medio, group = 1)) +
  geom_line(color = "#2b8cbe", size = 1.2) + # Linha da tendência
  geom_point(color = "#084081", size = 4) +   # Pontos em cada categoria
  scale_y_continuous(
    limits = c(1, 4), 
    breaks = 1:4, 
    labels = c("Baixo (1)", "Médio-Baixo (2)", "Médio-Alto (3)", "Alto (4)")
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Linha de Tendência: Desempenho Médio por Escolaridade Materna",
    subtitle = "Demonstração visual do Teste de Associação Linear (CMH)",
    x = "Escolaridade da Mãe",
    y = "Score Médio de Desempenho"
  ) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1, face = "bold"),
    panel.grid.minor = element_blank()
  )

########################################################
library(dplyr)
library(ggplot2)
library(scales)

tab_prop <- prop.table(tab, margin = 1) |>  # proporção por escolaridade
  as.data.frame()

names(tab_prop) <- c(
  "Escolaridade",
  "Desempenho",
  "Proporcao"
)

ggplot(tab_prop,
       aes(x = Desempenho,
           y = Escolaridade,
           fill = Proporcao)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = percent(Proporcao, accuracy = 0.1)),
            size = 3.5) +
  scale_fill_gradient(
    low = "#F7FBFF",
    high = "#2171B5",
    labels = percent
  ) +
  labs(
    x = "Desempenho",
    y = "Escolaridade da mãe",
    fill = "Proporção"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )


#================================================================================

## REGRESSAO ORDINAL

#===============================================================================


library(MASS)
library(brant)

# 2. Ajustando o modelo ordinal com ambas as variáveis fatores
modelo_ord <- polr(setor_desempenho ~ escolaridade_mae, data = dados_concluintes, Hess = TRUE)
summary(modelo_ord)

# 3. Executando o Teste de Brant
# (Esperamos p-valor > 0.05 pois geramos os dados sob paralelismo)
brant(modelo_ord)


#todos com probabilidade 0, ou seja não funcionou 



library(VGAM)

# Modelo apenas com as duas variáveis, relaxando o pressuposto de linhas paralelas
modelo_vglm <- vglm(setor_desempenho ~ escolaridade_mae, 
                    family = cumulative(parallel = FALSE), 
                    data = dados_concluintes)

summary(modelo_vglm)

exp(confint(modelo_vglm))

anova(modelo_ord, modelo_vglm)
#===================================
library(VGAM)
library(ggplot2)
library(tidyr)
library(dplyr)

# 1. Criar uma base de dados sintética com as categorias únicas de escolaridade
novos_dados <- data.frame(
  escolaridade_mae = c("Nunca estudou", "Não completou EF (até 5º ano)", 
                       "EF incompleto (até 9º ano)", "Médio completo", 
                       "Superior completo", "Pós-graduação")
)

# Garantir a ordenação correta das categorias
novos_dados$escolaridade_mae <- factor(
  novos_dados$escolaridade_mae, 
  levels = levels(dados_concluintes$escolaridade_mae)
)

# 2. Predizer as probabilidades para cada categoria individualmente
# 2. Predizer as probabilidades para cada categoria individualmente (CORRIGIDO)
predicoes <- predict(modelo_vglm, newdata = novos_dados, type = "response")

# 3. Juntar as colunas das predições com os nomes das categorias
dados_plot <- cbind(novos_dados, predicoes)

# 4. Transformar para o formato longo (necessário para o ggplot2)
dados_plot_longo <- dados_plot %>%
  pivot_longer(
    cols = -escolaridade_mae, 
    names_to = "Desempenho", 
    values_to = "Probabilidade"
  )

# Garantir que a legenda do desempenho respeite a ordem original do seu modelo
dados_plot_longo$Desempenho <- factor(
  dados_plot_longo$Desempenho,
  levels = c("Baixo (até Q1)", "Médio-Baixo (Q1 a Mediana)", "Médio-Alto (Mediana a Q3)", "Alto (acima de Q3)")
)

# 5. GERAR O GRÁFICO DE CURVAS DE PROBABILIDADE (Como o da sua imagem)
ggplot(dados_plot_longo, aes(x = escolaridade_mae, y = Probabilidade, color = Desempenho, group = Desempenho)) +
  geom_line(size = 1.2) +      # Desenha as curvas contínuas para cada status de desempenho
  geom_point(size = 3) +       # Adiciona os pontos exatos de cada categoria nas linhas
  scale_color_manual(values = c(
    "Baixo (até Q1)" = "#d9534f",             # Vermelho (Equivalente ao "Leve" da sua imagem)
    "Médio-Baixo (Q1 a Mediana)" = "#f0ad4e",   # Laranja
    "Médio-Alto (Mediana a Q3)" = "#337ab7",    # Azul (Equivalente ao "Moderada")
    "Alto (acima de Q3)" = "#5cb85c"            # Verde (Equivalente ao "Severa")
  )) +
  labs(
    title = "Curvas de Probabilidade: Modelo de Chances Não-Proporcionais",
    subtitle = "Probabilidades estimadas via modelo não-proporcional (vglm)",
    x = "Escolaridade da Mãe",
    y = "Probabilidade Estimada",
    color = "Faixa de Desempenho"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1, size = 10),
    legend.position = "right",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14, color = "#1a365d")
  )

# 5. Gerar o Gráfico de Barras Empilhadas (Excelente para Modelos Ordinais)
ggplot(dados_plot_longo, aes(x = escolaridade_mae, y = Probabilidade, fill = Desempenho)) +
  geom_bar(stat = "identity", position = "fill", width = 0.7) +
  scale_fill_brewer(palette = "RdYlBu") + # Paleta de cores do Vermelho (Baixo) ao Azul (Alto)
  labs(
    title = "Probabilidade Predita de Desempenho no ENEM por Escolaridade da Mãe",
    subtitle = "Modelo de Odds Proporcionais Parciais (VGAM) - ENEM 2023",
    x = "Escolaridade da Mãe",
    y = "Probabilidade Acumulada",
    fill = "Faixa de Desempenho"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "bottom",
    panel.grid.major.x = element_blank()
  )



## Obten??o de uma tabela-resumo completa

gtsummary::tbl_regression(modelo_vglm, exponentiate = TRUE,
                          estimate_fun = purrr::partial(style_ratio, digits = 3)) %>% 
  gtsummary::add_global_p()

#=====================================

# 1. Modelo Ordinal com Liga ̧c~ao Probit
mod_probit <- polr(setor_desempenho ~ escolaridade_mae, method = "probit",
                   data = dados_concluintes, Hess = TRUE)

# 2. Modelo Ordinal com Liga ̧c~ao Cloglog
mod_cloglog <- polr(setor_desempenho ~ escolaridade_mae, method = "cloglog",
                    data = dados_concluintes, Hess = TRUE)
summary(mod_cloglog)
# 3. Executando o Teste de Brant
# (Esperamos p-valor > 0.05 pois geramos os dados sob paralelismo)
brant(mod_probit)
brant(mod_cloglog)

AIC(modelo_ord, mod_probit, mod_cloglog)


logLik(modelo_ord)
logLik(modelo_vglm)

gl <- length(coef(modelo_vglm)) - attr(logLik(modelo_ord), "df")
LR <- 2 * (as.numeric(logLik(modelo_vglm)) -
             as.numeric(logLik(modelo_ord)))

pchisq(LR, df = gl, lower.tail = FALSE)

mod_prop <- vglm(
  setor_desempenho ~ escolaridade_mae,
  family = cumulative(parallel = TRUE),
  data = dados_concluintes
)

mod_nao <- vglm(
  setor_desempenho ~ escolaridade_mae,
  family = cumulative(parallel = FALSE),
  data = dados_concluintes
)

VGAM::lrtest(mod_prop, modelo_vglm)
anova(mod_prop, modelo_vglm, test = "LR")
