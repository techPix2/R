# Script Completo de Análise de Desempenho de Sistema
# Autor: [Seu Nome]
# Data: format(Sys.Date(), "%d/%m/%Y")

# 1. Instalação e Carregamento de Pacotes --------------------------------------
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("moments")) install.packages("moments")  # Para análise de assimetria e curtose

library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(gridExtra)
library(moments)

# 2. Geração de Dados Simulados ------------------------------------------------
set.seed(123)
n_capturas <- 20000
n_maquinas <- 3

dados <- data.frame(
  timestamp = rep(seq.POSIXt(
    from = as.POSIXct("2023-01-01 08:00:00"),
    by = "5 min",
    length.out = n_capturas
  ), n_maquinas),
  maquina = rep(paste0("Máquina_", LETTERS[1:n_maquinas]), each = n_capturas),
  CPU_perc = round(pmin(100, pmax(0, rnorm(n_capturas * n_maquinas, mean = 30, sd = 15))), 2),
  CPU_freq = round(runif(n_capturas * n_maquinas, min = 1.5, max = 3.5), 2),
  RAM_perc = round(pmin(100, pmax(0, rnorm(n_capturas * n_maquinas, mean = 50, sd = 20))), 2),
  RAM_bytes = round(runif(n_capturas * n_maquinas, min = 4e9, max = 16e9)),
  Disco_perc = round(pmin(100, pmax(0, rnorm(n_capturas * n_maquinas, mean = 40, sd = 15))), 2),
  Disco_bytes = round(runif(n_capturas * n_maquinas, min = 100e9, max = 500e9))
)

# Adicionar variação temporal realista
dados <- dados %>%
  group_by(maquina) %>%
  mutate(
    CPU_perc = CPU_perc * (1 + 0.3 * sin(hour(timestamp) * pi / 12)),
    RAM_perc = RAM_perc * (1 + 0.2 * sin(hour(timestamp) * pi / 12)),
    Disco_perc = Disco_perc * (1 + 0.1 * (sin((hour(timestamp) - 8) * pi / 8) + 
                                            sin((hour(timestamp) - 16) * pi / 8)))
  ) %>%
  ungroup()

# 3. Cálculo de Estatísticas Descritivas ---------------------------------------
estatisticas <- dados %>%
  select(maquina, CPU_perc, RAM_perc, Disco_perc) %>%
  pivot_longer(cols = -maquina, names_to = "recurso", values_to = "valor") %>%
  group_by(maquina, recurso) %>%
  summarise(
    Media = mean(valor),
    Mediana = median(valor),
    Desvio_Padrao = sd(valor),
    Minimo = min(valor),
    Maximo = max(valor),
    Assimetria = skewness(valor),
    Curtose = kurtosis(valor),
    .groups = 'drop'
  )

print("Estatísticas Descritivas:")
print(estatisticas)

# 4. Visualização da Distribuição com Histogramas ------------------------------
create_histogram <- function(data, var, title, xlab, bins = 30) {
  ggplot(data, aes_string(x = var)) +
    geom_histogram(aes(y = ..density..), 
                   bins = bins, 
                   fill = "skyblue", 
                   color = "black",
                   alpha = 0.7) +
    geom_density(color = "blue", size = 0.8) +
    stat_function(fun = dnorm, 
                  args = list(mean = mean(data[[var]], na.rm = TRUE), 
                              sd = sd(data[[var]], na.rm = TRUE)),
                  color = "red", 
                  size = 1,
                  linetype = "dashed") +
    labs(title = title, x = xlab, y = "Densidade") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

# Histogramas gerais
hist_cpu <- create_histogram(dados, "CPU_perc", "Distribuição do Uso de CPU", "Uso de CPU (%)")
hist_ram <- create_histogram(dados, "RAM_perc", "Distribuição do Uso de RAM", "Uso de RAM (%)")
hist_disco <- create_histogram(dados, "Disco_perc", "Distribuição do Uso de Disco", "Uso de Disco (%)")

# Histogramas por máquina
hist_cpu_maq <- ggplot(dados, aes(x = CPU_perc, fill = maquina)) +
  geom_histogram(aes(y = ..density..), bins = 25, alpha = 0.6, position = "identity") +
  geom_density(alpha = 0.3) +
  facet_wrap(~maquina, ncol = 1) +
  labs(title = "Distribuição do Uso de CPU por Máquina",
       x = "Uso de CPU (%)", y = "Densidade") +
  theme_minimal() +
  theme(legend.position = "none")

hist_ram_maq <- ggplot(dados, aes(x = RAM_perc, fill = maquina)) +
  geom_histogram(aes(y = ..density..), bins = 25, alpha = 0.6, position = "identity") +
  geom_density(alpha = 0.3) +
  facet_wrap(~maquina, ncol = 1) +
  labs(title = "Distribuição do Uso de RAM por Máquina",
       x = "Uso de RAM (%)", y = "Densidade") +
  theme_minimal() +
  theme(legend.position = "none")

# Exibir todos os histogramas
grid.arrange(
  hist_cpu, hist_ram, hist_disco,
  hist_cpu_maq, hist_ram_maq,
  ncol = 2,
  top = "Análise de Distribuição dos Recursos do Sistema"
)

# 5. Análise Temporal ---------------------------------------------------------
# Gráficos de série temporal
ts_cpu <- ggplot(dados, aes(x = timestamp, y = CPU_perc, color = maquina)) +
  geom_line(size = 0.7) +
  labs(title = "Uso de CPU ao Longo do Tempo",
       x = "Tempo", y = "Uso de CPU (%)") +
  theme_minimal() +
  theme(legend.position = "bottom")

ts_ram <- ggplot(dados, aes(x = timestamp, y = RAM_perc, color = maquina)) +
  geom_line(size = 0.7) +
  labs(title = "Uso de RAM ao Longo do Tempo",
       x = "Tempo", y = "Uso de RAM (%)") +
  theme_minimal() +
  theme(legend.position = "bottom")

ts_disco <- ggplot(dados, aes(x = timestamp, y = Disco_perc, color = maquina)) +
  geom_line(size = 0.7) +
  labs(title = "Uso de Disco ao Longo do Tempo",
       x = "Tempo", y = "Uso de Disco (%)") +
  theme_minimal() +
  theme(legend.position = "bottom")

# 6. Análise de Picos e Comparação --------------------------------------------
# Identificar picos
picos <- dados %>%
  group_by(maquina) %>%
  summarise(
    pico_CPU = max(CPU_perc),
    hora_pico_CPU = timestamp[which.max(CPU_perc)],
    pico_RAM = max(RAM_perc),
    hora_pico_RAM = timestamp[which.max(RAM_perc)],
    pico_Disco = max(Disco_perc),
    hora_pico_Disco = timestamp[which.max(Disco_perc)],
    .groups = 'drop'
  )

print("\nPicos de Utilização:")
print(picos)

# 7. Análise por Período do Dia -----------------------------------------------
dados_periodo <- dados %>%
  mutate(
    periodo = case_when(
      hour(timestamp) >= 8 & hour(timestamp) < 12 ~ "Manhã (8h-12h)",
      hour(timestamp) >= 12 & hour(timestamp) < 14 ~ "Meio-Dia (12h-14h)",
      hour(timestamp) >= 14 & hour(timestamp) < 18 ~ "Tarde (14h-18h)",
      TRUE ~ "Noite (18h-8h)"
    ),
    periodo = factor(periodo, levels = c("Manhã (8h-12h)", "Meio-Dia (12h-14h)", 
                                         "Tarde (14h-18h)", "Noite (18h-8h)"))
  ) %>%
  group_by(maquina, periodo) %>%
  summarise(
    CPU_media = mean(CPU_perc),
    RAM_media = mean(RAM_perc),
    Disco_media = mean(Disco_perc),
    .groups = 'drop'
  )

# Gráfico de uso por período
plot_periodo <- ggplot(dados_periodo %>% 
                         pivot_longer(cols = -c(maquina, periodo), 
                                      names_to = "recurso", values_to = "valor"), 
                       aes(x = periodo, y = valor, fill = maquina)) +
  geom_col(position = "dodge") +
  facet_wrap(~recurso, scales = "free_y", ncol = 1) +
  labs(title = "Uso Médio de Recursos por Período do Dia",
       x = "Período", y = "Uso Médio (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# 8. Exibição de Todos os Gráficos --------------------------------------------
grid.arrange(
  ts_cpu, ts_ram, ts_disco,
  plot_periodo,
  ncol = 1,
  heights = c(1, 1, 1, 1.5)
)

# 9. Testes de Normalidade ----------------------------------------------------
testes_normalidade <- dados %>%
  select(maquina, CPU_perc, RAM_perc, Disco_perc) %>%
  pivot_longer(cols = -maquina, names_to = "recurso", values_to = "valor") %>%
  group_by(maquina, recurso) %>%
  summarise(
    Shapiro_Wilk = list(shapiro.test(valor)),
    .groups = 'drop'
  ) %>%
  rowwise() %>%
  mutate(
    Estatistica_W = Shapiro_Wilk$statistic,
    Valor_p = Shapiro_Wilk$p.value,
    Normalidade = ifelse(Valor_p > 0.05, "Normal", "Não-normal")
  ) %>%
  select(-Shapiro_Wilk)

print("\nResultados dos Testes de Normalidade (Shapiro-Wilk):")
print(testes_normalidade)

# 10. Exportação dos Resultados -----------------------------------------------
# Criar diretório de resultados se não existir
if (!dir.exists("resultados")) {
  dir.create("resultados")
}

# Salvar estatísticas descritivas
write.csv(estatisticas, "resultados/estatisticas_descritivas.csv", row.names = FALSE)

# Salvar picos de utilização
write.csv(picos, "resultados/picos_utilizacao.csv", row.names = FALSE)

# Salvar testes de normalidade
write.csv(testes_normalidade, "resultados/testes_normalidade.csv", row.names = FALSE)

# Salvar gráficos principais
ggsave("resultados/histogramas_recursos.png", 
       grid.arrange(hist_cpu, hist_ram, hist_disco, ncol = 1),
       width = 8, height = 10)

ggsave("resultados/series_temporais.png", 
       grid.arrange(ts_cpu, ts_ram, ts_disco, ncol = 1),
       width = 10, height = 8)

message("\nAnálise concluída! Resultados salvos na pasta 'resultados'.")
