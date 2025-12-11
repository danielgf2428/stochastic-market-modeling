# Paquetes
library(quantmod)
library(ggplot2)
library(reshape2)
library(ggtext)

set.seed(123)

# 1) Datos históricos BBVA
sym <- "BBVA.MC"
bbva <- getSymbols(sym, src = "yahoo", from = "2024-01-01", auto.assign = FALSE)
P <- Ad(bbva)                          
S0 <- as.numeric(last(P))              

# 2) Retornos logarítmicos diarios
r <- diff(log(P))
r <- na.omit(r)

# 3) Estimadores diarios
m_d <- mean(r)        
s_d <- sd(r)           

# 4) Estimadores anualizados
sigma_ann <- sqrt(252) * s_d
mu_ann    <- 252 * m_d + 0.5 * sigma_ann^2

# 5) Parámetros simulación
Npaths <- 10000
Hdays  <- 252
tvec   <- 0:Hdays

# 6) Simulación empírica
Z <- matrix(rnorm(Hdays * Npaths), nrow = Hdays, ncol = Npaths)
incr_emp <- m_d + s_d * Z
log_paths_emp <- rbind(rep(log(S0), Npaths),
                       log(S0) + apply(incr_emp, 2, cumsum))
paths_emp <- exp(log_paths_emp)

# 7) Estadísticos simulados por día
mean_sim <- apply(paths_emp, 1, mean)
median_sim <- apply(paths_emp, 1, median)
VaR95_sim <- apply(paths_emp, 1, function(x) quantile(x, 0.05))

# 8) Valor esperado teórico
E_St <- S0 * exp(mu_ann * (tvec/252))  

# 9) Dataframes
Mshow <- 200
df <- data.frame(day = tvec, paths_emp[, 1:Mshow])
df_m <- melt(df, id.vars = "day", variable.name = "path", value.name = "price")

stats <- data.frame(
  day = tvec,
  E_St = E_St,
  mean_sim = mean_sim,
  median_sim = median_sim,
  VaR95 = VaR95_sim
)

# 10) Resumen final en el último día
final_vals <- data.frame(
  S0 = round(S0, 2),
  mu_ann = round(mu_ann, 4),
  sigma_ann = round(sigma_ann, 4),
  E_St = round(E_St[Hdays+1], 2),
  mean_sim = round(mean_sim[Hdays+1], 2),
  median_sim = round(median_sim[Hdays+1], 2),
  VaR95 = round(VaR95_sim[Hdays+1], 2)
)

# 11) Texto con colores integrados
subtitle_text <- paste0(
  "S0 = ", final_vals$S0, " | ",
  "<span style='color:blue;'>Esperanza Teórica = ", final_vals$E_St, "</span> | ",
  "<span style='color:red;'>Media Empírica = ", final_vals$mean_sim, "</span> | ",
  "<span style='color:green;'>Mediana = ", final_vals$median_sim, "</span> | ",
  "<span style='color:purple;'>VaR95 = ", final_vals$VaR95, "</span><br>",
  "μ = ", final_vals$mu_ann, " | σ = ", final_vals$sigma_ann
)

# 12) Gráfica final
ggplot() +
  geom_line(data = df_m, aes(x = day, y = price, group = path),
            alpha = 0.15, color = "black") +
  geom_line(data = stats, aes(x = day, y = E_St),
            color = "blue", size = 1.2, linetype = "dashed") +
  geom_line(data = stats, aes(x = day, y = mean_sim),
            color = "red", size = 1) +
  geom_line(data = stats, aes(x = day, y = median_sim),
            color = "green", size = 1) +
  geom_line(data = stats, aes(x = day, y = VaR95),
            color = "purple", size = 1, linetype = "dotdash") +
  labs(
    title = "Simulación de mercado BBVA",
    subtitle = subtitle_text,
    x = "Días", y = "Prezo"
  ) +
  theme_minimal() +
  theme(
    plot.subtitle = element_markdown(size = 10, hjust = 0.5), # centrado
    legend.position = "none"
  )
