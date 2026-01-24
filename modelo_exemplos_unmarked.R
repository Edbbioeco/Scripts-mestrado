# -------------------------------
# Script completo: unmarked + mapas de ocupação
# -------------------------------

# Carregar pacotes
library(unmarked)
library(dplyr)
library(raster)
library(terra)

set.seed(123)

# -------------------------------
# 1. Simular dados
# -------------------------------

n_sites <- 11   # número de unidades amostrais
n_visits <- 3   # número de visitas por unidade

# Variáveis ambientais por site
env <- data.frame(
  site = 1:n_sites,
  temp = runif(n_sites, 15, 25),  # temperatura
  veg = runif(n_sites, 0, 100)    # cobertura vegetal
)

# Abundância real (lambda) dependendo das variáveis ambientais
lambda <- exp(0.1 * env$temp + 0.02 * env$veg)
N <- rpois(n_sites, lambda)  # abundância verdadeira

# Contagens observadas (y) considerando probabilidade de detecção p
p <- 0.6
y <- matrix(nrow = n_sites, ncol = n_visits)
for(i in 1:n_sites){
  y[i, ] <- rbinom(n_visits, N[i], p)
}

# Visualizar dados simulados
cbind(env, N, y)

# -------------------------------
# 2. Criar objeto unmarked e ajustar modelo N-mixture
# -------------------------------

umf <- unmarkedFramePCount(
  y = y,
  siteCovs = env[, c("temp","veg")]
)

# Modelo: abundância como função de temp e veg, detecção constante
model <- pcount(~1 ~ temp + veg, data = umf, K = max(N)*2)
summary(model)

# -------------------------------
# 3. Criar raster de probabilidade de ocorrência
# -------------------------------

# Criar raster de exemplo (10x10 células)
r <- raster(nrow=10, ncol=10, xmn=0, xmx=10, ymn=0, ymx=10)
values(r) <- runif(ncell(r), 15, 25)  # exemplo: temperatura

# Para simplificação, veg = 50 em todo raster
temp_raster <- r
veg_raster <- r
values(veg_raster) <- 50

# Predizer lambda usando coeficientes do modelo
coefs <- coef(model)
lambda_r <- exp(coefs["(Intercept)"] + coefs["temp"]*values(temp_raster) + coefs["veg"]*values(veg_raster))

# Probabilidade de ocorrência: P(ocorre pelo menos 1 indivíduo)
prob_occ <- 1 - exp(-lambda_r)

# Criar raster de probabilidade
r_prob <- temp_raster
values(r_prob) <- prob_occ

# Raster binário (ocorrência/ausência) com corte em 0.5
r_bin <- r_prob
values(r_bin) <- ifelse(values(r_prob) > 0.5, 1, 0)

# -------------------------------
# 4. Visualizar mapas
# -------------------------------

r_prob_terra <- rast(r_prob)
r_bin_terra <- rast(r_bin)

plot(r_prob_terra, main="Probabilidade de Ocorrência")
plot(r_bin_terra, main="Ocorrência Binária (0/1)")
