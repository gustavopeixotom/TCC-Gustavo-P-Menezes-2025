# Implementando os pacotes e funções --------------------------------------

source("~/tcc_functions.R")

set.seed(42)

# Obtendo os dados --------------------------------------------------------

###Hiato do produto externo

##Cálculo utilizando o volume de importações externas
##Ponderação igual entre os principais parceiros do Brasil: Eurozona, EUA, China

url <- "https://www.cpb.nl/sites/default/files/omnidownload/CPB-World-Trade-Monitor-December-2024.xlsx"
tmp <- tempfile()
download.file(url,tmp, mode="wb")

hiato_externo <- suppressMessages(readxl::read_excel(tmp, sheet=1, skip=3)) %>%
  dplyr::select(-2,-3, -4) %>%
  dplyr::slice(8,9,15) %>% 
  dplyr::rename("id"=1) %>% 
  tidyr::pivot_longer(-id, names_to="date", values_to="value") %>% 
  dplyr::mutate(date = gsub("m", "", date),
                date = ym(date)) %>% 
  tidyr::pivot_wider(names_from="id", values_from="value") %>%
  dplyr::mutate_if(is.numeric,~.x/lag(.x)-1) %>% 
  dplyr::mutate(value = (`Euro Area` + `China` + `United States`)/3) %>%
  na.omit() %>% 
  dplyr::select(date, value) %>% 
  dplyr::mutate(value = cumprod(1+value),
                date = floor_date(date, unit="quarters")) %>% 
  dplyr::group_by(date) %>% 
  dplyr::summarize(value = mean(value)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(date) %>% 
  hp_filter(., freq="q")

###Hiato do produto doméstico

##Média simples entre o hiato do IFI e do BCB após 2003, quando a série do BCB inicia; antes, somente hiato do IFI

url <- "https://www.bcb.gov.br/content/ri/relatorioinflacao/202412/ri202412anp.xlsx"
tmp <- tempfile()
download.file(url,tmp, mode="wb")

hiato_bcb <- readxl::read_excel(tmp, sheet="Graf 2.2.8", skip=8) %>% 
  dplyr::select(date=1, value=6) %>% 
  dplyr::slice(-1) %>% 
  dplyr::mutate(date = as.Date(date)-months(2),
                value = value/100)

url <- "https://www12.senado.leg.br/ifi/dados/arquivos/estimativas-do-hiato-do-produto-ifi/@@download/file/estimativas-do-hiato-do-produto-ifi.xlsx"
tmp <- tempfile()
download.file(url,tmp, mode="wb")
hiato_ifi <- readxl::read_excel(tmp, sheet=2, skip=1) %>% 
  dplyr::select(date=1, value=3) %>% 
  dplyr::mutate(date = date-months(2)) %>% 
  dplyr::mutate(date = as.Date(date))

hiato_domestico <- purrr::reduce(list(hiato_bcb, hiato_ifi), merge, by="date", all=T) %>% 
  purrr::set_names("date","bcb","ifi") %>% 
  dplyr::mutate(value = ifelse(date < ymd("2003-10-01"), ifi, (bcb+ifi)/2)) %>% 
  dplyr::select(date, value)

###Taxa anual de inflação

##Medida conforme o IPCA "cheio"

inflacao <- rbcb::get_series(433, start_date="1900-01-01", end_date=Sys.Date()) %>% 
  purrr::set_names("date","value") %>%
  dplyr::filter(date >= ymd("1995-01-01")) %>% 
  dplyr::mutate(value = cumprod(1+value/100)) %>% 
  dplyr::mutate(date = floor_date(date, unit="quarters")) %>% 
  dplyr::group_by(date) %>% 
  dplyr::summarize(value = last(value)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(value = value/lag(value,4)-1) %>%
  dplyr::filter(date >= ymd("2001-01-01"))

###Meta de inflação

##Medida conforme definida pelo CMN; ajuste feito na série para refletir as mudanças feitas na Carta Aberta de 21/1/2003

meta_inflacao <- rbcb::get_series(13521, start_date="1900-01-01", end_date=Sys.Date()) %>% 
  purrr::set_names("date","value") %>% 
  dplyr::mutate(value = ifelse(date == ymd("2003-01-01"), 8.5, value),
                value = ifelse(date == ymd("2004-01-01"), 5.5, value),
                value = value/100)

###Hiato da inflação

##Calculado conforme a diferença entre o IPCA e a meta

hiato_inflacao <- purrr::reduce(list(inflacao, meta_inflacao), merge, by="date", all=T) %>% 
  purrr::set_names("date","ipca","meta") %>% 
  zoo::na.locf() %>% 
  dplyr::mutate(value = ipca - meta) %>% 
  dplyr::select(date, value)

###Juro real ex-ante

##Mensuração do juro real esperado pelo mercado para o próximo ano; entendemos que essa medida é mais acurada como proxy da política monetária

juro_exante <- ipeadatar::ipeadata("BMF12_SWAPDI36012") %>% 
  dplyr::select(date, value) %>% 
  dplyr::mutate(value = value/100) %>% 
  dplyr::mutate(date = floor_date(date, unit="quarters")) %>% 
  dplyr::group_by(date) %>% 
  dplyr::summarize(value = mean(value)) %>% 
  dplyr::ungroup()

pi_exp <- rbcb::get_twelve_months_inflation_expectations(indic="IPCA") %>% 
  dplyr::filter(smoothed=="S") %>% 
  dplyr::filter(base==0) %>% 
  dplyr::select(date,value=median) %>%
  dplyr::mutate(date = floor_date(date, unit="quarters")) %>% 
  dplyr::group_by(date) %>% 
  dplyr::summarize(value = mean(value)/100) %>% 
  dplyr::ungroup()

juroreal_exante <- purrr::reduce(list(juro_exante, pi_exp), merge, by="date") %>% 
  purrr::set_names("date","juro_exante","pi_exp") %>% 
  dplyr::mutate(juroreal_exante = (1+juro_exante)/(1+pi_exp)-1) %>% 
  dplyr::select(date, juroreal_exante)

###Juro real neutro

##Utilizando a medida dos modelos do BC.
#Antes do 2T2005, supomos uma taxa constante de 8% (debatível!), conforme evidências do Focus e dos filtros AF/BF
#Após o 2T2024, supomos continuidade da taxa mais recente

url <- "https://www.bcb.gov.br/content/ri/relatorioinflacao/202406/ri202406anp.xlsx"
tmp <- tempfile()
download.file(url,tmp, mode="wb")

rstar <- readxl::read_excel(tmp, sheet="C2 Boxe2 Graf 1", skip=8) %>% 
  dplyr::select(date=1, value=6) %>% 
  dplyr::slice(-1) %>% 
  dplyr::mutate(year = as.numeric(substr(date,1,4)),
                month = as.numeric(substr(date,6,6))*3-2,
                month = ifelse(month<10, paste0("0",month), month),
                date = ym(paste0(year,month)),
                value = value/100) %>% 
  dplyr::select(date, value)

###Hiato da política monetária

##Diferença entre o juro real ex-ante e o juro real neutro

hiato_juros <- purrr::reduce(list(juroreal_exante, rstar), merge, by="date", all=T) %>% 
  purrr::set_names("date","exp_r","rstar") %>% 
  dplyr::mutate(rstar = ifelse(date > ymd("2024-04-01"), 0.0436, rstar),
                rstar = ifelse(date < ymd("2005-04-01"), 0.08, rstar)) %>% 
  dplyr::mutate(value = (1+exp_r)/(1+rstar)-1) %>% 
  dplyr::select(date, value)

data <- purrr::reduce(list(hiato_externo, hiato_domestico, hiato_inflacao, hiato_juros), merge, by="date", all=T) %>% 
  purrr::set_names("date","ystar_tilde","y_tilde","pi_tilde","r_tilde") %>% 
  dplyr::filter(between(date, ymd("2001-10-01"), ymd("2024-10-01"))) %>% 
  dplyr::mutate(ystar_tilde = log(1+ystar_tilde),
                y_tilde = log(1+y_tilde),
                pi_tilde = log(1+pi_tilde),
                r_tilde = log(1+r_tilde))

# saveRDS(data, file="tcc_gustavo_dataset.RDS")
# data <- readRDS("~/tcc_gustavo_dataset.RDS")

data %>% 
  tidyr::pivot_longer(-date, names_to="id", values_to="value") %>% 
  default_line_plot(source="BCB, CPB, IFI, Autoria própria",
                    title="Hiatos das variáveis utilizadas",
                    subtitle="Expressos em log") +
  scale_x_date(labels = function(x) paste0(quarter(x), "T", year(x)),
               breaks=scales::date_breaks("2 years"))

start = first(data$date)
end = last(data$date)

data.ts <- data %>%
  dplyr::select(-date) %>% 
  ts(start = c(year(start), quarter(start)),
     end = c(year(end), quarter(end)),
     frequency = 4)

# Testes de heterocedasticidade -------------------------------------------

#Breusch-Pagan
#H0: homocedástico
#H1: heterocedástico

lmtest::bptest(ystar_tilde ~ date, data=data)
# p = 0.7443

lmtest::bptest(y_tilde ~ date, data=data)
# p = 0.1505

lmtest::bptest(pi_tilde ~ date, data=data)
# p = 0.9463

lmtest::bptest(r_tilde ~ date, data=data)
# p = 0.5421

plot(data.ts)

#Sem evidências de heterocedasticidade nos testes

# Superfície dos critérios de verossimilhança -----------------------------

par(mfrow=c(2,2))

vars::VARselect(data.ts, lag.max=12)$criteria[1,] %>% plot(type="l", main="AIC", xlab="Lags (trimestres)", ylab="Log-verossimilhança")

vars::VARselect(data.ts, lag.max=12)$criteria[2,] %>% plot(type="l", main="Hannan-Quinn", xlab="Lags (trimestres)", ylab="Log-verossimilhança")

vars::VARselect(data.ts, lag.max=12)$criteria[3,] %>% plot(type="l", main="BIC", xlab="Lags (trimestres)", ylab="Log-verossimilhança")

vars::VARselect(data.ts, lag.max=12)$criteria[4,] %>% plot(type="l", main="FPE", xlab="Lags (trimestres)", ylab="Log-verossimilhança")

# Testes de raiz unitária -------------------------------------------------

#Augmented Dickey Fuller
#H0: Presença de raiz unitária (não-estacionário)
#H1: Ausência de raiz unitária (estacionário)

fUnitRoots::adfTest(data$ystar_tilde, lags=5)

# p < 0.01

fUnitRoots::adfTest(data$y_tilde, lags=5)

# p = 0.02215

fUnitRoots::adfTest(data$pi_tilde, lags=5)

# p < 0.01

fUnitRoots::adfTest(data$r_tilde, lags=5)

# p = 0.04442

# VAR reduzido ------------------------------------------------------------

reduced_var <- vars::VAR(data.ts, p=5)

summary(reduced_var)

# Testes de Granger -------------------------------------------------------

#H0: A variável analisada não Granger-causa as demais variáveis do sistema
#H1: A variável analisada Granger-causa as demais variáveis do sistema

vars::causality(reduced_var, cause="ystar_tilde")$Granger

#p = 0.1831

vars::causality(reduced_var, cause="y_tilde")$Granger

#p = 0.009708

vars::causality(reduced_var, cause="pi_tilde")$Granger

#p = 0.03371

vars::causality(reduced_var, cause="r_tilde")$Granger

#p = 0.0367

# Testes de normalidade ---------------------------------------------------

#Jarque-Bera
#H0: Normalidade nos resíduos
#H1: Não-normalidade nos resíduos

vars::normality.test(reduced_var, multivariate.only=FALSE)

#ystar_tilde: p < 0.01

#y_tilde: p < 0.01

#pi_tilde: p = 0.4412

#r_tilde: p = 0.02556



# Funções de Impulso-Resposta (FIR) ---------------------------------------

par(mfrow=c(2,2))

vars::irf(reduced_var, impulse="ystar_tilde", ci=0.68) %>% plot(main = "FIR em respeito ao hiato externo: VAR reduzido", col=palette_list("indigo"))

vars::irf(reduced_var, impulse="y_tilde", ci=0.68) %>% plot(main = "FIR em respeito ao hiato doméstico: VAR reduzido", col=palette_list("indigo"))

vars::irf(reduced_var, impulse="pi_tilde", ci=0.68) %>% plot(main = "FIR em respeito ao hiato da inflação: VAR reduzido", col=palette_list("indigo"))

vars::irf(reduced_var, impulse="r_tilde", ci=0.68) %>% plot(main = "FIR em respeito ao hiato de juros: VAR reduzido", col=palette_list("indigo"))


# Decomposição da Variância do Erro de Previsão (FEVD) --------------------

vars::fevd(reduced_var, n.ahead=16) %>% plot(col=palette_list("indigo"))

# SVAR com restrições de zeros --------------------------------------------

#Matriz B: restrições nas relações contemporâneas
Bmat = matrix(c(NA, 0, 0, 0,
                NA,NA, 0, 0,
                NA,NA,NA, 0,
                NA,NA,NA,NA), 4, 4) %>% t()

zeros_svar <- vars::SVAR(reduced_var, Bmat=Bmat, estmethod="direct")

vars::irf(zeros_svar, impulse="ystar_tilde", ci=0.68) %>% plot(main = "FIR em respeito ao hiato externo: SVAR com zeros", col=palette_list("indigo"))

vars::irf(zeros_svar, impulse="y_tilde", ci=0.68) %>% plot(main = "FIR em respeito ao hiato doméstico: SVAR com zeros", col=palette_list("indigo"))

vars::irf(zeros_svar, impulse="pi_tilde", ci=0.68) %>% plot(main = "FIR em respeito ao hiato da inflação: SVAR com zeros", col=palette_list("indigo"))

vars::irf(zeros_svar, impulse="r_tilde", ci=0.68) %>% plot(main = "FIR em respeito ao hiato de juros: SVAR com zeros", col=palette_list("indigo"))

vars::fevd(zeros_svar, n.ahead=16) %>% plot(col=palette_list("indigo"))

# BVAR com restrições de zeros --------------------------------------------

B = matrix(c(T, F, F, F,
             T, T, F, F,
             T, T, T, F,
             T, T, T, T), 4, 4) %>% t()


spec <- bsvars::specify_bsvar_sv$new(data = data.ts,
                                  p = 5,
                                  B = B)

burn <- bsvars::estimate(spec, S=10000, thin=1)

zeros_bvar <- bsvars::estimate(burn, S=10000, thin=1)

summary(zeros_bvar)

par(mfrow=c(3,4), oma=c(0,0,2,0))
plot(t(zeros_bvar$posterior$hyper[,1,])[,1], type="l", ylab="[B_11,...,B_14] shrinkage", xlab="Iterações")
plot(t(zeros_bvar$posterior$hyper[,1,])[,2], type="l", ylab="[B_21,...,B_24] shrinkage", xlab="Iterações")
plot(t(zeros_bvar$posterior$hyper[,1,])[,3], type="l", ylab="[B_31,...,B_34] shrinkage", xlab="Iterações")
plot(t(zeros_bvar$posterior$hyper[,1,])[,4], type="l", ylab="[B_41,...,B_44] shrinkage", xlab="Iterações")
plot(apply(as.matrix(t(zeros_bvar$posterior$hyper[,1,])[,1]), 2, density)[[1]]$x, apply(as.matrix(t(zeros_bvar$posterior$hyper[,1,])[,1]), 2, density)[[1]]$y, type="l", ylab="Densidade posterior", xlab="[B_11,...,B_14] shrinkage")
plot(apply(as.matrix(t(zeros_bvar$posterior$hyper[,1,])[,2]), 2, density)[[1]]$x, apply(as.matrix(t(zeros_bvar$posterior$hyper[,1,])[,2]), 2, density)[[1]]$y, type="l", ylab="Densidade posterior", xlab="[B_21,...,B_24] shrinkage")
plot(apply(as.matrix(t(zeros_bvar$posterior$hyper[,1,])[,3]), 2, density)[[1]]$x, apply(as.matrix(t(zeros_bvar$posterior$hyper[,1,])[,3]), 2, density)[[1]]$y, type="l", ylab="Densidade posterior", xlab="[B_31,...,B_34] shrinkage")
plot(apply(as.matrix(t(zeros_bvar$posterior$hyper[,1,])[,4]), 2, density)[[1]]$x, apply(as.matrix(t(zeros_bvar$posterior$hyper[,1,])[,4]), 2, density)[[1]]$y, type="l", ylab="Densidade posterior", xlab="[B_41,...,B_44] shrinkage")
acf(t(zeros_bvar$posterior$hyper[,1,])[,1], main="")
acf(t(zeros_bvar$posterior$hyper[,1,])[,2], main="")
acf(t(zeros_bvar$posterior$hyper[,1,])[,3], main="")
acf(t(zeros_bvar$posterior$hyper[,1,])[,4], main="")
mtext("Diagnóstico de convergência do MCMC para a matriz B", outer=T, cex=1.5)

par(mfrow=c(3,4), oma=c(0,0,2,0))
plot(t(zeros_bvar$posterior$hyper[,2,])[,1], type="l", ylab="[A_11,...,A_14] shrinkage", xlab="Iterações")
plot(t(zeros_bvar$posterior$hyper[,2,])[,2], type="l", ylab="Densidade", xlab="Iterações")
plot(t(zeros_bvar$posterior$hyper[,2,])[,3], type="l", ylab="Densidade", xlab="Iterações")
plot(t(zeros_bvar$posterior$hyper[,2,])[,4], type="l", ylab="Densidade", xlab="Iterações")
plot(apply(as.matrix(t(zeros_bvar$posterior$hyper[,2,])[,1]), 2, density)[[1]]$x, apply(as.matrix(t(zeros_bvar$posterior$hyper[,2,])[,1]), 2, density)[[1]]$y, type="l", ylab="Densidade posterior", xlab="[A_11,...,A_14] shrinkage")
plot(apply(as.matrix(t(zeros_bvar$posterior$hyper[,2,])[,2]), 2, density)[[1]]$x, apply(as.matrix(t(zeros_bvar$posterior$hyper[,2,])[,2]), 2, density)[[1]]$y, type="l", ylab="Densidade posterior", xlab="[A_21,...,A_24] shrinkage")
plot(apply(as.matrix(t(zeros_bvar$posterior$hyper[,2,])[,3]), 2, density)[[1]]$x, apply(as.matrix(t(zeros_bvar$posterior$hyper[,2,])[,3]), 2, density)[[1]]$y, type="l", ylab="Densidade posterior", xlab="[A_31,...,A_34] shrinkage")
plot(apply(as.matrix(t(zeros_bvar$posterior$hyper[,2,])[,4]), 2, density)[[1]]$x, apply(as.matrix(t(zeros_bvar$posterior$hyper[,2,])[,4]), 2, density)[[1]]$y, type="l", ylab="Densidade posterior", xlab="[A_41,...,A_44] shrinkage")
acf(t(zeros_bvar$posterior$hyper[,2,])[,1], main="")
acf(t(zeros_bvar$posterior$hyper[,2,])[,2], main="")
acf(t(zeros_bvar$posterior$hyper[,2,])[,3], main="")
acf(t(zeros_bvar$posterior$hyper[,2,])[,4], main="")
mtext("Diagnóstico de convergência do MCMC para a matriz A", outer=T, cex=1.5)

bsvars::compute_fitted_values(zeros_bvar) %>% plot(probability=0.68, col=palette_list("indigo"), main="Qualidade do fit das variáveis: BVAR com restrições de zeros")

bsvars::compute_impulse_responses(zeros_bvar, horizon=16) %>% plot(probability=0.68, col=palette_list("indigo"), main="FIR das variáveis: BVAR com restrições de zeros")

bsvars::compute_variance_decompositions(zeros_bvar, horizon=16) %>% plot(col=palette_list("indigo"), main="Decomposição da variância das variáveis: BVAR com restrições de zeros")

bsvars::compute_historical_decompositions(zeros_bvar) %>% plot(col=palette_list("indigo"), main="Decomposição histórica das variáveis: BVAR com restrições de zeros")

# BVAR com restrições de zeros, forçando exogeneidade ---------------------

head(data.ts)

B = matrix(c(T, F, F,
             T, T, F,
             T, T, T), 3, 3) %>% t()

spec <- bsvars::specify_bsvar_sv$new(data = data.ts[,2:4],
                                     p = 5,
                                     B = B,
                                     exogenous = matrix(data.ts[,1]))

burn <- bsvars::estimate(spec, S=10000, thin=1)

zeros_exo_bvar <- bsvars::estimate(burn, S=10000, thin=1)

summary(zeros_exo_bvar)

par(mfrow=c(3,3), oma=c(0,0,2,0))
plot(t(zeros_exo_bvar$posterior$hyper[,1,])[,1], type="l", ylab="[B_11,...,B_13] shrinkage", xlab="Iterações")
plot(t(zeros_exo_bvar$posterior$hyper[,1,])[,2], type="l", ylab="[B_21,...,B_23] shrinkage", xlab="Iterações")
plot(t(zeros_exo_bvar$posterior$hyper[,1,])[,3], type="l", ylab="[B_31,...,B_33] shrinkage", xlab="Iterações")
plot(apply(as.matrix(t(zeros_exo_bvar$posterior$hyper[,1,])[,1]), 2, density)[[1]]$x, apply(as.matrix(t(zeros_exo_bvar$posterior$hyper[,1,])[,1]), 2, density)[[1]]$y, type="l", ylab="Densidade posterior", xlab="[B_11,...,B_13] shrinkage")
plot(apply(as.matrix(t(zeros_exo_bvar$posterior$hyper[,1,])[,2]), 2, density)[[1]]$x, apply(as.matrix(t(zeros_exo_bvar$posterior$hyper[,1,])[,2]), 2, density)[[1]]$y, type="l", ylab="Densidade posterior", xlab="[B_21,...,B_23] shrinkage")
plot(apply(as.matrix(t(zeros_exo_bvar$posterior$hyper[,1,])[,3]), 2, density)[[1]]$x, apply(as.matrix(t(zeros_exo_bvar$posterior$hyper[,1,])[,3]), 2, density)[[1]]$y, type="l", ylab="Densidade posterior", xlab="[B_31,...,B_33] shrinkage")
acf(t(zeros_exo_bvar$posterior$hyper[,1,])[,1], main="")
acf(t(zeros_exo_bvar$posterior$hyper[,1,])[,2], main="")
acf(t(zeros_exo_bvar$posterior$hyper[,1,])[,3], main="")
mtext("Diagnóstico de convergência do MCMC - hiperparâmetros da matriz B", outer=T, cex=1.5)

par(mfrow=c(3,3), oma=c(0,0,2,0))
plot(t(zeros_exo_bvar$posterior$hyper[,2,])[,1], type="l", ylab="[A_11,...,B_13] shrinkage", xlab="Iterações")
plot(t(zeros_exo_bvar$posterior$hyper[,2,])[,2], type="l", ylab="[A_21,...,B_23] shrinkage", xlab="Iterações")
plot(t(zeros_exo_bvar$posterior$hyper[,2,])[,3], type="l", ylab="[A_31,...,B_33] shrinkage", xlab="Iterações")
plot(apply(as.matrix(t(zeros_exo_bvar$posterior$hyper[,2,])[,1]), 2, density)[[1]]$x, apply(as.matrix(t(zeros_exo_bvar$posterior$hyper[,2,])[,1]), 2, density)[[1]]$y, type="l", ylab="Densidade posterior", xlab="[A_11,...,A_13] shrinkage")
plot(apply(as.matrix(t(zeros_exo_bvar$posterior$hyper[,2,])[,2]), 2, density)[[1]]$x, apply(as.matrix(t(zeros_exo_bvar$posterior$hyper[,2,])[,2]), 2, density)[[1]]$y, type="l", ylab="Densidade posterior", xlab="[A_21,...,A_23] shrinkage")
plot(apply(as.matrix(t(zeros_exo_bvar$posterior$hyper[,2,])[,3]), 2, density)[[1]]$x, apply(as.matrix(t(zeros_exo_bvar$posterior$hyper[,2,])[,3]), 2, density)[[1]]$y, type="l", ylab="Densidade posterior", xlab="[A_31,...,A_33] shrinkage")
acf(t(zeros_exo_bvar$posterior$hyper[,2,])[,1], main="")
acf(t(zeros_exo_bvar$posterior$hyper[,2,])[,2], main="")
acf(t(zeros_exo_bvar$posterior$hyper[,2,])[,3], main="")
mtext("Diagnóstico de convergência do MCMC para a matriz A", outer=T, cex=1.5)

bsvars::compute_impulse_responses(zeros_exo_bvar, horizon=16) %>% plot(probability=0.68, col=palette_list("indigo"), main="FIR das variáveis: BVAR exógeno com restrições de zeros")

bsvars::compute_fitted_values(zeros_exo_bvar) %>% plot(probability=0.68, col=palette_list("indigo"), main="Qualidade do fit das variáveis: BVAR exógeno com restrições de zeros")

bsvars::compute_variance_decompositions(zeros_exo_bvar, horizon=16) %>% plot(col=palette_list("indigo"), main="Decomposição da variância das variáveis: BVAR exógeno com restrições de zeros")

bsvars::compute_historical_decompositions(zeros_exo_bvar) %>% plot(col=palette_list("indigo"), main="Decomposição histórica das variáveis: BVAR exógeno com restrições de zeros")

B_11 = matrix(NA, nrow=10000, ncol=1)
B_21 = matrix(NA, nrow=10000, ncol=1)
B_31 = matrix(NA, nrow=10000, ncol=1)
B_22 = matrix(NA, nrow=10000, ncol=1)
B_32 = matrix(NA, nrow=10000, ncol=1)
B_33 = matrix(NA, nrow=10000, ncol=1)

for(ii in 1:10000){
  B_11[ii] = zeros_exo_bvar$posterior$B[,,ii][1,1]
  B_21[ii] = zeros_exo_bvar$posterior$B[,,ii][2,1]
  B_31[ii] = zeros_exo_bvar$posterior$B[,,ii][3,1]
  B_22[ii] = zeros_exo_bvar$posterior$B[,,ii][2,2]
  B_32[ii] = zeros_exo_bvar$posterior$B[,,ii][3,2]
  B_33[ii] = zeros_exo_bvar$posterior$B[,,ii][3,3]
}

par(mfrow=c(3,3), oma=c(0,0,2,0))
plot.ts(apply(B_11, 2, density)[[1]]$x,apply(B_11, 2, density)[[1]]$y, type="l", ylab="Densidade", xlab="B_11")
plot.new()
plot.new()
plot.ts(apply(B_21, 2, density)[[1]]$x,apply(B_21, 2, density)[[1]]$y, type="l", ylab="Densidade", xlab="B_21")
plot.ts(apply(B_22, 2, density)[[1]]$x,apply(B_22, 2, density)[[1]]$y, type="l", ylab="Densidade", xlab="B_22")
plot.new()
plot.ts(apply(B_31, 2, density)[[1]]$x,apply(B_31, 2, density)[[1]]$y, type="l", ylab="Densidade", xlab="B_31")
plot.ts(apply(B_32, 2, density)[[1]]$x,apply(B_32, 2, density)[[1]]$y, type="l", ylab="Densidade", xlab="B_32")
plot.ts(apply(B_33, 2, density)[[1]]$x,apply(B_33, 2, density)[[1]]$y, type="l", ylab="Densidade", xlab="B_33")
mtext("Diagnóstico das posteriors para a matriz B", outer = TRUE, cex = 1.5)

# BVAR exógeno com restrições de sinal ------------------------------------

#Incorporando dados de commodities e crédito para adicionar robustez na análise

#Commodities: IC-Br filtrado com Hodrick-Prescott

hiato_commodities <- rbcb::get_series(27574, start_date="1900-01-01", end_date=Sys.Date()) %>% 
  purrr::set_names("date","value") %>% 
  dplyr::mutate(date = floor_date(date, unit="quarter"),
                value = log(value)) %>% 
  dplyr::group_by(date) %>% 
  dplyr::summarize(value = mean(value)) %>% 
  dplyr::ungroup() %>% 
  hp_filter(., freq="q")

#Crédito: concessões encadeadas, ajustadas sazonalmente, deflacionadas e filtradas com Hodrick-Prescott

hiato_credito <- purrr::reduce(list(rbcb::get_series(21277, start_date="1900-01-01", end_date=Sys.Date()),
                                    rbcb::get_series(433, start_date="1996-01-01", end_date=Sys.Date())),
                               merge, by="date", all=T) %>% 
  purrr::set_names("date","credito","ipca") %>% 
  dplyr::mutate(credito = credito/cumprod(1+ipca/100)) %>% 
  dplyr::select(date, credito) %>% 
  create_seas_col(wch.col="credito") %>% 
  zoo::na.locf() %>% #filtro sazonal falhou em algumas, poucas, observaçoes
  dplyr::mutate(date = floor_date(date, unit="quarters")) %>% 
  dplyr::group_by(date) %>% 
  dplyr::summarize(value = mean(log(credito_seasonal))) %>% 
  dplyr::ungroup() %>% 
  hp_filter(., freq="q")

second_data <- purrr::reduce(list(hiato_externo, #Por ser restrição de sinais, o ordenamento aqui não importa
                                  hiato_commodities,
                                  hiato_credito,
                                  hiato_domestico,
                                  hiato_inflacao,
                                  hiato_juros),
                             merge, by="date", all=T) %>% 
  purrr::set_names("date",
                   "hiato_externo", #Exogeneizado
                   "hiato_commodities",
                   "hiato_credito",
                   "hiato_domestico",
                   "hiato_inflacao",
                   "hiato_juros"
                   ) %>% 
  dplyr::filter(between(date, ymd("2001-10-01"), ymd("2024-10-01")))

# saveRDS(second_data, file="tcc_gustavo_dataset2.RDS")
# second_data <- readRDS("~/tcc_gustavo_dataset2.RDS")

second_data %>% 
  tidyr::pivot_longer(-date, names_to="id", values_to="value") %>% 
  default_line_plot(source="BCB, CPB, IFI, Autoria própria",
                    title="Hiatos das variáveis utilizadas",
                    subtitle="Expressos em log") +
  scale_x_date(labels = function(x) paste0(quarter(x), "T", year(x)),
               breaks=scales::date_breaks("1 years"))

start = min(second_data$date)
end = max(second_data$date)

second_data.ts <- second_data %>% 
  dplyr::select(-date) %>% 
  ts(.,
     start = c(year(start), quarter(start)),
     end = c(year(end), quarter(end)),
     frequency = 4)

#Choque positivo de oferta doméstica na coluna 1: (+) produto (-) inflação (0) crédito <-- impacto contemporâneo
#Choque positivo de demanda doméstica na coluna 2: (+) produto (+) inflação (+) crédito <-- impacto contemporâneo

sign_irf <- matrix(c(
                      0, 1,NA,NA, #hiato de crédito
                      1, 1,NA,NA, #hiato do produto doméstico
                     -1, 1,NA,NA, #hiato da inflação
                     NA,NA,NA,NA  #hiato de juros
                     ), 4, 4) %>% t()

spec <- bsvarSIGNs::specify_bsvarSIGN$new(data = second_data.ts[,c(3:6)],
                                          exogenous = second_data.ts[,c(1:2)],
                                          p = 5,
                                          sign_irf = sign_irf)

spec$prior$estimate_hyper(
  S = 20000 + 20000,
  burn_in = 20000,
  mu = TRUE,
  delta = TRUE,
  lambda = TRUE,
  psi = TRUE
)

par(mfrow=c(3,6), oma=c(0,0,2,0))
plot(t(spec$prior$hyper)[,1], type="l", xlab="Iterações", ylab="mu")
plot(t(spec$prior$hyper)[,2], type="l", xlab="Iterações", ylab="delta")
plot(t(spec$prior$hyper)[,3], type="l", xlab="Iterações", ylab="lambda")
plot(t(spec$prior$hyper)[,4], type="l", xlab="Iterações", ylab="psi_1")
plot(t(spec$prior$hyper)[,5], type="l", xlab="Iterações", ylab="psi_2")
plot(t(spec$prior$hyper)[,6], type="l", xlab="Iterações", ylab="psi_3")
plot(apply(matrix(t(spec$prior$hyper)[,1]), 2, density)[[1]]$x, apply(matrix(t(spec$prior$hyper)[,1]), 2, density)[[1]]$y, type="l", xlab="mu", ylab="Densidade posterior")
plot(apply(matrix(t(spec$prior$hyper)[,2]), 2, density)[[1]]$x, apply(matrix(t(spec$prior$hyper)[,2]), 2, density)[[1]]$y, type="l", xlab="delta", ylab="Densidade posterior")
plot(apply(matrix(t(spec$prior$hyper)[,3]), 2, density)[[1]]$x, apply(matrix(t(spec$prior$hyper)[,3]), 2, density)[[1]]$y, type="l", xlab="lambda", ylab="Densidade posterior")
plot(apply(matrix(t(spec$prior$hyper)[,4]), 2, density)[[1]]$x, apply(matrix(t(spec$prior$hyper)[,4]), 2, density)[[1]]$y, type="l", xlab="psi_1", ylab="Densidade posterior")
plot(apply(matrix(t(spec$prior$hyper)[,5]), 2, density)[[1]]$x, apply(matrix(t(spec$prior$hyper)[,5]), 2, density)[[1]]$y, type="l", xlab="psi_2", ylab="Densidade posterior")
plot(apply(matrix(t(spec$prior$hyper)[,6]), 2, density)[[1]]$x, apply(matrix(t(spec$prior$hyper)[,6]), 2, density)[[1]]$y, type="l", xlab="psi_3", ylab="Densidade posterior")
acf(matrix(t(spec$prior$hyper)[,1]), main="")
acf(matrix(t(spec$prior$hyper)[,2]), main="")
acf(matrix(t(spec$prior$hyper)[,3]), main="")
acf(matrix(t(spec$prior$hyper)[,4]), main="")
acf(matrix(t(spec$prior$hyper)[,5]), main="")
acf(matrix(t(spec$prior$hyper)[,6]), main="")
mtext("Diagnóstico de convergência dos hiperparâmetros", outer=T, cex=1.5)

signs_exo_bvar <- bsvars::estimate(spec,S=3000)

bsvars::compute_fitted_values(signs_exo_bvar) %>% plot(probability=0.68, col=palette_list("indigo"), main="Qualidade do fit das variáveis: BVAR com restrições de sinal")

bsvars::compute_impulse_responses(signs_exo_bvar, horizon=16) %>% plot(probability=0.68, col=palette_list("indigo"), main="FIR das variáveis: BVAR com restrições de sinal")

choques <- bsvars::compute_structural_shocks(signs_exo_bvar)

choque_demanda = matrix(NA, nrow=88, ncol=3000)
choque_oferta = matrix(NA, nrow=88, ncol=3000)


for(tt in 1:88){
  for(ii in 1:3000){
    choque_demanda[,ii][tt] = choques[,,ii][2,][tt]
    choque_oferta[,ii][tt] = choques[,,ii][1,][tt]
  }
}

data.frame(
  "date" = data$date[-c(1:5)],
  "q16" = apply(choque_oferta, 1, quantile, probs=0.16),
  "q50" = apply(choque_oferta, 1, quantile, probs=0.50),
  "q84" = apply(choque_oferta, 1, quantile, probs=0.84)) %>% 
  dplyr::mutate_if(is.numeric,~(.x-mean(q50))/sd(q50)) %>% 
  ggplot(aes(x=date)) +
  geom_ribbon(aes(ymin = q16, ymax = q84), fill=palette_list("indigo")[1], alpha=0.3) +
  geom_line(aes(y = q50), color=palette_list("indigo")[1], linewidth=1) +
  theme_bw(base_size=11, base_family="JetBrains Mono") +
  labs(x=NULL,
       y=NULL,
       title="Choque estrutural de oferta - BVAR exógeno com sinais",
       subtitle="Com intervalos de 68%, normalizados para N(0,1)",
       caption="Fonte: Autoria própria") +
  theme(legend.position="bottom",
        plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(angle=45, hjust=1)) +
  scale_x_date(labels=scales::date_format("%Y"),
               breaks=scales::date_breaks("1 year"))

data.frame(
  "date" = data$date[-c(1:5)],
  "q16" = apply(choque_demanda, 1, quantile, probs=0.16),
  "q50" = apply(choque_demanda, 1, quantile, probs=0.50),
  "q84" = apply(choque_demanda, 1, quantile, probs=0.84)) %>% 
  dplyr::mutate_if(is.numeric,~(.x-mean(q50))/sd(q50)) %>% 
  ggplot(aes(x=date)) +
  geom_ribbon(aes(ymin = q16, ymax = q84), fill=palette_list("indigo")[1], alpha=0.3) +
  geom_line(aes(y = q50), color=palette_list("indigo")[1], linewidth=1) +
  theme_bw(base_size=11, base_family="JetBrains Mono") +
  labs(x=NULL,
       y=NULL,
       title="Choque estrutural de demanda - BVAR exógeno com sinais",
       subtitle="Com intervalos de 68%, normalizados para N(0,1)",
       caption="Fonte: Autoria própria") +
  theme(legend.position="bottom",
        plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(angle=45, hjust=1)) +
  scale_x_date(labels=scales::date_format("%Y"),
               breaks=scales::date_breaks("1 year"))

signs_fevd <- bsvars::compute_variance_decompositions(signs_exo_bvar, horizon=6) #Considerando o "horizonte relevante" de 6 trimestres do BC

signs_fevd.credito <- matrix(NA, nrow=3000, ncol=4)
signs_fevd.produto <- matrix(NA, nrow=3000, ncol=4)
signs_fevd.inflacao <- matrix(NA, nrow=3000, ncol=4)
signs_fevd.juros <- matrix(NA, nrow=3000, ncol=4)

for(ii in 1:3000){
  signs_fevd.credito[ii,] = signs_fevd[,,6,ii][1,]
  signs_fevd.produto[ii,] = signs_fevd[,,6,ii][2,]
  signs_fevd.inflacao[ii,] = signs_fevd[,,6,ii][3,]
  signs_fevd.juros[ii,] = signs_fevd[,,6,ii][4,]
}

fortify.zoo(signs_fevd.credito) %>% 
  dplyr::select(2,3) %>% 
  purrr::set_names("choque_oferta","choque_demanda") %>% 
  ggplot() +
  geom_violin(aes(y="Choque de oferta", x=choque_oferta), draw_quantiles = c(0.16, 0.50, 0.84), fill=palette_list("usual")[1]) +
  geom_violin(aes(y="Choque de demanda", x=choque_demanda), draw_quantiles = c(0.16, 0.50, 0.84), fill=palette_list("usual")[1]) +
  theme_bw(base_size=11, base_family="JetBrains Mono") +
  labs(x=NULL,
       y=NULL,
       title="Decomposição da variância - Crédito",
       subtitle="Densidade posterior com delineamento dos intervalos de 68%",
       caption="Fonte: Autoria própria") +
  theme(legend.position="none") +
  scale_x_continuous(labels=scales::number_format(suffix="%"),
                     breaks=scales::pretty_breaks(n=10),
                     limits=c(0,100))

fortify.zoo(signs_fevd.produto) %>% 
  dplyr::select(2,3) %>% 
  purrr::set_names("choque_oferta","choque_demanda") %>% 
  ggplot() +
  geom_violin(aes(y="Choque de oferta", x=choque_oferta), draw_quantiles = c(0.16, 0.50, 0.84), fill=palette_list("usual")[1]) +
  geom_violin(aes(y="Choque de demanda", x=choque_demanda), draw_quantiles = c(0.16, 0.50, 0.84), fill=palette_list("usual")[1]) +
  theme_bw(base_size=11, base_family="JetBrains Mono") +
  labs(x=NULL,
       y=NULL,
       title="Decomposição da variância - PIB",
       subtitle="Densidade posterior com delineamento dos intervalos de 68%",
       caption="Fonte: Autoria própria") +
  theme(legend.position="none") +
  scale_x_continuous(labels=scales::number_format(suffix="%"),
                     breaks=scales::pretty_breaks(n=10),
                     limits=c(0,100))

fortify.zoo(signs_fevd.inflacao) %>% 
  dplyr::select(2,3) %>% 
  purrr::set_names("choque_oferta","choque_demanda") %>% 
  ggplot() +
  geom_violin(aes(y="Choque de oferta", x=choque_oferta), draw_quantiles = c(0.16, 0.50, 0.84), fill=palette_list("usual")[1]) +
  geom_violin(aes(y="Choque de demanda", x=choque_demanda), draw_quantiles = c(0.16, 0.50, 0.84), fill=palette_list("usual")[1]) +
  theme_bw(base_size=11, base_family="JetBrains Mono") +
  labs(x=NULL,
       y=NULL,
       title="Decomposição da variância - Inflação",
       subtitle="Densidade posterior com delineamento dos intervalos de 68%",
       caption="Fonte: Autoria própria") +
  theme(legend.position="none") +
  scale_x_continuous(labels=scales::number_format(suffix="%"),
                     breaks=scales::pretty_breaks(n=10),
                     limits=c(0,100))

fortify.zoo(signs_fevd.juros) %>% 
  dplyr::select(2,3) %>% 
  purrr::set_names("choque_oferta","choque_demanda") %>% 
  ggplot() +
  geom_violin(aes(y="Choque de oferta", x=choque_oferta), draw_quantiles = c(0.16, 0.50, 0.84), fill=palette_list("usual")[1]) +
  geom_violin(aes(y="Choque de demanda", x=choque_demanda), draw_quantiles = c(0.16, 0.50, 0.84), fill=palette_list("usual")[1]) +
  theme_bw(base_size=11, base_family="JetBrains Mono") +
  labs(x=NULL,
       y=NULL,
       title="Decomposição da variância - Juros",
       subtitle="Densidade posterior com delineamento dos intervalos de 68%",
       caption="Fonte: Autoria própria") +
  theme(legend.position="none") +
  scale_x_continuous(labels=scales::number_format(suffix="%"),
                     breaks=scales::pretty_breaks(n=10),
                     limits=c(0,100))
