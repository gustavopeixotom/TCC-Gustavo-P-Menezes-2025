

# Implementando os pacotes e funções --------------------------------------

source("tcc_package.R")

set.seed(42)

# Obtendo os dados --------------------------------------------------------

###Hiato do produto externo

##Cálculo utilizando o volume de importações externas
##Ponderação igual entre os principais parceiros do Brasil: Eurozona, EUA, China

url <- "https://www.cpb.nl/sites/default/files/omnidownload/CPB-World-Trade-Monitor-December-2024.xlsx"
tmp <- tempfile()
download.file(url,tmp, mode="wb")

demanda_externa <- suppressMessages(readxl::read_excel(tmp, sheet=1, skip=3)) %>%
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
  dplyr::arrange(date)

##Utilizando o filtro HP, com lambda conforme Ravn & Uhlig (2002)

start = first(demanda_externa$date)
end = last(demanda_externa$date)

demanda_externa <- demanda_externa %>%
  dplyr::select(-date) %>% 
  ts(start=c(year(start), quarter(start)),
     end=c(year(end), quarter(end)),
     frequency=4)

hiato_externo <- mFilter::hpfilter(demanda_externa, type="lambda", freq=1600)

hiato_externo <- tibble(date = seq.Date(from=start, to=end, by="quarter"),
                        value = hiato_externo$cycle)

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
# data <- readRDS("tcc_gustavo_dataset.RDS")

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

vars::irf(reduced_var, impulse="ystar_tilde", ci=0.68) %>% plot(main = "FIR em respeito ao hiato externo: VAR reduzido", col=palette_list("indigo"))

vars::irf(reduced_var, impulse="y_tilde", ci=0.68) %>% plot(main = "FIR em respeito ao hiato doméstico: VAR reduzido", col=palette_list("indigo"))

vars::irf(reduced_var, impulse="pi_tilde", ci=0.68) %>% plot(main = "FIR em respeito ao hiato da inflação: VAR reduzido", col=palette_list("indigo"))

vars::irf(reduced_var, impulse="r_tilde", ci=0.68) %>% plot(main = "FIR em respeito ao hiato de juros: VAR reduzido", col=palette_list("indigo"))


# Decomposição da Variância do Erro de Previsão (FEVD) --------------------

vars::fevd(reduced_var, n.ahead=16) %>% plot(col=palette_list("indigo"))

# SVAR com restrições de zeros --------------------------------------------

#Matriz A: restrições nas relações contemporâneas
Amat = matrix(c(NA, 0, 0, 0,
                NA,NA, 0, 0,
                NA,NA,NA, 0,
                NA,NA,NA,NA), 4, 4) %>% t()

zeros_svar <- vars::SVAR(reduced_var, Amat=Amat, estmethod="direct")

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

burn <- bsvars::estimate(spec, S=1000)

plot.ts(t(burn$posterior$hyper[,1,]), main="Convergência do MCMC para os hiperparâmetros do BVAR", col=palette_list("indigo"), xlab="Iterações")
plot.ts(t(burn$posterior$hyper[,2,]), main="Convergência do MCMC para os hiperparâmetros do BVAR", col=palette_list("indigo"), xlab="Iterações")

burn <- bsvars::estimate(spec, S=5000)

plot.ts(t(burn$posterior$hyper[,1,]), main="Convergência do MCMC para os hiperparâmetros do BVAR", col=palette_list("indigo"), xlab="Iterações")
plot.ts(t(burn$posterior$hyper[,2,]), main="Convergência do MCMC para os hiperparâmetros do BVAR", col=palette_list("indigo"), xlab="Iterações")

burn <- bsvars::estimate(spec, S=10000)

plot.ts(t(burn$posterior$hyper[,1,]), main="Convergência do MCMC para os hiperparâmetros do BVAR", col=palette_list("indigo"), xlab="Iterações")
plot.ts(t(burn$posterior$hyper[,2,]), main="Convergência do MCMC para os hiperparâmetros do BVAR", col=palette_list("indigo"), xlab="Iterações")

zeros_bvar <- bsvars::estimate(burn, S=10000)

plot.ts(t(zeros_bvar$posterior$hyper[,1,]), main="Convergência do MCMC para os hiperparâmetros do BVAR", col=palette_list("indigo"), xlab="Iterações")
plot.ts(t(zeros_bvar$posterior$hyper[,2,]), main="Convergência do MCMC para os hiperparâmetros do BVAR", col=palette_list("indigo"), xlab="Iterações")

summary(zeros_bvar)

bsvars::compute_fitted_values(zeros_bvar) %>% plot(probability=0.68, col=palette_list("indigo"), main="Qualidade do fit das variáveis: BVAR com restrições de zeros")

bsvars::compute_impulse_responses(zeros_bvar, horizon=16) %>% plot(probability=0.68, col=palette_list("indigo"), main="FIR das variáveis: BVAR com restrições de zeros")

bsvars::compute_variance_decompositions(zeros_bvar, horizon=16) %>% plot(col=palette_list("indigo"), main="Decomposição da variância das variáveis: BVAR com restrições de zeros")

#Weird
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

burn <- bsvars::estimate(spec, S=1000)

plot.ts(t(burn$posterior$hyper[,1,]), main="Convergência do MCMC para os hiperparâmetros do BVAR: matriz B", col=palette_list("indigo"), xlab="Iterações")
plot.ts(t(burn$posterior$hyper[,2,]), main="Convergência do MCMC para os hiperparâmetros do BVAR: matriz A", col=palette_list("indigo"), xlab="Iterações")

burn <- bsvars::estimate(spec, S=5000)

plot.ts(t(burn$posterior$hyper[,1,]), main="Convergência do MCMC para os hiperparâmetros do BVAR: matriz B", col=palette_list("indigo"), xlab="Iterações")
plot.ts(t(burn$posterior$hyper[,2,]), main="Convergência do MCMC para os hiperparâmetros do BVAR: matriz A", col=palette_list("indigo"), xlab="Iterações")

burn <- bsvars::estimate(spec, S=10000)

plot.ts(t(burn$posterior$hyper[,1,]), main="Convergência do MCMC para os hiperparâmetros do BVAR: matriz B", col=palette_list("indigo"), xlab="Iterações")
plot.ts(t(burn$posterior$hyper[,2,]), main="Convergência do MCMC para os hiperparâmetros do BVAR: matriz A", col=palette_list("indigo"), xlab="Iterações")

zeros_exo_bvar <- bsvars::estimate(burn, S=10000)

plot.ts(t(zeros_exo_bvar$posterior$hyper[,1,]), main="Convergência do MCMC para os hiperparâmetros do BVAR: matriz B", col=palette_list("indigo"), xlab="Iterações")
plot.ts(t(zeros_exo_bvar$posterior$hyper[,2,]), main="Convergência do MCMC para os hiperparâmetros do BVAR: matriz A", col=palette_list("indigo"), xlab="Iterações")

summary(zeros_exo_bvar)

bsvars::compute_fitted_values(zeros_exo_bvar) %>% plot(probability=0.68, col=palette_list("indigo"), main="Qualidade do fit das variáveis: BVAR exógeno com restrições de zeros")

bsvars::compute_impulse_responses(zeros_exo_bvar, horizon=16) %>% plot(probability=0.68, col=palette_list("indigo"), main="FIR das variáveis: BVAR exógeno com restrições de zeros")

bsvars::compute_variance_decompositions(zeros_exo_bvar, horizon=16) %>% plot(col=palette_list("indigo"), main="Decomposição da variância das variáveis: BVAR exógeno com restrições de zeros")

bsvars::compute_historical_decompositions(zeros_exo_bvar) %>% plot(col=palette_list("indigo"), main="Decomposição histórica das variáveis: BVAR exógeno com restrições de zeros")

# BVAR com restrições de sinal --------------------------------------------

sign_irf = matrix(c(NA, NA, NA, NA,
                     1, NA, NA, NA,
                    NA, NA, NA, -1,
                    NA,  1,  1, NA), 4, 4) %>% t()

spec <- bsvarSIGNs::specify_bsvarSIGN$new(data = data.ts,
                                           p = 10,
                                           sign_irf = sign_irf)

signs_bvar <- bsvars::estimate(spec, S=5000)

#convergência?

bsvars::compute_fitted_values(signs_bvar) %>% plot(probability=0.68, col=palette_list("indigo"), main="Qualidade do fit das variáveis: BVAR com restrições de sinal")

bsvars::compute_impulse_responses(signs_bvar, horizon=16) %>% plot(probability=0.68, col=palette_list("indigo"), main="FIR das variáveis: BVAR com restrições de sinal")

bsvars::compute_variance_decompositions(signs_bvar, horizon=16) %>% plot(col=palette_list("indigo"), main="Decomposição da variância das variáveis: BVAR com restrições de sinal")

# bsvars::compute_historical_decompositions(signs_bvar) %>% plot(col=palette_list("indigo"), main="Decomposição histórica das variáveis: BVAR com restrições de sinal")

  

