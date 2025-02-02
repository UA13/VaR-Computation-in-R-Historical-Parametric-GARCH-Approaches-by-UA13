library(quantmod)
library(rugarch)
library(PerformanceAnalytics)

getSymbols("AAPL", from = "2020-01-01", to = "2023-12-31")
prices <- Ad(AAPL)
AAPL_returns <- ROC(prices, type = "discrete")
AAPL_returns <- na.omit(AAPL_returns)

alpha <- 0.95
p_tail <- 1 - alpha

VaR_hist <- quantile(AAPL_returns, probs = p_tail, na.rm = TRUE)
ES_hist <- mean(AAPL_returns[AAPL_returns < VaR_hist])

mu <- mean(AAPL_returns)
sigma <- sd(AAPL_returns)
z_05 <- qnorm(p_tail)
VaR_param_normal <- mu + z_05 * sigma

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
  mean.model = list(armaOrder = c(0, 0)),  
  distribution.model = "norm"
)

fit <- ugarchfit(spec = spec, data = AAPL_returns)
forecast <- ugarchforecast(fit, n.ahead = 1)
mu_garch <- as.numeric(forecast@forecast$seriesFor)
sigma_garch <- as.numeric(forecast@forecast$sigmaFor)
VaR_garch_95 <- mu_garch + z_05 * sigma_garch

cat("Historical VaR (95%):", VaR_hist, "\n")
cat("Historical ES (95%) :", ES_hist, "\n")
cat("Parametric VaR (95%):", VaR_param_normal, "\n")
cat("GARCH-based VaR (95%):", VaR_garch_95, "\n")

VaR(AAPL_returns, p = alpha, method = "historical")
VaR(AAPL_returns, p = alpha, method = "gaussian")
ES(AAPL_returns, p = alpha, method = "historical")
ES(AAPL_returns, p = alpha, method = "gaussian")
