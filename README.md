# Financial Risk Management
This repository deals with Financial Risk Management(VaR and ES calculation) on investing in **Nifty50 Index** (Index which invests in the market portfolio of the top 50 companies in weights of their market capitalization.) using R. {VaR = Value-at-Risk and ES = Expected Shortfall(Conditional VaR)}

**VaR and ES are metrics used in Risk Management to get an idea of how much loss we can make on investment in a certain asset or portfolio within a certain confidence level. The most commonly used confidence levels are 90%,95% and 99%. If in case, where our losses exceed the VaR , then comes the role of ES(Expected Shortfall) where we want to get an estimation of how much more we can loss on our investment after our losses had breached that certain confidence bar**.

Here, I have used log returns for further calculations because it is much more easier to work with log returns and for small values of absolute returns, log returns are almost equal to them. VaR and ES have been calculated as per Normal Distributions. 

Normal Distributions don't deal well with Leptokurtosis, so the probability of extreme events are relatively much lesser than actually observed in real markets. To tackle this problem, more general class of Standard Normal Distribution, **"Student T-Distribution"** has been used. The **rescaled T-Distribution** deals well with luptokurtosis due to its varying degrees of freedom. 

At last, the concept of **"Volatility Clustering"** has been introduced. In the financial markets, there is an observation that higher volatility days are succeeded by higher volatility days and lower volatility days are succeeded by lower volatility days. In such cases, if we calculate VaR and ES over a 10 days horizon without taking volatility into account, we may get inaccurate results. To resolve this issue, **GARCH(Generalized Auto Regressive Conditional Heteroskedasticity) Model** has been used to calculate the VaR and ES.
