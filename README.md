# FinanicalRiskManagement
This repository deals with Financial Risk Management(VaR and ES calculation) on investing in **Nifty50 Index Fund** (Funds which track the performance of an index such as Nifty50) using R. {VaR = Value-at-Risk and ES = Expected Shortfall(Conditional VaR)}

**VaR and ES are metrics used in Risk Management to get an idea of how much loss we can make on investment in a certain asset or portfolio within a certain confidence level. The most commonly used confidence levels are 90%,95% and 99%. If in case, where our losses exceed the VaR , then comes the role of ES(Expected Shortfall) where we want to get an estimation of how much more we can loss on our investment after our losses had breached that certain confidence bar**.

Initially,log returns have been calculated to convert returns data into a normal distribution usable format. VaR and ES have been calculated as per Normal Distributions. 

But, **normal distributions don't deal well with leptokurtosis**, so inaccurate results are obtained. To tackle this problem, more general class of Standard Normal Distribution, "Student T-Distribution" has been used. The **rescaled T-Distribution** deals well with luptokurtosis due to its varying degrees of freedom. 

At last, the concept of **"Volatility Clustering"** has been introduced. In the financial markets, there is an observation that higher volatility days are succeeded by higher volatility days and lower volatility days are succeeded by lower volatility days. In such cases, if we calculate VaR and ES over a 10 days horizon withour taking volatility into account, we may get inaccurate results. To resolve this issue, **GARCH(Generalized Auto Regressive Conditional Heteroskadesticity) Model** has been used at last to calculate the VaR and ES.
