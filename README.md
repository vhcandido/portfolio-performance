# Performance analysis of trading strategies

The purpose of this work is to study the performance of different trading strategies that are based on Technical Analysis.

The aproaches done so far are the following
* 2 parameters
    * SMA crossover
    * EMA crossover

Experiments are run by doing an explicit enumeration of all possible combinations and computing the strategy return and the account balance evolution through time.
Then some performance metrics are taken from the strategy return series, as follows
* final balance
* mean return
* mean profit (positive returns)
* mean loss (negative returns)
* standart deviation (SD) of the returns
* SD of the losses (assuming x_mean = 0)
* Adjusted profit over risk
    * Sharpe ratio (with SD, VaR, ES as risk measures)
    * Sortino ratio
    * Upside potential ratio
* Risk measures
    * Value at Risk (VaR)
    * Conditional Value at Risk (CVaR)
    * Largest drawdown

For each strategy `S` it's possible to obtain a surface/heatmap for a given time `t`, so that each `z` value is the balance of `S` in `t` when executed with parameters `X = {x_1, ..., x_n}`, with `n` being the dimension of the problem.

This is a relevant information for 2D problems because the surface of this problem can be visualized and well known. However for problems bigger than 3D the visualization won't be so straightforward.


# Languages and libraries
This work is mostly done in R and the following libraries are needed.
* xts
* TTR
* quantmod
* PerformanceAnalysis
* reshape2
* plot3D
* ggplot2
* parallel
