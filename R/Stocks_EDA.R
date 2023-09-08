

# packages
library(tidyquant) # for importing stock data

library(tidyverse)   # for working with data
library(broom)     # for tidying output from various statistical procedures
library(knitr)       # for tables
library(kableExtra)  # for improving the appearance of tables
library(ggplot2)
library(timetk)
# Add any additional packages that you use to this code chunk

```

# Import the Data (2 points)



## 1) Import your assigned stocks

## Use the package tidyquant. You may need to install this package first.

## Replace Stock1, Stock2, Stock3 with your assigned stock names (in quotation marks), uncomment the code, and Run

stocks<-c("GE", "LOW","CI") %>%
  tq_get(get = "stock.prices", from = "2000-01-01")%>%
  select(symbol, date, adjusted)

## This is your data set for this project (rename yourDataName to something more descriptive)

## output the first 6 rows of your data frame:

head(stocks, n = 6 )
```


# The Analysis

## Plot prices over time (3 points)

Plot the **prices** of each asset over time separately. Succinctly describe in words the evolution of each asset over time.  (limit: 100 words for each time series).

```{r timeSeriesPrices}

## Don't forget to add fig.cap= "Your caption" to the code chunk header. 

## facet_wrap() may be useful
stocks %>%
  ggplot(aes(x = date, y = adjusted, color = symbol)) +
  geom_line() +
  ggtitle("Price chart for stocks")

stocks %>%
  ggplot(aes(x = date, y = adjusted)) +
  geom_line() +
  facet_wrap(~symbol, scales = "free_y") +  # facet_wrap is used to make diff frames
  theme_classic() +       # using a new theme
  labs(x = "Date", y = "Price") +
  ggtitle("Price chart stocks")

```

## Calculate returns and plot returns over time (4 points)

Calculate **the daily percentage returns of each asset** using the following formula: 
  $$
  r_t = 100*\ln\Big(\frac{P_t}{P_{t-1}}\Big) 
$$
  Where $P_t$ is the asset price at time $t$. Then plot the **returns** for each asset over time.

```{r timeSeriesReturns}

## Hint: you need to add a column to your data frame (yourDataName).

## You can use the mutate() function

## Don't forget to group_by()

## The lag() function can be used to find the price in the previous date
#tq_get(get = "stock.prices", from = "2000-01-01")%>%
tq_get(c("GE","LOW","CI"), get="stock.prices") %>%
  group_by(symbol) %>%
  mutate(close = 100*close/first(close)) %>%
  ggplot(aes(date, close, color=symbol)) +
  geom_line()

stock_daily_returns <- stocks %>%
  group_by(symbol) %>%                            # We are grouping the stocks by the stock symbol
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'returns')

## Double check your results!!
stock_monthly_returns <- stocks %>%
  group_by(symbol) %>%                             # We are grouping the stocks by symbol
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'monthly',
               col_rename = 'returns')



stock_daily_returns %>%
  ggplot(aes(x = date, y = returns)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  facet_wrap(~symbol, scales = "free_y") +
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Daily returns for stock") +
  labs(x = "Date", y = "Returns") +
  scale_color_brewer(palette = "Set2",
                     name = "",
                     guide = FALSE) +
  theme_classic()

stock_monthly_returns %>%
  ggplot(aes(x = date, y = returns)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0) +
  facet_wrap(~symbol, scales = "free_y") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(-0.5,0.75,0.05)) +
  ggtitle("Monthly returns for stock") +
  labs(x = "Date", y = "Returns") +
  scale_fill_brewer(palette = "Set1",   # We will give them different colors instead of black
                    name = "",
                    guide = FALSE) +
  theme_classic()


```



## Histogram of returns

Create *a histogram* for *each of the returns series*.

```{r histogramReturns}
stock_daily_returns %>%
  ggplot(aes(x = returns)) +
  geom_histogram(binwidth = 0.015) +
  facet_wrap(~symbol, scales = "free_y") +
  theme_classic() +
  labs(x = "Daily returns") +
  ggtitle("Daily Returns for stocks") +
  scale_x_continuous(breaks = seq(-0.5,0.6,0.05),
                     labels = scales::percent) +
  annotate(geom = 'text', x = -0.30, y= 200, label = "Extremely\nnegative\nreturns") +
  annotate(geom = 'segment', x = -0.305, xend = -0.35,  y = 120, yend = 20, color = 'red', arrow = arrow()) +
  annotate(geom = 'segment', x = 0.405, xend = 0.42,  y = 120, 
           yend = 20, color = 'blue', arrow = arrow(type = "open")) +
  annotate(geom = 'text', x = 0.430, y = 200, label = "Extremely\npositive\nreturns")



stock_monthly_returns %>%
  mutate(returns = if_else(date == "2000-01-01", 0, returns)) %>%
  group_by(symbol) %>%  # Need to group multiple stocks
  mutate(cr = cumprod(1 + returns)) %>%
  mutate(cumulative_returns = cr - 1) %>%
  ggplot(aes(x = date, y = cumulative_returns, color = symbol)) +
  geom_line() +
  labs(x = "Date", y = "Cumulative Returns") +
  ggtitle("Cumulative returns for all since 2000") +
  scale_y_continuous(breaks = seq(0,20,2),
                     labels = scales::percent) +
  scale_color_brewer(palette = "Set1",
                     name = "") +
  theme_bw()


```

## Summary table of returns
  
  ```{r summaryTable}

## Your summary table here. Be sure to format the table appropriately.
stock_daily_returns %>%
  group_by(symbol) %>%
  summarise(mean = mean(returns),
            sd = sd(returns)) 


stock_monthly_returns %>%
  mutate(year = year(date)) %>%
  group_by(symbol, year) %>%
  summarise(mean = mean(returns),
            sd = sd(returns)) %>%
  ggplot(aes(x = year, y = mean, fill = symbol)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_y_continuous(breaks = seq(-0.1,0.4,0.02),
                     labels = scales::percent) +
  scale_x_continuous(breaks = seq(2009,2018,1)) +
  labs(x = "Year", y = "Mean Returns") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_fill_brewer(palette = "Set1",
                    name = "Stocks") +
  ggtitle("Monthly Mean returns forstocks")


stock_monthly_returns %>%
  mutate(year = year(date)) %>%
  group_by(symbol, year) %>%
  summarise(mean = mean(returns),
            sd = sd(returns)) %>%
  ggplot(aes(x = year, y = sd, fill = symbol)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_y_continuous(breaks = seq(-0.1,0.4,0.02),
                     labels = scales::percent) +
  scale_x_continuous(breaks = seq(2009,2018,1)) +
  labs(x = "Year", y = "Std Dev") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_fill_brewer(palette = "Set1",
                    name = "Stocks") +
  ggtitle("Monthly Standard Deviation returns for stocks")

```

## Are average returns significantly different from zero? (5 points)

Under the assumption that the returns of each asset are drawn from an **independently and identically distributed normal distribution**, are the expected returns of each asset statistically different from zero at the 1\% level of significance? 
  
  Part 1: Provide details for **all 5 steps to conduct a hypothesis test**, including **the equation for the test statistic**. 

Part 2: Calculate and report all the relevant values for your conclusion and be sure to provide an interpretation of the results. (Hint: you will need to repeat the test for expected returns of each asset)


```{r sigTests}

## Hint: you can extract specific values from t.test objects using the $

## Eg. using  t.test(x,y)$statistic   will extract the value of the test statistic.

## Consult the help file for the other values generated by the t.test() function.

## The relevant values are: the t-test method, the estimated mean ,  the test statistic, whether the test is one or two tailed, the degrees of freedom, and the p-value. (You might wish to present this in a table)

t.test(stocks$adjusted, mu = 50)


```
## Are average returns different from each other?

Assume the returns of each asset are **independent from each other**. With this assumption, are the mean returns statistically different from each other at the 1\% level of significance? 
  

```{r multiSigTests}
library(ggpubr)
library(car)
library(rstatix)
#(install.packages("hms"))
library(hms)
## Decide on which test is appropriate for testing differences in mean returns
stocks %>% levene_test(adjusted ~ symbol)


anov <- stocks %>% anova_test(adjusted ~ symbol)
anov

## Correlations 

Calculate and present the correlation matrix of the returns. 

Discuss the direction and strength of the correlations.

```{r correlations}
# determining the Co-variance
stock_monthly_returns %>%
  spread(symbol, value = returns) %>%
  tk_xts(silent = TRUE) %>%
  cov()

## Include a formatted correlation matrix here

## Testing the significance of correlations

Is the assumption of independence of stock returns realistic? 
  
  Provide evidence (the hypothesis test including **all 5 steps of the hypothesis test** and **the equation for the test statistic**) and a rationale to support your conclusion. 


```{r correlationTest}
# determining the Co-variance
stock_monthly_returns %>%
  spread(symbol, value = returns) %>%
  tk_xts(silent = TRUE) %>%
  cov()

# determining the correlation

stock_monthly_returns %>%
  spread(symbol, value = returns) %>%
  tk_xts(silent = TRUE) %>%
  cor() 

library(corrplot)
stock_monthly_returns %>%
  spread(symbol, value = returns) %>%
  tk_xts(silent = TRUE) %>%
  cor() %>%
  corrplot()

#library(rmarkdown)
#render("Econn_stocks.R")
