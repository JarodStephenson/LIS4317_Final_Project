library(gapminder)
library(ggplot2)
library(reshape2)
library(corrgram)
library(dplyr)
library(ggExtra)

data <- gapminder
data

#correlation analysis
data_subset <- data[, c("gdpPercap", "lifeExp")]
corr_coeff <- cor(data_subset$gdpPercap, data_subset$lifeExp)

print(paste("The correlation coefficient between GdpPercap and lifeExp is:", corr_coeff))

corrgram(data, main = "Gapminder Correlogram", order = TRUE, lower.panel = panel.shade,
         upper.panel = panel.pie)

#scatter plot and densigram
plot <- ggplot(data, aes(x = gdpPercap, y = lifeExp, color = continent)) +
  geom_point(alpha = 0.80) +
  xlab("GDP Per Capita") +
  ylab("Life Expectancy") +
  ggtitle("Effect Of GDP Per Capita On Life Expectancy") +
  theme_classic()
ggMarginal(plot, type = "densigram", fill = "lightblue", color = "purple")


#ranking analysis based on GDP per capita and life expectancy

# Subset that includes the variables of interest (Country, GDP per capita, and life expectancy)
data_sub <- gapminder[, c("country", "gdpPercap", "lifeExp")]

# Ranking the countries based on GDP per capita
ranked_gdp <- data_sub %>%
  group_by(country) %>%
  slice(which.max(gdpPercap)) %>%
  arrange(desc(gdpPercap))

# Ranking the countries based on life expectancy
ranked_lifeExp <- data_sub %>%
  group_by(country) %>%
  slice(which.max(lifeExp)) %>%
  arrange(desc(lifeExp))

# Printing the top 10 countries ranked by GDP per capita and life expectancy
print("Top 10 countries ranked by GDP per capita:")
print(head(ranked_gdp, 10))

print("Top 10 countries ranked by life expectancy:")
print(head(ranked_lifeExp, 10))

TopGDP <- head(ranked_gdp,10)
ggplot(TopGDP, aes(x = country, y = gdpPercap)) +
  geom_col(fill = "green") +
  ggtitle("Top 10 Countries by GDP Per Capita")

TopLifeExp <- head(ranked_lifeExp, 10)
ggplot(TopLifeExp, aes(x = country, y = lifeExp, size = lifeExp, color = lifeExp)) +
  geom_point() +
  ggtitle("Top 10 Countries By Life Expectancy")

#Distribution analysis to visualize the distribution of GDP per capita and life expectancy.
gapminder_sub <- gapminder[, c("gdpPercap", "lifeExp")]

# Histogram of GDP per capita.
ggplot(gapminder_sub, aes(x = gdpPercap)) +
  geom_histogram(binwidth = 5500, color = "black", fill = "lightblue") +
  xlab("GDP Per Capita") +
  ylab("Count") +
  ggtitle("Distribution of GDP Per Capita")

# Histogram of life expectancy.
ggplot(gapminder_sub, aes(x = lifeExp)) +
  geom_histogram(binwidth = 4, color = "black", fill = "lightpink") +
  xlab("Life Expectancy") +
  ylab("Count") +
  ggtitle("Distribution of Life Expectancy")

#Scatter plot
ggplot(data, aes(x = lifeExp, y = gdpPercap, color = year, size = pop)) +
  geom_point() +
  ggtitle("Distribution of lifeExp vs gdpPercap")


#Deviation analysis.

#Calculating the global averages for life expectancy and GDP per capita.
data_avg <- data %>%
  summarize(lifeExp_avg = mean(lifeExp),
            gdpPercap_avg = mean(gdpPercap))
data_avg
#Calculating the deviation from the global average for each continent.
deviation_data <- gapminder %>%
  group_by(continent) %>%
  reframe(lifeExp_dev = lifeExp - data_avg$lifeExp_avg,
            gdpPercap_dev = gdpPercap - data_avg$gdpPercap_avg)

#Deviation bar charts.
ggplot(deviation_data, aes(x = continent, y = lifeExp_dev)) +
  geom_col(aes(fill = ifelse(lifeExp_dev > 0, "Above Average", "Below Average")), width = 0.7) +
  scale_fill_manual(values = c("Above Average" = "red", "Below Average" = "blue"), name = "Life Expectancy Deviation") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_flip() +
  ggtitle("Life Expectancy - Deviation of Global Average") +
  xlab("Continent") +
  ylab("Deviation") +
  theme_classic()

ggplot(deviation_data, aes(x = continent, y = gdpPercap_dev)) +
  geom_col(aes(fill = ifelse(gdpPercap_dev > 0, "Above Average", "Below Average")), width = 0.7) +
  scale_fill_manual(values = c("Above Average" = "red", "Below Average" = "blue"), name = "GDP Per Capita Deviation") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_flip() +
  ggtitle("Deviation of Global Average - GDP Per Capita") +
  xlab("Continent") +
  ylab("Deviation") +
  theme_classic()

