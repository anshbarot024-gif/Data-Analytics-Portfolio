data <- read.csv("manufacturing_defects_40.csv")
data$DefectRate <- data$DefectiveUnits / data$TotalUnits
pop_mean <- 0.05  
sample_mean <- mean(data$DefectRate)
sample_sd <- sd(data$DefectRate)
n <- nrow(data)
se <- sample_sd / sqrt(n)
z_stat <- (sample_mean - pop_mean) / se
p_value <- 2 * (1 - pnorm(abs(z_stat)))
cat("Sample mean:", round(sample_mean,4), "\n")
cat("Sample SD:", round(sample_sd,4), "\n")
cat("SE:", round(se,6), "\n")
cat("Z-value:", round(z_stat,3), "\n")
cat("p-value:", round(p_value,4), "\n")






#Two Sample Z-Test

library(BSDA)
defects_data <- read.csv("manufacturing_defects_40.csv")
day_shift <- defects_data[defects_data$Shift == "Day", "DefectRate_percent"]
night_shift <- defects_data[defects_data$Shift == "Night", "DefectRate_percent"]
z_test_result <- z.test(
  x = day_shift,
  y = night_shift,
  alternative = "two.sided", 
  mu = 0, # The null hypothesis is that the difference in means is zero
  sigma.x = sd(day_shift),
  sigma.y = sd(night_shift)
)

# Print the test results
print(z_test_result)










# Data Analysis
library(ggplot2)
library(dplyr)
library(readxl)


#IQR for Defective Units
# Load the data from the CSV file
defects_data <- read.csv("manufacturing_defects_40.csv")

# Calculate the Interquartile Range (IQR) for the 'DefectiveUnits' column
iqr_defective_units <- IQR(defects_data$DefectiveUnits)

# Alternatively, find the quartiles and calculate the IQR manually
quartiles <- quantile(defects_data$DefectiveUnits, probs = c(0.25, 0.75))
calculated_iqr <- quartiles[2] - quartiles[1]

# Print the results
cat("The Interquartile Range (IQR) for Defective Units is:", iqr_defective_units, "\n")
cat("The first quartile (Q1) is:", quartiles[1], "\n")
cat("The third quartile (Q3) is:", quartiles[2], "\n")


#IQR for Temperature
# Load the data from the CSV file
defects_data <- read.csv("manufacturing_defects_40.csv")

# Calculate the Interquartile Range (IQR) for the 'Temperature_C' column
iqr_temperature <- IQR(defects_data$Temperature_C)

# Print the result
cat("The Interquartile Range (IQR) for Temperature is:", iqr_temperature, "\n")



#Box Plot for Defective Units

library(ggplot2)

defects_data <- read.csv("manufacturing_defects_40.csv")

ggplot(defects_data, aes(y = DefectiveUnits)) +
  geom_boxplot(fill = "steelblue") +
  labs(
    title = "Distribution of Defective Units",
    y = "Number of Defective Units"
  ) +
  theme_minimal()



#Box Plot for Temperature
library(ggplot2)

defects_data <- read.csv("manufacturing_defects_40.csv")

ggplot(defects_data, aes(y = Temperature_C)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Distribution of Temperatures",
    y = "Temperature (°C)"
  ) +
  theme_minimal()


#Box Plot to Compare Defect Rate by Shift

install.packages("ggplot2")
library(ggplot2)

defects_data <- read.csv("manufacturing_defects_40.csv")

ggplot(defects_data, aes(x = Shift, y = DefectRate_percent, fill = Shift)) +
  geom_boxplot() +
  labs(
    title = "Defect Rate by Shift",
    x = "Shift",
    y = "Defect Rate (%)"
  ) +
  theme_minimal()





#Line plot for Defect Rate Over Time
library(ggplot2)

defects_data <- read.csv("manufacturing_defects_40.csv")

defects_data$InspectionDate <- as.Date(defects_data$InspectionDate)

ggplot(defects_data, aes(x = InspectionDate, y = DefectRate_percent)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(
    title = "Defect Rate Over Time",
    x = "Inspection Date",
    y = "Defect Rate (%)"
  ) +
  theme_minimal()



