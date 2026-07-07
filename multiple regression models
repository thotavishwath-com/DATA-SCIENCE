#Exp:7
#Q. MULTIPLE REGRESSION MODEL 
#Apply multiple regression if data have continuous independent variables. Apply on above dataset
#R code:
  # Create dataset
  df <- data.frame(
    age = c(33, 45, 39, 27, 45, 65),
    gender = c(0, 1, 0, 1, 1, 1),
    height = c(5.5, 5.1, 6, 6.6, 6.9, 7),
    weight = c(56, 67, 49, 50, 69, 74)
  )

# View dataset and dimensions
df
dim(df)

# Create multiple regression model
model <- lm(weight ~ age + gender + height, data = df)
summary(model)

# Predict for a new data point
tst <- data.frame(age = 46, gender = 0, height = 5.6)
prdct <- predict(model, tst)
print(prdct)

tst <- data.frame(age = 40, gender = 1, height = 5.9)
prdct <- predict(model, tst)
print(prdct)

tst <- data.frame(age = 42, gender = 0, height = 5.4)
prdct <- predict(model, tst)
print(prdct)

tst <- data.frame(age = 43, gender = 01, height = 5.3)
prdct <- predict(model, tst)
print(prdct)

tst <- data.frame(age = 47, gender = 0, height = 5.7)
prdct <- predict(model, tst)
print(prdct)

# Predict for existing data
prdct_value <- predict(model, df)
prdct_value

# Plot actual vs predicted
plot(df$weight, prdct_value, col = "red", pch = 16,
     xlab = "Actual Weight", ylab = "Predicted Weight",
     main = "Actual vs Predicted Weight")
abline(a = 0, b = 1, col = "blue", lwd = 2)

# Calculate RMSE (Root Mean Squared Error)
err <- sqrt(mean((df$weight - prdct_value)^2))
err







# Plot Actual vs Predicted values
plot(df$weight, prdct_value, 
     col = "darkgreen", pch = 16,
     xlab = "Actual Weight", ylab = "Predicted Weight",
     main = "Actual vs Predicted Weight",
     xlim = range(c(df$weight, prdct_value)),
     ylim = range(c(df$weight, prdct_value)))

# Add identity line (perfect predictions line)
abline(a = 0, b = 1, col = "blue", lwd = 2, lty = 2)

# Add regression line between actual and predicted
fit_line <- lm(prdct_value ~ df$weight)
abline(fit_line, col = "red", lwd = 2)

# Add grid and legend
grid()
legend("topleft", legend = c("Predicted Points", "Ideal Line (y = x)", "Fit Line"),
       col = c("darkgreen", "blue", "red"), pch = c(16, NA, NA),
       lty = c(NA, 2, 1), lwd = c(NA, 2, 2), bty = "n")

# Calculate RMSE (Root Mean Squared Error)
err <- sqrt(mean((df$weight - prdct_value)^2))
print(paste("RMSE:", round(err, 2)))
