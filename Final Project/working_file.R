setwd("C:/repos/STAT-420/Final Project")

library(lmtest)
library(faraway)

#Calculates LOOCV RMSE for a Model
get_loocv_rmse = function(model) {
  sqrt(mean((resid(model) / (1 - hatvalues(model))) ^ 2))
}

#Calculates Adjusted R Squared Value for a Model
get_adj_r2 = function(model) {
  summary(model)$adj.r.squared
}

#Finds observations with leverage greater than 2 times the average leverage
get_high_leverage = function(model){
  hatvalues(model) > 2 * mean(hatvalues(model))
}

#Finds observations with a standardized residual greater than a magnitude of 2
get_outliers = function(model){
  abs(rstandard(model)) > 2
}

#Finds observations with a cooks.distance greater than 4 / number of observations
get_high_influcence = function(model){
  cooks.distance(model) > 4 / length(cooks.distance(model))
}

#Uses Breusch-Pagan Test to validate Constant Variance assumption
get_bp_p_value = function(model) {
  unname(bptest(model)$p.value)
}

get_sw_p_value = function(model) {
	if (length(resid(model)) > 5000) {
		p_val = shapiro.test(sample(resid(model),5000))$p.value
	} else {
		p_val = shapiro.test(resid(model))$p.value
	}
	unname(p_val)
}

diagnostic_plots = function(model, pcol = "grey", lcol = "dodgerblue") {

	par(mfrow=c(1,2))
	# A fitted versus residuals plot
	plot(fitted(model), resid(model), col = pcol, pch = 20,
		xlab = "Fitted", ylab = "Residuals", main = "Fitted vs Residuals Plot")
	abline(h = 0, col = lcol, lwd = 2)
	# A Normal Q-Q plot
	qqnorm(resid(model), main = "Normal Q-Q Plot", col = pcol)
	qqline(resid(model), col = lcol, lwd = 2)

}

get_model_diagnostics = function(model, pcol = "grey", lcol = "dodgerblue", plot_it = FALSE) {
	if (plot_it == TRUE) {
		diagnostic_plots(model, pcol, lcol)
	}
	res = c("BP Test P-Value" = get_bp_p_value(model),"SW Test P-Value" = get_sw_p_value(model)) 
	res
}

evaluate_model = function(model) {
	res = c("LOOCV_RMSE" = get_loocv_rmse(model),"Adj. R-squared" = get_adj_r2(model)) 
	res
}

nvo_data = read.table("nvo.txt", sep = "\t",header = TRUE, fileEncoding = "UTF-8")
nvo_data$Sex = as.factor(nvo_data$Sex)
nvo_data$Own.Home. = as.factor(nvo_data$Own.Home.)
nvo_data$Total.Wealth = as.factor(nvo_data$Total.Wealth)

# Feature List
# Largest.Gift
# Smallest.Gift
# Current.Gift
# Previous.Gift
# Age
# Own.Home.
# Num.Children
# Total.Wealth
# Sex
# Number.of.Gifts
# Time.Between.Gifts
# Other.Gifts
# Average.Gift

# remove unnecessary columns from data to make model selection easier later on
nvo_cur_data = subset(nvo_data, select = -c(
	Average.Gift
	,Income
	,Sqrt.Smallest.Gift
	,Sqrt.Current.Gift
	,Sqrt.Largest.Gift
	,Sqrt.Previous.Gift
	,Sqrt.Average.Gift)
	)

# we are only intrested in predicting size of gifts received, not gifts not received
nvo_cur_data = nvo_cur_data[nvo_cur_data$Current.Gift > 0, ]

# this being 0 doesn't make sense. 0 time between 2 gifts should just be 1 larger gift
nvo_cur_data = nvo_cur_data[nvo_cur_data$Time.Between.Gifts > 0, ]

# people should only be in the dataset if they have contributed before
# their smallest and largest gifts would be > 0 logically
nvo_cur_data = nvo_cur_data[nvo_cur_data$Smallest.Gift > 0, ]
nvo_cur_data = nvo_cur_data[nvo_cur_data$Largest.Gift > 0, ]


nvo_trn_idx  = sample(nrow(nvo_cur_data), size = trunc(0.80 * nrow(nvo_cur_data)))
nvo_trn_data = nvo_cur_data[nvo_trn_idx, ]
nvo_tst_data = nvo_cur_data[-nvo_trn_idx, ]

cur_mod = lm(log(Current.Gift) ~ (log(Previous.Gift) + log(Largest.Gift) + log(Smallest.Gift)) ^ 3 + (Age + Own.Home. + Num.Children + Total.Wealth + Sex + Number.of.Gifts + Time.Between.Gifts + Other.Gifts) ^ 3, data = nvo_trn_data)
evaluate_model(cur_mod)
get_model_diagnostics(cur_mod, plot_it = TRUE)

pairs(nvo_cur_data, col = "dodgerblue")

# simple models
add_cur_model = lm(Current.Gift ~ ., data = nvo_cur_data)
evaluate_model(add_cur_model)
get_model_diagnostics(add_cur_model)

vif(add_cur_model)

two_way_cur_model = lm(Current.Gift ~ . ^ 2, data = nvo_cur_data)
evaluate_model(two_way_cur_model)
get_model_diagnostics(two_way_cur_model)

three_way_cur_model = lm(Current.Gift ~ . ^ 3, data = nvo_cur_data)
evaluate_model(three_way_cur_model)
get_model_diagnostics(three_way_cur_model)

#possible transformations
hist(nvo_data$Current.Gift,
     xlab   = "Value",
     main   = "Current.Gift",
     col    = "darkorange",
     border = "dodgerblue",
     breaks = 50)	 
hist(nvo_cur_data$Smallest.Gift,
     xlab   = "Value",
     main   = "Smallest.Gift",
     col    = "darkorange",
     border = "dodgerblue",
     breaks = 50)
hist(nvo_cur_data$Previous.Gift,
     xlab   = "Value",
     main   = "Previous.Gift",
     col    = "darkorange",
     border = "dodgerblue",
     breaks = 50)
hist(nvo_cur_data$Age,
     xlab   = "Value",
     main   = "Age",
     col    = "darkorange",
     border = "dodgerblue",
     breaks = 50)
hist(nvo_cur_data$Number.of.Gifts,
     xlab   = "Value",
     main   = "Number.of.Gifts",
     col    = "darkorange",
     border = "dodgerblue",
     breaks = 50)
hist(nvo_cur_data$Time.Between.Gifts,
     xlab   = "Value",
     main   = "Time.Between.Gifts",
     col    = "darkorange",
     border = "dodgerblue",
     breaks = 50)
hist(nvo_cur_data$Other.Gifts,
     xlab   = "Value",
     main   = "Other.Gifts",
     col    = "darkorange",
     border = "dodgerblue",
     breaks = 50)

# best
cur_mod = lm(log(Current.Gift) ~ (log(Previous.Gift) + log(Largest.Gift) + log(Smallest.Gift)) ^ 3 + (Age + Own.Home. + Num.Children + Total.Wealth + Sex + Number.of.Gifts + Time.Between.Gifts + Other.Gifts) ^ 3, data = nvo_cur_data)
evaluate_model(cur_mod)
get_model_diagnostics(cur_mod, plot_it = TRUE)

# removed outliers
new_cur_data = nvo_cur_data[-get_outliers(cur_mod), ]
new_cur_mod = lm(log(Current.Gift) ~ (log(Previous.Gift) + log(Largest.Gift) + log(Smallest.Gift)) ^ 3 + (Age + Own.Home. + Num.Children + Total.Wealth + Sex + Number.of.Gifts + Time.Between.Gifts + Other.Gifts) ^ 3, data = new_cur_data)
evaluate_model(new_cur_mod)
get_model_diagnostics(new_cur_mod, plot_it = TRUE)

# removed high leverage
new_cur_data = nvo_cur_data[-get_high_leverage(cur_mod), ]
new_cur_mod = lm(log(Current.Gift) ~ (log(Previous.Gift) + log(Largest.Gift) + log(Smallest.Gift)) ^ 3 + (Age + Own.Home. + Num.Children + Total.Wealth + Sex + Number.of.Gifts + Time.Between.Gifts + Other.Gifts) ^ 3, data = new_cur_data)
evaluate_model(new_cur_mod)
get_model_diagnostics(new_cur_mod, plot_it = TRUE)

# removed high influence
new_cur_data = nvo_cur_data[-get_high_influcence(cur_mod), ]
new_cur_mod = lm(log(Current.Gift) ~ (log(Previous.Gift) + log(Largest.Gift) + log(Smallest.Gift)) ^ 3 + (Age + Own.Home. + Num.Children + Total.Wealth + Sex + Number.of.Gifts + Time.Between.Gifts + Other.Gifts) ^ 3, data = new_cur_data)
evaluate_model(new_cur_mod)
get_model_diagnostics(new_cur_mod, plot_it = TRUE)

cur_mod_back_aic = step(smaller_cur_mod, direction = "backward", trace = 0)


