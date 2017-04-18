############################################
# Simple Regression - Just Trend is modeled
############################################
srAnalysis<-function(df) {
  # Running simple regression, gives us the overall trend of revenues.
  # Todo: It would be nice to have color regions to indicate holidays and weekends, color?
  simple <-
    qplot(
      x = df$Day,
      y = df$Revenues,
      geom = "point",
      ylab = "Revenues"
    ) + geom_line() + theme(plot.title = element_text(hjust = 0.5,face = "bold"))
    simple <-
    simple + ggtitle("Revenue Trend")
  simple <-
    simple + scale_y_continuous(name = "Revenues", labels = dollar)
  simple <-
    simple + scale_x_continuous(name = "Day", breaks = round(seq(min(df$Day), max(df$Day), by = 2), 2))
  simple <-
    simple + geom_smooth(method = "lm", se = FALSE, color = "blue")
  
  # As is evident from the chart, simple regression misses the effect of
  # seasonality (holidays and weekends) on revenues, there by trendline
  # accentuates the trend. In the example of "wine", fourth week revenues
  # seem negative.
  
  # Now if we run simple linear regression , we get the regression model ddetails
  # like fitted values, residuals, Residual std error, R-squared etc.
  sr_model <- lm(formula = Revenues ~ Day, data = df)
  
  # Create a table with fitted values and residuals from the regression model created above
  df_sr <-
    data.frame(df ,
               PredictedRevenues = fitted (sr_model),
               Residual = resid(sr_model))
  df$PredictedRevenues <- fitted (sr_model)
  
  # Model equation (equation representing the trendline)
  sr_eq <-
    paste("Y = ",
          round(sr_model$coefficients[1]),
          " + ",
          round(sr_model$coefficients[2]),
          "X",
          sep = "")
  
  # Based on this equation/model, calculate predicted values
  df_psr <- predictRevenues(sr_model, df)
  
  sr_summary<-summary.lm(sr_model)
  #If residuals are highly auto autocorrelated, it means there is a potential seanoality that is missed in the model
  # Typically auto correlation of greater than .30 means it highly correlated, hence not a
  # good regression model.
  autocorr_sr <-
    cor(x = sr_model$residuals[1:nrow(df) - 1], y = sr_model$residuals[2:nrow(df)])
  
  # Create residual plot - If you notice a visible pattern not a good indication
  sr_residual_plot <-
    qplot(df$Day, sr_model$residuals, ylab = "Residuals", xlab = "Day") + geom_abline(aes(slope = 0, intercept = 0)) +
    ggtitle("Residual Plot") +
    theme(plot.title = element_text(hjust = 0.5,face = "bold"))
  # Create histogram of residuals, if it doesn't look "normal", not good.
  sr_hist <-
    qplot(sr_model$residuals, geom = "histogram", binwidth = 1000000)+
    scale_y_continuous(name = "Count") +
    scale_x_continuous(name = "Residuals") +
    ggtitle("Histogram of Resuduals") +
    theme(plot.title = element_text(hjust = 0.5,face = "bold"))
  
    srData<-list(simple, sr_hist, sr_residual_plot, df_psr, autocorr_sr, sr_summary, sr_model, df)
  srData
}
###############################################################################
# Multiplle Regression with Dummy Variables - Trend and Seasonality is modeled
###############################################################################
mrAnalysis<-function(df) {
  # Take natural logarithm of Revenues.
  df$LNRevenues <- log(df$Revenues)
  df <- setDummyVariables(df)
  # Now create multiple regression model for LNRevenues over timme series and Dummy variables
  # Dummy variables here, indicates whether a day is Friday, Saturday, Sunday or a Holiday.
  mr_model <-
    lm(formula = LNRevenues ~ Day + Friday + Saturday + Sunday + Holiday,
       data = df)
  #Use exp function reverse log, to get the dollar amounts of predicted revenues
  df$PredictedRevenues <- exp(fitted(mr_model))
  #Get summary of the model, it provides R Squared, Adj R Squared, Standard deviation etc.
  mr_summary <- summary.lm(mr_model)
  #mr_summary$r.squared #Get R Square
  #mr_summary$adj.r.squared #Get Adj R Square
  #mr_summary$sigma #Residual Std Error
  #Multiple R??
  #ANOVA - Analysis of variance
  mr_anova <- anova(mr_model)
  # Check autocorrelation of residuals. Less than 0.3 is good
  autocorr_mr <-
    cor(x = mr_model$residuals[1:nrow(df) - 1], y = mr_model$residuals[2:nrow(df)])
  # Based on this equation/model, calculate predicted values
  df_pmr <- predictRevenues(mr_model, df, multipleReg = TRUE)
  # Create residual plot - If you notice a visible pattern not a good indication
  mr_residual_plot <-
    qplot(df$Day, mr_model$residuals, ylab = "Residuals", xlab = "Day") + geom_abline(aes(slope = 0, intercept = 0)) +
    ggtitle("Residual Plot") +
    theme(plot.title = element_text(hjust = 0.5,face = "bold"))
  
  # Create histogram of residuals, if it doesn't look "normal", not good.
  mr_hist <-
    qplot(mr_model$residuals, geom = "histogram", binwidth = 0.1) +
    ggtitle("Histogram of Resuduals") +
    scale_y_continuous(name = "Count") +
    scale_x_continuous(name = "Residuals") +
    theme(plot.title = element_text(hjust = 0.5,face = "bold"))
  # Plot the Actual and Predcited Revenues
  multiple <-
    qplot(
      x = Day,
      data = df_pmr,
      ylab = "Revenues",
      binwidth = 1
    ) +
    theme(plot.title = element_text(hjust = 0.5,face = "bold")) +
    geom_point(aes(y = Revenues, colour = "Actual"), na.rm = TRUE) +
    geom_line(aes(y = Revenues, colour = "Actual"), na.rm = TRUE) +
    geom_point(aes(y = PredictedRevenues, colour = "Predicted")) +
    geom_line(aes(y = PredictedRevenues, colour = "Predicted"))
  multiple <-
    multiple + scale_y_continuous(name = "Revenues", labels = dollar)
  multiple <-
    multiple + scale_x_continuous(name = "Day", breaks = round(seq(min(df_pmr$Day), max(df_pmr$Day), by = 2), 2))
  multiple <-
    multiple + ggtitle("Actual Vs Predicted Revenues")
  mr_data<-list(multiple, mr_hist, mr_residual_plot, df_pmr, autocorr_mr, mr_anova, mr_summary,mr_model, df)
  mr_data
}
#
# Generate future predicted values based on the model
#
predictRevenues<-function(model, df, noOfDays=7, multipleReg=FALSE) {
  Day<-seq(from = nrow(df)+1, length.out = noOfDays, by = 1)
  Date<-seq(from = df$Date[nrow(df)]+1, length.out = noOfDays, by = 1)
  if (multipleReg == TRUE) {
    df_pred<-data.frame(Day, Date, Revenues=NA, Weekday=NA, LNRevenues=NA)
    df_pred = setDummyVariables(df_pred)
    df_pred$PredictedRevenues<-exp(predict(model, df_pred))
  } else {
    df_pred<-data.frame(Day, Date, Revenues=NA, Weekday=NA)
    df_pred$PredictedRevenues<-predict(model, df_pred)
  }
  df_pred$Weekday<-factor(substr(weekdays(df_pred$Date), 1, 3))
  row.names(df_pred)<-df_pred$Day
  df_pred<-rbind(df, df_pred)
}
