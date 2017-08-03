####################################
### Election Forecasting Project ###
### HU Seminar SoSe 2017         ###
####################################


### Requierements ------------------
source("packages.r")
source("functions.r")



### Data Manipulation --------------

# Import
df <- read.csv2("data/kandidatinnen_90_17_long.csv",
                 sep = ";", stringsAsFactors = FALSE)


# Generate 'election id'
election_years <- unique(df$year)
election_years_id <- seq_along(election_years)
election_years_df <- data.frame(year = election_years, election_id = election_years_id)
head(election_years_df)

df <- merge(df, election_years_df, by = c("year"), all.x = TRUE)


# Add BTW results
btw <- read.csv2("data/btw_zweit.csv",
                 sep = ";", stringsAsFactors = FALSE)
df <- merge(df, btw, by = c("year", "party"), all.x = TRUE)


# Generate Percentage of Erst- and Zweitsimme
df <- mutate(df, per_erst = erst / glt_erst, per_zweit = zweit / glt_zweit)
df %>% mutate(per_erst = round(per_erst, 3) * 100) -> df
df %>% mutate(per_zweit = round(per_zweit, 3) * 100) -> df


# Generate lag Variables
df$btw_zweit <- as.numeric(df$btw_zweit)
df <- group_by(df, wkr_nummer, party) %>% arrange(year) %>% 
  mutate(btw_l1 = lag(btw_zweit, 1), 
         btw_l2 = lag(btw_zweit, 2),
         btw_l3 = lag(btw_zweit, 3)
         )

df <- group_by(df, wkr_nummer, party) %>% arrange(year) %>% 
  mutate(erst_l1 = lag(per_erst, 1)
  )


# Save
save(df, file = "data/uniform_data_complete.RData")


# Order Data
df  %>%  select(wkr_nummer, year, election_id, party, k_idname,
                per_erst, erst_l1, k_winner,
                wkr_name, wkr_new, wkr_change, wkr_nr2017, bula,
                bula_ost,
                k_vname, k_nname, k_inc, k_platz,
                party_inc, per_zweit, btw_zweit,
                btw_l1, btw_l2, btw_l3) -> btw_data
                
btw_data <- arrange(btw_data, year, wkr_nummer, party)


#  Generate Swing Variable
btw_data <- mutate(btw_data, btw_swing = btw_zweit - btw_l1)


# Split Sample
btw_data %>% filter(between(year, 2005, 2013)) -> btw0513
btw_data %>% filter(year == 2017) -> btw2017


# save
save(btw0513, file = "data/btw0513.RData")


# Restrict sample to the two most competitive candidates
btw0513_2k <- group_by(btw0513, year, wkr_nummer) %>% arrange(desc(per_erst)) %>% mutate(rank = seq_along(per_erst))

btw0513_2k <- filter(btw0513_2k, rank <= 2)
btw0513_2k <- arrange(btw0513_2k, year, wkr_nummer, party)


# save
save(btw0513_2k, file = "data/btw0513_2k.RData")



### Uniform Swing Model ------------------

# Model
model_out <- lm(per_erst ~ erst_l1 + btw_swing - 1, data = btw0513)
summary(model_out)

# >Comment: 
# Adj. R-squared of simple uniform swing model is 0.9745. 
# How much room for improvement does this leave for our
# gtrends approach?
# <


# Evaluate Fit
model_out_fit <- augment(model_out)
model_out_fit$party <- btw0513$party[as.numeric(model_out_fit$.rownames)]
model_out_fit$year <- btw0513$year[as.numeric(model_out_fit$.rownames)]
model_out_fit$wkr <- btw0513$wkr_nummer[as.numeric(model_out_fit$.rownames)]


# MAE 
mean(abs(model_out_fit$.resid))
group_by(model_out_fit, year, wkr) %>% summarize(mae = mean(abs(.resid)))
group_by(model_out_fit, year, party) %>% summarize(mae = mean(abs(.resid)))


# Plot
plot(model_out_fit$.fitted, model_out_fit$per_erst, cex = .5, pch = 20)
text(model_out_fit$.fitted, model_out_fit$per_erst, paste0(model_out_fit$party, str_sub(as.character(model_out_fit$year), -2, -1)), pos = 3, offset = .15, cex = .6)
grid()
abline(0, 1)


# >Working Points: 

# Extend simpel uniform swing model (i.e including incumbency, pioneer status etc.)
# Run out-of-sample checks
# Build graphs
# Identify subsample of most competitive constituencies



### 2017 Forecast ------------------

# Forecast from http://zweitstimme.org , accessed 01.08.2017
forecast_2017 <- c(37.4, 37.4, 25.5, 7.9, 7.9, 8.7)
party <- c("CDU", "CSU", "SPD", "FDP", "GRU", "PDS")
year <- "2017"

forecast <- data.frame(party = party,
                       btw17_zweitstimme_org = forecast_2017,
                       year = year)

# per_erst = pastvoteshare + national-level vote swing
btw2017 <- merge(btw2017, forecast, by = c("year", "party"), all.x = TRUE)
btw2017 %>% mutate(btw_swing = btw17_zweitstimme_org - btw_l1) -> btw2017
# btw2017 %>% mutate(per_erst = erst_l1 + btw_swing) -> btw2017


model_out <- lm(per_erst ~ erst_l1 + btw_swing - 1, data = btw0513)

btw2017 <- augment(model_out, newdata = btw2017)
(predict_conf <- predict(model_out, btw2017, se.fit = TRUE, interval = "confidence"))
(predict_pred <- predict(model_out, btw2017, se.fit = TRUE, interval = "prediction"))
# Where's the difference between 'conf' and 'pred'?


# Sort Data
btw2017 <- arrange(btw2017, year, wkr_nummer, party)
btw2017 %>% 
  mutate(.fitted = round(.fitted, 1),
         .se.fit = round(.se.fit, 2)) -> btw2017

# Mark winning candidate
btw2017 <- group_by(btw2017, year, wkr_nummer) %>% arrange(desc(.fitted)) %>% mutate(rank = seq_along(.fitted))

btw2017 <- arrange(btw2017, year, wkr_nummer, party)


# Save forecast Data as btw17_forecast
save(btw2017, file = "data/btw17_forecast.RData")



### Confidence Bounds ------------------
# # manual computation of standard error used for prediction interval
# se_pred <- sqrt(predict_pred$se.fit^2+sum((model_out$residuals^2 / model_out$df.residual))) # see http://stats.stackexchange.com/questions/154247/what-are-the-formulae-used-in-r-by-predict-lm-when-interval-a-none-b-pred
# 
# conf_fit <- data.frame(fit = predict_pred$fit[,1],
#                        lwr = predict_pred$fit[,1] + qt(0.025, predict_pred$df) * predict_pred$se.fit,
#                        upr = predict_pred$fit[,1] - qt(0.025, predict_pred$df) * predict_pred$se.fit
# )
# conf_fit
# 
# pred_fit <- data.frame(fit = predict_pred$fit[,1],
#                        lwr = predict_pred$fit[,1] + qt(0.025, predict_pred$df) * se_pred,
#                        upr = predict_pred$fit[,1] - qt(0.025, predict_pred$df) * se_pred
# )
# pred_fit
# 
# # plot forecast
# pred_fit$party <-  dat_2017$party
# preds_df <- arrange(pred_fit, fit)
# preds_df$partyrank <- seq_along(preds_df$party)
# preds_df$partyname <- recode_partynames(preds_df$party)
# 
# par(mar=c(3.5,7,0,3)+.1)
# par(oma=c(0,0,0,0)+.1)
# plot(preds_df$fit, preds_df$partyrank, ylim = c(0.5, 7.5), xlim = c(0, 45), xaxt = "n", yaxt = "n", ylab = "", xlab = "", pch = 20)
# axis(1, seq(0, 45, 5), seq(0, 45, 5))
# axis(1, mean(c(0, 45)), "Forecasted vote share (%)", line = 1, tick = F)
# axis(2, preds_df$partyrank, labels = preds_df$partyname, las = 1, tick = F)
# axis(4, preds_df$partyrank, labels = paste0(format(preds_df$fit, digits = 2, trim = TRUE),  "%")
#      , line = 1.5, tick = F,las = 2, hadj = 1)
# 
# abline(v = seq(0, 45, 5), col = "darkgrey", lty = 2)
# for (i in preds_df$partyrank){
#   lines(x=c(preds_df$lwr[i],preds_df$upr[i]), y=c(i,i), lwd = 1)
# }



### Explorative / Descriptive Analysis ------------------

# # past vote share
# plot(btw0517$erst_l1, btw0517$per_erst, xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = "(a)", xlim = c(0, .8), ylim = c(0, .8))
# axis(1, seq(0, 1, 0.10), seq(0, 1, 0.10))
# axis(1, 0.5, "previous vote share (%)", line = 1, tick = F)
# axis(2, seq(0, 1, 0.10), seq(0, 1, 0.10))
# axis(2, 0.5, "vote share (%)", line = 1, tick = F)
# 
# 
# # run model, add regression line
# model_out <- lm(per_erst ~ erst_l1 - 1, data = btw0517)
# model_out_aug <- augment(model_out)
# model_out_aug$case_label <- paste(btw0517$wkr_nummer, btw0517$year, btw0517$party, sep = "_") %>% .[model_out_aug$.rownames %>% num()] 
# abline(model_out, lty = 2)
# 
# 
# # identify important outliers
# obs_id <- abs(model_out_aug$.std.resid) > 1.53
# points(model_out_aug$erst_l1[obs_id], model_out_aug$per_erst[obs_id], pch = 20)
# 
# # plot labels of outliers based on resid or cooksd 
# label_position <- ifelse(model_out_aug$.resid > 0, 3, 1)
# text(model_out_aug$erst_l1[obs_id], model_out_aug$per_erst[obs_id], label = model_out_aug$case_label[obs_id], cex = .7, pos = label_position[obs_id], offset = .47)
# grid()



# ### Out-of-Sample Checks ------------------
# 
# # prepare formula
# vars <- c("erst_l1", "swing")
# fmla <- as.formula(paste("per_erst ~ ", paste(vars, collapse= "+")))
# 
# # run out-of-sample predictions
# model_out <- list()
# model_pred <- list()
# for(i in seq_along(years)) {
#   insample <- filter(ger_df_long, year != election_years[i])
#   outsample <- filter(ger_df_long, year == election_years[i])
#   model_out[[i]] <- lm(fmla, data = insample)
#   model_pred[[i]] <- augment(model_out[[i]], newdata = outsample, type.predict = "response")
# }
# 
# # evaluate fit
# model_pred_df <- do.call(rbind, model_pred)
# mean(abs(model_pred_df$voteshare - model_pred_df$.fitted), na.rm = TRUE)
# group_by(model_pred_df, party) %>% summarize(mae = mean(abs(voteshare - .fitted), na.rm = TRUE))
# plot(model_pred_df$.fitted, model_pred_df$voteshare, cex = .5, pch = 20)
# text(model_pred_df$.fitted, model_pred_df$voteshare, paste0(model_pred_df$party, str_sub(as.character(model_pred_df$year), -2, -1)), pos = 3, offset = .15, cex = .6)
# grid()
# abline(0, 1)


# End
