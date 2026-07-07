#####################
#Installing and loading the packages 
#install.packages('tidyverse')
library(tidyverse)
library(dplyr)
library(ggplot2)
library(cluster)
#install.packages('factoextra')
library(factoextra)
#install.packages('caret')
library(caret)
library(readxl)


#######################
#Data Exploration

data <- read_excel(file.choose())
summary(data)
glimpse(data)

#######################
#data cleaning
#Converting YES & NO into binary variables
data <- data %>%
  mutate(
    Subscription_Status = ifelse(`Subscription Status` == "Yes", 1, 0),
    Discount_Applied = ifelse(`Discount Applied` == "Yes", 1, 0),
    Promo_Code_Used = ifelse(`Promo Code Used` == "Yes", 1, 0)
  )

#Converting frequency of scale into numeric value 
data <- data %>%
  mutate(Frequency_Num = case_when(
    `Frequency of Purchases` == "Daily" ~ 30,
    `Frequency of Purchases` == "Weekly" ~ 4,
    `Frequency of Purchases` == "Fortnightly" ~ 2,
    `Frequency of Purchases` == "Quarterly" ~ 0.75,
    `Frequency of Purchases` == "Annually" ~ 0.08,
    TRUE ~ 1
  ))

# Standardize numeric variable 
cluster_data <- data %>%
  select(Age, 'Purchase Amount (USD)', 'Previous Purchases', 'Review Rating',
         Subscription_Status, Frequency_Num)

#cluster_data <- data %>%
  #select(-`Customer ID`)   # removes the column

# Then scale & run kmeans
cluster_data_scaled <- scale(cluster_data)

#########################
ggplot(data, aes(x = `Purchase Amount (USD)`)) + geom_histogram(bins = 20)
ggplot(data, aes(x = `Previous Purchases`)) + geom_histogram(bins = 20)
ggplot(data, aes(x = `Frequency of Purchases`, fill = `Discount Applied`)) + geom_bar()


########################
#clustering
fviz_nbclust(cluster_data_scaled, kmeans, method = "wss")  # elbow plot
fviz_nbclust(cluster_data_scaled, kmeans, method = "silhouette")  # silhouette

set.seed(123)
kmeans_result <- kmeans(cluster_data_scaled, centers = 3, nstart = 25)
data$cluster <- kmeans_result$cluster

table(data$cluster)
#converting into percentage
prop.table(table(data$cluster)) * 100

#profiling 
#data %>%
  #group_by(cluster) %>%
  #summarise(across(where(is.numeric), mean)) %>%
  #as.data.frame()

##########################
cluster_profiles <- data %>%
  group_by(cluster) %>%
  summarise(avg_purchase_amount = mean(`Purchase Amount (USD)`),
            avg_prev_purchases  = mean(`Previous Purchases`),
            avg_rating          = mean(`Review Rating`),
            sub_status_rate     = mean(Subscription_Status),
            disc_use_rate       = mean(Discount_Applied),
            promo_use_rate      = mean(Promo_Code_Used),
            avg_frequency_num   = mean(Frequency_Num),
            .groups = "drop")
print(cluster_profiles)

#“We will target Cluster 1 (price-sensitive shoppers) with a tailored discount campaign 
#and avoid giving discounts to Cluster 2 (full-price loyalists). 
#This should reduce unnecessary discount expense and increase conversion among deal-seekers.”

# Visualize cluster differences
library(reshape2)
cluster_profiles_long <- melt(cluster_profiles, id = "cluster")

ggplot(cluster_profiles_long, aes(x = variable, y = value, fill = factor(cluster))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(x = "Feature", y = "Average Value", fill = "Cluster",
       title = "Cluster Profiles (Means)")

# Per-cluster summary for simulation
cluster_summary <- data %>%
  group_by(cluster) %>%
  summarise(
    n_customers = n(),
    avg_spend   = mean(`Purchase Amount (USD)`),
    .groups = "drop"
  )
cluster_summary

target_cluster <- cluster_profiles %>%
  mutate(sensitivity_proxy = disc_use_rate + promo_use_rate) %>%
  arrange(desc(sensitivity_proxy)) %>%
  slice(1) %>%
  pull(cluster)

base_discount  <- 0.10
target_cluster <- 1
uplift_cluster <- c(`1`=0.15, `2`=0.03, `3`=0.08)  # Example: adjust if target_cluster ≠ 1
email_cost     <- 0.02

##########################
#sensitivity test on target and blanket
sim_baseline <- cluster_summary %>%
  mutate(
    uplift = uplift_cluster[as.character(cluster)],
    # Blanket: everyone
    incr_rev_blanket  = avg_spend * uplift        * n_customers,
    disc_cost_blanket = avg_spend * base_discount * n_customers,
    send_cost_blanket = email_cost * n_customers,
    net_gain_blanket  = incr_rev_blanket - disc_cost_blanket - send_cost_blanket,
    # Targeted: only target_cluster
    incr_rev_targeted  = ifelse(cluster==target_cluster, avg_spend*uplift*n_customers, 0),
    disc_cost_targeted = ifelse(cluster==target_cluster, avg_spend*base_discount*n_customers, 0),
    send_cost_targeted = ifelse(cluster==target_cluster, email_cost*n_customers, 0),
    net_gain_targeted  = incr_rev_targeted - disc_cost_targeted - send_cost_targeted
  )
totals_baseline <- sim_baseline %>%
  summarise(
    total_cost_blanket   = sum(disc_cost_blanket + send_cost_blanket),
    total_cost_targeted  = sum(disc_cost_targeted + send_cost_targeted),
    total_net_blanket    = sum(net_gain_blanket),
    total_net_targeted   = sum(net_gain_targeted),
    ROI_blanket  = total_net_blanket  / pmax(total_cost_blanket, 1e-9),
    ROI_targeted = total_net_targeted / pmax(total_cost_targeted, 1e-9)
  )
totals_baseline

# ROI Improvement Summary
ROI_summary <- totals_baseline %>%
  mutate(Improvement = (ROI_targeted - ROI_blanket) / ROI_blanket * 100) %>%
  select(ROI_blanket, ROI_targeted, Improvement) %>%
  rename(
    `Blanket ROI` = ROI_blanket,
    `Targeted ROI` = ROI_targeted,
    `% Improvement` = Improvement
  )
print(ROI_summary)

#Assessing the best discount rate for ROI and net gain
disc_grid <- seq(0.05, 0.25, by = 0.01)

##########################
# Realistic diminishing-returns uplift function
uplift_function <- function(dr) {
  1.0 * dr * exp(-10 * (dr - 0.12)^2) + 0.05
}
avg_spend      <- mean(cluster_summary$avg_spend)
n_customers    <- sum(cluster_summary$n_customers)

# Simulate ROI & Net Gain
sens <- data.frame(dr = disc_grid) %>%
  mutate(
    uplift = uplift_function(dr),
    incr_rev = avg_spend * uplift * n_customers,
    disc_cost = avg_spend * dr * n_customers,
    send_cost = email_cost * n_customers,
    net_gain = incr_rev - disc_cost - send_cost,
    ROI = net_gain / (disc_cost + send_cost)
  )

##########################
# plot 1: ROI curve

ggplot(sens, aes(x = dr, y = ROI)) +
  geom_line(color = "#0073C2", size = 1.2) +
  geom_vline(xintercept = sens$dr[which.max(sens$ROI)],
             linetype = "dashed", color = "red") +
  annotate("text", 
           x = sens$dr[which.max(sens$ROI)] + 0.005, 
           y = max(sens$ROI),
           label = paste0("Peak ROI = ", round(max(sens$ROI), 2)), 
           color = "red", hjust = 0) +
  theme_minimal() +
  labs(title = "ROI vs Discount Rate (Targeted Campaign)",
       x = "Discount Rate",
       y = "ROI")

##########################
# plot 2: ROI vs net gain

ggplot(sens, aes(x = dr)) +
  geom_line(aes(y = ROI), color = "#0073C2", size = 1.2) +
  geom_line(aes(y = net_gain / max(net_gain)), 
            color = "darkgreen", linetype = "dashed", size = 1) +
  geom_vline(xintercept = sens$dr[which.max(sens$ROI)], 
             linetype = "dashed", color = "red") +
  annotate("text", 
           x = sens$dr[which.max(sens$ROI)] + 0.005, 
           y = 0.9,
           label = paste0("Peak ROI = ", round(max(sens$ROI), 2)), 
           color = "red", hjust = 0) +
  theme_minimal() +
  labs(title = "ROI vs Discount Rate with Net Gain (Normalized)",
       x = "Discount Rate",
       y = "ROI (solid blue) / Normalized Net Gain (dashed green)")

peak_ROI <- max(sens$ROI)
peak_ROI_rate <- sens$dr[which.max(sens$ROI)]
peak_NetGain <- max(sens$net_gain)
peak_NetGain_rate <- sens$dr[which.max(sens$net_gain)]

##########################
#realistic sensitivity analysis
disc_grid <- seq(0.05, 0.25, by = 0.01)
uplift_grid <- seq(0.05, 0.25, by = 0.01)

avg_spend <- mean(cluster_summary$avg_spend)
n_customers <- sum(cluster_summary$n_customers)
email_cost <- 0.02

# non-linear uplift function
uplift_response <- function(dr, u1) {
  # baseline uplift = u1 peak potential
  u1 * exp(-((dr - 0.12)^2) / 0.0025)  # peaks around 12%
}

sens_realistic <- expand_grid(dr = disc_grid, u1 = uplift_grid) %>%
  rowwise() %>%
  mutate(
    uplift_effect = uplift_response(dr, u1),
    incr_rev_targeted = avg_spend * uplift_effect * n_customers,
    disc_cost_targeted = avg_spend * dr * n_customers,
    send_cost_targeted = email_cost * n_customers,
    net_gain_targeted = incr_rev_targeted - disc_cost_targeted - send_cost_targeted,
    ROI_targeted = net_gain_targeted / (disc_cost_targeted + send_cost_targeted)
  ) %>%
  ungroup()

#plot heatap
library(ggplot2)
ggplot(sens_realistic, aes(x = dr, y = u1, fill = ROI_targeted)) +
  geom_tile() +
  scale_fill_gradient2(
    name = "ROI (Targeted)",
    midpoint = 0,
    low = "red",
    mid = "white",
    high = "blue",
    labels = scales::percent_format(accuracy = 1)
  ) +
  geom_contour(aes(z = ROI_targeted), breaks = 0, color = "black") +
  labs(
    title = "Realistic Sensitivity of ROI by Discount and Uplift",
    x = "Discount Rate",
    y = "Peak Potential Uplift (u1)"
  ) +
  theme_minimal()


#evaluation for cluster quality
#average silhouette score
sil_values <- numeric(5)
for (k in 2:6) {
km <- kmeans(cluster_data_scaled, centers = k, nstart = 25)
sil <- silhouette(km$cluster, dist(cluster_data_scaled))
sil_values[k - 1] <- mean(sil[, 3])
}
sil_values
plot(2:6, sil_values, type = "b", pch = 19,
xlab = "Number of Clusters (k)",
ylab = "Average Silhouette Score",
main = "Silhouette Scores for Different k")

#wss
kmeans_result <- kmeans(cluster_data_scaled, centers = 3, nstart = 25)
set.seed(123)
wss <- numeric(9)
for (k in 1:9) {
km <- kmeans(cluster_data_scaled, centers = k, nstart = 25, iter.max = 100)
wss[k] <- km$tot.withinss
}
wss

#ANOVA

summary(aov(Frequency_Num ~ factor(cluster), data = data))
summary(aov(Discount_Applied ~ factor(cluster), data = data))
summary(aov(Promo_Code_Used ~ factor(cluster), data = data))

#ARI
install.packages("mclust")   # if not already installed
library(mclust)

set.seed(1)

#reference clustering
ref_km <- kmeans(cluster_data_scaled, centers = 3, nstart = 25)
ref_labels <- ref_km$cluster

#repeat with different random seeds and measure similarity
n_runs <- 30
ari_scores <- numeric(n_runs)

for (i in 1:n_runs) {
  km_i <- kmeans(cluster_data_scaled, centers = 3, nstart = 25)
  ari_scores[i] <- adjustedRandIndex(ref_labels, km_i$cluster)
}

mean_ARI <- mean(ari_scores)
sd_ARI   <- sd(ari_scores)

##########################
#simulation evaluation
#t-test
ROI_summary <- totals_baseline %>%
  mutate(
    ROI_Improvement = (ROI_targeted - ROI_blanket) / ROI_blanket * 100
  ) %>%
  select(ROI_blanket, ROI_targeted, ROI_Improvement)

print(ROI_summary)


disc_grid <- seq(0.05, 0.25, by = 0.01)
roi_curve <- data.frame()

for (dr in disc_grid) {
  sim_test <- cluster_summary %>%
    mutate(
      # assume same uplift pattern as before
      incr_rev = ifelse(cluster == target_cluster, avg_spend * 0.15 * n_customers, 0),
      disc_cost = ifelse(cluster == target_cluster, avg_spend * dr * n_customers, 0),
      send_cost = ifelse(cluster == target_cluster, email_cost * n_customers, 0),
      net_gain = incr_rev - disc_cost - send_cost
    )
  
  totals <- sim_test %>%
    summarise(
      total_net = sum(net_gain),
      total_cost = sum(disc_cost + send_cost),
      ROI = total_net / total_cost
    )
  
  roi_curve <- rbind(roi_curve, data.frame(discount_rate = dr, ROI = totals$ROI))
}

# Find the peak ROI point
peak_point <- roi_curve[which.max(roi_curve$ROI), ]
peak_point

#  Monte-Carlo simulation, adding noises to test if the model is stable 
set.seed(1)
sim_runs <- 100
peak_ROIs <- numeric(sim_runs)
peak_rates <- numeric(sim_runs)

for (i in 1:sim_runs) {
  # randomize each cluster’s spend
  noise <- rnorm(nrow(cluster_summary), mean = 1, sd = 0.05)
  sim_data <- cluster_summary %>%
    mutate(avg_spend = avg_spend * noise)
  
  # recalculate ROI 
  sens_temp <- data.frame(dr = disc_grid) %>%
    rowwise() %>%
    mutate(
      uplift = uplift_function(dr),
      incr_rev = sum(sim_data$avg_spend * uplift * sim_data$n_customers),
      disc_cost = sum(sim_data$avg_spend * dr * sim_data$n_customers),
      send_cost = email_cost * sum(sim_data$n_customers),
      net_gain = incr_rev - disc_cost - send_cost,
      ROI = net_gain / (disc_cost + send_cost)
    ) %>%
    ungroup()
  
  peak_ROIs[i]  <- max(sens_temp$ROI)
  peak_rates[i] <- sens_temp$dr[which.max(sens_temp$ROI)]
}

mean_peak_ROI  <- mean(peak_ROIs)
sd_peak_ROI    <- sd(peak_ROIs)
mean_peak_rate <- mean(peak_rates)
sd_peak_rate   <- sd(peak_rates)

#results for monte carlo showing no significant variation
mean_peak_ROI
sd_peak_ROI
mean_peak_rate
sd_peak_rate


