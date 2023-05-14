library(dplyr)
library(ggplot2)
library(rmarkdown)
library(readr)
library(tibble)
library(DataExplorer)
library(tidyr)
library(factoextra) #clustering

data = read_csv("R Lab programs/Lab programs/lung cancer dataset   ---classification.csv")
head(data)
summary(data)
glimpse(data)
colnames(data) <- c("index", "Patient_id", "Age", "Gender", "Air_Pollution", "Alcohol_use", "Dust_Allergy",
                    "Occupational_Hazards", "Genetic_Risk", "Chronic_Lung_Disease", "Balanced_Diet", "Obesity", "Smoking",
                    "Passive_Smoker", "Chest_Pain", "Coughing_of_Blood", "Fatigue", "Weight_Loss", "Shortness_of_Breath",
                    "Wheezing", "Swallowing_Difficulty", "Clubbing_of_Finger_Nails", "Frequent_Cold", "Dry_Cough", "Snoring", "Level")
names(data)
#report <- create_report(data)
data <-data %>% 
  select(-c(index, Patient_id))

pivot_data <- pivot_longer(data,cols=-(ncol(data)))
ggplot(pivot_data, aes(x = value, y = after_stat(count))) +
  geom_bar(stat = "count", fill = "blue") +
  facet_wrap(~ name, scales = "free_x") +
  labs(x = "Attribute Value", y = "Count")


# Get unique values in name column
attributes <- unique(pivot_data$name)
for (attr in attributes) {
  # Subset data for current attribute
  plot_data <- pivot_data[pivot_data$name == attr, ]
  
  # Create plot
  plot <- ggplot(plot_data, aes(x = value, y = after_stat(count))) +
    geom_bar(stat = "count", fill = "red") +
    labs(x = "Attribute Value", y = "Count") +
    ggtitle(attr)
  
  # Print plot
  print(plot)
}

unique(data$Level)
low_df <- data %>%
  filter(Level=="Low")

high_df <- data %>%
  filter(Level=="High")
low_data <- pivot_longer(low_df,cols=-ncol(low_df))
high_data <- pivot_longer(high_df,cols=-ncol(high_df))

ggplot(low_data, aes(x = value, y = after_stat(count))) +
  geom_bar(stat = "count", fill = "yellow") +
  facet_wrap(~ name, scales = "free_x") +
  labs(x = "Attribute Value", y = "Count")

ggplot(high_data, aes(x = value, y = after_stat(count))) +
  geom_bar(stat = "count", fill = "purple") +
  facet_wrap(~ name, scales = "free_x") +
  labs(x = "Attribute Value", y = "Count")

g <- ggplot(data, aes(Age,Level))
g + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Box plot", 
       x="Class of Vehicle",
       y="City Mileage")

data <- data %>% mutate(Level = recode(Level, "Low" = 1, "Medium" = 2, "High" = 3))
pca_data <- data[, -ncol(data)]
pca_data <- scale(pca_data)



cor_result1 <- cor(data, method = "pearson")
#cor_result2 <- cor(data, method = "spearman")
library(reshape2)
melted_cor <- melt(cor_result1)
ggplot(melted_cor, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "Correlation Heatmap")

library(caret)
library(factoextra)
high_corr_vars <- findCorrelation(cor_result1, cutoff = 0.8)
data_highcorr <- data[, high_corr_vars]
cluster_df <- data_highcorr[, -which(names(data_highcorr) == "Level")]
fviz_nbclust(cluster_df, kmeans, method = "wss")
fviz_nbclust(cluster_df, kmeans, method = "silhouette")

set.seed(123)
km <- kmeans(cluster_df, 2, nstart = 25)
fviz_cluster(km,data = cluster_df, geom = "point")

library(cluster)
silhouette_score <- silhouette(km$cluster, dist(cluster_df))
silhouette_avg <- mean(silhouette_score)
silhouette_avg



