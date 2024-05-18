# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)

# Read CSV file
df <- read.csv("ifood_df.csv")

# Display first few rows of the dataset
head(df, 10)

# Get a concise summary of the dataframe
str(df)
summary(df)

# Calculate mean
colMeans(df, na.rm = TRUE)

# Drop columns
# We are dropping these columns since they have a unique value throughout, and hence will not be useful in our analysis
df <- subset(df, select = -c(Z_CostContact, Z_Revenue))

# Convert columns to categorical variables
df$Kidhome <- as.factor(df$Kidhome)
df$Teenhome <- as.factor(df$Teenhome)
df$AcceptedCmp1 <- as.factor(df$AcceptedCmp1)
df$AcceptedCmp2 <- as.factor(df$AcceptedCmp2)
df$AcceptedCmp3 <- as.factor(df$AcceptedCmp3)
df$AcceptedCmp4 <- as.factor(df$AcceptedCmp4)
df$AcceptedCmp5 <- as.factor(df$AcceptedCmp5)
df$Complain <- as.factor(df$Complain)
df$marital_Divorced <- as.factor(df$marital_Divorced)
df$marital_Married <- as.factor(df$marital_Married)
df$marital_Single <- as.factor(df$marital_Single)
df$marital_Together <- as.factor(df$marital_Together)
df$marital_Widow <- as.factor(df$marital_Widow)
df$education_Basic <- as.factor(df$education_Basic)
df$education_Graduation <- as.factor(df$education_Graduation)
df$education_Master <- as.factor(df$education_Master)
df$education_PhD <- as.factor(df$education_PhD)

# Display updated information about the dataframe
str(df)

# Load required libraries
library(dplyr)      # For data manipulation
library(ggplot2)    # For data visualization
library(factoextra) # For clustering visualization
library(cluster)    # For clustering analysis

# Assuming your dataset is named "df"
# Select relevant features for clustering
cluster_data <- df %>%
  select(-Response) # Remove the response variable for clustering

# Standardize numerical variables
scaled_data <- scale(cluster_data[,c('Income','Recency','MntWines','MntFruits',
                                     'MntMeatProducts','MntFishProducts','MntSweetProducts',
                                     'MntGoldProds','NumDealsPurchases','NumWebPurchases',
                                     'NumWebVisitsMonth','Age','Customer_Days','MntTotal','MntRegularProds')])

scaled_data <- scale(cluster_data)



# Determine the optimal number of clusters using the Elbow method
wss <- numeric(10) # Initialize within-cluster sum of squares vector
for (i in 1:10) {
  kmeans_model <- kmeans(scaled_data, centers = i)
  wss[i] <- kmeans_model$tot.withinss
}
plot(1:10, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters",
     ylab = "Within-cluster Sum of Squares (WSS)",
     main = "Elbow Method for Optimal Number of Clusters")

# From the plot, determine the optimal number of clusters (e.g., based on the elbow point)

# Perform K-means clustering with the optimal number of clusters
k <- 4 # Update with the optimal number of clusters
kmeans_model <- kmeans(scaled_data, centers = k, nstart = 25) # Adjust nstart for multiple initializations


print(kmeans_model$centers)

# Add cluster labels to the original dataset
df$Cluster <- as.factor(kmeans_model$cluster)

ggplot(df, aes(x = Income, y = Age, color = Cluster)) +
  geom_point(size = 3) +
  labs(title = "Customer Segmentation by Income and Age",
       x = "Income", y = "Age") +
  scale_color_manual(values = c("red","blue","pink","green")) +  # Adjust colors if needed
  theme_minimal()

cluster_1 = df[df$Cluster==1,]
cluster_2 = df[df$Cluster==2,]
cluster_3 = df[df$Cluster==3,]
cluster_4 = df[df$Cluster==4,]

conversion_cluster_1 = 
  
  
  positive_response_rates <- numeric(k)
for (cluster in 1:k) {
  cluster_data <- df[df$Cluster == cluster, ]
  positive_responses <- sum(cluster_data$Response == 1)
  total_responses <- nrow(cluster_data)
  positive_response_rate <- (positive_responses / total_responses) * 100
  positive_response_rates[cluster] <- positive_response_rate
  cat("Cluster", cluster, ": Positive Response Rate =", positive_response_rate, "%\n")
}  


# Plotting the positive response rates for each cluster
barplot(positive_response_rates, 
        main = "Positive Response Rates by Cluster",
        xlab = "Cluster",
        ylab = "Positive Response Rate (%)",
        names.arg = 1:length(positive_response_rates),
        col = "skyblue",
        ylim = c(0, max(positive_response_rates) * 1.2),
        beside = TRUE)

text(x = 1:length(positive_response_rates), 
     y = positive_response_rates, 
     label = sprintf("%.2f%%", positive_response_rates), 
     pos = 3, 
     cex = 0.8, 
     col = "black", 
     offset = 0.5)

# Analyze cluster characteristics
cluster_summary <- df %>%
  group_by(Cluster) %>%
  summarize_if(is.numeric, list(mean = mean, sd = sd))  # Apply summary functions only to numeric variables

# Print summary statistics for each cluster
print(cluster_summary)





