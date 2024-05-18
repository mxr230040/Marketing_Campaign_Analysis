# Marketing Campaign Analysis: Customer Response Prediction
Hello! Please find information about the Marketing Campaign Analysis I worked on!

## Overview
Our goal was to analyze and understand customer characteristics that will help us in predicting their response to a marketing campaign conducted by a Food Company. By performing this analysis, the aim was to enhance profitability in future marketing endeavors.

The outcome of interest (Y variable) is the "Response" variable, indicating whether a customer responded positively or negatively to a marketing campaign.

## Dataset Description
The dataset includes socio-economic and firmographic features of about 2200 customers who were contacted through a marketing campaign. It encompasses 39 variables and 2205 rows, including duplicate entries. It is an imbalanced dataset where the response variable is not equally distributed. Link to the dataset - (https://github.com/nailson/ifood-data-business-analyst-test/tree/master)

From this extensive dataset, we have identified and focused on the 10 most significant features. Our analysis and response model are centered around these key features.

## Descriptive Analysis
We performed basic descriptive analysis on the dataset, including handling duplicate entries, studying response rate across different age groups, income groups, etc. Key insights were drawn from the analysis to guide the predictive modeling process.

## Machine Learning Models
We created multiple machine learning models to predict the customer's response to the marketing campaigns:

### Random Forest Classifier
- **Performance:** Highest accuracy, sensitivity, and AUC
- **Effectiveness:** Best overall model for predicting customer response based on various attributes

### Logistic Regression
- **Performance:** Moderate accuracy and interpretability
- **Effectiveness:** Useful for understanding the influence of individual features

### Decision Tree Classifier
- **Performance:** Good accuracy, easy to interpret
- **Effectiveness:** Helpful for understanding the decision-making process

## K-Means Clustering
To identify the target group for the marketing campaigns, we performed k-means clustering with 4 clusters to study the characteristics. Key findings include:

- **High Income:** The cluster with the best response rate exhibited the highest average income of approximately $78,800.03.
- **High Spending on Products:** This cluster also had the highest average expenditure across multiple product categories.
- **Acceptance of Overall Campaigns:** High average acceptance rate for overall campaigns, indicating a positive response to diverse marketing initiatives.

## Conclusion
- **Predictive Modeling:** Allowed accurate forecasting of customer responsiveness to marketing campaigns.
- **Best Model:** Random Forest model performed the best overall, with an accuracy of 87%
- **Key Insights:**
  - Customers with higher income levels, strong purchasing power, high engagement with the brand, and a positive attitude towards marketing campaigns exhibited a positive response towards campaigns.
  - Targeting these customers with tailored marketing strategies and offers is likely to yield the best results in terms of response rates and profitability.
  - Understanding customer behavior and characteristics empowers the marketing team to craft more effective campaigns.
  - You can find more details on the project here - (https://github.com/mxr230040/Marketing_Campaign_Analysis)


