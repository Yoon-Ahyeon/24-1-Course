install.packages("readxl")
library(readxl)
data <- read_excel("C:/Users/ahyeo/Downloads/kwater_data.xlsx")
install.packages("dplyr")
library(dplyr)
data <- read_excel("C:/Users/ahyeo/Downloads/kwater_data.xlsx")
colnames(data) <- c("logTime", "침전수탁도", "정수지탁도", "탁도", "알칼리도", "전기전도도", "원수유입유량", "pH", "수온", "PAC", "PACS", "응집지pH", "Co2주입량", "Co2주입기")
summary(data)
data <- data %>% mutate(across(-logTime, as.numeric))
summary(data)
df_new <- data %>% select(logTime, 탁도, 수온, 알칼리도, 전기전도도, 원수유입유량, pH, PAC, PACS)
summary(df_new)
df_new <- df_new %>%
     mutate(
         PAC = ifelse(is.na(PAC), 0, PAC),
         PACS = ifelse(is.na(PACS), 0, PACS),
         응집제주입량 = PAC + PACS)
summary(df_new)
df_new <- df_new %>% filter(응집제주입량 != 0)
colSums(is.na(df_new))
install.packages("zoo")
df_new <- df_new %>%
     mutate(across(-logTime, ~na.approx(.)))
df_new <- df_new %>% select(-PAC, -PACS)
boxplot(df_new$탁도, main = "탁도 Boxplot", ylab = "탁도")
boxplot(df_new$탁도, main = "탁도 Boxplot", ylab = "탁도")
boxplot(df_new$수온, main = "수온 Boxplot", ylab = "수온")
boxplot(df_new$전기전도도, main = "전기전도도 Boxplot", ylab = "전기전도도")
boxplot(df_new$pH, main = "pH Boxplot", ylab = "pH")
boxplot(df_new$알칼리도, main = "알칼리도도 Boxplot", ylab = "알칼리도")
df_new <- df_new %>%
     mutate(응집제주입률 = (응집제주입량 / 원수유입유량) * 1000)
df_new <- df_new %>% select(-응집제주입량, -원수유입유량)
boxplot(df_new$응집제주입률, main = "응집제주입률 Boxplot", ylab = "응집제주입률")
df_outlier <- df_new %>%
     filter(수온 < 100) %>%
     filter(pH >= 5) %>%
     filter(알칼리도 > 20) %>%
     filter(전기전도도 >= 10) %>%
     filter(응집제주입률 < 100)
df_outlier  <- df_outlier  %>% filter(is.finite(응집제주입률))
install.packages("ggcorrplot")
library(ggcorrplot)
df_corr <- df_outlier %>% select(-logTime)
correlation_matrix <- cor(df_corr)
ggcorrplot(correlation_matrix, method = "circle", type = "lower", lab = TRUE, lab_size = 3, tl.cex = 10, tl.srt = 45, title = "Correlation Matrix")
ggcorrplot(correlation_matrix, method = "square", type = "lower", lab = TRUE, lab_size = 3, tl.cex = 10, tl.srt = 45, title = "Correlation Matrix")
install.packages("scale")
library(scales)
df_kmeans <- df_outlier %>% select(탁도, 응집제주입률)
df_kmeans <- df_kmeans %>% mutate(log_탁도 = log(탁도))
data_scaled <- scale(df_kmeans[, c("log_탁도", "응집제주입률")])
set.seed(123)  # 결과 재현을 위한 시드 설정
kmeans_result <- kmeans(data_scaled, centers = 4, nstart = 25)
df_kmeans <- df_kmeans %>% mutate(cluster = kmeans_result$cluster)
df_kmeans_result <- df_outlier %>% 
     select(탁도, 응집제주입률) %>% 
     mutate(cluster = df_kmeans$cluster)
ggplot(df_kmeans_result, aes(x = 탁도, y = 응집제주입률, color = factor(cluster))) +
     geom_point() +
     xlim(0, 300) +
     labs(color = "Cluster", title = "K-means Clustering of Turbidity and Coagulant Dosage") +
     xlab("탁도") +
     ylab("응집제 주입률")
View(df_kmeans_result)
df_cluster_means <- df_kmeans_result %>%
     group_by(cluster) %>%
     summarise(
         mean_탁도 = mean(탁도, na.rm = TRUE),
         mean_응집제주입률 = mean(응집제주입률, na.rm = TRUE))
print(df_cluster_means)
df_outlier$cluster <- df_kmeans_result$cluster
View(df_outlier)
df_cluster_1_3 <- df_kmeans_result %>% filter(cluster %in% c(1, 3))
ggplot(df_cluster_1_3, aes(x = 탁도, y = 응집제주입률, color = factor(cluster))) +
     geom_point() +
     labs(color = "Cluster", title = "Scatter Plot for Clusters 1 and 3", x = "탁도", y = "응집제 주입률")
df_cluster_1_3 <- df_outlier %>% filter(cluster %in% c(1, 3))
ggplot(df_cluster_1_3, aes(x = 전기전도도, fill = factor(cluster))) +
     geom_density(alpha = 0.5) +
     labs(fill = "Cluster", title = "Density Plot of Conductivity for Clusters 1 and 3", x = "전기전도도", y = "Density") +
     theme_minimal()
ggplot(df_cluster_1_3, aes(x = 수온, fill = factor(cluster))) +
     geom_density(alpha = 0.5) +
     labs(fill = "Cluster", title = "Density Plot of Temperature for Clusters 1 and 3", x = "수온", y = "Density") +
     theme_minimal()
df_outlier <- df_outlier %>%
     mutate(month = as.numeric(substr(logTime, 6, 7)))
df_cluster_1_3 <- df_outlier %>% filter(cluster %in% c(1, 3))
df_monthly_counts <- df_cluster_1_3 %>%
     group_by(cluster, month) %>%
     summarise(count = n())
ggplot(df_monthly_counts, aes(x = month, y = count, fill = factor(cluster))) +
     geom_bar(stat = "identity", position = "dodge") +
     labs(fill = "Cluster", title = "Monthly Data Counts for Clusters 1 and 3", x = "Month", y = "Count") +
     scale_x_continuous(breaks = 1:12, labels = month.name) +
     theme_minimal()
ggplot(df_cluster_1_3, aes(x = month, color = factor(cluster), fill = factor(cluster))) +
     geom_density(alpha = 0.5) +
     labs(color = "Cluster", fill = "Cluster", title = "Monthly Data Density for Clusters 1 and 3", x = "Month", y = "Density") +
     scale_x_continuous(breaks = 1:12, labels = month.name) +
     theme_minimal()
df_monthly_density <- df_cluster_1_3 %>%
     group_by(cluster, month) %>%
     summarise(count = n()) %>%
     ungroup() %>%
     group_by(cluster) %>%
     mutate(density = count / sum(count))
ggplot(df_monthly_density, aes(x = month, y = density, color = factor(cluster), group = cluster)) +
     geom_line() +
     geom_point() +
     labs(color = "Cluster", title = "Monthly Data Density for Clusters 1 and 3", x = "Month", y = "Density") +
     scale_x_continuous(breaks = 1:12, labels = month.name) +
     theme_minimal()
df_cluster_2 <- df_outlier %>% filter(cluster == 2)
df_monthly_density_2 <- df_cluster_2 %>%
     group_by(month) %>%
     summarise(count = n()) %>%
     mutate(density = count / sum(count))
ggplot(df_monthly_density_2, aes(x = month, y = density, group = 1)) +
     geom_line() +
     geom_point() +
     labs(title = "Monthly Data Density for Cluster 2", x = "Month", y = "Density") +
     scale_x_continuous(breaks = 1:12, labels = month.name) +
     theme_minimal()
install.packages("xgboost")
library(xgboost)
library(caret)
data <- df_outlier[, c("탁도", "pH", "수온", "전기전도도", "알칼리도", "cluster")]
data[, -6] <- scale(data[, -6])
set.seed(123)
index <- createDataPartition(data$cluster, p = 0.8, list = FALSE)
train <- data[index, ]
test <- data[-index, ]
train_matrix <- xgb.DMatrix(data = as.matrix(train[, -6]), label = as.numeric(train$cluster) - 1)
test_matrix <- xgb.DMatrix(data = as.matrix(test[, -6]), label = as.numeric(test$cluster) - 1)
params <- list(
     booster = "gbtree",
     objective = "multi:softmax",
     num_class = length(unique(data$cluster)),
     eta = 0.1,
     gamma = 0,
     max_depth = 6,
     min_child_weight = 1,
     subsample = 0.8,
     colsample_bytree = 0.8)
xgb_model <- xgb.train(params = params, data = train_matrix, nrounds = 100, verbose = 0)
preds <- predict(xgb_model, test_matrix)
confusion_matrix <- confusionMatrix(factor(preds, levels = 0:(length(unique(data$cluster))-1)),
                                     factor(as.numeric(test$cluster) - 1, levels = 0:(length(unique(data$cluster))-1)))
print(confusion_matrix)
conf_matrix_df <- as.data.frame(confusion_matrix$table)
names(conf_matrix_df) <- c("Prediction", "Reference", "Freq")
ggplot(data = conf_matrix_df, aes(x = Reference, y = Prediction)) +
     geom_tile(aes(fill = Freq), color = "white") +
     scale_fill_gradient(low = "white", high = "blue") +
     geom_text(aes(label = Freq), vjust = 1) +
     labs(x = "Actual", y = "Predicted", title = "Confusion Matrix") +
     theme_minimal()
install.packages("ggplot2")
library(arules)
library(arulesViz)
data <- df_outlier[, c("탁도", "pH", "수온", "전기전도도", "알칼리도", "응집제주입률")]
breaks_탁도 <- quantile(data$탁도, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE)
data$탁도 <- cut(data$탁도, breaks = breaks_탁도, include.lowest = TRUE, labels = c("Low", "Medium", "High"))
breaks_pH <- quantile(data$pH, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE)
data$pH <- cut(data$pH, breaks = breaks_pH, include.lowest = TRUE, labels = c("Low", "Medium", "High"))
breaks_수온 <- quantile(data$수온, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE)
data$수온 <- cut(data$수온, breaks = breaks_수온, include.lowest = TRUE, labels = c("Low", "Medium", "High"))
breaks_전기전도도 <- quantile(data$전기전도도, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE)
data$전기전도도 <- cut(data$전기전도도, breaks = breaks_전기전도도, include.lowest = TRUE, labels = c("Low", "Medium", "High"))
breaks_알칼리도 <- quantile(data$알칼리도, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE)
data$알칼리도 <- cut(data$알칼리도, breaks = breaks_알칼리도, include.lowest = TRUE, labels = c("Low", "Medium", "High"))
breaks_응집제주입률<- quantile(data$응집제주입률, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE)
data$응집제주입률<- cut(data$응집제주입률, breaks = breaks_응집제주입률, include.lowest = TRUE, labels = c("Low", "Medium", "High"))
transactions <- as(data, "transactions")
rules_excluding_coagulant <- apriori(transactions, 
                                      parameter = list(supp = 0.1, conf = 0.8),
                                      appearance = list(none = c("응집제주입률=Low", "응집제주입률=Medium", "응집제주입률=High")))
num_rules_excluding_coagulant <- length(rules_excluding_coagulant)
if (num_rules_excluding_coagulant > 0) {
  inspect(sort(rules_excluding_coagulant, by = "confidence")[1:min(10, num_rules_excluding_coagulant)])
} else {
  cat("No rules found for rules_excluding_coagulant.\n")
}

rules_towards_coagulant <- apriori(transactions, 
+                                    parameter = list(supp = 0.05, conf = 0.6),  
+                                    appearance = list(rhs = c("응집제주입률=Low", "응집제주입률=Medium", "응집제주입률=High")))

num_rules_towards_coagulant <- length(rules_towards_coagulant)
if (num_rules_towards_coagulant > 0) {
  inspect(sort(rules_towards_coagulant, by = "confidence")[1:min(10, num_rules_towards_coagulant)])
} else {
  cat("No rules found for rules_towards_coagulant.\n")
}