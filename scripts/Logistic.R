# ------------------- 加载依赖库 -------------------
library(ROCR)

# ------------------- 数据读取 -------------------
data0 <- read.csv("C:/Users/姚竞帆/Desktop/APP2/代码+数据/数据/LASSO筛选后的特征.csv", 
                  header = TRUE, sep = ",", stringsAsFactors = FALSE)

# ------------------- 存储100次实验的关键指标 -------------------
results_summary <- data.frame(
  Iteration = 1:100,
  Threshold = numeric(100),
  AUC = numeric(100),
  ACC = numeric(100),
  TPR = numeric(100),
  TNR = numeric(100),
  Precision = numeric(100),
  F1 = numeric(100)
)

# ------------------- 保存最后一次训练的模型 -------------------
final_model <- NULL
final_threshold <- NULL

# ------------------- 主循环（100次实验） -------------------
set.seed(123)

for (i in 1:100) {
  # --- 分层抽样 ---
  temp0 <- data0[data0$REC == 1, ]
  temp1 <- data0[data0$REC == 0, ]
  
  # 正样本拆分
  x <- sample(1:nrow(temp0))
  split_index <- round(length(x) * 0.3)
  temp01 <- temp0[x[1:split_index], ]
  temp02 <- temp0[x[(split_index+1):length(x)], ]
  
  # 负样本拆分
  x <- sample(1:nrow(temp1))
  split_index <- round(length(x) * 0.3)
  temp11 <- temp1[x[1:split_index], ]
  temp12 <- temp1[x[(split_index+1):length(x)], ]
  
  # 合并数据集
  dataCS <- rbind(temp01, temp11)
  dataXL <- rbind(temp02, temp12)
  
  # --- 模型训练（数据已经是标准化后的）---
  logit <- glm(REC ~ ., data = dataXL, family = binomial(link = "logit"))
  
  # --- 阈值确定（Youden指数）---
  fit_train <- fitted(logit)
  pred_train <- prediction(fit_train, dataXL$REC)
  perf_roc <- performance(pred_train, "tpr", "fpr")
  youden_index <- which.max(perf_roc@y.values[[1]] - perf_roc@x.values[[1]])
  threshold <- perf_roc@alpha.values[[1]][youden_index]
  
  # --- 预测测试集 ---
  prob_test <- predict(logit, dataCS, type = "response")
  class_test <- ifelse(prob_test >= threshold, 1, 0)
  
  # --- 计算性能指标 ---
  conf_matrix <- table(class_test, dataCS$REC)
  TP <- ifelse(nrow(conf_matrix) > 1, conf_matrix[2,2], 0)
  TN <- ifelse(nrow(conf_matrix) > 1, conf_matrix[1,1], conf_matrix[1,1])
  FP <- ifelse(nrow(conf_matrix) > 1, conf_matrix[2,1], 0)
  FN <- ifelse(nrow(conf_matrix) > 1, conf_matrix[1,2], 0)
  
  TPR <- ifelse((TP + FN) > 0, TP / (TP + FN), 0)
  TNR <- ifelse((TN + FP) > 0, TN / (TN + FP), 0)
  ACC <- (TP + TN) / (TP + TN + FP + FN)
  Precision <- ifelse((TP + FP) > 0, TP / (TP + FP), 0)
  F1 <- ifelse((Precision + TPR) > 0, 2 * (Precision * TPR) / (Precision + TPR), 0)
  
  # ROC和AUC
  roc_pred <- prediction(prob_test, dataCS$REC)
  AUC <- performance(roc_pred, "auc")@y.values[[1]]
  
  # 保存结果
  results_summary[i, ] <- c(i, threshold, AUC, ACC, TPR, TNR, Precision, F1)
  
  # 保存最后一次的模型和参数
  if (i == 100) {
    final_model <- logit
    final_threshold <- threshold
  }
}

# ------------------- 查看模型 -------------------
cat("\n========== 最终模型 ==========\n")
print(summary(final_model))
cat("\n阈值 (Youden指数):", final_threshold, "\n")

# ------------------- 保存模型 -------------------
saveRDS(final_model, "C:/Users/姚竞帆/Desktop/APP2//logit_model.rds")
saveRDS(final_threshold, "C:/Users/姚竞帆/Desktop/APP2//threshold.rds")

# ------------------- 保存100次实验结果 -------------------
write.csv(results_summary, "C:/Users/姚竞帆/Desktop/APP2//100次实验结果.csv", row.names = FALSE)

# ------------------- 计算汇总统计 -------------------
summary_stats <- data.frame(
  指标 = c("阈值", "AUC", "准确率", "敏感度", "特异度", "精确率", "F1分数"),
  均值 = round(colMeans(results_summary[, -1]), 4),
  标准差 = round(apply(results_summary[, -1], 2, sd), 4),
  最小值 = round(apply(results_summary[, -1], 2, min), 4),
  中位数 = round(apply(results_summary[, -1], 2, median), 4),
  最大值 = round(apply(results_summary[, -1], 2, max), 4)
)

write.csv(summary_stats, "C:/Users/姚竞帆/Desktop/APP2/代码+数据/模型/模型性能汇总统计.csv", row.names = FALSE)

# ------------------- 输出 -------------------
cat("\n========== 100次实验模型性能汇总 ==========\n")
print(summary_stats)

cat("\n模型文件已保存至：C:/Users/姚竞帆/Desktop/APP2/代码+数据/模型/\n")
cat("- logit_model.rds\n")
cat("- threshold.rds\n")
cat("- 100次实验结果.csv\n")
cat("- 模型性能汇总统计.csv\n")
