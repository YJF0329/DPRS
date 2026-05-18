# ------------------- 加载必要包 -------------------
library(e1071)
library(ROCR)

# ------------------- 读取数据 -------------------
data0 <- read.csv("C:/Users/姚竞帆/Desktop/APP2/代码+数据/数据/LASSO筛选后的特征.csv", 
                  header = TRUE, sep = ",", stringsAsFactors = FALSE)

# ------------------- 存储100次实验的关键指标 -------------------
results_summary <- data.frame(
  Iteration = 1:100,
  AUC = numeric(100),
  ACC = numeric(100),
  TPR = numeric(100),
  TNR = numeric(100),
  Precision = numeric(100),
  F1 = numeric(100)
)

# ------------------- 保存最后一次训练的模型 -------------------
final_model <- NULL

# ------------------- 主循环（100次实验） -------------------
set.seed(123)

for (i in 1:100) {
  # 分层抽样
  temp0 <- data0[data0$REC == 1, ]
  temp1 <- data0[data0$REC == 0, ]
  
  # 正样本拆分
  x <- sample(1:nrow(temp0))
  split_index <- round(length(x) * 0.3)
  temp01 <- temp0[x[1:split_index], ]
  temp02 <- temp0[x[(split_index + 1):length(x)], ]
  
  # 负样本拆分
  x <- sample(1:nrow(temp1))
  split_index <- round(length(x) * 0.3)
  temp11 <- temp1[x[1:split_index], ]
  temp12 <- temp1[x[(split_index + 1):length(x)], ]
  
  # 合并数据集
  dataCS <- rbind(temp01, temp11)
  dataXL <- rbind(temp02, temp12)
  
  # 建模
  dataXL$REC <- as.factor(dataXL$REC)
  wts <- c(1, 5)
  names(wts) <- c("0", "1")
  svm_model <- svm(REC ~ ., data = dataXL, type = "C-classification",
                   kernel = "linear", probability = TRUE, class.weights = wts)
  
  # 预测
  prob_pred <- predict(svm_model, dataCS, probability = TRUE)
  prob_matrix <- attr(prob_pred, "probabilities")
  positive_prob <- prob_matrix[, "1"]
  pred_class <- predict(svm_model, dataCS)
  
  # 计算混淆矩阵
  conf_matrix <- table(pred_class, dataCS$REC)
  TP <- ifelse(nrow(conf_matrix) > 1, conf_matrix[2,2], 0)
  TN <- ifelse(nrow(conf_matrix) > 1, conf_matrix[1,1], conf_matrix[1,1])
  FP <- ifelse(nrow(conf_matrix) > 1, conf_matrix[2,1], 0)
  FN <- ifelse(nrow(conf_matrix) > 1, conf_matrix[1,2], 0)
  
  TPR <- ifelse((TP + FN) > 0, TP / (TP + FN), 0)
  TNR <- ifelse((TN + FP) > 0, TN / (TN + FP), 0)
  ACC <- (TP + TN) / (TP + TN + FP + FN)
  Precision <- ifelse((TP + FP) > 0, TP / (TP + FP), 0)
  F1 <- ifelse((Precision + TPR) > 0, 2 * (Precision * TPR) / (Precision + TPR), 0)
  
  # 计算AUC
  pred_obj <- prediction(positive_prob, dataCS$REC)
  auc_val <- performance(pred_obj, "auc")@y.values[[1]]
  
  # 保存结果
  results_summary[i, ] <- c(i, auc_val, ACC, TPR, TNR, Precision, F1)
  
  # 保存最后一次的模型
  if (i == 100) {
    final_model <- svm_model
  }
}

# ------------------- 保存模型 -------------------
saveRDS(final_model, "C:/Users/姚竞帆/Desktop/APP2//svm_model.rds")

# ------------------- 保存100次实验结果 -------------------
write.csv(results_summary, "C:/Users/姚竞帆/Desktop/APP2/SVM_100次实验结果.csv", row.names = FALSE)

# ------------------- 计算汇总统计 -------------------
summary_stats <- data.frame(
  指标 = c("AUC", "准确率", "敏感度", "特异度", "精确率", "F1分数"),
  均值 = round(colMeans(results_summary[, -1]), 4),
  标准差 = round(apply(results_summary[, -1], 2, sd), 4),
  最小值 = round(apply(results_summary[, -1], 2, min), 4),
  中位数 = round(apply(results_summary[, -1], 2, median), 4),
  最大值 = round(apply(results_summary[, -1], 2, max), 4)
)

write.csv(summary_stats, "C:/Users/姚竞帆/Desktop/APP2/SVM_模型性能汇总统计.csv", row.names = FALSE)

# ------------------- 输出 -------------------
cat("\n========== SVM模型训练完成 ==========\n")
print(summary_stats)
cat("\n模型文件已保存至：C:/Users/姚竞帆/Desktop/APP2/代码+数据/模型/svm_model.rds\n")
final_model
