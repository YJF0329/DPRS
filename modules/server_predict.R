server_predict <- function(input, output, session) {
  
  # 加载三个模型
  logit_model <- readRDS("logit_model.rds")
  rf_model <- readRDS("randomForest_model.rds")
  svm_model <- readRDS("svm_model.rds")
  
  # 加载阈值
  threshold <- readRDS("threshold.rds")
  
  # 变量顺序（必须与训练时完全一致）
  var_order <- c("Location", "DH", "B2", "C5", "CGI", "Elect",
                 "Weight", "GAF", "MADRS", "NSQ", "NEO", 
                 "PSQI", "BHS", "SF", "LymC", "EosP")
  
  # 存储预测结果
  prediction_result <- reactiveVal(NULL)
  
  # ==================== 从输入模块获取患者基本信息 ====================
  observeEvent(input$submit_btn, {
    # 保存患者基本信息到 userData
    session$userData$patient_data <- list(
      name = input$patient_name,
      id = input$patient_id,
      age = input$patient_age
    )
    
    # 显示成功消息
    shinyalert::shinyalert(
      title = "提交成功",
      text = paste("患者", input$patient_name, "信息已保存，请前往风险预测页面"),
      type = "success",
      timer = 2000
    )
  })
  
  # 风险预测按钮事件
  observeEvent(input$predict_btn, {
    
    # 检查是否有已标准化的数据（来自 server_input）
    if(is.null(session$userData$processed_data)) {
      shinyalert::shinyalert(
        title = "提示",
        text = "请先在「患者信息录入」页面填写完整数据并点击「提交」按钮",
        type = "warning"
      )
      return()
    }
    
    # 检查患者基本信息
    if(is.null(session$userData$patient_data)) {
      shinyalert::shinyalert(
        title = "提示",
        text = "请先在「患者信息录入」页面填写患者基本信息并提交",
        type = "warning"
      )
      return()
    }
    
    # 显示加载动画
    waiter::waiter_show(
      html = tagList(
        spin_ring(),
        h3("正在进行风险预测...")
      )
    )
    
    Sys.sleep(0.5)
    
    # ========== 修复：正确构建数据框 ==========
    # 获取已处理的数据（标准化后的连续变量 + 编码后的分类变量）
    processed_list <- session$userData$processed_data
    
    # 调试：打印查看数据结构
    print("=== Processed Data Structure ===")
    print(names(processed_list))
    print(str(processed_list))
    
    # 按照变量顺序提取数据
    newdata_list <- list()
    for(var in var_order) {
      if(var %in% names(processed_list)) {
        newdata_list[[var]] <- processed_list[[var]]
      } else {
        # 如果找不到变量，报错提示
        stop(paste("变量", var, "不存在于processed_data中"))
      }
    }
    
    # 转换为数据框（注意：需要保持为单行数据框，而不是转置）
    newdata <- as.data.frame(newdata_list)
    
    # 确保所有变量都是数值型
    newdata[] <- lapply(newdata, as.numeric)
    
    # 调试：打印最终数据框
    print("=== Final Prediction Data ===")
    print(newdata)
    print(names(newdata))
    
    # 检查是否有缺失值
    if(any(is.na(newdata))) {
      waiter::waiter_hide()
      shinyalert::shinyalert(
        title = "数据错误",
        text = paste("标准化数据存在缺失值:", 
                     paste(names(newdata)[sapply(newdata, is.na)], collapse = ", ")),
        type = "error"
      )
      return()
    }
    
    tryCatch({
      # 1. Logistic回归预测
      logit_prob <- predict(logit_model, newdata, type = "response")
      
      # 2. 随机森林预测
      rf_prob <- predict(rf_model, newdata, type = "prob")[, 2]
      
      # 3. SVM预测
      svm_prob_pred <- predict(svm_model, newdata, probability = TRUE)
      svm_prob <- attr(svm_prob_pred, "probabilities")[, "1"]
      
      # 三个模型平均概率
      avg_prob <- (logit_prob + rf_prob + svm_prob) / 3
      
      # 使用阈值
      thresh <- threshold
      pred_class <- ifelse(avg_prob >= thresh, 1, 0)
      
      # 风险等级
      risk_level <- ifelse(avg_prob < 0.3, "低风险",
                           ifelse(avg_prob < 0.7, "中风险", "高风险"))
      
      # 保存结果
      result <- list(
        logit_prob = as.numeric(logit_prob),
        rf_prob = as.numeric(rf_prob),
        svm_prob = as.numeric(svm_prob),
        avg_prob = as.numeric(avg_prob),
        class = pred_class,
        risk_level = risk_level,
        threshold = thresh
      )
      prediction_result(result)
      session$userData$final_prob <- as.numeric(avg_prob)
      session$userData$prediction <- result
      
      # 生成模拟SHAP值（用于演示）
      session$userData$shap_values <- data.frame(
        变量 = c("MADRS", "GAF", "PSQI", "BHS", "CGI", "Weight", "SF", "NSQ"),
        SHAP值 = c(0.32, -0.25, 0.18, 0.15, 0.12, 0.08, -0.06, 0.05),
        影响方向 = c("正向", "负向", "正向", "正向", "正向", "正向", "负向", "正向")
      )
      
      # 显示预测结果
      output$risk_score_display <- renderUI({
        
        risk_color <- ifelse(avg_prob < 0.3, "#10B981",
                             ifelse(avg_prob < 0.7, "#F59E0B", "#EF4444"))
        
        risk_badge <- ifelse(avg_prob < 0.3,
                             "badge bg-success fw-medium px-3 py-2 rounded",
                             ifelse(avg_prob < 0.7,
                                    "badge bg-warning fw-medium px-3 py-2 rounded",
                                    "badge bg-danger fw-medium px-3 py-2 rounded"))
        
        div(
          style = "padding: 24px; max-width: 900px; margin: 0 auto;",
          
          # 患者信息卡片
          div(
            style = "background:#f0f9ff; border-radius:12px; padding:15px; margin-bottom:20px;",
            h4("👤 患者信息", style = "margin:0 0 10px 0; color:#0369a1;"),
            div(style = "display:grid; grid-template-columns:1fr 1fr 1fr; gap:10px;",
                p(style = "margin:0;", tags$b("姓名："), session$userData$patient_data$name %||% "未填写"),
                p(style = "margin:0;", tags$b("病历号："), session$userData$patient_data$id %||% "未填写"),
                p(style = "margin:0;", tags$b("年龄："), ifelse(is.null(session$userData$patient_data$age), "未填写", paste0(session$userData$patient_data$age, "岁")))
            )
          ),
          
          # ===== 标题区 =====
          div(
            style = "text-align:center; margin-bottom:28px;",
            h3("📊 集成模型风险预测结果", style = "font-weight:600; color:#111827;")
          ),
          
          # ===== 核心风险卡片 =====
          div(
            style = paste0(
              "background:#ffffff; border-radius:16px; padding:28px; margin-bottom:28px;",
              "box-shadow:0 8px 24px rgba(0,0,0,0.06);",
              "border-top: 5px solid ", risk_color, ";"
            ),
            
            div(style = "text-align:center; margin-bottom:16px;",
                h4("总体风险评分", style = "font-weight:500; color:#4B5563; margin:0;")
            ),
            
            div(
              style = paste0(
                "font-size:52px; font-weight:700; color:", risk_color, "; text-align:center; margin:12px 0;"
              ),
              paste0(round(avg_prob * 100, 1), "%")
            ),
            
            div(style = "text-align:center; margin-bottom:20px;",
                HTML(paste0('<span class="', risk_badge, '">', risk_level, '</span>'))
            ),
            
            tags$hr(style = "border-color:#E5E7EB; margin:20px 0;"),
            
            div(style = "font-size:15px; color:#374151; line-height:1.7;",
                tags$p(tags$b("阈值："), round(thresh, 4)),
                tags$p(tags$b("最终分类："), ifelse(pred_class == 1, "高风险患者", "低风险患者"))
            )
          ),
          
          # ===== 模型对比 =====
          div(
            style = "margin-bottom:20px;",
            h4("📈 模型预测对比", style = "font-weight:600; color:#111827;")
          ),
          
          div(
            style = "display:grid; grid-template-columns:1fr 1fr 1fr; gap:16px;",
            
            div(
              style = "background:#ffffff; padding:20px; border-radius:12px; box-shadow:0 4px 12px rgba(0,0,0,0.05);",
              div(style = "font-weight:600; color:#374151; margin-bottom:8px;", "Logistic 回归"),
              tags$h4(paste0(round(logit_prob * 100, 1), "%"),
                      style = "color:#2563EB; font-weight:700; margin:0;")
            ),
            
            div(
              style = "background:#ffffff; padding:20px; border-radius:12px; box-shadow:0 4px 12px rgba(0,0,0,0.05);",
              div(style = "font-weight:600; color:#374151; margin-bottom:8px;", "随机森林"),
              tags$h4(paste0(round(rf_prob * 100, 1), "%"),
                      style = "color:#059669; font-weight:700; margin:0;")
            ),
            
            div(
              style = "background:#ffffff; padding:20px; border-radius:12px; box-shadow:0 4px 12px rgba(0,0,0,0.05);",
              div(style = "font-weight:600; color:#374151; margin-bottom:8px;", "SVM"),
              tags$h4(paste0(round(svm_prob * 100, 1), "%"),
                      style = "color:#7C3AED; font-weight:700; margin:0;")
            )
          ),
          
          div(
            style = "margin-top:24px; font-size:13px; color:#6B7280; text-align:right;",
            paste0("Ensemble model | threshold = ", round(thresh, 4))
          )
        )
      })
      
      # 在 tryCatch 内部显示成功消息
      waiter::waiter_hide()
      shinyalert::shinyalert(
        title = "预测完成",
        text = paste("平均风险概率：", round(avg_prob * 100, 1), "%"),
        type = "success",
        timer = 1500
      )
      
    }, error = function(e) {
      waiter::waiter_hide()
      output$risk_score_display <- renderUI({
        div(
          style = "padding:20px; color:red; background:#fee; border-radius:10px;",
          h4("预测失败"),
          p("错误信息：", e$message),
          p("请检查所有输入字段是否已正确填写")
        )
      })
      shinyalert::shinyalert(
        title = "预测失败",
        text = e$message,
        type = "error"
      )
    })
  })
  
  # 获取预测结果
  getPrediction <- function() {
    return(prediction_result())
  }
  
  return(list(
    getPrediction = getPrediction
  ))
}
