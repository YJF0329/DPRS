server_input <- function(input, output, session) {
  
  # 连续变量（需要标准化）
  continuous_vars <- c("Weight", "GAF", "MADRS", "NSQ", "NEO", "PSQI", "BHS", "SF", "LymC", "EosP")
  
  # 分类变量
  categorical_vars <- c("Location", "DH", "B2", "C5", "CGI", "Elect")
  
  # 标准化参数
  scale_params <- list(
    Weight = list(mean = 61.9332, sd = 12.79138),
    GAF = list(mean = 48.98, sd = 12.927),
    MADRS = list(mean = 19.46, sd = 8.392),
    NSQ = list(mean = 40.53, sd = 4.238),
    NEO = list(mean = 186.08, sd = 19.525),
    PSQI = list(mean = 11.58, sd = 4.520),
    BHS = list(mean = 10.01, sd = 5.207),
    SF = list(mean = 26.73, sd = 6.372),
    LymC = list(mean = 1.9549, sd = 0.70145),
    EosP = list(mean = 2.3145, sd = 1.99972)
  )
  
  # 响应式数据存储
  raw_data     <- reactiveValues()
  scaled_data  <- reactiveValues()
  processed_data <- reactiveVal()
  
  # ====================== 提交按钮 ======================
  observeEvent(input$submit_btn, {
    
    # 收集所有数据
    raw <- list()
    for (v in categorical_vars) raw[[v]] <- input[[v]]
    for (v in continuous_vars) raw[[v]] <- input[[v]]
    
    # 缺失值检查
    missing <- names(raw)[sapply(names(raw), function(v) is.null(raw[[v]]) || is.na(raw[[v]]) || raw[[v]] == "")]
    if (length(missing) > 0) {
      shinyalert::shinyalert("数据不完整", paste("请填写：", paste(missing, collapse = ", ")), "warning")
      return()
    }
    
    raw_data$data <- raw
    
    # 连续变量标准化
    scaled <- lapply(continuous_vars, function(v) {
      (as.numeric(raw[[v]]) - scale_params[[v]]$mean) / scale_params[[v]]$sd
    })
    names(scaled) <- continuous_vars
    scaled_data$data <- scaled
    
    # 分类变量编码
    encoded <- list(
      Location = ifelse(raw$Location == "城市", 1, 0),
      DH       = ifelse(raw$DH == "有", 1, 0),
      B2       = ifelse(raw$B2 == "是", 1, 0),
      C5       = switch(raw$C5, "不太紧张" = 1, "较紧张" = 2, "很紧张，压力很大" = 3, 0),
      CGI      = c("正常完全无病" = 1, "边缘性精神病" = 2, "轻度有病" = 3, "中度有病" = 4, "明显有病" = 5, "严重有病" = 6, "疾病极严重" = 7)[raw$CGI],
      Elect    = ifelse(raw$Elect == "正常", 1, 0)
    )
    
    # 最终数据
    final_data <- c(encoded, scaled)[c(
      "Location","DH","B2","C5","CGI","Elect",
      "Weight","GAF","MADRS","NSQ","NEO","PSQI","BHS","SF","LymC","EosP"
    )]
    
    processed_data(final_data)
    
    # ========== 新增：保存到 session$userData 供其他模块使用 ==========
    session$userData$processed_data <- final_data   # 标准化+编码后的数据
    session$userData$raw_data <- raw                # 原始数据（可选）
    # ========== 新增结束 ==========
    
    # 成功提示
    shinyalert::shinyalert("✅ 提交成功", "患者信息已标准化处理", "success", timer = 1500)
    delay(1500, updateTabItems(session, "tabs", selected = "predict"))
  })
  
  # ====================== 重置按钮 ======================
  observeEvent(input$reset_btn, {
    updateSelectInput(session, categorical_vars, selected = "")
    updateNumericInput(session, continuous_vars, value = NA)
    processed_data(NULL)
    session$userData$patient_data <- NULL
    session$userData$raw_data <- NULL
    # ========== 新增：重置时也要清除标准化数据 ==========
    session$userData$processed_data <- NULL
    # ========== 新增结束 ==========
    shinyalert::shinyalert("已重置", "表单已清空", "info", timer = 1500)
  })
  
  # 输出数据接口
  return(list(
    getProcessedData = processed_data,
    getRawData = function() raw_data$data
  ))
}
