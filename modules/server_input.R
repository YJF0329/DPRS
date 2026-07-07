server_input <- function(input, output, session) {
  
  # 连续变量（需要标准化）
  continuous_vars <- c("Weight", "GAF", "MADRS", "NSQ", "NEO", "PSQI", "BHS", "SF", "LymC", "EosP")
  
  # 分类变量（注意：patient_id 已删除，patient_name 改为患者编号但作为文本存储）
  categorical_vars <- c("Location", "DH", "B2", "C5", "CGI", "Elect")
  
  # 患者信息变量（不参与建模，仅用于记录）
  patient_info_vars <- c("patient_name", "patient_age")
  
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
    
    # 收集所有数据（包含患者信息和表单数据）
    raw <- list()
    
    # 收集患者信息（不参与建模）
    raw$patient_name <- input$patient_name  # 患者编号
    raw$patient_age <- input$patient_age    # 年龄
    
    # 收集分类变量
    for (v in categorical_vars) raw[[v]] <- input[[v]]
    
    # 收集连续变量
    for (v in continuous_vars) raw[[v]] <- input[[v]]
    
    # ========== 新增：患者编号格式验证 ==========
    if (is.null(raw$patient_name) || raw$patient_name == "") {
      shinyalert::shinyalert("数据不完整", "请填写患者编号（8位）", "warning")
      return()
    }
    
    # 验证患者编号是否为8位数字
    if (!grepl("^[0-9]{8}$", raw$patient_name)) {
      shinyalert::shinyalert("格式错误", "患者编号必须为8位数字，如：20240001", "warning")
      return()
    }
    # ========== 新增结束 ==========
    
    # 缺失值检查（只检查建模用的变量）
    modeling_vars <- c(categorical_vars, continuous_vars)
    missing <- character()
    for (v in modeling_vars) {
      val <- raw[[v]]
      if (is.null(val) || is.na(val) || val == "") {
        missing <- c(missing, v)
      }
    }
    
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
    
    # 最终数据（只包含建模所需变量）
    final_data <- c(encoded, scaled)[c(
      "Location","DH","B2","C5","CGI","Elect",
      "Weight","GAF","MADRS","NSQ","NEO","PSQI","BHS","SF","LymC","EosP"
    )]
    
    processed_data(final_data)
    
    # ========== 保存到 session$userData 供其他模块使用 ==========
    session$userData$processed_data <- final_data        # 标准化+编码后的数据（用于建模）
    session$userData$raw_data <- raw                     # 原始数据（包含患者编号、年龄等所有信息）
    session$userData$patient_id <- raw$patient_name      # 单独存储患者编号，方便其他模块调用
    session$userData$patient_age <- raw$patient_age      # 单独存储患者年龄
    # ========== 新增结束 ==========
    
    # 成功提示
    shinyalert::shinyalert("✅ 提交成功", 
                           paste0("患者 ", raw$patient_name, " 信息已标准化处理"), 
                           "success", timer = 1500)
    delay(1500, updateTabItems(session, "tabs", selected = "predict"))
  })
  
  # ====================== 重置按钮 ======================
  observeEvent(input$reset_btn, {
    # 重置分类变量
    updateSelectInput(session, categorical_vars, selected = "")
    
    # 重置连续变量
    updateNumericInput(session, continuous_vars, value = NA)
    
    # 重置患者信息
    updateTextInput(session, "patient_name", value = "")
    updateNumericInput(session, "patient_age", value = NA)
    
    # 清空存储的数据
    processed_data(NULL)
    session$userData$patient_data <- NULL
    session$userData$raw_data <- NULL
    session$userData$processed_data <- NULL
    session$userData$patient_id <- NULL
    session$userData$patient_age <- NULL
    
    shinyalert::shinyalert("已重置", "表单已清空", "info", timer = 1500)
  })
  
  # 输出数据接口
  return(list(
    getProcessedData = processed_data,
    getRawData = function() raw_data$data,
    getPatientId = function() {
      if (!is.null(raw_data$data)) raw_data$data$patient_name else NULL
    },
    getPatientAge = function() {
      if (!is.null(raw_data$data)) raw_data$data$patient_age else NULL
    }
  ))
}
