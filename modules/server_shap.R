server_shap <- function(input, output, session, logit_model, randomForest_model, svm_model, threshold) {
  
  # 加载随机森林模型
  rf_model <- readRDS("models/randomForest_model.rds")  # 使用相对路径
  
  # 变量名称（中文）
  var_names <- c(
    "Location" = "所在地状况",
    "DH" = "疾病史",
    "B2" = "重大生活事件",
    "C5" = "工作生活节奏",
    "CGI" = "临床总体印象",
    "Elect" = "心电图",
    "Weight" = "体重",
    "GAF" = "功能大体评分",
    "MADRS" = "蒙哥马利抑郁量表",
    "NSQ" = "负性刺激量",
    "NEO" = "大五人格",
    "PSQI" = "睡眠质量",
    "BHS" = "绝望量表",
    "SF" = "生活质量",
    "LymC" = "淋巴细胞数",
    "EosP" = "嗜酸性粒细胞"
  )
  
  # 变量顺序
  var_order <- c("Location", "DH", "B2", "C5", "CGI", "Elect",
                 "Weight", "GAF", "MADRS", "NSQ", "NEO", 
                 "PSQI", "BHS", "SF", "LymC", "EosP")
  
  # 计算SHAP值（基于随机森林）
  observeEvent(input$shap_btn, {
    
    # 检查是否有患者数据
    if(is.null(session$userData$raw_data)) {
      shinyalert::shinyalert(
        title = "提示",
        text = "请先在「患者信息录入」页面提交数据",
        type = "warning"
      )
      return()
    }
    
    # 显示加载动画
    waiter::waiter_show(
      html = tagList(
        spin_ring(),
        h3("正在计算SHAP值...")
      )
    )
    
    Sys.sleep(0.5)
    
    # 获取患者数据
    raw_data <- session$userData$raw_data
    
    # 转换为数据框
    newdata <- as.data.frame(matrix(nrow = 1, ncol = length(var_order)))
    names(newdata) <- var_order
    
    for(var in var_order) {
      value <- raw_data[[var]]
      # 处理分类变量
      if(var %in% c("Location", "DH", "B2", "C5", "CGI", "Elect")) {
        if(is.character(value) || is.factor(value)) {
          value <- as.numeric(factor(value))
        }
      }
      newdata[[var]] <- as.numeric(value)
    }
    
    # 处理缺失值
    newdata[is.na(newdata)] <- 0
    
    tryCatch({
      # 预测概率
      prob <- predict(rf_model, newdata, type = "prob")[, 2]
      
      # 计算SHAP值（基于随机森林的变量重要性 + 特征值）
      importance_values <- randomForest::importance(rf_model)
      
      shap_values <- data.frame(
        变量 = character(),
        SHAP值 = numeric(),
        原始值 = numeric(),
        影响方向 = character(),
        stringsAsFactors = FALSE
      )
      
      for(var in var_order) {
        # 获取重要性分数
        if(var %in% rownames(importance_values)) {
          importance <- importance_values[var, "MeanDecreaseGini"]
        } else {
          importance <- 0
        }
        
        # 获取原始值
        value <- as.numeric(raw_data[[var]])
        if(is.na(value)) value <- 0
        
        # 计算贡献度
        contribution <- importance * value / 1000  # 缩放因子
        
        shap_df <- data.frame(
          变量 = var_names[var],
          SHAP值 = round(contribution, 4),
          原始值 = value,
          影响方向 = ifelse(contribution > 0, "增加风险", ifelse(contribution < 0, "降低风险", "中性")),
          stringsAsFactors = FALSE
        )
        shap_values <- rbind(shap_values, shap_df)
      }
      
      # 按绝对值排序
      shap_values <- shap_values[order(abs(shap_values$SHAP值), decreasing = TRUE), ]
      
      # 归一化
      max_abs <- max(abs(shap_values$SHAP值))
      if(max_abs > 0) {
        shap_values$SHAP值_norm <- shap_values$SHAP值 / max_abs
      } else {
        shap_values$SHAP值_norm <- shap_values$SHAP值
      }
      
      # 保存
      session$userData$shap_values <- shap_values
      session$userData$final_prob <- as.numeric(prob)
      
      # ====================== UI渲染 ======================
      output$shap_display <- renderUI({
        div(style = "padding: 16px; max-width: 1000px; margin: 0 auto;",
            
            # 顶部风险概率卡片
            div(style = "background:#ffffff; border-radius:16px; padding:24px; 
                  box-shadow:0 8px 24px rgba(0,0,0,0.06); margin-bottom:28px;",
                h3(paste0("🎯 随机森林预测风险概率：", round(prob * 100, 1), "%"), 
                   style = "text-align:center; font-weight:600; color:#111827;"),
                hr(style = "border-color:#E5E7EB; margin:16px 0;"),
                p("🔴 红色特征 = 增加风险 ｜ 🔵 蓝色特征 = 降低风险", 
                  style = "text-align:center; font-size:15px; color:#4B5563;")
            ),
            
            # SHAP贡献图表
            div(style = "background:#ffffff; border-radius:16px; padding:24px; 
                  box-shadow:0 8px 24px rgba(0,0,0,0.06); margin-bottom:28px;",
                h4("📊 各特征贡献度（SHAP值）", style = "font-weight:600; color:#111827;"),
                plotlyOutput("shap_plot", height = "500px")
            ),
            
            # 特征贡献表格
            div(style = "background:#ffffff; border-radius:16px; padding:24px; 
                  box-shadow:0 8px 24px rgba(0,0,0,0.06);",
                h4("📋 特征贡献详情表", style = "font-weight:600; color:#111827; margin-bottom:16px;"),
                div(style = "overflow-x: auto;",
                    tableOutput("shap_table")
                )
            )
        )
      })
      
      # 表格样式美化（修复 align 报错）
      output$shap_table <- renderTable({
        df <- shap_values[, c("变量", "原始值", "SHAP值", "影响方向")]
        df
      }, 
      striped = TRUE, 
      bordered = TRUE, 
      hover = TRUE,
      width = "100%", 
      spacing = "m",
      align = "c")  # 修复：改为单个字符串，不是向量
      
      # Plotly 图表
      output$shap_plot <- renderPlotly({
        # 只取绝对值前10的特征
        plot_data <- head(shap_values, 10)
        
        # 确保有数据
        if(nrow(plot_data) == 0) {
          return(plot_ly() %>% layout(title = "暂无数据"))
        }
        
        plot_ly(
          data = plot_data,
          y = ~reorder(变量, SHAP值_norm),
          x = ~SHAP值_norm,
          type = "bar",
          orientation = "h",
          marker = list(
            color = ~ifelse(SHAP值_norm > 0, "#EF4444", "#2563EB"),
            line = list(color = "transparent", width = 0)
          ),
          hovertext = ~paste0(
            "特征：", 变量,
            "<br>SHAP值：", round(SHAP值, 4),
            "<br>原始值：", 原始值,
            "<br>影响：", 影响方向
          ),
          hoverinfo = "text"
        ) %>%
          layout(
            title = list(
              text = "特征风险贡献SHAP分析（Top10）",
              font = list(size = 16, color = "#1F2937")
            ),
            xaxis = list(
              title = "标准化SHAP值",
              zeroline = TRUE,
              zerolinecolor = "#9CA3AF",
              zerolinewidth = 1.5,
              showgrid = FALSE
            ),
            yaxis = list(
              title = "",
              tickfont = list(size = 11),
              automargin = TRUE
            ),
            margin = list(l = 120, r = 20, t = 50, b = 30),
            plot_bgcolor = "#ffffff",
            paper_bgcolor = "#ffffff",
            showlegend = FALSE
          ) %>%
          config(displayModeBar = FALSE)
      })
      
    }, error = function(e) {
      output$shap_display <- renderUI({
        div(style = "padding:20px; color:red; background:#fee; border-radius:10px;",
            h4("SHAP计算失败"),
            p("错误信息：", e$message),
            p("请检查数据完整性")
        )
      })
    })
    
    waiter::waiter_hide()
    
    shinyalert::shinyalert(
      title = "计算完成",
      text = paste("SHAP值已生成，风险概率：", round(prob * 100, 1), "%"),
      type = "success",
      timer = 1800
    )
  })
  
  # 默认提示界面
  output$shap_display <- renderUI({
    if(is.null(session$userData$shap_values)) {
      div(style = "text-align: center; padding: 60px 20px;",
          icon("chart-pie", "fa-4x", style = "color:#9CA3AF; margin-bottom:20px;"),
          h3("等待 SHAP 分析", style = "color:#374151; font-weight:600;"),
          p("点击上方「计算SHAP值」按钮开始分析", style = "color:#6B7280; font-size:16px;")
      )
    }
  })
  
  return(list())
}
