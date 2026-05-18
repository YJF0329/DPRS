server_export <- function(input, output, session) {
  
  # 预览报告
  output$pdf_preview <- renderUI({
    req(
      session$userData$patient_data,
      session$userData$raw_data,
      session$userData$prediction
    )
    
    # 使用集成模型的平均概率
    p <- session$userData$prediction$avg_prob
    risk_color <- ifelse(p < 0.3, "#10B981", ifelse(p < 0.7, "#F59E0B", "#EF4444"))
    risk_label <- ifelse(p < 0.3, "低风险", ifelse(p < 0.7, "中风险", "高风险"))
    
    # 获取患者基本信息
    patient_name <- session$userData$patient_data$name
    patient_id <- session$userData$patient_data$id
    patient_age <- session$userData$patient_data$age
    
    # 处理空值
    if(is.null(patient_name) || patient_name == "") patient_name <- "未填写"
    if(is.null(patient_id) || patient_id == "") patient_id <- "未填写"
    if(is.null(patient_age) || is.na(patient_age)) patient_age <- "未填写"
    
    div(style = "line-height:1.8; font-family:system-ui;",
        
        h1("患者心理风险智能预测报告", style = "text-align:center; color:#165DFF;"),
        h4(paste0("生成时间：", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), style = "text-align:center; color:#666;"),
        hr(style = "border-top:2px solid #165DFF;"),
        
        h3("一、患者基础信息", style = "color:#222;"),
        p(paste("患者姓名：", patient_name)),
        p(paste("病历号：", patient_id)),
        p(paste("年龄：", patient_age, if(is.numeric(patient_age)) "岁" else "")),
        p(paste("所在地：", session$userData$raw_data$Location)),
        p(paste("疾病史：", session$userData$raw_data$DH)),
        p(paste("重大生活事件：", session$userData$raw_data$B2)),
        p(paste("工作节奏：", session$userData$raw_data$C5)),
        p(paste("临床印象：", session$userData$raw_data$CGI)),
        p(paste("心电图：", session$userData$raw_data$Elect)),
        
        hr(),
        
        h3("二、风险预测结果", style = "color:#222;"),
        div(style = "text-align:center;",
            h4("集成模型平均概率", style = "color:#666; margin-bottom:10px;"),
            h2(paste0(round(p*100,1),"%"), style = paste0("color:", risk_color, "; font-size:42px;")),
            span(risk_label, style = paste0("background:", risk_color, "; color:white; padding:6px 16px; border-radius:20px; font-size:16px;")),
            hr(style = "margin:20px 0;"),
            div(style = "display:grid; grid-template-columns:1fr 1fr 1fr; gap:10px; margin-top:15px;",
                div(style = "background:#f5f5f5; padding:10px; border-radius:8px;",
                    p(style = "margin:0; font-weight:bold;", "Logistic"),
                    p(style = "margin:5px 0 0 0; color:#2563EB;", paste0(round(session$userData$prediction$logit_prob * 100, 1), "%"))
                ),
                div(style = "background:#f5f5f5; padding:10px; border-radius:8px;",
                    p(style = "margin:0; font-weight:bold;", "随机森林"),
                    p(style = "margin:5px 0 0 0; color:#059669;", paste0(round(session$userData$prediction$rf_prob * 100, 1), "%"))
                ),
                div(style = "background:#f5f5f5; padding:10px; border-radius:8px;",
                    p(style = "margin:0; font-weight:bold;", "SVM"),
                    p(style = "margin:5px 0 0 0; color:#7C3AED;", paste0(round(session$userData$prediction$svm_prob * 100, 1), "%"))
                )
            )
        ),
        
        hr(),
        
        h3("三、关键影响特征", style = "color:#222;"),
        tableOutput("shap_table_pdf"),
        
        hr(),
        
        h3("四、临床建议", style = "color:#222;"),
        p(if(p >= 0.7) {
          "患者风险较高，建议重点关注、加强干预与随访。"
        } else if(p >= 0.3) {
          "患者存在中等风险，建议定期评估与心理疏导。"
        } else {
          "患者风险较低，建议保持良好生活习惯，常规随访。"
        }),
        
        p(style = "color:#777; font-size:12px; margin-top:30px;",
          "本报告由系统自动生成，仅供临床参考，不作为唯一诊断依据。")
    )
  })
  
  output$shap_table_pdf <- renderTable({
    req(session$userData$shap_values)
    shap_data <- session$userData$shap_values
    if("变量" %in% colnames(shap_data)) {
      head(shap_data[, c("变量", "SHAP值", "影响方向")], 5)
    } else {
      colnames(shap_data) <- c("变量", "SHAP值", "影响方向")
      head(shap_data, 5)
    }
  }, striped = TRUE, bordered = TRUE, align = "c")
  
  # 导出HTML报告
  output$download_pdf <- downloadHandler(
    filename = function() {
      patient_name <- session$userData$patient_data$name
      if(is.null(patient_name) || patient_name == "") patient_name <- "患者"
      paste0(patient_name, "_风险报告_", Sys.Date(), ".html")
    },
    content = function(file) {
      req(session$userData$patient_data, session$userData$raw_data, session$userData$prediction)
      
      html_content <- generate_html_report(session)
      writeLines(html_content, file, useBytes = TRUE)
    }
  )
  
  # 刷新预览
  observeEvent(input$preview_pdf_btn, {
    if(is.null(session$userData$patient_data) || is.null(session$userData$raw_data) || is.null(session$userData$prediction)) {
      shinyalert::shinyalert("数据未就绪", "请先在'风险预测'页面点击'开始风险预测'按钮生成结果", type = "warning")
      return()
    }
    
    output$pdf_preview <- renderUI({
      req(session$userData$patient_data, session$userData$raw_data, session$userData$prediction)
      
      p <- session$userData$prediction$avg_prob
      risk_color <- ifelse(p < 0.3, "#10B981", ifelse(p < 0.7, "#F59E0B", "#EF4444"))
      risk_label <- ifelse(p < 0.3, "低风险", ifelse(p < 0.7, "中风险", "高风险"))
      
      patient_name <- session$userData$patient_data$name
      patient_id <- session$userData$patient_data$id
      patient_age <- session$userData$patient_data$age
      
      if(is.null(patient_name) || patient_name == "") patient_name <- "未填写"
      if(is.null(patient_id) || patient_id == "") patient_id <- "未填写"
      if(is.null(patient_age) || is.na(patient_age)) patient_age <- "未填写"
      
      div(style = "line-height:1.8; font-family:system-ui; padding:20px;",
          h2("患者风险报告", style = "text-align:center; color:#165DFF;"),
          p(paste("生成时间：", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), style = "text-align:center; color:#666;"),
          hr(),
          h3("一、基础信息"),
          p(paste("患者姓名：", patient_name)),
          p(paste("病历号：", patient_id)),
          p(paste("年龄：", patient_age, if(is.numeric(patient_age)) "岁" else "")),
          p(paste("所在地：", session$userData$raw_data$Location)),
          p(paste("疾病史：", session$userData$raw_data$DH)),
          p(paste("重大生活事件：", session$userData$raw_data$B2)),
          p(paste("工作节奏：", session$userData$raw_data$C5)),
          hr(),
          h3("二、风险预测结果"),
          div(style = "text-align:center;",
              h2(paste0(round(p*100,1),"%"), style = paste("color:", risk_color, "; font-size:48px;")),
              span(risk_label, style = paste("background:", risk_color, "; color:white; padding:8px 20px; border-radius:25px;"))
          ),
          hr(),
          h3("三、关键影响特征"),
          tableOutput("shap_table_pdf")
      )
    })
    
    shinyalert::shinyalert("预览已刷新", "已加载最新数据", type = "success", timer = 1500)
  })
}

# 生成HTML报告的辅助函数
generate_html_report <- function(session) {
  p <- session$userData$prediction$avg_prob
  risk_color <- ifelse(p < 0.3, "#10B981", ifelse(p < 0.7, "#F59E0B", "#EF4444"))
  risk_label <- ifelse(p < 0.3, "低风险", ifelse(p < 0.7, "中风险", "高风险"))
  
  patient_name <- session$userData$patient_data$name
  patient_id <- session$userData$patient_data$id
  patient_age <- session$userData$patient_data$age
  
  if(is.null(patient_name) || patient_name == "") patient_name <- "未填写"
  if(is.null(patient_id) || patient_id == "") patient_id <- "未填写"
  if(is.null(patient_age) || is.na(patient_age)) patient_age <- "未填写"
  
  # 生成SHAP表格HTML
  shap_html <- ""
  if(!is.null(session$userData$shap_values)) {
    shap_df <- head(session$userData$shap_values, 5)
    if(nrow(shap_df) > 0) {
      shap_html <- '<table style="width:100%; border-collapse:collapse; margin:20px 0;">
        <thead>
          <tr style="background-color:#f2f2f2;">
            <th style="border:1px solid #ddd; padding:8px;">变量</th>
            <th style="border:1px solid #ddd; padding:8px;">SHAP值</th>
            <th style="border:1px solid #ddd; padding:8px;">影响方向</th>
           </tr>
        </thead>
        <tbody>'
      for(i in 1:nrow(shap_df)) {
        shap_html <- paste0(shap_html, '
          <tr>')
        shap_html <- paste0(shap_html, '<td style="border:1px solid #ddd; padding:8px;">', shap_df[i, 1], '</td>')
        shap_html <- paste0(shap_html, '<td style="border:1px solid #ddd; padding:8px;">', round(as.numeric(shap_df[i, 2]), 4), '</td>')
        shap_html <- paste0(shap_html, '<td style="border:1px solid #ddd; padding:8px;">', shap_df[i, 3], '</td>')
        shap_html <- paste0(shap_html, '</tr>')
      }
      shap_html <- paste0(shap_html, '</tbody> </table>')
    } else {
      shap_html <- "<p>暂无SHAP分析数据</p>"
    }
  } else {
    shap_html <- "<p>暂无SHAP分析数据</p>"
  }
  
  # 完整HTML报告
  paste0(
    '<!DOCTYPE html>
    <html>
    <head>
      <meta charset="UTF-8">
      <title>患者心理风险预测报告</title>
      <style>
        body {
          font-family: "Microsoft YaHei", "SimHei", Arial, sans-serif;
          margin: 50px auto;
          padding: 20px;
          max-width: 800px;
          line-height: 1.6;
          color: #333;
        }
        h1 {
          color: #165DFF;
          text-align: center;
          border-bottom: 2px solid #165DFF;
          padding-bottom: 15px;
        }
        h3 {
          color: #222;
          margin-top: 30px;
          border-left: 4px solid #165DFF;
          padding-left: 15px;
        }
        .info-box {
          background: #f8f9fa;
          padding: 15px 20px;
          border-radius: 8px;
          margin: 20px 0;
        }
        .info-box p {
          margin: 8px 0;
        }
        .risk-box {
          text-align: center;
          margin: 30px 0;
          padding: 20px;
          background: #f8f9fa;
          border-radius: 10px;
        }
        .risk-percent {
          font-size: 52px;
          font-weight: bold;
          color: ', risk_color, ';
        }
        .risk-label {
          display: inline-block;
          padding: 8px 25px;
          border-radius: 25px;
          color: white;
          background: ', risk_color, ';
          font-size: 18px;
          margin-top: 10px;
        }
        .model-grid {
          display: grid;
          grid-template-columns: 1fr 1fr 1fr;
          gap: 15px;
          margin-top: 20px;
        }
        .model-card {
          background: #f5f5f5;
          padding: 12px;
          border-radius: 8px;
          text-align: center;
        }
        .footer {
          text-align: center;
          font-size: 12px;
          color: #777;
          margin-top: 50px;
          padding-top: 20px;
          border-top: 1px solid #ddd;
        }
        hr {
          margin: 20px 0;
          border: none;
          border-top: 1px solid #e0e0e0;
        }
        table {
          width: 100%;
          margin: 20px 0;
        }
        th {
          background-color: #f2f2f2;
        }
      </style>
    </head>
    <body>
      <h1>患者心理风险智能预测报告</h1>
      <p style="text-align:center; color:#666;">生成时间：', format(Sys.time(), "%Y-%m-%d %H:%M:%S"), '</p>
      
      <h3>一、患者基础信息</h3>
      <div class="info-box">
        <p><strong>患者姓名：</strong>', patient_name, '</p>
        <p><strong>病历号：</strong>', patient_id, '</p>
        <p><strong>年龄：</strong>', patient_age, if(is.numeric(patient_age)) "岁" else "", '</p>
        <p><strong>所在地：</strong>', session$userData$raw_data$Location, '</p>
        <p><strong>疾病史：</strong>', session$userData$raw_data$DH, '</p>
        <p><strong>重大生活事件：</strong>', session$userData$raw_data$B2, '</p>
        <p><strong>工作节奏：</strong>', session$userData$raw_data$C5, '</p>
        <p><strong>临床印象：</strong>', session$userData$raw_data$CGI, '</p>
        <p><strong>心电图：</strong>', session$userData$raw_data$Elect, '</p>
      </div>
      
      <h3>二、风险预测结果</h3>
      <div class="risk-box">
        <div class="risk-percent">', round(p * 100, 1), '%</div>
        <div class="risk-label">', risk_label, '</div>
        <div class="model-grid">
          <div class="model-card">
            <strong>Logistic</strong><br>
            ', round(session$userData$prediction$logit_prob * 100, 1), '%
          </div>
          <div class="model-card">
            <strong>随机森林</strong><br>
            ', round(session$userData$prediction$rf_prob * 100, 1), '%
          </div>
          <div class="model-card">
            <strong>SVM</strong><br>
            ', round(session$userData$prediction$svm_prob * 100, 1), '%
          </div>
        </div>
      </div>
      
      <h3>三、关键影响特征</h3>
      ', shap_html, '
      
      <h3>四、临床建议</h3>
      <div class="info-box">
        <p>', if(p >= 0.7) {
          "🔴 患者风险较高，建议重点关注、加强干预与随访。"
        } else if(p >= 0.3) {
          "🟡 患者存在中等风险，建议定期评估与心理疏导。"
        } else {
          "🟢 患者风险较低，建议保持良好生活习惯，常规随访。"
        }, '</p>
      </div>
      
      <div class="footer">
        <p>本报告由系统自动生成，仅供临床参考，不作为唯一诊断依据。</p>
        <p>建议结合临床医生专业判断综合评估</p>
      </div>
    </body>
    </html>'
  )
}
