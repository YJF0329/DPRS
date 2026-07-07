ui_input <- function() {
  tagList(
    box(
      title = div(icon("user-md"), " 患者信息录入"),
      width = 12,
      status = "primary",
      solidHeader = TRUE,
      elevation = 3,
      
      # 说明提示
      div(class = "text-center mb-3 text-primary",
          p("请完整填写以下信息，所有项均为必填", style = "font-size:16px; font-weight:500;")),
      
      # ====================== 分组卡片式表单 ======================
      # 第一组：基本信息
      box(
        title = "📋 基本信息", width = 12, status = "info", solidHeader = FALSE,
        fluidRow(
          column(4, textInput("patient_name", "患者编号（8位）", placeholder = "请输入8位编号，如：20240001")),
          column(4, numericInput("patient_age", "年龄", value = NA, min = 0, max = 120, step = 1))
          # 病历号已删除
        ),
        fluidRow(
          column(6, selectInput("Location", "所在地状况", c("", "城市", "农村"))),
          column(6, selectInput("DH", "疾病史", c("", "有", "无")))
        )
      ),
      
      # 第二组：生活与心理状态
      box(
        title = "🧠 生活与心理状态", width = 12, status = "info", solidHeader = FALSE,
        fluidRow(
          column(6, selectInput("B2", "重大生活事件", c("", "是", "否"))),
          column(6, selectInput("C5", "工作生活节奏", c("", "不太紧张", "较紧张", "很紧张，压力很大")))
        )
      ),
      
      # 第三组：临床评估
      box(
        title = "🏥 临床评估", width = 12, status = "info", solidHeader = FALSE,
        fluidRow(
          column(6, selectInput("CGI", "临床总体印象", c("", "正常完全无病", "边缘性精神病", "轻度有病", "中度有病", "明显有病", "严重有病", "疾病极严重"))),
          column(6, selectInput("Elect", "心电图", c("", "正常", "异常")))
        )
      ),
      
      # 第四组：身体指标（带范围说明）
      box(
        title = "📊 身体与量表指标", width = 12, status = "info", solidHeader = FALSE,
        fluidRow(
          column(4, 
                 numericInput("Weight", "体重", value = NA),
                 div(class = "text-muted small", "单位：kg")
          ),
          column(4, 
                 numericInput("GAF", "功能大体评分", value = NA, min = 0, max = 100),
                 div(class = "text-muted small", "范围：0～100")
          ),
          column(4, 
                 numericInput("MADRS", "蒙哥马利抑郁量表", value = NA, min = 0, max = 60),
                 div(class = "text-muted small", "范围：0～60")
          )
        ),
        fluidRow(
          column(4, 
                 numericInput("NSQ", "负性刺激量", value = NA, min = 0, max = 100),
                 div(class = "text-muted small", "范围：0～100")
          ),
          column(4, 
                 numericInput("NEO", "大五人格", value = NA, min = 1, max = 5, step = 0.1),
                 div(class = "text-muted small", "范围：1～5")
          ),
          column(4, 
                 numericInput("PSQI", "睡眠质量", value = NA, min = 0, max = 21),
                 div(class = "text-muted small", "范围：0～21")
          )
        ),
        fluidRow(
          column(4, 
                 numericInput("BHS", "绝望量表", value = NA),
                 div(class = "text-muted small", "范围：0～21")
          ),
          column(4, 
                 numericInput("SF", "生活质量", value = NA, min = 0, max = 50),
                 div(class = "text-muted small", "范围：0～50")
          ),
          column(4, 
                 numericInput("LymC", "淋巴细胞数", value = NA, min = 0, max = 10, step = 0.01),
                 div(class = "text-muted small", "范围：0～10 ×10^9/L")
          )
        ),
        fluidRow(
          column(12, 
                 numericInput("EosP", "嗜酸性粒细胞", value = NA, min = 0, max = 10, step = 0.01),
                 div(class = "text-muted small", "范围：0～10 ×10^9/L")
          )
        )
      ),
      
      # ====================== 按钮区 ======================
      div(style = "text-align:center; margin-top:30px;",
          actionButton("submit_btn", "✅ 提交并保存", 
                       class = "btn-lg btn-success mr-2",
                       style = "padding:10px 30px; border-radius:8px;"),
          actionButton("reset_btn", "🔄 清空重置", 
                       class = "btn-lg btn-default",
                       style = "padding:10px 30px; border-radius:8px;"),
          actionButton("btn_predict_now", "📊 开始风险预测", 
                       class = "btn-lg btn-primary",
                       style = "padding:10px 30px; border-radius:8px; margin-left:10px;")
      )
    )
  )
}
