ui_predict <- function() {
  tagList(
    box(
      title = div(icon("clipboard-check"), " 风险预测"),
      width = 12,
      status = "primary",    # 改为高级蓝色
      solidHeader = TRUE,
      elevation = 3,        # 增加卡片阴影
      collapsible = FALSE,
      
      # 按钮区域（居中、更美观）
      div(
        style = "text-align: center; margin: 10px 0 30px 0;",
        actionButton(
          "predict_btn", 
          "开始预测", 
          class = "btn-lg btn-primary",
          icon = icon("chart-line"),
          style = "padding: 10px 30px; font-size: 16px; border-radius: 8px;"
        )
      ),
      
      # 预测结果展示
      uiOutput("risk_score_display")
    )
  )
}