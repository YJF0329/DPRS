ui_shap <- function() {
  tagList(
    box(
      title = div(icon("brain"), " SHAP 模型解释（基于随机森林）"),
      width = 12,
      status = "primary",
      solidHeader = TRUE,
      elevation = 3,
      collapsible = FALSE,
      
      # 按钮区域（美观居中）
      fluidRow(
        column(
          width = 12,
          div(
            style = "text-align: center; margin: 10px 0 30px 0;",
            actionButton(
              "shap_btn", 
              "计算 SHAP 值", 
              class = "btn-lg btn-primary",
              icon = icon("calculator"),
              style = "padding: 10px 35px; font-size: 16px; border-radius: 8px;"
            )
          )
        )
      ),
      
      # 展示区域
      fluidRow(
        column(
          width = 12,
          uiOutput("shap_display")
        )
      )
    )
  )
}
