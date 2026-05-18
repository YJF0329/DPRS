ui_export <- function() {
  tagList(
    box(
      title = div(icon("file-pdf"), " 导出PDF报告"),
      width = 12,
      status = "primary",
      solidHeader = TRUE,
      elevation = 3,
      
      fluidRow(
        column(12, align = "center",
               div(style = "margin-bottom:20px;",
                   actionButton("preview_pdf_btn", "刷新预览", 
                                icon = icon("eye"), class = "btn-lg btn-info",
                                style = "padding:8px 22px; border-radius:8px;"),
                   downloadButton("download_pdf", "📄 下载报告",
                                  style = "margin-left:10px; padding:8px 30px; font-size:16px; border-radius:8px;")
               )
        )
      ),
      
      h4(icon("eye"), " 报告预览", style = "margin-left:5px;"),
      div(
        style = "background:#fff; border:1px solid #ddd; border-radius:12px;
                padding:30px; height:700px; overflow-y:scroll;
                box-shadow:0 2px 8px rgba(0,0,0,0.08);",
        uiOutput("pdf_preview")
      )
    )
  )
}
