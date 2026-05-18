library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyalert)
library(waiter)
library(plotly)
library(DT)
library(randomForest)
library(e1071)
library(shinydashboardPlus)

source("modules/ui_Input.R")
source("modules/ui_predict.R")
source("modules/ui_shap.R")
source("modules/ui_export.R")

source("modules/server_input.R")
source("modules/server_predict.R")
source("modules/server_shap.R")
source("modules/server_export.R")

ui <- dashboardPage(
  # 顶部导航栏（全新深蓝色）
  dashboardHeader(title = "抑郁障碍复发风险险预测系统",
                  titleWidth = 250),
  
  # 侧边栏（全新浅蓝配色）
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs",
      menuItem("患者信息录入", tabName = "input", icon = icon("user-plus")),
      menuItem("风险预测", tabName = "predict", icon = icon("chart-line")),
      menuItem("结果解释", tabName = "shap", icon = icon("chart-bar")),
      menuItem("导出报告", tabName = "export", icon = icon("file-pdf"))
    ),
    # 侧边栏背景色
    tags$style(HTML(".main-sidebar { background-color: #2A508C !important; }"))
  ),
  
  # 主体内容（全新清爽配色）
  dashboardBody(
    useShinyjs(),
    use_waiter(),
    tags$head(
      tags$style(HTML("
        /* 整体背景 */
        .content-wrapper, .right-side {
          background-color: #F5F7FA !important;
        }
        
        /* 顶部导航栏 */
        .main-header {
          background-color: #1A3A6B !important;
        }
        
        /* 侧边栏菜单选中颜色 */
        .sidebar-menu > li.active > a {
          background-color: #427D9D !important;
          border-left-color: #9BBEC8 !important;
        }
        
        /* 侧边栏菜单 hover 效果 */
        .sidebar-menu > li:hover > a {
          background-color: #326282 !important;
        }
        
        /* 卡片样式 */
        .box {
          border-radius: 12px !important;
          box-shadow: 0 4px 10px rgba(0,0,0,0.08) !important;
          border-top: 3px solid #427D9D !important;
        }
        
        /* 按钮主色 */
        .btn-primary {
          background-color: #427D9D !important;
          border-color: #427D9D !important;
          border-radius: 6px !important;
        }
        
        /* 按钮 hover */
        .btn-primary:hover {
          background-color: #326282 !important;
          border-color: #326282 !important;
        }
        
        /* 输入框样式 */
        .form-control {
          border-radius: 6px !important;
          border: 1px solid #DDE6ED !important;
        }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "input", ui_input()),
      tabItem(tabName = "predict", ui_predict()),
      tabItem(tabName = "shap", ui_shap()),
      tabItem(tabName = "export", ui_export())
    )
  )
)

server <- function(input, output, session) {
  server_input(input, output, session)
  server_predict(input, output, session)
  server_shap(input, output, session)
  server_export(input, output, session)
}

shinyApp(ui, server)
