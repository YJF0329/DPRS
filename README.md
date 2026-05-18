
🧠 抑郁障碍复发风险预测系统 (DRRPS)
Depression Recurrence Risk Prediction System
基于 R-Shiny 开发的抑郁障碍复发风险智能预测与可解释分析平台，整合逻辑回归、随机森林、SVM 三种经典机器学习模型，支持临床数据输入、复发风险预测、SHAP 可解释性可视化、结果导出等功能，界面轻量化、操作简单，适用于临床辅助评估与科研分析。
✨ 项目特色
- 多模型融合预测：集成 Logistic、RandomForest、SVM 三种机器学习模型，适配不同临床数据场景
- 风险量化评估：内置最优截断阈值，精准划分抑郁复发高/低风险等级
- 可解释性分析：支持 SHAP 可视化分析，直观展示各临床特征对复发风险的影响权重
- 完整交互功能：数据上传、模型预测、动态可视化、结果报表导出一站式服务
- 轻量化部署：基于原生 Shiny 开发，无需复杂配置，本地一键启动运行
- 模块化架构：代码分层规范，UI 与服务端逻辑分离，易于二次开发与迭代
📁 项目目录结构（规范版）
DRRPS/
├── APP.R                 # 项目主启动入口文件
├── modules/              # 前后端模块化代码
│   ├── ui_input.R        # 数据上传与参数输入界面
│   ├── ui_predict.R      # 模型预测结果展示界面
│   ├── ui_shap.R         # SHAP可解释性分析界面
│   ├── ui_export.R       # 结果导出界面
│   ├── server_input.R    # 输入数据后端逻辑
│   ├── server_predict.R  # 模型预测后端逻辑
│   ├── server_shap.R     # SHAP可视化后端逻辑
│   └── server_export.R   # 报表导出后端逻辑
├── models/               # 预训练模型与阈值文件
│   ├── logit_model.rds
│   ├── randomForest_model.rds
│   ├── svm_model.rds
│   └── threshold.rds
└── scripts/              # 模型训练原始脚本
    ├── logistic/
    ├── RandomForest/
    └── SVM/
🛠️ 运行环境与依赖包
本项目基于 R 4.0+ 开发，所需依赖包如下，首次运行可直接批量安装：
# 批量安装依赖包
packages <- c("shiny","shinydashboard","shinyjs","shinyalert","waiter",
              "plotly","DT","randomForest","e1071","shinydashboardPlus")
install.packages(packages)

# 加载运行所需包
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
🚀 快速启动
1. 下载本项目全部文件，解压至本地文件夹
2. 打开 R/RStudio，设置工作目录为项目根目录
3. 运行主程序文件：APP.R
4. 自动唤起浏览器页面，即可使用全部预测与分析功能
📌 核心功能模块
1. 数据输入模块
支持手动录入临床特征数据或批量上传数据集，内置数据校验功能，避免无效数据输入。
2. 多模型复发预测
调用三种预训练机器学习模型，自动计算抑郁障碍复发概率，结合最优阈值输出风险分级结果。
3. SHAP可解释性可视化
动态展示单样本/全样本特征贡献度、特征重要性排序，解决机器学习模型“黑箱”问题，贴合临床科研需求。
4. 结果导出模块
支持预测结果表格、可视化图表、分析报告的本地导出，方便科研汇总与临床存档。
📄 适用场景
- 精神科临床复发风险辅助筛查
- 抑郁障碍预后评估科研分析
- 机器学习临床可解释性教学与二次开发
📝 开源说明
本项目为学术开源项目，仅供科研与学习使用，未经授权禁止用于商业用途。欢迎 Star、Fork 与二次改进，如有问题可提交 Issue 交流。
⭐ Star History
如果本项目对你有帮助，欢迎点亮 Star，感谢支持！
