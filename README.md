# 🧠 Depression Recurrence Risk Prediction System (DRRPS)

> 基于 R-Shiny 开发的抑郁障碍复发风险智能预测与可解释分析平台  
> An interpretable machine learning platform for predicting depression recurrence risk using R-Shiny.

---
#🔗 在线演示地址 
> Demo：https://019e3aa2-561d-00c2-9c23-d05db794d346.share.connect.posit.cloud/

## ✨ Features

### 🔹 Multi-model Prediction
- Integrated Logistic Regression, Random Forest, and Support Vector Machine (SVM)
- Adaptable to different clinical data scenarios

### 🔹 Risk Quantification
- Built-in optimal cutoff threshold
- Automatic high-/low-risk stratification

### 🔹 Explainable AI (SHAP)
- SHAP-based feature contribution visualization
- Enhanced interpretability for clinical machine learning models

### 🔹 Interactive Workflow
- Clinical data upload
- Real-time prediction
- Dynamic visualization
- Exportable reports

### 🔹 Lightweight Deployment
- Pure Shiny-based implementation
- No complicated environment configuration required

### 🔹 Modular Architecture
- Separated UI and server logic
- Easy maintenance and secondary development

---

# 📁 Project Structure

```bash
DRRPS/
├── APP.R                         # Main application entry
├── modules/                      # Modular UI & server scripts
│   ├── ui_input.R                # Data input interface
│   ├── ui_predict.R              # Prediction result interface
│   ├── ui_shap.R                 # SHAP visualization interface
│   ├── ui_export.R               # Export interface
│   ├── server_input.R            # Backend logic for data input
│   ├── server_predict.R          # Backend logic for prediction
│   ├── server_shap.R             # Backend logic for SHAP analysis
│   └── server_export.R           # Backend logic for report export
│
├── models/                       # Pretrained models & thresholds
│   ├── logit_model.rds
│   ├── randomForest_model.rds
│   ├── svm_model.rds
│   └── threshold.rds
│
└── scripts/                      # Original training scripts
    ├── logistic/
    ├── RandomForest/
    └── SVM/
```

---

# 🛠️ Environment & Dependencies

## R Version
- R >= 4.0.0

## Install Required Packages

```r
packages <- c(
  "shiny",
  "shinydashboard",
  "shinyjs",
  "shinyalert",
  "waiter",
  "plotly",
  "DT",
  "randomForest",
  "e1071",
  "shinydashboardPlus"
)

install.packages(packages)
```

## Load Libraries

```r
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
```

---

# 🚀 Quick Start

## 1. Clone Repository

```bash
git clone https://github.com/YJF0329/DRRPS.git
```

## 2. Open Project in RStudio

Set the working directory to the project root folder.

## 3. Run Application

```r
source("APP.R")
```

## 4. Launch Web Interface

The browser page will automatically open after startup.

---

# 📌 Core Functional Modules

## 1️⃣ Data Input Module

Supports:
- Manual clinical feature input
- Batch dataset upload
- Built-in data validation

---

## 2️⃣ Multi-model Prediction Module

Integrated models:
- Logistic Regression
- Random Forest
- Support Vector Machine (SVM)

Outputs:
- Recurrence probability
- Risk classification
- Threshold-based prediction result

---

## 3️⃣ SHAP Explainability Module

Supports:
- Single-sample feature contribution analysis
- Global feature importance ranking
- Dynamic SHAP visualization

Helps address the “black-box” problem in machine learning.

---

## 4️⃣ Export Module

Export:
- Prediction tables
- Visualization figures
- Analysis reports

Suitable for:
- Clinical archiving
- Research summary
- Publication materials

---
