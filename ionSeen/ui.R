library(shiny)

# some useful functions 
## 选择是否去除异常值
checkboxInput.outRemover <- function(){
  checkboxInput("outRemover", div("Remove Outlier", style = "font-weight: bold;"),value = FALSE)
}
## 选择去除异常值系数
sliderInput.outCoef <- function(){
  sliderInput("outCoef", "Outlier Coef", value = 5, min = 3, max = 8)
}

## main UI ####
shinyUI(
  fluidPage(
    sidebarLayout(
      ## side panel ####
      sidebarPanel(
        tabsetPanel(
          id = "tabsetSide",
          # 侧边栏第一页，用于数据输入和整体分析
          tabPanel(title = "Input",
                   # upload files
                   fileInput("upload_ion", "Upload Ion Result"),
                   fileInput("upload_table", "Upload Sample Table"),
                   # select plot type
                   radioButtons('plot', 'General View',
                                c(Heatmap='heatmap',
                                  PCA='pca'),
                                'heatmap'),
                   #fluidRow(column(6, "Fuild6"), column(6, "Fluid 6-2"))
          ),
          ## 侧边栏第二页####
          ## 用于选择一些元素与样本进行分析
          tabPanel(title = "Discovery",
                   value = "Discovery",
                   # 选择元素进行分析
                   uiOutput("elementSelector"),
                   # 添加重置按钮1
                   #actionButton("resetION1", "Reset"),
                   # 选择分析的样本
                   uiOutput("idSelector"),
                   # 选择是否去除异常值，默认不去除
                   checkboxInput.outRemover(),
                   sliderInput.outCoef(),
          ),
          ## 侧边栏第三页 ####
          ## 选择单个元素画柱状图
          tabPanel(title = "Barplot",
                   value = "Barplot",
                   # 选择单个元素
                   uiOutput("singleElementSelector"),
                   # 选择样本
                   uiOutput("idSelector2"),
                   # 选择是否去除异常值，默认不去除
                   checkboxInput.outRemover(),
                   sliderInput.outCoef(),
          ),
          type = "tabs"
        ),
      ),
      ## main panel ####
      mainPanel(
        tabsetPanel(
          id = "tabsetMain",
          tabPanel("Input Data Viewer",
                   tableOutput("head")),
          tabPanel("General View",
                   plotOutput("content")),
          tabPanel("Discovery", plotOutput("partPlot")),
          tabPanel("Barplot", plotOutput("barPlot")),
          type = "tabs"
        )
      )
    )
  )
  )