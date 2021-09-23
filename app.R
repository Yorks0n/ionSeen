library(shiny)
# 加载过程中需要依赖的软件包
library(dplyr)
library(stringr)
library(ggplot2)
library(pheatmap)
library(reshape2)
## ui ####
ui <- fluidPage(
  sidebarLayout(
    ## side panel ####
    sidebarPanel(
      tabsetPanel(
        id = "tabset2",
        # 侧边栏第一页，用于数据输入和整体分析
        tabPanel("Input",
                 # upload files
                 fileInput("upload_ion", "Upload Ion Result"),
                 fileInput("upload_table", "Upload Sample Table"),
                 # select plot type
                 radioButtons('plot', 'General View',
                              c(Heatmap='heatmap',
                                PCA='pca'),
                              'heatmap')
                 ),
        ## 侧边栏第二页####
        ## 用于选择一些元素与样本进行分析
        tabPanel("Discovery",
                 # 选择元素进行分析
                 uiOutput("elementSelector"),
                 # 选择分析的样本
                 uiOutput("idSelector"),
                 # 选择是否去除异常值，默认不去除
                 checkboxInput("outRemover", div("Remove Outlier", style = "font-weight: bold;"),value = FALSE),
                 sliderInput("outCoef", "Outlier Coef", value = 5, min = 3, max = 8),
                 ),
        ## 侧边栏第三页 ####
        ## 选择单个元素画柱状图
        tabPanel("Barplot",
                 # 选择单个元素
                 uiOutput("singleElementSelector"),
                 # 选择样本
                 uiOutput("idSelector2"),
                 # 选择是否去除异常值，默认不去除
                 checkboxInput("outRemover", div("Remove Outlier", style = "font-weight: bold;"),value = FALSE),
                 sliderInput("outCoef", "Outlier Coef", value = 5, min = 3, max = 8),
                 ),
        type = "tabs"
      ),
    ),
    ## main panel ####
    mainPanel(
      tabsetPanel(
        id = "tabset",
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
## server #### 
server <- function(input, output, session) {
  output$panel <- renderText({
    paste("Current selection: ", input$plot)
  })
  # 上传文件并读取数据
  getIonInput <- reactive({
    # First. read data and sampleTable in
    # 需要保证sampleTable中的顺序与测离子计算结果中的顺序对应好
    req(input$upload_ion)
    req(input$upload_table)
    icpppm <- read.csv(input$upload_ion$datapath)
    sampleTable <- read.csv(input$upload_table$datapath)
    # 将第一列改变为sample名称
    icpppm[,1] <- sampleTable[,2]
    # 修改元素列名称
    colnames(icpppm) <- str_extract(colnames(icpppm), "[A-z]{1,}")
    colnames(icpppm)[1] <- "ID"
    icpppm
  })
  getIonMatrix <- reactive({
    icpppm <- getIonInput()
    icp_matrix <- as.matrix(icpppm[,-1])
    rownames(icp_matrix) <- icpppm[,1]
    icp_matrix
  })
  
  output$content <- renderPlot({
    icpppm <- getIonInput()
    icp_matrix <- getIonMatrix()

    # plot 
    # 根据选择绘图
    # 如果选择heatmap
    if(input$plot == "heatmap"){p <- pheatmap(icp_matrix, scale = "column")}
    
    # 如果选择PCA图
    if(input$plot == "pca"){
      # PCA
      pca <- prcomp(icp_matrix, scale. = TRUE)

      # 生成分组信息
      pca_result <-as.data.frame(pca$x)
      pca_result$genotype_group_factor <- factor(icpppm[,1])
      leg <- theme(title=element_text(size=15), 
                   axis.text.x=element_text(size=14),
                   axis.text.y=element_text(size=14),
                   legend.text=element_text(size=14))
      summ<-summary(pca)
      p <- ggplot(pca_result, aes(x=PC1,y=PC2, color = genotype_group_factor)) +
        geom_point(size=2) +
        stat_ellipse(aes(fill=genotype_group_factor),
                     type ="t", geom ="polygon",alpha=0.2,color=NA) +
        theme_minimal() +
        xlab(paste0("PC1(",round(summ$importance[2,1]*100,2),"%)")) +
        ylab(paste0("PC2(",round(summ$importance[2,2]*100,2),"%)")) +
        leg
    }
    # 返回给ui
    p
  })
  output$head <- renderTable({
    icpppm <- getIonInput()
    return(head(icpppm))
  })
  
  # create selection for Ion
  output$elementSelector <- renderUI({
    icpppm <- getIonInput()
    ionList <- colnames(icpppm)[-1]
    checkboxGroupInput("element", "Choose the element:",selected = ionList[1], as.list(ionList))
  })
  
  # choose sample ID to analysis
  output$idSelector <- renderUI({
    icpppm <- getIonInput()
    checkboxGroupInput("ID", "Choose the materials:", 
                       selected = unique(icpppm[,1])[1],
                       as.list(unique(icpppm[,1])))
  })
  
  # create selection ui for single ion
  output$singleElementSelector <- renderUI({
    icpppm <- getIonInput()
    ionList <- colnames(icpppm)[-1]
    selectInput("elementSingle", "Choose the element:",selected = ionList[1], as.list(ionList))
  })
  
  # choose sample ID to analysis 2
  output$idSelector2 <- renderUI({
    icpppm <- getIonInput()
    checkboxGroupInput("ID2", "Choose the materials:", selected = unique(icpppm[,1]),
                       as.list(unique(icpppm[,1])))
  })
  
  ## Part analysis ####
  # 分析选中的元素与材料
  # 创建函数用于去除异常值
  outliner_remover <- function(df, remove_outliner = FALSE, outliner_coef = 5,
                               scale = FALSE){
    if ((remove_outliner == FALSE)&(scale == FALSE)) {return(df)}
    if (remove_outliner == TRUE) {
      temp_matrix <- df[,-1]
      temp_matrix[abs(scale(temp_matrix))>outliner_coef] <- NA
      df[,-1] <- temp_matrix
    }
    if (scale == FALSE) {return(df)}
    if (scale == TRUE) {
      temp_matrix <- df[,-1]
      df[,-1] <- scale(temp_matrix)
      return(df)
    }
  }
  
  output$partPlot <- renderPlot({
    icpppm <- getIonInput()
    icp_matrix <- getIonMatrix()
    # 获取选择的元素
    choosed_element <- input$element
    # 获取选择的样本
    choosed_ID <- input$ID
    
    # 多元素绘图
    icpppm_out_removed <- outliner_remover(icpppm, 
                                           remove_outliner = input$outRemover, 
                                           outliner_coef = input$outCoef, 
                                           scale = TRUE)
    icpppm_melt <- melt(icpppm_out_removed, 
                        id.vars = "ID", 
                        variable.name = "element",
                        value.name = "content") %>% na.omit()
    p <- icpppm_melt %>%
      filter(ID %in% choosed_ID) %>%
      filter(element %in% choosed_element) %>%
      ggplot(aes(x = element, y = content, fill = ID)) +
      geom_boxplot() 
    
    # return plot
    p
  })
  
  # 选择单个元素绘制柱状图
  output$barPlot <- renderPlot({
    icpppm <- getIonInput()
    icp_matrix <- getIonMatrix()
    # 获取选择的元素
    choosed_element <- input$elementSingle
    # 获取选择的样本
    choosed_ID <- input$ID2
    
    icpppm_out_removed <- outliner_remover(icpppm, 
                                           remove_outliner = input$outRemover, 
                                           outliner_coef = input$outCoef, 
                                           scale = FALSE)
    icpppm_melt <- melt(icpppm_out_removed, 
                        id.vars = "ID", 
                        variable.name = "element",
                        value.name = "content") %>% na.omit()
    icpppm_summarise <- icpppm_melt %>% 
      group_by(ID, element) %>% 
      summarise(mean_content = mean(content), se = sd(content)/sqrt(n()))
    # plot bar 
    p <- icpppm_summarise %>%
      filter(element == choosed_element) %>%
      filter(ID %in% choosed_ID) %>%
      ggplot(aes(x = ID, y = mean_content)) + 
      geom_bar(position = position_dodge(0.7), stat = "identity", width = 0.5)+
      geom_errorbar(aes(x=ID, ymin=mean_content-se, ymax=mean_content+se), width=0.2, colour="black", alpha=0.7, size=1) +
      theme_minimal() +
      theme(text = element_text(size = 20))
    p
  })


}
shinyApp(ui, server)
