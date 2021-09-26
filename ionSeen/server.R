library(shiny)

# 加载过程中需要依赖的软件包
library(dplyr, warn.conflicts = FALSE)
library(stringr)
library(ggplot2)
library(pheatmap)
library(reshape2)
library(agricolae)
options(stringsAsFactors = FALSE)


## main server ####
shinyServer(function(input, output, session){
  # 根据Panel选择自动切换页面
  observeEvent(input$tabsetSide, {
    updateTabsetPanel(session, "tabsetMain",
                      selected = input$tabsetSide)
  })
  observeEvent(input$tabsetMain, {
    updateTabsetPanel(session, "tabsetSide",
                      selected = input$tabsetMain)
  })
  
  # 定义一些ggplot绘图参数
  leg <- theme(title=element_text(size=15), 
               axis.text.x=element_text(size=14),
               axis.text.y=element_text(size=14),
               legend.text=element_text(size=14))
  
  # 上传文件并读取数据
  getIonInput <- reactive({
    # First. read data and sampleTable in
    # 需要保证sampleTable中的顺序与测离子计算结果中的顺序对应好
    req(input$upload_ion)
    req(input$upload_table)
    icpppm <- load_file(input$upload_ion$name, input$upload_ion$datapath)
    sampleTable <- load_file(input$upload_table$name, input$upload_table$datapath)
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
      summ<-summary(pca)
      p <- ggplot(pca_result, aes(x=PC1,y=PC2, color = genotype_group_factor)) +
        geom_point(size=2) +
        stat_ellipse(aes(fill=genotype_group_factor),
                     type ="t", geom ="polygon",alpha=0.2,color=NA) +
        theme_minimal() +
        xlab(paste0("PC1(",round(summ$importance[2,1]*100,2),"%)")) +
        ylab(paste0("PC2(",round(summ$importance[2,2]*100,2),"%)")) +
        leg + coord_fixed()
    }
    # 返回给ui
    p
  })
  output$head <- renderTable({
    icpppm <- getIonInput()
    return(head(icpppm))
  })
  
  # create selection for Ion1
  output$elementSelector <- renderUI({
    icpppm <- getIonInput()
    ionList <- colnames(icpppm)[-1]
    checkboxGroupInput("element", "Choose the element:",selected = ionList[1], as.list(ionList))
  })
  # adapt for reset ION1
  observeEvent(input$resetION1,{
    icpppm <- getIonInput()
    ionList <- colnames(icpppm)[-1]
    updateCheckboxGroupInput(inputId = "element", selected = ionList[1])
  })
  # choose sample ID to analysis
  output$idSelector <- renderUI({
    icpppm <- getIonInput()
    idList <- unique(icpppm[,1])
    checkboxGroupInput("ID", "Choose the materials:", 
                       selected = idList[1],
                       as.list(idList))
  })
  # adapt for reset ID1
  observeEvent(input$resetID1,{
    icpppm <- getIonInput()
    idList <- unique(icpppm[,1])
    updateCheckboxGroupInput(inputId = "ID", selected = idList[1])
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
    idList <- unique(icpppm[,1])
    checkboxGroupInput("ID2", "Choose the materials:", 
                       selected = idList[1],
                       as.list(idList))
  })
  # adapt for reset ID2
  observeEvent(input$resetID2,{
    icpppm <- getIonInput()
    idList <- unique(icpppm[,1])
    updateCheckboxGroupInput(inputId = "ID2", selected = idList[1])
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
    df_to_plot <- icpppm_melt %>%
      filter(ID %in% choosed_ID) %>%
      filter(element %in% choosed_element)
    coord_ratio <- 1
    if(nrow(df_to_plot) > 0){
      coord_ratio <- length(choosed_ID) / (max(df_to_plot$content) - min(df_to_plot$content)) / length(choosed_element)
    }
    p <- df_to_plot %>%
      ggplot(aes(x = element, y = content, fill = ID)) +
      geom_boxplot() +
      # coord_fixed(ratio = coord_ratio) +
      leg
    
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
                                           remove_outliner = input$outRemover2, 
                                           outliner_coef = input$outCoef2, 
                                           scale = FALSE)
    icpppm_melt <- melt(icpppm_out_removed, 
                        id.vars = "ID", 
                        variable.name = "element",
                        value.name = "content") %>% na.omit()
    
    icpppm_summarise <- icpppm_melt %>% 
      group_by(ID, element) %>% 
      summarise(mean_content = mean(content), se = sd(content)/sqrt(n()))
    # 如果ID长度等于1，随便画画
    if((length(choosed_ID) == 1)&(length(choosed_element) == 1)) {
      # plot bar 
      df_to_plot <- icpppm_summarise %>%
        filter(element == choosed_element) %>%
        filter(ID %in% choosed_ID) 
      coord_ratio <- length(unique(df_to_plot$ID)) / mean(df_to_plot$mean_content)
      p <- df_to_plot %>% 
        ggplot(aes(x = ID, y = mean_content)) + 
        geom_bar(position = position_dodge(0.7), stat = "identity", width = 0.5)+
        geom_errorbar(aes(x=ID, ymin=mean_content-se, ymax=mean_content+se), width=0.2, colour="black", alpha=0.7, size=1) +
        theme_minimal() +
        #coord_fixed(ratio = coord_ratio) +
        leg
    }
    
    # 如果ID length 等于2，进行welch t.test
    if((length(choosed_ID) == 2)&(length(choosed_element) == 1)) {
      df_for_ttest <- icpppm_melt %>% 
        filter(element %in% choosed_element) %>%
        filter(ID %in% choosed_ID)
      
      ttest.res <- t.test(content ~ ID, data = df_for_ttest)
      ttest_summarise <- icpppm_summarise %>%  
        filter(element %in% choosed_element) %>%
        filter(ID %in% choosed_ID)
      
      # 添加ttest结果的标注
      ttest_text <- paste0("t.test p value = ", round(ttest.res$p.value, 4))
      coord_ratio <- 1
      if(nrow(ttest_summarise) > 0){
        coord_ratio <- length(unique(ttest_summarise$ID)) / mean(ttest_summarise$mean_content)
      }
      p <- ttest_summarise %>%
        ggplot(aes(x = ID, y = mean_content)) + 
        geom_bar(position = position_dodge(0.7), stat = "identity", width = 0.5)+
        geom_errorbar(aes(x=ID, ymin=mean_content-se, ymax=mean_content+se), width=0.2, colour="black", alpha=0.7, size=1) +
        theme_minimal() +
        xlab(ttest_text) +
        #coord_fixed(ratio = coord_ratio) +
        leg 
    }
    # 如果ID数量达到3才进行ANOVA
    if((length(choosed_ID) >= 3)&(length(choosed_element) == 1)) {
      df_for_ANOVA <- icpppm_melt %>% 
        filter(element %in% choosed_element) %>%
        filter(ID %in% choosed_ID)
      oneway <- aov(content ~ ID, data = df_for_ANOVA)
      anova.res <- anova(oneway)
      
      # LSD
      LSD.result <- LSD.test(oneway, trt = "ID", p.adj = "none")
      LSD.marker <- LSD.result$groups
      
      # 总结数据并绘图
      AOV_summarise <- icpppm_summarise %>%  
        filter(element %in% choosed_element) %>%
        filter(ID %in% choosed_ID)
      AOV_summarise$marker <- LSD.marker[as.character(AOV_summarise$ID),2]
      coord_ratio <- 1
      if(nrow(AOV_summarise) > 0 ){
        coord_ratio <- length(unique(AOV_summarise$ID)) / mean(AOV_summarise$mean_content)
        }
      # plot
      if(anova.res[1,5] < 0.05){AOV_text <- "ANOVA test significant"}
      if(anova.res[1,5] >= 0.05){AOV_text <- "ANOVA test insignificant"}
      
      p <- AOV_summarise %>%
        ggplot(aes(x = ID, y = mean_content)) + 
        geom_bar(position = position_dodge(0.7), stat = "identity", width = 0.5)+
        geom_errorbar(aes(x=ID, ymin=mean_content-se, ymax=mean_content+se), width=0.2, colour="black", alpha=0.7, size=1) +
        geom_text(aes(x = ID, y = mean_content+se*2, label = marker), size = 8)+
        theme_minimal() +
        xlab(AOV_text) +
        #coord_fixed(ratio = coord_ratio) +
        leg
    }
    p
  })
  #generalViewServer("GV", getIonInput = getIonInput, getIonMatrix = getIonMatrix)
  #generalViewServerDemo("GV", passed = "Try")
  #inputDataPanelServer("dataIn")
})