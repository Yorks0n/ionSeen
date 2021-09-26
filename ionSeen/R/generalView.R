generalViewPanelUI <- function(id){
  tagList(
    radioButtons(NS(id, 'plot2'), 'General View',
                       c(Heatmap='heatmap',
                         PCA='pca'),
                       'heatmap'), 
    #textOutput(NS(id, "demoText"))
    )
}
generalViewPlotUI <- function(id){
  tagList(
    plotOutput(NS(id, "generalPlot")),
)
}
generalViewServerDemo <- function(id, passed = NULL){
  moduleServer(id, function(input, output, session){
    output$demoText <- renderText(passed)
  })
}
generalViewServer <- function(id, getIonInput = NULL, getIonMatrix = NULL){
  moduleServer(id, function(input, output, session){
    icpppm <- getIonInput()
    icp_matrix <- getIonMatrix()
    leg <- theme(title=element_text(size=15), 
                 axis.text.x=element_text(size=14),
                 axis.text.y=element_text(size=14),
                 legend.text=element_text(size=14))
    # 根据选择绘图
    # 如果选择heatmap
    p <- reactive({
      if(input$plot2 == "heatmap"){pheatmap(icp_matrix, scale = "column")}
      # 如果选择PCA图
      if(input$plot2 == "pca"){
        # PCA
        pca <- prcomp(icp_matrix, scale. = TRUE)
        # 生成分组信息
        pca_result <-as.data.frame(pca$x)
        pca_result$genotype_group_factor <- factor(icpppm[,1])
        summ<-summary(pca)
        ggplot(pca_result, aes(x=PC1,y=PC2, color = genotype_group_factor)) +
          geom_point(size=2) +
          stat_ellipse(aes(fill=genotype_group_factor),
                       type ="t", geom ="polygon",alpha=0.2,color=NA) +
          theme_minimal() +
          xlab(paste0("PC1(",round(summ$importance[2,1]*100,2),"%)")) +
          ylab(paste0("PC2(",round(summ$importance[2,2]*100,2),"%)")) +
          leg
      }
    })

    output$generalPlot <- renderPlot(p())
  })
}