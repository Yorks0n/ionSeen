# A module to load uploaded data
inputDataPanelUI <- function(id){
  tagList(
    fileInput(NS(id, "upload_ion"), "Upload Ion Result"),
    fileInput(NS(id, "upload_table"), "Upload Sample Table"),
    #textOutput(NS(id, "demoText"))
  )
}

inputDataPanelServer <- function(id){
  moduleServer(id, function(input, output, session){
    load_file <- function(name, path){
      ext <- tools::file_ext(name)
      switch(ext,
             csv = read.csv(path, sep = ","),
             tsv = read.csv(path, sep = "\t"),
             validate("Invalid file; Please upload a .csv or .tsv file"))
    }
    # First. read data and sampleTable in
    # 需要保证sampleTable中的顺序与测离子计算结果中的顺序对应好
    upload_ion_re <- reactive(input$upload_ion)
    upload_table_re <- reactive(input$upload_table)
    req(upload_ion_re())
    req(upload_table_re())
    icpppm <- load_file(input$upload_ion$name, input$upload_ion$datapath)
    sampleTable <- load_file(input$upload_table$name, input$upload_table$datapath)
    # 将第一列改变为sample名称
    icpppm[,1] <- sampleTable[,2]
    # 修改元素列名称
    colnames(icpppm) <- str_extract(colnames(icpppm), "[A-z]{1,}")
    colnames(icpppm)[1] <- "ID"
    
    # demo output
    output$demoText <- renderText(colnames(icpppm))
  })
}