## Server ####
# some useful functions
## 将读取文件的过程分离出来
load_file <- function(name, path){
  ext <- tools::file_ext(name)
  switch(ext,
         csv = read.csv(path, sep = ","),
         tsv = read.csv(path, sep = "\t"),
         validate("Invalid file; Please upload a .csv or .tsv file"))
}


## UI #### 
## 选择是否去除异常值
checkboxInput.outRemover <- function(id){
  checkboxInput(id, div("Remove Outlier", style = "font-weight: bold;"),value = FALSE)
}
## 选择去除异常值系数
sliderInput.outCoef <- function(id){
  sliderInput(id, "Outlier Coef", value = 5, min = 3, max = 8)
}