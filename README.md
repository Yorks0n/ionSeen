# ionSeen使用说明

网址：http://58.87.96.81:3838/ionSeen/



## 准备输入文件

1. 测离子计算后的输出结果文件（.csv格式）<img src="/Users/chaolab/Library/Application Support/typora-user-images/image-20210922165729174.png" alt="image-20210922165729174" style="zoom:75%;" />
2. 样品编号，与测离子输出文件要顺序一致（.csv格式）<img src="/Users/chaolab/Library/Application Support/typora-user-images/image-20210922165902283.png" alt="image-20210922165902283" style="zoom:50%;" />



## 使用

**Input**

上传数据，如果上传后正常会在右侧显示一部分数据。

![image-20210922170101612](/Users/chaolab/Library/Application Support/typora-user-images/image-20210922170101612.png)



**Discovery**

选中左侧与右侧Discovery后，可以选择要观察的样本与离子，右侧展示绘图

![image-20210922170238152](/Users/chaolab/Library/Application Support/typora-user-images/image-20210922170238152.png)



**Barplot**

左侧与右侧都选择**Barplot**即可进行单元素绘图。选中一种元素，选择多个样品。如果选择两个材料，使用Welch t.test计算显著性；如果选择三个及以上材料，进行单因素方差分析，并用LSD法进行多重比较【注：如果ANOVA下面显示insignificant则上方标记字母结果无意义】。

![image-20210922171632288](/Users/chaolab/Library/Application Support/typora-user-images/image-20210922171632288.png)