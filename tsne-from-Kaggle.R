library(data.table)
library(Rtsne)
library(ggplot2)

library(ggrepel)
cccol(cor_out),
                    perplexity = 322, #floor((ncol(cor_out)-1)/3)
                    theta = 0.00,
                    check_duplicates = FALSE,
                    pca = FALSE,
                    max_iter = 1350,
                    verbose = TRUE,
                    is_distance = FALSE)
corMatrix_out <- as.data.frame(tsne_model$Y)
cor_kmeans <- kmeans(corMatrix_out, centers = 5, iter.max = 10, nstart = 3)
corMatrix_outclust <- as.factor(c(cor_kmeans$cluster[1:968], 6))
corMatrix_names <- colnames(cor_out)
ggplot(corMatrix_out, aes(x = V1, y = V2,
                          color = corMatrix_outclust, shape = corMatrix_outclust))+ geom_point(size = 2.5)+ geom_rug() + stat_ellipse(type = "norm") + ggtitle("T-SNE of Features") + xlab("X") + ylab("Y") + labs(color = "Cluster", shape = "Cluster") + geom_text_repel(aes(x = V1, y = V2, label = corMatrix_names), size = 2.8)
