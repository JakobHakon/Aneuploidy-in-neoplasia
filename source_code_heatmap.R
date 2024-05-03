library("readxl")
library(Rtsne)
library(pheatmap)
library(data.table)
library(tidyverse)
library(pheatmap)
library(stringr)
library(genefilter)
library(pvclust)
## read in data from excel sheet
ano = read_excel("File.xlsx", sheet = 'Annotations') %>% 
  dplyr::select(Abbreviations, Category, Final) # read in annotations
a = read_excel("File.xlsx", sheet = 'all') %>%
  column_to_rownames('Chrom') # read in data
########### color selector ####
martin_k_color_reader <- function(n_colors){
  color_df <- read.table(str_c("/Users/jhofvand/Dropbox/UBC/colors/colors." , n_colors, ".txt")) %>%
    dplyr::select(V3,V4,V5)
  color_vec <- 1:n_colors %>% lapply(function(x){rgb(color_df[x,], maxColorValue = 255)}) %>% unlist
  return(color_vec)
}
color_vec_8 <- martin_k_color_reader(8)
color_vec_9 = c("white", color_vec_8)
names(color_vec_9) <- unique(ano$Category)
mycolors <- list(Category = color_vec_9,
                 group = c('<45' = "snow4", '45-47' = "aquamarine3", '48-57' = "azure3", '> 57' = "darkred"))

### select labels fir columns
col.lable = ano %>% 
  dplyr::select(Abbreviations, Final) %>% 
  column_to_rownames('Abbreviations')

ano2 = ano %>% dplyr::select(Abbreviations, Category) %>% column_to_rownames('Abbreviations') # select annotations for column

### set color breaks for heatmap
bk1 <- c(seq(-3,-0.995,by=0.1))
bk2 <- c(seq(-0.995, 2.01,by=0.1))
bk <- c(bk1,bk2)  #combine the break limits for purpose of graphing

my_palette <- c(colorRampPalette(colors = c("darkblue", "#7A99AC"))(n = length(bk1)-1),
                c(colorRampPalette(colors = c("white", "yellow", "red"))(n = length(bk2)-1)))

### cluster and heatmap generation
heatmap = pheatmap(log10(a+0.001), 
                   show_rownames = FALSE, 
                   show_colnames = TRUE, 
                   cluster_rows = FALSE,
                   cluster_cols = TRUE,
                   clustering_distance_cols="euclidean", 
                   clustering_method="complete",
                   annotation_col = ano2,
                   annotation_colors = mycolors,
                   labels_col = col.lable$Final,
                   annotation_row = row.ano,
                   main = "method=complete, dist=euclidean",
                   fontsize = 8,
                   fontsize_row = 9,
                   treeheight_col = 50,
                   fontsize_col = 8,
                   border_color = "NA")

save_pheatmap_png <- function(x, filename, width=7500, height=2900, res = 600) {
  png(filename, width = width, height = height, res = res)
  grid::grid.newpage()
  grid::grid.draw(x$gtable)
  dev.off()
}

save_pheatmap_png(heatmap, "pHeatmap_complete_euclidean.png")


