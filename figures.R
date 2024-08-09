## ---------------------------
##
## Script name: Bibliometric figures for review
##
## Purpose of script: Make supplemental figures about deconvolution methods
##
## Author: Lucie Gaspard-Boulinc
##
## Date Created: 2024-30-07
##
## Copyright (c) Lucie Gaspard-Boulinc, 2024
## Email: lucie.gaspard-boulinc@curie.fr
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## Library ---------------------------
library(ggplot2)
library(ggimage)
library(readxl)
library(ggtextures)
library(magick)
library(tibble)
# Color theme: wesanderson for sure
library(wesanderson)

## Data ---------------------------
# Data 
deconvolution_review <- read.csv("~/Documents/Literature/Deconvolution_review/deconvolution_method_table.csv", sep=";")
deconvolution_review <- deconvolution_review[deconvolution_review$Title != "",]

## Fig A - Cumulative frequency ---------------------------

deconvolution_review$year <- paste0("20",sapply(strsplit(deconvolution_review$Publication.date, split='-'), "[[",2))

ggplot(deconvolution_review, aes(x=as.factor(year))) + geom_histogram(stat="count", fill="#0A9F9D") +
  xlab("Year of release") + ylab("Number of publications") + theme_light()

ggsave("./figures/cumulative_frequency_publication.svg", width=6, height=5)


## Fig B - Programming languages ---------------------------

logo_path <- "~/Documents/Literature/Deconvolution_review/img/"

progr <- deconvolution_review[deconvolution_review$Programming.language %in% c("Python", "R", "Python/R"),]

prog_sum <- as.data.frame(table(progr$Programming.language))
colnames(prog_sum) <- c("Lang", "Freq")

# restrict to Python and R at the moment

data <- tibble(
  count = prog_sum$Freq,
  lang = prog_sum$Lang,
  image = list(
    image_read(paste0(logo_path,"Python_logo.png")),
    image_read(paste0(logo_path,"mixed_logo.png")),
    image_read(paste0(logo_path,"R_logo.png"))
    
  )
)


ggplot(data, aes(lang, count, image = image)) +
  geom_isotype_col(
    img_width = grid::unit(4, "native"), img_height = NULL,
    ncol = NA, nrow = 1, hjust = 0, vjust = 0.5
  ) +
  coord_flip() + ylab("Number of methods") + xlab("Programming language")+
  theme_classic()

ggsave("./figures/programming_languages.svg", width=10, height=5)



## Fig C - Reference free or based ---------------------------

deconvolution_review$Reference.based...Reference.free <- ifelse(deconvolution_review$Reference.based...Reference.free == "Reference-based ", 
                                                                "Reference-based", deconvolution_review$Reference.based...Reference.free)

framework <- as.data.frame(table(deconvolution_review$Reference.based...Reference.free))
framework <- framework[order(framework$Freq),]
framework$Var1 <- factor(framework$Var1, levels = framework$Var1)

ggplot(framework, aes(y=as.factor(Var1), x=Freq)) + geom_histogram(stat="identity", fill="#F2AD00") +
  xlab("Number of method") + ylab("Use a single-cell reference") + theme_light() + theme(axis.text=element_text(size=16)) + 
  geom_text(aes(label = Freq), vjust = 0.5, hjust=-0.5,size = 5)

ggsave("./figures/reference_use.svg", width=10, height=5)


## Fig D- Frameworks ---------------------------

pal_asteroid <- wes_palette("AsteroidCity1",5,"discrete")

Category_levels <- as.data.frame(table(deconvolution_review$Category))
Category_levels <- Category_levels[order(Category_levels$Freq),]

Category <- as.data.frame(table(deconvolution_review$Category, deconvolution_review$Reference.based...Reference.free))
Category <- Category[order(Category$Freq),]
Category$Var1 <- factor(Category$Var1, levels=Category_levels$Var1)

ggplot(Category, aes(y=as.factor(Var1), x=Freq, fill=Var2)) + geom_histogram(stat="identity") +
  xlab("Number of method") + ylab("Framework") + scale_fill_manual(values=pal_asteroid[c(2,1,3)])+
  theme_light() + theme(axis.text=element_text(size=16), axis.title = element_text(size=28))

ggsave("./figures/framework_classification.svg", width=15, height=10)


## Fig E- Data synergy in deconvolution ---------------------------
colors_pal <- wesanderson::wes_palette("AsteroidCity1",5,"discrete")

input_data <- deconvolution_review[,c("ST.counts", "ST.coordinates", "Image", "Reference.based...Reference.free", "Method.name")]

# I split ref-based and ref-free alg. to make the Venn diagram 

input_data_ref <- input_data[input_data$Reference.based...Reference.free %in% c("Reference-based","Both"),]
input_data_free <- input_data[!input_data$Reference.based...Reference.free %in% c("Reference-based", "Both"),]

# function
get_sets <- function(df, ref_type){
  return(list(
    `Use Coordinates` = which(df$ST.coordinates == "Yes"),
    `Use Image` = which(df$Image == "Yes",),
    `Use scRNA-seq` = which(df$Reference.based...Reference.free %in% ref_type),
    `No scRNA-seq` =which(!df$Reference.based...Reference.free %in% ref_type)))
  
}


# Prepare the sets
sets_ref <- get_sets(input_data_ref, c("Reference-based","Both"))
sets_free <- get_sets(input_data_free, c("Reference-free"))#,"Both"))

## As Venn diagram
library(ggvenn)
ggvenn(sets_ref, c("Reference","Use_Coord", "Use_Image"), show_percentage = F, fill_color = colors_pal[c(3,1,2)], text_size=10, set_name_size=5) 
ggsave("./figures/venn_diagram_methods_ref_based.svg", width=15, height=10)

ggvenn(sets_free, c("Reference","Use_Coord", "Use_Image"), show_percentage = F, fill_color = colors_pal[c(4,1,2)], text_size=10, set_name_size=5) 
ggsave("./figures/venn_diagram_methods_ref_free.svg", width=15, height=10)

## As Upset plot
sets <- get_sets(input_data, c("Reference-based","Both"))
library(UpSetR)
upset(fromList(sets), order.by = "freq", decreasing = T, text.scale = c(2, 2, 1.5, 1.5, 2, 2))

## Do same but with framework information
framework_df <- data.frame(name=deconvolution_review$Category, index = seq_along(deconvolution_review$Category))
split_data <- split(framework_df$index, framework_df$name)
# merge with previous set
sets_test <- c(sets,split_data)

# Generate upset for each 
upset_list <- list()
for (set_name in names(split_data)){
  # select only Bayesian methods
  input_data_test <- deconvolution_review[deconvolution_review$Category == set_name,c("ST.counts", "ST.coordinates", "Image", "Reference.based...Reference.free", "Method.name")]
  # get sets
  sets <- get_sets(input_data_test, c("Reference-based","Both"))
  # subset 
  upset_list[[set_name]] <- upset(fromList(sets), order.by = "freq", decreasing = T, text.scale = c(2, 2, 1.5, 1.5, 2, 2), nsets=20)
}
# arrange
ggarrange(plotlist=upset_list, ncol=4, nrow=4)

## Figure F -  Data synergy per framework ---------------------------

## Ref free vs ref-based
Category_levels <- as.data.frame(table(deconvolution_review$Category))
Category_levels <- Category_levels[order(Category_levels$Freq),]

Category <- as.data.frame(table(deconvolution_review$Category, deconvolution_review$Reference.based...Reference.free))
Category <- Category[order(Category$Freq),]
Category$Var1 <- factor(Category$Var1, levels=Category_levels$Var1)

ggplot(Category, aes(y=as.factor(Var1), x=Freq, fill=Var2)) + geom_histogram(stat="identity") +
  xlab("Number of method") + ylab("Framework") + theme_light() + theme(axis.text=element_text(size=15), axis.title = element_text(size=20))

ggsave("./figures/reference_per_framework.svg", width=15, height=8)


## Image and Coordinate int
int_data <- as.data.frame(paste0(deconvolution_review$Image,deconvolution_review$ST.coordinates))
colnames(int_data) <- "int_data"
  
correspondances <- as.data.frame(unique(int_data))
correspondances$group <- c("Image and coordinates", "Image and coordinates", "None", 
                            "Coordinates", "Image", "Both optional", "Image", "Coordinates")

df_matched <- merge(int_data, correspondances, by.x = "int_data", by.y="int_data")

deconvolution_review$Data_int <- df_matched$group

## make plot
Img_coord <- as.data.frame(table(deconvolution_review$Category, deconvolution_review$Data_int))
Img_coord <- Img_coord[order(Img_coord$Freq),]
Img_coord$Var1 <- factor(Img_coord$Var1, levels=Category_levels$Var1)

ggplot(Img_coord, aes(y=as.factor(Var1), x=Freq, fill=Var2)) + geom_histogram(stat="identity") +
  xlab("Number of method") + ylab("Framework") + theme_light() + theme(axis.text=element_text(size=15), axis.title = element_text(size=20))

ggsave("./figures/data_synergy_per_framework.svg", width=15, height=8)


## Table 1- List of methods ---------------------------

selected.cols <- c("Method.name", "Category", "Reference.based...Reference.free", "ST.coordinates", "Image","Main.output", "Programming.language")

# order by framework and 
table1 <- deconvolution_review[order(deconvolution_review$Reference.based...Reference.free,deconvolution_review$Category,deconvolution_review$Method.name), selected.cols]


# save as excel for futher processing 
write.csv(table1, "table1_list_methods.csv")

## Table 2- Data Synergy ---------------------------

selected.cols2 <- c("Method.name", "Category", "Reference.based...Reference.free", #overall
                    "Reference", #single-cell data used
                    "ST.coordinates", "ST.coordinates.integration" , #coordinates
                    "Image", "Image.integration" #image, 
                    )

table2 <- deconvolution_review[, selected.cols2]

# create new groups for data combinaison: sc alone, sc+coord, sc+image, sc+ etc. (how many can I spots?)
table2$group1 <- apply(table2[,c("Reference.based...Reference.free", "ST.coordinates","Image")], 1, function(row) paste(row, collapse = "_"))

# factor by data combinaison
table2$Reference.based...Reference.free <- factor(table2$Reference.based...Reference.free, levels= c("Reference-based", "Reference-free", "Both"))
table2$ST.coordinates <- factor(table2$ST.coordinates, levels= c("No", "Optional", "Yes"))
table2$Image <- factor(table2$Image, levels= c("No", "No ", "Optional", "Yes"))

## check reference type
freq <- as.data.frame(table(table2$Reference))
freq <- freq[order(-freq$Freq),]
table2$Reference <- factor(table2$Reference, levels = freq$Var1)

# Check category type
freq_cat <- as.data.frame(table(table2$Category))
freq_cat <- freq_cat[order(-freq_cat$Freq),]
table2$Category <- factor(table2$Category, levels = freq_cat$Var1)

# order
table2 <- table2[order(table2$Reference.based...Reference.free,table2$ST.coordinates, table2$Image,table2$Reference,table2$Category),]

#then reoder columns 
table2 <- table2[c("Reference.based...Reference.free","ST.coordinates","Image","Reference","Category","Method.name", 
                   "ST.coordinates.integration","Image.integration")]
                       
# save as excel for futher processing 
write.csv(table2, "table2_data_synergy.csv")


