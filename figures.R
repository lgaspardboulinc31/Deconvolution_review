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
library(tidyr)
library(ggpubr)
## Data ---------------------------
# Data 
deconvolution_review <- read.csv("../Shiny_Deconvolution/data/deconvolution_methods_v4.csv", sep=";")
deconvolution_review <- deconvolution_review[deconvolution_review$Title != "",]

## Remove TESLA
deconvolution_review <- deconvolution_review[-11,]


## Fig A - Cumulative frequency ---------------------------

deconvolution_review$year <- paste0("20",sapply(strsplit(deconvolution_review$Publication.date, split='-'), "[[",2))

svg("../figures/number_methods_per_year.svg",height = 10, width=10)
ggplot(deconvolution_review, aes(x=as.factor(year))) + geom_histogram(stat="count", fill="#0A9F9D") +
  xlab("Year of release") + ylab("Number of publications") + 
  theme_light()+
  theme(axis.text = element_text(size=30), axis.title = element_text(size=40))

dev.off()
ggsave("../figures/cumulative_frequency_publication.svg", width=6, height=5)


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


svg("../figures/number_methods_per_progr.svg",height = 10, width=10)

ggplot(data, aes(lang, count, image = image)) +
  geom_isotype_col(
    img_width = grid::unit(4, "native"), img_height = NULL,
    ncol = NA, nrow = 1, hjust = 0, vjust = 0.5
  ) +
  coord_flip() + ylab("Number of methods") + xlab("Programming language")+
  theme_light()+
  theme(axis.text = element_text(size=30), axis.title = element_text(size=40))

dev.off()

ggsave("../figures/programming_languages.svg", width=10, height=5)



## Fig C - Reference free or based ---------------------------

deconvolution_review$Reference.based_Reference.free <- ifelse(deconvolution_review$Reference.based_Reference.free == "Reference-based ", 
                                                                "Reference-based", deconvolution_review$Reference.based_Reference.free)

framework <- as.data.frame(table(deconvolution_review$Reference.based_Reference.free))
framework <- framework[order(framework$Freq),]
framework$Var1 <- factor(framework$Var1, levels = framework$Var1)

svg("../figures/number_methods_per_reference.svg",height = 10, width=15)

ggplot(framework, aes(y=as.factor(Var1), x=Freq)) + geom_histogram(stat="identity", fill="#F2AD00") +
  xlab("Number of method") + ylab("Use a single-cell reference") + theme_light() + 
  theme(axis.text = element_text(size=30), axis.title = element_text(size=40))+
  geom_text(aes(label = Freq), vjust = 0.5, hjust=-0.5,size = 5)
dev.off()

ggsave("../figures/reference_use.svg", width=10, height=5)


## Fig D- Frameworks ---------------------------

pal_asteroid <- wes_palette("AsteroidCity1",5,"discrete")

Category_levels <- as.data.frame(table(deconvolution_review$Category))
Category_levels <- Category_levels[order(Category_levels$Freq),]

Category <- as.data.frame(table(deconvolution_review$Category, deconvolution_review$Reference.based_Reference.free))
Category <- Category[order(Category$Freq),]
Category$Var1 <- factor(Category$Var1, levels=Category_levels$Var1)

svg("../figures/number_methods_per_framework.svg",height = 10, width=15)
ggplot(Category, aes(y=as.factor(Var1), x=Freq, fill=Var2)) + geom_histogram(stat="identity") +
  xlab("Number of method") + ylab("Framework") + scale_fill_manual(values=pal_asteroid[c(2,1,3)])+
  theme_light() + theme(axis.text=element_text(size=30), axis.title = element_text(size=40))

dev.off()
ggsave("../figures/framework_classification.svg", width=15, height=10)


## Fig E- Data synergy in deconvolution ---------------------------
colors_pal <- wesanderson::wes_palette("AsteroidCity1",5,"discrete")

input_data <- deconvolution_review[,c("ST.counts", "ST.coordinates", "Image", "Reference.based_Reference.free", "Method.name")]

# I split ref-based and ref-free alg. to make the Venn diagram 

input_data_ref <- input_data[input_data$Reference.based_Reference.free %in% c("Reference-based","Both"),]
input_data_free <- input_data[!input_data$Reference.based_Reference.free %in% c("Reference-based", "Both"),]

# function
get_sets <- function(df, ref_type){
  return(list(
    `Use Coordinates` = which(df$ST.coordinates == "Yes"),
    `Use Image` = which(df$Image == "Yes",),
    `Use scRNA-seq` = which(df$Reference.based_Reference.free %in% ref_type),
    `No scRNA-seq` =which(!df$Reference.based_Reference.free %in% ref_type)))
  
}


# Prepare the sets
sets_ref <- get_sets(input_data_ref, c("Reference-based","Both"))
sets_free <- get_sets(input_data_free, c("Reference-free"))#,"Both"))

## As Venn diagram
library(ggvenn)
ggvenn(sets_ref, c("Reference","Use_Coord", "Use_Image"), show_percentage = F, fill_color = colors_pal[c(3,1,2)], text_size=10, set_name_size=5) 
ggsave("../figures/venn_diagram_methods_ref_based.svg", width=15, height=10)

ggvenn(sets_free, c("Reference","Use_Coord", "Use_Image"), show_percentage = F, fill_color = colors_pal[c(4,1,2)], text_size=10, set_name_size=5) 
ggsave("../figures/venn_diagram_methods_ref_free.svg", width=15, height=10)

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
  input_data_test <- deconvolution_review[deconvolution_review$Category == set_name,c("ST.counts", "ST.coordinates", "Image", "Reference.based_Reference.free", "Method.name")]
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

Category <- as.data.frame(table(deconvolution_review$Category, deconvolution_review$Reference.based_Reference.free))
Category <- Category[order(Category$Freq),]
Category$Var1 <- factor(Category$Var1, levels=Category_levels$Var1)

ggplot(Category, aes(y=as.factor(Var1), x=Freq, fill=Var2)) + geom_histogram(stat="identity") +
  xlab("Number of method") + ylab("Framework") + theme_light() + theme(axis.text=element_text(size=15), axis.title = element_text(size=20))+
  labs(fill = "Reference use")

ggsave("../figures/reference_per_framework.svg", width=15, height=8)


## Image and Coordinate int
int_data <- as.data.frame(paste0(deconvolution_review$Image,deconvolution_review$ST.coordinates))
colnames(int_data) <- "int_data"
  
correspondances <- as.data.frame(unique(int_data))
correspondances$group <- c("None","Image", "Coordinates", "Image and Coordinates", "Image and Coordinates", "Image", "Image and Coordinates")

df_matched <- merge(int_data, correspondances, by.x = "int_data", by.y="int_data")

deconvolution_review$Data_int <- df_matched$group

## make plot
Img_coord <- as.data.frame(table(deconvolution_review$Category, deconvolution_review$Data_int))
Img_coord <- Img_coord[order(Img_coord$Freq),]
Img_coord$Var1 <- factor(Img_coord$Var1, levels=Category_levels$Var1)

ggplot(Img_coord, aes(y=as.factor(Var1), x=Freq, fill=Var2)) + geom_histogram(stat="identity") +
  xlab("Number of method") + ylab("Framework") + theme_light() + theme(axis.text=element_text(size=15), axis.title = element_text(size=20))+
  labs(fill = "Extra modality")

ggsave("../figures/data_synergy_per_framework.svg", width=15, height=8)


## Figure G - Output of deconvolution ---------------------------

outputs <-  deconvolution_review[,c("Main.output", "Reference.based_Reference.free", "Method.name")]

## Table 1- List of methods ---------------------------

### Version 1- Table ---------------------------

selected.cols <- c("Method.name", "Category", "Reference.based_Reference.free", "ST.coordinates", "Image","Main.output", "Programming.language")

# order by framework and 
table1 <- deconvolution_review[order(deconvolution_review$Reference.based_Reference.free,deconvolution_review$Category,deconvolution_review$Method.name), selected.cols]


# save as excel for futher processing 
write.csv(table1, "table1_list_methods.csv")

### Version 2- Extended per item ---------------------------

## Modified table 1
table1$Programming.language[table1$Programming.language == "-"] = "R"
df <- table1
# Split the 'Main.output' into lists by separating on ', ' and then trim any leading/trailing spaces
df_split <- strsplit(as.character(df$Main.output), ",\\s*")
unique_items <- unique(unlist(df_split)) # Find all unique values in the 'Main.output' column
for (item in unique_items) {# Create a new column for each unique item and mark "Yes" or "No"
  df[[item]] <- sapply(df_split, function(x) if (item %in% x) "Yes" else "No")
}

# Do the same for the code
# Split the 'Programming.language' into lists by separating on ', ' and then trim any leading/trailing spaces
df_split <- strsplit(as.character(df$Programming.language), "/\\s*")
unique_items <- unique(unlist(df_split)) # Find all unique values in the 'Main.output' column
for (item in unique_items) {# Create a new column for each unique item and mark "Yes" or "No"
  df[[item]] <- sapply(df_split, function(x) if (item %in% x) "Yes" else "No")
}

# Do the same for ref free/ ref-based
df$Reference.based_Reference.free <- ifelse(df$Reference.based_Reference.free == "Both", 
                                              "Reference-based, Reference-free",
                                              df$Reference.based_Reference.free)
# Split the 'Main.output' into lists by separating on ', ' and then trim any leading/trailing spaces
df_split <- strsplit(as.character(df$Reference.based_Reference.free), ",\\s*")
unique_items <- unique(unlist(df_split)) # Find all unique values in the 'Main.output' column
for (item in unique_items) {# Create a new column for each unique item and mark "Yes" or "No"
  df[[item]] <- sapply(df_split, function(x) if (item %in% x) "Yes" else "No")
}

# Re-select the columns 
df <- df%>%
  arrange(Reference.based_Reference.free, Category, Method.name)

write.csv(df,"../figures/table1_full.csv")

### Version 3 - Visual per item ---------------------------


# Reshape the data into long format
df_long <- df %>%
  pivot_longer(cols = c(`Reference-based`, `Reference-free` ,Image, ST.coordinates, Counts, Probabilities,
                        Proportions, `Cell location`, `Single-cell gene expression`,Mapping,Python,R, MATLAB)
               , 
               names_to = "Variable", values_to = "Value")

# Arrange columns in the good order

df_long$Variable <- factor(df_long$Variable, rev(c("Category","Reference-based", "Reference-free","ST.coordinates","Image",
                                          "Proportions","Counts","Probabilities","Single-cell gene expression","Mapping","Cell location","Super-pixel gene expression",
                                          "Python","R","MATLAB"
                                          )))

df_long$Method.name <- factor(df_long$Method.name, levels = method_level)

# Create a plot
ggplot(df_long, aes(x = Variable, y = Method.name)) +
  geom_point(data = df_long %>% filter(Value == "Yes"), aes(shape = Value), size = 5) +
  scale_shape_manual(values = c("X")) +  # Use "X" for Yes
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Item", y = "Main Output", title = "Table1")+ 
  
  scale_x_discrete(position = "top")+
  
  # Add the columns
  geom_vline(xintercept=seq(0.5, length(unique(df_long$Variable))+0.5, by = 1), 
             color = "grey", size = 0.5)+
  
  # Add manual horizontal grid lines between method names on y-axis
  geom_hline(yintercept = seq(0.5, length(unique(df_long$Method.name))+0.5, by = 1), 
             color = "grey", size = 0.5)+  # Place grid lines between methods
  
  # Check theme
  theme_classic()+theme(
    axis.text.x = element_text(angle = 45, hjust = 0), 
    axis.ticks = element_blank(),
    axis.line = element_blank())

ggsave("../figures/table1_visual.png",plot=last_plot(), height = 15, width=5, dpi=300)

df_long <- df_long %>%
  mutate(Group = case_when(
    Variable %in% c("Reference-based","Reference-free") ~ "Use of scRNA-seq",
    Variable %in% c("Image","ST.coordinates") ~ "Extra-modality",
    Variable %in% c("Counts","Probabilities","Proportions","Cell location", "Single-cell gene expression", "Mapping","Super-pixel gene expression") ~ "Output type",
    Variable %in% c("Python", "R", "MATLAB") ~ "Programming language"
  ))

df_long_plot <- df_long[df_long$Value %in% c("Yes","Optional"),]


### Version 4 - Visual version enhanced --------------------------------------------


svg("../figures/summary_deconvolution_method_heatmap.svg",height = 11, width=30)

ggplot(df_long_plot, aes(x = Method.name, y = Variable, fill = Value)) +
  geom_tile(color = "white", width = 0.4, height = 0.9) + # Adds a border between tiles
  scale_fill_manual(values = c("Yes" = "#5fa475", "No" = "#ca5e57", "Optional" ="#9e6ebe", "Both"="#9f9d39")) + # Set Yes to green, No to red
  theme_minimal() +# Add spacing between the studies (x-axis) and methods (y-axis)
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1),
        axis.text= element_text(size=20)) + # Rotate x-axis labels for better readability
  labs(title = "",
       x = "",
       y = "Method Name",
       color = "") +
  scale_x_discrete(labels = c("Single-cell gene expression" = "SCGex", 
                              "Super-pixel gene expression" = "Super-pixel Gex", 
                              "Ref_cat" = "Use reference"), position = "bottom")

dev.off()


### Version 5- Per item short ---------------------------

table1_cat <- table1
table1_cat$Ref_cat <- recode(table1_cat$Reference.based_Reference.free, 
                                                                "Reference-based" = "Yes", 
                                                                "Reference-free" = "No",
                                                                "Both" = "Both")

table1_cat <- table1_cat%>%
  arrange(Ref_cat, ST.coordinates, Image)
#or 
#table1_cat <- table1_cat%>%
 #arrange(Ref_cat, Category, Method.name)

method_level <- table1_cat$Method.name

## Transform table 1 in categorical variables


table1_long <- table1_cat %>%
  pivot_longer(cols = c(Image, ST.coordinates, Ref_cat), 
               names_to = "Variable", values_to = "Value")

table1_long$Variable <- factor(table1_long$Variable, levels = c("Ref_cat", "ST.coordinates", "Image"))
table1_long$Method.name <- factor(table1_long$Method.name, levels = method_level)
table1_long$Value<- ifelse(table1_long$Value =="No ", "No", table1_long$Value)
table1_long$Value <- factor(table1_long$Value, levels=c("Yes", "No", "Optional", "Both"))


svg("../figures/summary_deconvolution_method.svg",height = 20, width=7)
# Plot the dot plot with separate columns for each variable (Image, ST.coordinates, Reference.based.free)
ggplot(table1_long, aes(x = Variable, y = Method.name)) +
  geom_point(aes(color = Value), size = 6, shape = 16) +  # Use dots with colors
  scale_color_manual(values = c("Yes" = "#66c2a5", "No" = "#fc8d62", "Both" = "#8da0cb", "Optional"="#e78ac3")) +  # Define custom colors
  labs(title = "",
       x = "Variables",
       y = "Method Name",
       color = "") +
  scale_x_discrete(labels = c("Image" = "Use image", 
                              "ST.coordinates" = "Use coordinates", 
                              "Ref_cat" = "Use reference")) +
  
  # Add the columns
  geom_vline(xintercept = 1.5, color = "grey", linewidth = 0.5) +
  geom_vline(xintercept = 2.5, color = "grey", linewidth = 0.5) +
  geom_vline(xintercept = 3.5, color = "grey", linewidth = 0.5) +
  
  # Add manual horizontal grid lines between method names on y-axis
  geom_hline(yintercept = seq(0.5, length(unique(table1_long$Method.name))+0.5, by = 1), 
             color = "grey", size = 0.5)+  # Place grid lines between methods
  
  # Check theme
  theme_classic() +  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20, angle = 45, hjust=1),  # Remove x-axis text
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    axis.text.y = element_text(size = 20),  # Retain y-axis method names
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    panel.grid.major = element_blank(),  # Light grid lines for table structure
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top",# Center title
    line = element_blank()
  )
dev.off()
ggsave("../figures/summary_deconvolution_method.png", plot=last_plot(), height = 15, width=5, dpi=)

## Table 2- Data Synergy ---------------------------

selected.cols2 <- c("Method.name", "Category", "Reference.based_Reference.free", #overall
                    "Reference", #single-cell data used
                    "ST.coordinates", "ST.coordinates.integration" , #coordinates
                    "Image", "Image.integration" #image, 
                    )

table2 <- deconvolution_review[, selected.cols2]

# create new groups for data combinaison: sc alone, sc+coord, sc+image, sc+ etc. (how many can I spots?)
table2$group1 <- apply(table2[,c("Reference.based_Reference.free", "ST.coordinates","Image")], 1, function(row) paste(row, collapse = "_"))

# factor by data combinaison
table2$Reference.based_Reference.free <- factor(table2$Reference.based_Reference.free, levels= c("Reference-based", "Reference-free", "Both"))
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
table2 <- table2[order(table2$Reference.based_Reference.free,table2$ST.coordinates, table2$Image,table2$Reference,table2$Category),]

#then reoder columns 
table2 <- table2[c("Reference.based_Reference.free","ST.coordinates","Image","Reference","Category","Method.name", 
                   "ST.coordinates.integration","Image.integration")]
                       
# save as excel for futher processing 
write.csv(table2, "table2_data_synergy.csv")


# Table 3 - Benchmark studies ---------------------------------------------

# REad file
library(readr)
V0_review_table_benchmark <- read_delim("~/Desktop/Deconvolution_Review/Figures/tables/V0_review_table_benchmark.csv", 
                                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Step 1: Separate methods into individual rows
benchmark_long <- V0_review_table_benchmark %>%
  separate_rows(Methods, sep = ", ") 

# Step 2: Create a pivot table where methods are rows and presence is "Yes"/"No"
benchmark_long_wide <- benchmark_long %>%
  mutate(Present = "Yes") %>%
  pivot_wider(names_from = Study, values_from = Present, values_fill = "No")

df_long_for_plot <- benchmark_long_wide %>%
  pivot_longer(cols = -Methods, names_to = "Study", values_to = "Present")

#factor methods
freq_df <- as.data.frame(table(df_long_for_plot$Methods,df_long_for_plot$Present))
freq_df <- freq_df[freq_df$Var2 == "Yes",]
freq_df <- freq_df[order(freq_df$Freq),]
methods_order <- rev(freq_df$Var1)

df_long_for_plot$Methods <- factor(df_long_for_plot$Methods, levels=methods_order)

pal_benchmark <- c(wes_palette("Royal1")[2], wes_palette("Royal2")[5])

svg("../figures/benchmark_studies.svg",height = 5, width=20)
ggplot(df_long_for_plot, aes(x = Methods, y = Study, fill = Present)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) + # Adds a border between tiles
  scale_fill_manual(values = c("Yes" = "#74A089", "No" = "#C93312")) + # Set Yes to green, No to red
  theme_minimal() +# Add spacing between the studies (x-axis) and methods (y-axis)
  theme(axis.text.x = element_text(angle = 45, hjust = 0, vjust=1, size = 20),
        axis.text.y=element_text(size=15)) + # Rotate x-axis labels for better readability
  labs(title = "", x = "Methods", y = "Benchmarks")+
  scale_x_discrete(position = "top")
dev.off()

ggsave("../figures/summary_benchmark.png", plot=last_plot(), height = 5, width=20, dpi=300)
