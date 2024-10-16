# Table 1 figures

## Library ---------------------------
library(ggplot2)
library(ggimage)
library(readxl)
library(ggtextures)
library(magick)
library(tibble)
# Color theme: wesanderson for sure
library(wesanderson)
library(dplyr)
library(tidyr)

## Table 1- List of methods ---------------------------
deconvolution_review <- read.csv("~/Documents/Literature/Deconvolution_review/deconvolution_table/deconvolution_method_table_vf.csv", sep=";")
deconvolution_review <- deconvolution_review[deconvolution_review$Title != "",]

### Version 1- Table ---------------------------

selected.cols <- c("Method.name", "Category", "Reference.based...Reference.free", "ST.coordinates", "Image","Main.output", "Programming.language")

# order by framework and 
table1 <- deconvolution_review[order(deconvolution_review$Reference.based...Reference.free,deconvolution_review$Category,deconvolution_review$Method.name), selected.cols]

table1_save <- table1
# Change Optional to Yes,No 
## Modified table 1
table1$Programming.language[table1$Programming.language == "-"] = "R"
table1$Reference.based...Reference.free[table1$Reference.based...Reference.free == "Both"] = "Reference-based, Reference-free"
#table1$ST.coordinates[table1$ST.coordinates == "Optional"] = "Yes, No"

##
#table1$Image[table1$Image == "Optional"] = "Yes, No"


# Split columns whith multiple items

split_df <- function(df, col){
  df_split <- strsplit(as.character(df[,col]), ",\\s*")
  unique_items <- unique(unlist(df_split)) # Find all unique values in the 'Main.output' column
  for (item in unique_items) {# Create a new column for each unique item and mark "Yes" or "No"
    df[[item]] <- sapply(df_split, function(x) if (item %in% x) "Yes" else "No")
  }
  return(df)
}

## Need to split Reference.based...Reference.free,ST.coordinates,Image and Main.output

table1_split <- split_df(table1, "Reference.based...Reference.free")
#table1_split <- split_df(table1_split, "ST.coordinates")
#table1_split <- split_df(table1_split, "Image")
table1_split <- split_df(table1_split, "Main.output")
table1_split <- split_df(table1_split, "Category")

## Peculiar split for programming
df_split <- strsplit(as.character(table1_split$Programming.language), "/\\s*")
unique_items <- unique(unlist(df_split)) # Find all unique values in the 'Main.output' column
for (item in unique_items) {# Create a new column for each unique item and mark "Yes" or "No"
  table1_split[[item]] <- sapply(df_split, function(x) if (item %in% x) "Yes" else "No")
}

## Pivot table
table1_long <- table1_split %>%
  pivot_longer(cols = -Method.name,
               names_to = "Variable", values_to = "Value")

## Keep only lines with yes and Image_Yes, Image_No
table1_long_plot <- table1_long[table1_long$Value %in% c("Yes", "Optional"),]

# Order columns 
ref_order <- c("Reference-based","Reference-free")
category_order <- unique(table1$Category)
output_order <- unique(unlist(strsplit(table1$Main.output, split = ",\\s*")))
progr_order <- unique(unlist(strsplit(table1$Programming.language, split = "/\\s*")))

table1_long_plot$Variable <- factor(table1_long_plot$Variable,
                                    levels = rev(c(ref_order, "ST.coordinates", "Image", progr_order,output_order,category_order)))

## Order rows

table1_save <- table1_save%>%
  arrange(Reference.based...Reference.free, ST.coordinates, Image)

method_level <- table1_save$Method.name #first order, except I want to put "Both" in between
ref_both <- method_level[1:6]
ref_free <- tail(method_level,10)

# Reorder table 1 save with new order
final_method_level <- c(ref_free,ref_both,method_level[7:29], method_level[30], method_level[46:48],method_level[49:53],method_level[31:45])


table1_long_plot$Method.name <- factor(table1_long_plot$Method.name, levels =final_method_level)


# Make the plot

svg("./figures/summary_deconvolution_method_heatmap_2.svg",height = 23, width=60)

# Base plot with squares around the tiles
ggplot(table1_long_plot, aes(x = Method.name, y = Variable)) +
  # Draw the main data tiles
  geom_tile(aes(fill = Value), color = "white", width = 0.8, height = 0.8) +  # Main tiles with a slight size reduction
  
  # Add a transparent tile to create a border effect
  geom_tile(color = "gray80", width = 1, height = 1, fill = NA) +  # Border tiles
  
  scale_fill_manual(values = c("Yes" = "#5fa475", "No" = "#ca5e57", "Optional" = "#9e6ebe", "Both" = "#9f9d39")) +  # Custom colors
  
  # Add gridlines along the axes
  geom_hline(yintercept = seq(0.5, 70,1), color = "gray80", size = 0.5) +  # Horizontal gridlines
  geom_vline(xintercept = seq(0.5, 70,1), color = "gray80", size = 0.5) +  # Vertical gridlines
  
  scale_y_discrete(labels = c("Single-cell gene expression" = "SCGex", 
                              "Super-pixel gene expression" = "Super-pixel Gex",
                              "Reference-based" = "Reference-based",
                              "Reference-free" = "Reference-free",
                              "ST.coordinates" = "Coordinates"),
                   position = "left") +
  # Custom theme to hide gridlines
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Rotate x-axis labels
    axis.text = element_text(size = 30, colour = "black"),
    axis.text.y = element_text(size=40),
    panel.grid = element_blank()  # Remove default gridlines
  ) +
  
  labs(x = "", y = "", title = "")
dev.off()
