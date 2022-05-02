# Load ggplot2
library(ggplot2)
library(ggsci)
library(tidyverse)
library(ggpubr)
library(gridExtra)
library(cowplot)

get_cell_number_array <- function(cell){
  as.numeric(unlist(strsplit(cell,",")),10)
}

# function to get average for a cell
get_cell_avg <- function(cell) {
  mean(get_cell_number_array(cell))
}

get_cell_sd <- function(cell) {
  sd(get_cell_number_array(cell))
}

color_Ga_P1 <- "brown2"
color_Ga_P2 <- "darkblue"
color_Gb_P1 <- "darkgreen"
color_Gb_P2 <- "darkgoldenrod1"

data_folder <- "chart_data/"
graph_folder <- "chart_graphs/"

metric_names <- c("adjustment_target_level",
                  "attack_avoidance_efficiency",
                  "attack_window_efficiency",
                  "completion_time",
                  "damage_dealt_per_10s",
                  "deaths_per_level",
                  "health_lost_per_encounter")

user_group_names <- c("beginner",
                      "intermediate",
                      "veteran")

output_metric_plot <- function(metric_name, user_group_name)
{
  graph_name <- paste(metric_name,"-",user_group_name,"_players", sep="")
  fname <- paste(data_folder, graph_name, ".txt", sep = "")
  
  # load data from file
  df <- read.table( fname, header = TRUE, sep = "\t", dec = "." ) %>% 
                pivot_longer( !Group, names_to = "Level", values_to = "Samples" ) %>%
                group_by( Group, Level ) %>%
                summarize(
                            avg=get_cell_avg(Samples),
                            sd=get_cell_sd(Samples),
                         )
  
  ggplot_metric_subtable <- function(frame, color1, color2, x_label)
  {
    ggplot( frame, aes(x = Level,y = avg, fill=Group ) ) + 
      scale_fill_manual( values=c(color1, color2) ) +
      theme( plot.margin = margin( b=5, l=-10, r=5 ) ) +
      geom_bar( aes(x=Level, y=avg), position=position_dodge(), stat="identity", alpha=0.65, width = 0.8) + 
      theme( legend.position = "top" , legend.title = element_blank(), legend.direction = "vertical", legend.margin = margin() ) +
      geom_errorbar( aes(x=Level, ymin=avg-sd, ymax=avg+sd), position=position_dodge(), width=0.8, alpha=1.0, size=0.8) +
      xlab( x_label  ) +
      ylab( "" )
  }
  
  plot1 <- ggplot_metric_subtable(rbind(df[1:5,],df[6:10,]), color_Ga_P1, color_Ga_P2, "\n(a) Group A (Part 1 vs 2)")
  plot2 <- ggplot_metric_subtable(rbind(df[11:15,],df[16:20,]), color_Gb_P1, color_Gb_P2, "\n(b) Group B (Part 1 vs 2)")
  plot3 <- ggplot_metric_subtable(rbind(df[1:5,],df[11:15,]), color_Ga_P1, color_Gb_P1, "\n(c) Group A vs B (Part 1)")
  plot4 <- ggplot_metric_subtable(rbind(df[6:10,],df[16:20,]), color_Ga_P2, color_Gb_P2, "\n(d) Group A vs B (Part 2)")
  
  jpeg_fname <- paste(graph_folder, graph_name,".png",sep="")
  chart_title <- paste("Average ", str_to_title(str_replace_all(metric_name,"_"," ")), " (", str_to_title(user_group_name), " Players)" , sep = "")
  
  png(filename = jpeg_fname, width = 2048, height = 684, res = 200, pointsize = 12)
  grid.arrange(plot1,plot2,plot3,plot4,ncol=4)
  dev.off()
}

sapply(metric_names, output_metric_plot, user_group_name="beginner")
sapply(metric_names, output_metric_plot, user_group_name="intermediate")
sapply(metric_names, output_metric_plot, user_group_name="veteran")
