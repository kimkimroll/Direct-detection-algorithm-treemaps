setwd("//cdc.gov/project/CCID_NCIRD_DVD_PPLB/_PMDDL/Kim/R/chels")

dat<-read.csv("simkim2.csv", header = TRUE)
library(treemap)
library(ggplot2)
library(treemapify)

png(filename="tree.png",width=1500, height=1200)
map<-treemap(dat,
        index=c("Action_long","Result"),
        vSize="DASH",
        type="index",
        align.labels=list(
          c("left", "top"), 
          c("left", "bottom")
        ),
        algorithm = "squarified",
        #sortID = "-size",
        inflate.labels=F,
        fontcolor.labels=c("black","grey20"),
        fontsize.labels=c(30,20),
        fontface.labels = c("bold", "italic"),
        bg.labels=c("transparent"),
        border.col=c("black","white"),
        border.lwds = c("5", "2"),
        palette = "PuBuGn",
        title = "n = 10,000",
        position.legend = "bottom",
        force.print.labels = T,
        overlap.labels = .5,
        #command.line.output = T
       
) 
dev.off()


map

ggsave("treekw1.png", dpi = 500, height = 22, width = 30, units = "cm")

#install treemap package ---not working with R version
install_version("treemap", version = "0.4.9", repos = "http://cran.us.r-project.org")


#setting colors
kcolors     <-c("Failed Extraction" = "#bc4b51",
                "Indeterminate" = "#ee6c4d",
                "Negative" = "#e0fbfc",
                "Negative-Inhibition" = "#bc4b51", 
                "Invalid" = "#46494c",
                "NSL1" = "#ee6c4d", 
                "NSL1 PV2" = "#ee6c4d", 
                "Presumptive Poliovirus" = "#3d5a80", 
                "Presumptive Poliovirus-Inhibition" = "#bc4b51", 
                "PV2" = "#ee6c4d", 
                "SL1" = "#98c1d9", 
                "SL1 NSL1" = "#ee6c4d", 
                "SL1 PV2" = "#ee6c4d", 
                "SL3" = "#98c1d9", 
                "SL3 NSL1" = "#ee6c4d", 
                "SL3 NSL1 PV2" = "#ee6c4d",
                "SL3 PV2" = "#ee6c4d",
                "SL3 SL1" = "#98c1d9",
                "SL3 SL1 NSL1" = "#ee6c4d", 
                "SL3 SL1 PV2" = "#ee6c4d", 
                "SL3 NSL1 PV2" = "#ee6c4d")


map1<-ggplot(dat, aes(area = DASH, 
                      fill = Result, 
                      label = Action_long,
                      subgroup = Action_long,
                      subgroup2 = Result)) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "black") +
  geom_treemap_subgroup_text(place = "centre", grow = F, alpha = .9, 
                             colour = "black", min.size = 0) +
  scale_fill_manual(values = kcolors)
  #geom_treemap_text(colour = "white", place = "topleft", reflow = T)

map1

ggsave("treekw.png", dpi = 500, height = 22, width = 30, units = "cm")


library(janitor)

#table summary of results

table_k <- dat %>%
  group_by(Action_long, Result) %>%
  summarise(overall = n()) # %>%
  
  #adorn_totals("row") 

table_k
write.csv(table_k, paste0("dat_fig", Sys.Date(), ".csv", sep = ""))


#stacked plot supplement values
bar1<-ggplot(table_k, aes(fill=Result, x=Action_long)) + 
  geom_bar(position="stack")+
  scale_fill_manual(values = kcolors)+
  geom_text(aes(label=overall), stat = 'count', position=position_dodge(width=0.9), vjust=-0.25)+
  theme_classic()
bar1

bar2<-ggplot(dat)+
  geom_tile(aes(x = Action_long, y = Result), fill = "#d6e2e9",
            na.rm = TRUE)+
  geom_jitter(aes(x = Action_long, y = Result, color = Result),
              shape = 1,
              stroke = 1,
              size = 2,
              width = 0.4, height = 0.4,
              na.rm = TRUE)+
  geom_text(data = table_k, aes(y=Result, x = Action_long, label=overall), color = "black", size=7)+

#  geom_text(colour = "white", nudge_y = .1, aes(label = paste0('Count: ', sum(n)))) + 
#  stat_summary(fun = sum, aes(label = ..y.., x = Action_long, y = Result), geom = "text")+
#  geom_point(aes(x = Type, y = itd5_1, fill = dash),
#             color = "darkgrey",
#             shape = 3,
#             size = 3,
#             alpha = 1,
#             na.rm = TRUE)+
#  scale_y_discrete(limits = c("PV1 Sabin", "PV1 NSL", "PV2", "PV3")
#  )+
  scale_color_manual(values = kcolors, name = "Result")+
#  scale_fill_manual(name = "n = 12 samples", values = t_col)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 20),
    axis.text.x = element_text(size = 14, angle = 35, vjust = 1, hjust =1),
    axis.text = element_text(size = 14)
    #legend.position = "none"
  )+
  labs(x = "Action",
       y = "Result")
#  facet_grid(.~arm)



bar2
ggsave("bar2.png", dpi = 500, units = "cm", height = 18, width = 32)
