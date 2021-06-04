library(tidyverse)

#Set working directory to where the Git repo lives on your computer
setwd("~/GitHub/deltaoutflowviz")

##############
#Read the csv from the CDEC webpage
#https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST
WSI_HIST<-read.csv("WSIHIST.csv")
str(WSI_HIST)

#We're only interested in Sac Valley Index (at least for now, as it makes up 80% of flow into the Delta on average)
#Remove NA from data
WSI_HIST_sac<-WSI_HIST[complete.cases(WSI_HIST$Sac_Index),]
#Add color list
WSI_HIST_sac <- WSI_HIST_sac %>% 
        mutate(Color = as.character(case_when(Sac_WY %in% "W" ~ "dodgerblue",
                                              Sac_WY %in% "AN" ~ "lightskyblue",
                                              Sac_WY %in% "BN" ~ "gold1",
                                              Sac_WY %in% "D" ~ "darkorange2",
                                              Sac_WY %in% "C" ~ "red"))) %>%
        filter(WY>=1928) #Subset to just 1928 and on in order to match with Denise's figure


###############
#Create figure
tiff(filename="Water_index_timeseries.tiff", 
     units="in", bg="white", height=2.4, width=8, res=600, pointsize=6.8, 
     compression="lzw")
par(mar=c(0.5,3,0,0) + 0.2, mgp=c(2,0.8,0), lend=1, lwd=0.5)
use_seq <- seq(1, nrow(WSI_HIST_sac), by=4)
WSI_HIST_sac$YearLabel <- NA
WSI_HIST_sac$YearLabel[use_seq] <- WSI_HIST_sac$WY[use_seq]
WSI_HIST_sac$Dummy <- ""

aa <- barplot(height=WSI_HIST_sac$Sac_Index, names.arg=WSI_HIST_sac$Dummy, ylim=c(-1,16), 
              yaxt="n", bty="n", space=0.2, xlab="", 
              ylab="               Index Value", col=NA, border=NA)

a <- barplot(height=WSI_HIST_sac$Sac_Index, names.arg=WSI_HIST_sac$Dummy, ylim=c(-1,16), 
             xlim=c(0,(max(aa) + 10)),
             yaxt="n", bty="n", space=0.2, xlab="", 
             ylab="               Index Value", col=NA, border=NA)



## Put water year type cutoff lines in the background:
## http://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST
index_cutoffs <- c(5.4,6.5,7.8,9.2)
text_y <- c(index_cutoffs[1] - 0.6, mean(index_cutoffs[1:2]),
            mean(index_cutoffs[2:3]), mean(index_cutoffs[3:4]),
            index_cutoffs[4] + 0.6)
text_lab <- c("Critically Dry", "Dry", "Below Normal", "Above Normal", "Wet")
abline(h=index_cutoffs, lty=2, col="gray50")
text(x=120.9, y=text_y, labels=text_lab, adj=1)

barplot(height=WSI_HIST_sac$Sac_Index, names.arg=WSI_HIST_sac$Dummy, ylim=c(-1,16), 
        xlim=c(0,(max(aa) + 2)),
        yaxt="n", bty="n", space=0.2, xlab="", 
        ylab="", col=WSI_HIST_sac$Color, border="black", 
        add=TRUE)
axis(side=1, at=a, labels=WSI_HIST_sac$YearLabel, pos=0, lwd=0.5)
axis(side=2, at=seq(0, 15, by=5), lwd=0.5)

dev.off()