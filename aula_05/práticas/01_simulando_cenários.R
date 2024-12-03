library(phytools)

## first simulate tree with no extinction
tree.noExtinction<-pbtree(b=0.039,
                          n=100,
                          t=100,
                          method="direct"
                          )