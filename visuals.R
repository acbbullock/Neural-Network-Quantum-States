library("gplots")

corr_F_data <- read.csv('./data/correlations_F.csv', header=TRUE, sep=',')
corr_F = as.matrix(corr_F_data)
corr_F_rows = as.character(nrow(corr_F))

corr_A_data <- read.csv('./data/correlations_A.csv', header=TRUE, sep=',')
corr_A = as.matrix(corr_A_data)
corr_A_rows = as.character(nrow(corr_A))

heatmap_colors = colorpanel(100, 'salmon2', 'white', 'skyblue2')

png('./data/correlations_F.png', res=600, width=10.67, height=6, units='in')
heatmap.2(  
            corr_F, col=heatmap_colors, trace='none', margins = c(10.67, 6), 
            main=paste('Ising Chain Spin Correlations (J>0, ', 'n=', corr_F_rows, ')', sep=''), ylab='Site', 
            key.title='Correlation Strength', key.xlab='', key.ylab='', 
            density.info='density', symkey=TRUE, dendrogram='both'
         )
dev.off()

png('./data/correlations_A.png', res=600, width=10.67, height=6, units='in')
heatmap.2(  
            corr_A, col=heatmap_colors, trace='none', margins = c(10.67, 6), 
            main=paste('Ising Chain Spin Correlations (J<0, ', 'n=', corr_A_rows, ')', sep=''), ylab='Site', 
            key.title='Correlation Strength', key.xlab='', key.ylab='', 
            density.info='density', symkey=TRUE, dendrogram='both'  
         )
dev.off()
