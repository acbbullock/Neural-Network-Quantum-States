#######################################################################################################################
##  This script file writes png files for visualizing the csv data output of the stochastic_optimization subroutine.
#######################################################################################################################
if (!require('pacman')) install.packages('pacman')                                            ## Require pacman package
pacman::p_load(gplots, ggplot2, cowplot, latex2exp)                                                ## Load dependencies

## Load data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
corr_F_data <- read.csv('./data/correlations_F.csv', header=TRUE, sep=',')                ## Ferromagnetic correlations
corr_F <- as.matrix(corr_F_data)                                                                  ## Convert to numeric

corr_A_data <- read.csv('./data/correlations_A.csv', header=TRUE, sep=',')            ## Antiferromagnetic correlations
corr_A <- as.matrix(corr_A_data)                                                                  ## Convert to numeric

energies_F_data <- read.csv('./data/energies_F.csv', header=TRUE, sep=',')                    ## Ferromagnetic energies
energies_A_data <- read.csv('./data/energies_A.csv', header=TRUE, sep=',')                ## Antiferromagnetic energies

## Visualize correlations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
heatmap_colors <- colorpanel(100, 'salmon2', 'white', 'skyblue2')                       ## Set color panel for heatmaps

png('./data/correlations_F.png', res=600, width=10.67, height=6, units='in')              ## Open png device for saving
heatmap.2(                                                                        ## Ferromagnetic correlations heatmap
	corr_F, col=heatmap_colors, trace='none', margins = c(10.67, 6), 
	main='Ferromagnetic Ising Correlations ', ylab='Site', 
	key.title='Correlation Strength', key.xlab='', key.ylab='', 
	density.info='density', symkey=TRUE, dendrogram='both'
)
dev.off()                                                                                               ## Close device

png('./data/correlations_A.png', res=600, width=10.67, height=6, units='in')              ## Open png device for saving
heatmap.2(                                                                    ## Antiferromagnetic correlations heatmap
	corr_A, col=heatmap_colors, trace='none', margins = c(10.67, 6), 
	main='Anti-ferromagnetic Ising Correlations ', ylab='Site', 
	key.title='Correlation Strength', key.xlab='', key.ylab='', 
	density.info='density', symkey=TRUE, dendrogram='both'  
)
dev.off()                                                                                               ## Close device

## Visualize energies ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
energies_F_plot <- ggplot(data=energies_F_data, aes(x=c(1:nrow(energies_F_data)), y=Energy)) +           ## Energy plot
	geom_line(color='skyblue') +
	geom_point(size=0.5) +
	ggtitle('Energy') +
	labs(x='Epoch', y=TeX(r'(E[\psi])')) +
	theme_bw()

errors_F_plot <- ggplot(data=energies_F_data, aes(x=c(1:nrow(energies_F_data)), y=Error)) +               ## Error plot
	geom_line(color='skyblue') +
	geom_point(size=0.5) +
	ggtitle('Error') +
	labs(x='Epoch', y='') +
	theme_bw()

p <- plot_grid(energies_F_plot, errors_F_plot, labels='AUTO')                                   ## Combine to grid plot
save_plot('./data/energies_F.png', p, ncol=2)                                                            ## Save as png

energies_A_plot <- ggplot(data=energies_A_data, aes(x=c(1:nrow(energies_A_data)), y=Energy)) +           ## Energy plot
	geom_line(color='skyblue') +
	geom_point(size=0.5) +
	ggtitle('Energy') +
	labs(x='Epoch', y=TeX(r'(E[\psi])')) +
	theme_bw()

errors_A_plot <- ggplot(data=energies_A_data, aes(x=c(1:nrow(energies_A_data)), y=Error)) +               ## Error plot
	geom_line(color='skyblue') +
	geom_point(size=0.5) +
	ggtitle('Error') +
	labs(x='Epoch', y='') +
	theme_bw()

p <- plot_grid(energies_A_plot, errors_A_plot, labels='AUTO')                                   ## Combine to grid plot
save_plot('./data/energies_A.png', p, ncol=2)                                                            ## Save as png
