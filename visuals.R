if (!require('pacman')) install.packages('pacman')
pacman::p_load(gplots, ggplot2, cowplot, latex2exp)

corr_F_data <- read.csv('./data/correlations_F.csv', header=TRUE, sep=',')
corr_F <- as.matrix(corr_F_data)
num_spins_F <- as.character(nrow(corr_F))

corr_A_data <- read.csv('./data/correlations_A.csv', header=TRUE, sep=',')
corr_A <- as.matrix(corr_A_data)
num_spins_A <- as.character(nrow(corr_A))

heatmap_colors <- colorpanel(100, 'salmon2', 'white', 'skyblue2')

png('./data/correlations_F.png', res=600, width=10.67, height=6, units='in')
heatmap.2(  
            corr_F, col=heatmap_colors, trace='none', margins = c(10.67, 6), 
            main='Ferromagnetic Ising Chain Correlations ', ylab='Site', 
            key.title='Correlation Strength', key.xlab='', key.ylab='', 
            density.info='density', symkey=TRUE, dendrogram='both'
         )
dev.off()

png('./data/correlations_A.png', res=600, width=10.67, height=6, units='in')
heatmap.2(  
            corr_A, col=heatmap_colors, trace='none', margins = c(10.67, 6), 
            main='Antiferromagnetic Ising Chain Correlations ', ylab='Site', 
            key.title='Correlation Strength', key.xlab='', key.ylab='', 
            density.info='density', symkey=TRUE, dendrogram='both'  
         )
dev.off()

energies_F_data <- read.csv('./data/energies_F.csv', header=TRUE, sep=',')
energies_A_data <- read.csv('./data/energies_A.csv', header=TRUE, sep=',')

energies_F_plot <- ggplot(data=energies_F_data, aes(x=c(1:nrow(energies_F_data)), y=Energy, group=1)) +
    geom_line(color='skyblue') +
    geom_point(size=0.5) +
    ggtitle('Energy') +
    labs(x='Epoch', y=TeX(r'(E[\psi])')) +
    theme_bw()

errors_F_plot <- ggplot(data=energies_F_data, aes(x=c(1:nrow(energies_F_data)), y=Error, group=1)) +
    geom_line(color='skyblue') +
    geom_point(size=0.5) +
    ggtitle('Error') +
    labs(x='Epoch', y='') +
    theme_bw()

p <- plot_grid(energies_F_plot, errors_F_plot, labels='AUTO')
save_plot('./data/energies_F.png', p, ncol=2)

energies_A_plot <- ggplot(data=energies_A_data, aes(x=c(1:nrow(energies_A_data)), y=Energy, group=1)) +
    geom_line(color='skyblue') +
    geom_point(size=0.5) +
    ggtitle('Energy') +
    labs(x='Epoch', y=TeX(r'(E[\psi])')) +
    theme_bw()

errors_A_plot <- ggplot(data=energies_A_data, aes(x=c(1:nrow(energies_A_data)), y=Error, group=1)) +
    geom_line(color='skyblue') +
    geom_point(size=0.5) +
    ggtitle('Error') +
    labs(x='Epoch', y='') +
    theme_bw()

p <- plot_grid(energies_A_plot, errors_A_plot, labels='AUTO')
save_plot('./data/energies_A.png', p, ncol=2)
