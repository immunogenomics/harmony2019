do_umap <- function(V) {
    uwot::umap(
        X = V,
        n_threads = 6,
        n_neighbors = 30L,
        n_components = 2L,
        metric = 'cosine',
        n_epochs = NULL,
        learning_rate = 1.0,
        min_dist = 0.3,
        spread = 1.0,
        set_op_mix_ratio = 1.0,
        local_connectivity = 1L,
        repulsion_strength = 1,
        negative_sample_rate = 1,
        a = NULL,
        b = NULL,
        fast_sgd = FALSE,
        verbose = FALSE
    )    
    
}

do_scatter <- function (
    umap_use, meta_data, label_name, no_guides = FALSE, 
    do_labels = FALSE, nice_names, palette_use = tableau_color_pal()(10), 
    pt_size = 4, point_size = 0.5, pt_shape = '.',
    base_size = 12, do_points = TRUE, do_density = FALSE) {
    plt_df <- umap_use %>% data.frame() %>% cbind(meta_data) %>% 
        dplyr::sample_frac(1L)
    plt_df$given_name <- plt_df[[label_name]]
    if (!missing(nice_names)) {
        plt_df %<>% dplyr::inner_join(nice_names, by = "given_name") %>% 
            subset(nice_name != "" & !is.na(nice_name))
        plt_df[[label_name]] <- plt_df$nice_name
    }
    plt <- plt_df %>% ggplot(aes_string("X1", "X2", col = label_name, 
        fill = label_name)) + theme_tufte(base_size = base_size) + 
        theme(panel.background = element_rect(fill = NA, color = "black")) + 
        guides(color = guide_legend(override.aes = list(stroke = 1, 
            alpha = 1, shape = 16, size = 4)), alpha = FALSE) + 
        scale_color_manual(values = palette_use) + scale_fill_manual(values = palette_use) + 
        theme(plot.title = element_text(hjust = 0.5)) + labs(x = "UMAP 1", 
        y = "UMAP 2")
    if (do_points) 
        plt <- plt + geom_point(shape = pt_shape, size = point_size)
    if (do_density) 
        plt <- plt + geom_density_2d()
    if (no_guides) 
        plt <- plt + guides(col = FALSE, fill = FALSE, alpha = FALSE)
    if (do_labels) {
        plt <- plt + geom_text_repel(data = data.table(plt_df)[, 
            .(X1 = mean(X1), X2 = mean(X2)), by = label_name], 
            label.size = NA, aes_string(label = label_name), 
            color = "black", size = pt_size, alpha = 1, segment.size = 0) + 
            guides(col = FALSE, fill = FALSE)
    }
    return(plt)
}

fig.size <- function (height, width) 
{
    options(repr.plot.height = height, repr.plot.width = width)
}