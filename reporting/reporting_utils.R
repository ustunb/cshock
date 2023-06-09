# Reporting Helper Functions

required_packages = c('base', 'boot', 'dplyr', 'knitr', 'ggplot2', 'xtable', 'rmarkdown', 'stringr', 'reshape2', 'scales', 'grid', 'gridExtra')
for (pkg in required_packages){
    suppressPackageStartupMessages(library(pkg, character.only = TRUE, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE));
}

#### Constants #####

INTERCEPT_NAME = "(Intercept)"

.ALL_METHOD_NAMES = c(
    "slim",
    "risk_slim",
    "optimized_checklist",
    "lars_lasso",
    "lars_lasso_hc",
    "lars_ridge",
    "lars_elasticnet",
    "rounded_elasticnet",
    "logreg",
    "C5.0_rule",
    "C5.0_tree",
    "cart",
    "one_rule",
    "ripper",
    "part",
    "randomforest",
    "sgb",
    "svm_rbf",
    "trivial"
)

#### Plotting ####

default.plot.theme = function(){

    line_color = "#E9E9E9";

    default_theme = theme_bw() +
        theme(title = element_text(size = 18),
              plot.margin = margin(t = 0.25, r = 0, b = 0.75, l = 0.25, unit = "cm"),
              axis.line = element_blank(),
              panel.border = element_rect(size = 2.0, color = line_color),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_line(linetype="solid", size=1.0, color=line_color),
              #
              axis.title.x = element_text(size = 20, margin = margin(t = 20, unit = "pt")),
              axis.text.x   = element_text(size = 20),
              axis.ticks.x  = element_line(size = 1.0, color = line_color),
              #
              axis.title.y = element_text(size = 20, margin = margin(b = 20, unit = "pt")),
              axis.text.y   = element_text(size=20),
              axis.ticks.y	= element_line(size=1.0, color = line_color),
              #
              legend.position="none",
              legend.title = element_blank(),
              legend.text = element_text(face="plain",size=14,angle=0,lineheight=30),
              #legend.key.width = unit(1.5, "cm"),
              #legend.key.height = unit(1.5, "cm"),
              #legend.text.align = 0,
              legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"))

    return(default_theme);
}

default.plot.embellishments = function(){

    all_method_names = .ALL_METHOD_NAMES

    # Method Names / Labels / Colors
    n_methods = length(all_method_names);
    names(all_method_names) = all_method_names
    all_method_labels = all_method_names;
    all_method_colors = all_method_names;
    all_method_sizes = all_method_names;
    all_method_shapes= all_method_names;

    for (i in 1:n_methods){

        method_name = all_method_names[i]

        if (method_name=="lars_lasso" ) {
            method_label  = "Lasso";
            method_color  = "#E61A33";
            method_size   = 2;
            method_shape  = 15;
        } else if (method_name=="lars_lasso_hc") {
            #method_label = "Lasso (Constrained)";
            method_label = "Lasso";
            method_color = "#E61A33";
            method_size   = 2;
            method_shape  = 15;
        } else if (method_name=="lars_ridge") {
            method_label = "Ridge";
            method_color = "#FF8000";
            method_size   = 2;
            method_shape  = 15;
        } else if (method_name=="lars_elasticnet") {
            method_label = "Elastic Net";
            method_color = "#FFBF80";
            method_size   = 2;
            method_shape  = 15;
        } else if (method_name=="rounded_elasticnet" ) {
            method_label  = "Rounded Elastic Net";
            method_color  = "#E61A33";
            method_size   = 2;
            method_shape  = 15;
        } else if (method_name=="logreg") {
            method_label = "LR";
            method_color = "#CCBFFF";
            method_size   = 2;
            method_shape  = 15;
        } else if (method_name %in% c("C5.0_rule", "c50_rule")) {
            method_label = "C50R";
            method_color = "#B2FF8C";
            method_size   = 2;
            method_shape  = 15;
        } else if (method_name %in% c("C5.0_tree", "c50_tree")) {
            method_label = "C50T";
            method_color = "#33FF00";
            method_size   = 2;
            method_shape  = 15;
        } else if (method_name=="cart") {
            method_label = "CART";
            method_color = "#197319";
            method_size   = 2;
            method_shape  = 15;
        } else if (method_name=="sgb") {
            method_label = "SGB";
            method_color = "#887050";
            method_size   = 2;
            method_shape  = 15;
        } else if (method_name=="svm_rbf") {
            method_label = "SVM RBF";
            method_color = "#3d004c"
            method_size   = 2;
            method_shape  = 15;
        } else if (method_name=="randomforest"){
            method_label = "RF";
            method_color = "#a6edff";
            method_size   = 2;
            method_shape  = 15;
        } else if (method_name=="slim") {
            method_label = "SLIM";
            method_color = "#0066ff";
            method_size   = 15;
            method_shape  = 15;
        } else if (method_name=="risk_slim") {
            method_label = "Risk SLIM";
            method_color = "#0066ff";
            method_size   = 15;
            method_shape  = 15;
        } else if (method_name=="mnrules") {
            method_label = "MN Rules";
            method_color = "#FFFF33";
            method_size   = 8;
            method_shape  = 15;
        } else if (method_name=="trivial") {
            method_label = "Trivial";
            method_color = "#000000";
            method_size   = 8;
            method_shape  = 15;
        } else {
            method_label = paste(sapply(strsplit(method_name,"_"),toupper),collapse=' ')
            method_color = "#000000"
            method_size   = 8;
            method_shape  = 15;
        }
        all_method_colors[i]  = method_color;
        all_method_labels[i]  = method_label;
        all_method_sizes[i]   = method_size;
        all_method_shapes[i]  = method_color;
    }

    # More colors here: http://www.color-hex.com/color-palette/2698
    # col = c("#FFBF80", "#FF8000", "#FFFF33","#B2FF8C","#33FF00","#A6EDFF","#1AB2FF","#CCBFFF","#664CFF", "#FF99BF","#E61A33", "#197319","#e3c26c","#887050")
    # image(1:length(col),1,matrix(1:length(col), ncol=1),col=col)

    # Graph Settings
    plot_embellishments = list(all_method_names = all_method_names,
                               all_method_colors = all_method_colors,
                               all_method_labels = all_method_labels,
                               all_method_sizes = all_method_sizes,
                               all_method_shapes = all_method_shapes);

    return(plot_embellishments);
}

plot.color.array = function(color_array){
    par(mar=c(10,0,0,0));
    image(x = 1:length(col), y = 1, matrix(1:length(color_array), ncol=1), col=color_array, xlab = "" , ylab ="");
    axis(1, at=seq(1,length(color_array)), labels =names(color_array), las=2, cex=0.5, tick =FALSE);
}

human.numbers = function(x = NULL, smbl =""){
    #https://github.com/fdryan/R/blob/master/ggplot2_formatter.r
    humanity <- function(y){

        if (!is.na(y)){

            b <- round(abs(y) / 1e9, 0.1)
            m <- round(abs(y) / 1e6, 0.1)
            k <- round(abs(y) / 1e3, 0.1)

            if ( y >= 0 ){
                y_is_positive <- ""
            } else {
                y_is_positive <- "-"
            }

            if ( k < 1 ) {
                paste0(y_is_positive, smbl, y )
            } else if ( m < 1){
                paste0 (y_is_positive, smbl,  k , "K")
            } else if (b < 1){
                paste0 (y_is_positive, smbl, m ,"M")
            } else {
                paste0 (y_is_positive, smbl,  comma(b), "N")
            }
        }
    }
    sapply(x,humanity)
}

label.digits = function(l) {
    # turn in to character string in scientific notation
    na_ind = which(is.na(l))
    l <- format(l, nsmall = 3, scientific = FALSE);
    l[na_ind] = NA
    return(l)
}

label.auc = function(l) {
    # turn in to character string in scientific notation
    na_ind = which(is.na(l))
    l <- format(l, nsmall = 3, scientific = FALSE);
    l[na_ind] = NA
    return(l)
}

grab.legend = function(p){
    tmp = ggplot_gtable(ggplot_build(p))
    leg = which(sapply(tmp$grobs, function(y) y$name) == "guide-box")
    legend_plot = tmp$grobs[[leg]]
    return(legend_plot)
}

create.horizontal.legend.plot = function(plot_helper = default.plot.embellishments(),
                                         legend_method_names = NULL,
                                         df = NULL,
                                         n_rows = 1,
                                         right_label_padding = 2,
                                         left_label_padding  = 2){

    #df is a data.frame with method_name
    #plot_helper must have all_method_colors or all_method_labels
    if (is.null(legend_method_names)){
        if (is.null(df)){
            legend_method_names = names(plot_helper$all_method_colors);
        } else{
            legend_method_names = df %>% distinct(method_name) %>% pull()
        }
    }
    stopifnot(legend_method_names %in% names(plot_helper$all_method_colors));
    stopifnot(legend_method_names %in% names(plot_helper$all_method_labels));

    all_method_colors = plot_helper$all_method_colors[legend_method_names];
    all_method_labels = plot_helper$all_method_labels[legend_method_names];

    #add padding to the right of labels to space out the boxes
    all_method_labels = str_trim(all_method_labels);
    max_label_chars = max(sapply(all_method_labels, nchar));
    all_method_labels = str_pad(all_method_labels, width= max_label_chars + left_label_padding, side="right");
    all_method_labels = str_pad(all_method_labels, width= max_label_chars + left_label_padding + right_label_padding, side="left");
    names(all_method_labels) = legend_method_names

    #create sample data
    sample_data = expand.grid(method_name = legend_method_names,
                              xmin = 2.5, x= 5, xmax = 7.5,
                              ymin = 0.125, y= 0.15, ymax = 0.205)

    my_theme = theme_bw() + theme(title = element_blank()
                                  ,axis.title.x = element_blank()
                                  ,axis.text.x 	= element_blank()
                                  ,axis.ticks.x	= element_blank()
                                  ,axis.title.y = element_blank()
                                  ,axis.text.y 	= element_blank()
                                  ,axis.ticks.y	= element_blank()
                                  ,axis.line    = element_blank()
                                  ,panel.grid.major = element_blank()
                                  ,panel.grid.minor = element_blank()
                                  ,panel.border = element_blank()
                                  ,legend.position="bottom"
                                  ,legend.spacing = unit(200, "cm"))

    my_legend = guide_legend(title = element_blank()
                             ,title.theme = element_blank()
                             ,label.theme = element_text(face = "plain", size=14, angle=0, lineheight=30)
                             ,nrow = n_rows
                             ,byrow = FALSE
                             ,keywidth = 1.5
                             ,keyheight = 1.5)

    tmp_plot = ggplot(sample_data) +
        aes(x = x, y = y, xmax = xmax, xmin = xmin, ymin = ymin, ymax = ymax, color = method_name, fill = method_name) +
        geom_rect(color=NA, size=0, alpha=0.25) +
        geom_point(size=6, shape=19) +
        scale_fill_manual(name="Method", guide="legend", values = all_method_colors, labels = all_method_labels, drop=TRUE) +
        scale_color_manual(name="Method", guide="legend", values = all_method_colors, labels = all_method_labels, drop=TRUE) +
        my_theme + guides(fill = my_legend, color = my_legend, ncol = 5, byrow = TRUE);

    legend_plot = grab.legend(tmp_plot)
    return(legend_plot)
}
#### PDF Creation #####

safe.dir = function(dir_name){
    last_char = substr(dir_name,nchar(dir_name),nchar(dir_name));
    if (last_char != "/") {
        dir_name = paste0(dir_name,"/");
    }
    return(dir_name);
}

open.pdf  = function(pdf_file){
    system(sprintf("open \"%s\"", pdf_file))
}

jam.pdfs =  function(files_to_merge, merged_file_name="report.pdf", rows = NULL, cols = 2, landscape = FALSE, open_after=FALSE, delete_originals=FALSE){

    file_exists = array(sapply(files_to_merge, file.exists))
    stopifnot(all(file_exists))

    n_files = length(files_to_merge);
    if (n_files > 0){

        if (is.null(rows)){
            rows = n_files %/% cols;
        }
        if (!grepl("*.pdf", merged_file_name)){
            merged_file_name = paste0(merged_file_name,".pdf");
        }
        size_string = sprintf("%dx%d",cols, rows)
        files_string = paste(files_to_merge, collapse='\" \"');

        if (landscape){
            system(sprintf("pdfjam \"%s\" --nup %s --landscape --outfile \"%s\"",files_string, size_string, merged_file_name))
        } else {
            system(sprintf("pdfjam \"%s\" --nup %s --no-landscape --outfile \"%s\"",files_string, size_string, merged_file_name))
        }

        if (delete_originals){
            file.remove(files_to_merge);
        }

        if (open_after){
            system(sprintf("open \"%s\"", merged_file_name))
        }
        return(merged_file_name)
    }
}

#### Knitting Reports ####

knit.report = function(report_name,
                       report_data,
                       input_report_dir,
                       output_report_dir,
                       output_report_name,
                       remove_pdf=FALSE,
                       remove_tex=FALSE,
                       open_after = FALSE,
                       quiet = TRUE,
                       debug = FALSE){

    #set directories
    input_report_dir  = safe.dir(input_report_dir);
    output_report_dir = safe.dir(output_report_dir);
    report_name = gsub("\\.rnw", "", basename(report_name))

    #input files
    input_rnw_name     			= paste0(input_report_dir, report_name, ".Rnw");

    #output files and directories
    output_report_name              = basename(output_report_name)
    output_report_name              = gsub("\\.pdf", "", output_report_name);
    output_report_name              = gsub("\\.tex", "", output_report_name);
    output_pdf_name 				= paste0(output_report_dir, output_report_name, ".pdf");
    output_tex_name 				= paste0(output_report_dir, output_report_name, ".tex");
    output_fig_dir   				= paste0(output_report_dir,"figure/");

    dir.create(output_report_dir,showWarnings = FALSE);
    dir.create(output_fig_dir,showWarnings = FALSE);

    #compile files
    compile_dir     = "~/knitr_temp/"
    compile_fig_dir = "~/knitr_temp/figure/"

    #Delete Old Director
    unlink(compile_dir, recursive=TRUE, force=TRUE);
    dir.create(compile_dir, showWarnings = FALSE);
    dir.create(compile_fig_dir, showWarnings = FALSE);

    #Setup Compiled Filenames
    compile_Rnw_name   			= paste0(compile_dir,report_name,".rnw");
    compile_pdf_name     		= paste0(compile_dir,report_name,".pdf");
    compile_tex_name     		= paste0(compile_dir,report_name,".tex");

    #Copy Files from Working Directory to Compilation Directory
    file.copy(from = input_rnw_name, to = compile_Rnw_name)

    #Knit Report
    current_dir = getwd();
    if (debug){

        setwd(compile_dir);

        knit2pdf(input=compile_Rnw_name, quiet = FALSE);

        #Copy Tex/PDF
        file.copy(from=compile_pdf_name,to=output_pdf_name,overwrite=TRUE);
        file.copy(from=compile_tex_name,to=output_tex_name,overwrite=TRUE);

        #Copy Figure Files
        fig_files=list.files(path=compile_fig_dir);
        file.copy(from=paste0(compile_fig_dir,fig_files),to=paste0(output_fig_dir,fig_files));

        setwd(current_dir);
        unlink(compile_dir,recursive=TRUE,force=TRUE);

    } else {
        tryCatch({

            setwd(compile_dir);
            knit2pdf(input=compile_Rnw_name, quiet = quiet);

            #Copy Tex/PDF
            file.copy(from=compile_pdf_name, to=output_pdf_name, overwrite=TRUE);
            file.copy(from=compile_tex_name, to=output_tex_name, overwrite=TRUE);

            #Copy Figure Files
            fig_files=list.files(path=compile_fig_dir);
            file.copy(from=paste0(compile_fig_dir,fig_files),to=paste0(output_fig_dir,fig_files));

        }, error = function(e) {
            print("compilation error");
            print(sprintf("%s", print(e)));
        }, finally = {
            setwd(current_dir);
            unlink(compile_dir, recursive=TRUE, force=TRUE);
        })
    }

    if (remove_tex){
        file.remove(output_tex_name)
    }
    if (open_after){
        open.pdf(output_pdf_name)
    }

    if (remove_pdf) {
        file.remove(output_pdf_name)
        return(NULL)
    } else {
        return(output_pdf_name);
    }

}

#### Printing Models ######

get.linear.model.xtable = function(coefs, max_model_size=NULL, n_vars_per_row=3, xnames, yname, remove_score=FALSE){

    #remove zero coefficients
    nnz_ind = coefs!=0.0;
    xnames 	= xnames[nnz_ind];
    coefs 	= coefs[nnz_ind];
    n_coefs = length(coefs);

    if (is.null(max_model_size) || (n_coefs <= max_model_size)){

        #group positive and negative coefficients together and sort them by size
        pos_ind  = coefs>0;
        pos_coefs = coefs[pos_ind];
        pos_names = xnames[pos_ind];

        pos_coefs = sort(pos_coefs,decreasing=TRUE,index.return=TRUE);
        pos_ind 	= pos_coefs$ix;
        pos_coefs = pos_coefs$x;
        pos_names = pos_names[pos_ind];

        neg_ind  	= coefs<0;
        neg_coefs = coefs[neg_ind];
        neg_names = xnames[neg_ind];

        neg_coefs = sort(neg_coefs,decreasing=FALSE,index.return=TRUE);
        neg_ind 	= neg_coefs$ix;
        neg_coefs = neg_coefs$x;
        neg_names = neg_names[neg_ind];

        xnames = c(pos_names,neg_names);
        coefs = c(pos_coefs,neg_coefs);

        #shift intercept to the end of the model
        ind 	= xnames=="(Intercept)";
        xnames 	= c(xnames[!ind],xnames[ind]);
        coefs  	= c(coefs[!ind],coefs[ind]);

        #express model as a score string
        n_coefs = length(coefs);
        score_string = array(NA,2*n_coefs);
        integer_coefficients = all(ceiling(coefs)==floor(coefs));

        for (n in 1:n_coefs){

            coef_val 	= abs(coefs[n])
            coef_sign = sign(coefs[n]);
            coef_name = xnames[n];

            #Sign
            if ((n==1)&&(coef_sign==1)) {
                sign_string = ""
            } else if (coef_sign>0) {
                sign_string = "$\\scriptsize{+}$"
            } else if (coef_sign<0) {
                sign_string = "$\\scriptsize{-}$"
            }

            # Variable
            if (integer_coefficients){

                if (coef_name=="(Intercept)"){
                    coef_string = sprintf("$%d$",coef_val)
                } else {
                    if (coef_val == 1){
                        coef_string = sprintf("$\\textfn{%s}$", coef_name);
                    } else {
                        coef_string = sprintf("$%d ~\\textfn{%s}$", coef_val, coef_name);
                    }
                }

            } else {

                if ((abs(coef_val)<0.01)||abs(coef_val)>99999){
                    coef_val_string = sprintf("%1.2e",coef_val);
                    coef_val_string = sprintf("%s}",gsub("e"," \\\\times 10^{",coef_val_string));
                } else {
                    coef_val_string = sprintf("%1.2f",coef_val);
                }

                if (coef_name=="(Intercept)"){
                    coef_string = sprintf("$%s$",coef_val_string);
                } else {
                    coef_string = sprintf("$%s ~\\textfn{%s}$", coef_val_string, coef_name);
                }

            }

            score_string[2*n-1] = sign_string;
            score_string[2*n]  	= coef_string;
        }

        #extend score string so that it has empty entries in remaining rows
        n_last_col = n_coefs %% n_vars_per_row;
        if (n_last_col < n_vars_per_row){
            n_empty_cols = n_vars_per_row - n_last_col;
            score_string = c(score_string,rep("",n_empty_cols*2))
        }

        #create model table
        if(remove_score){

            #create model table
            n_rows   		= ceiling(n_coefs/n_vars_per_row)
            n_cols  		= 2*n_vars_per_row
            model_table = array("",c(n_rows,n_cols));
            model_table[1:n_rows,1:n_cols] = t(array(score_string,dim=c(n_cols,n_rows)))

            #put in latex form
            model_table = xtable(as.data.frame(model_table))
            align(model_table)  = c("l",rep(c("p{1mm}","l"),n_vars_per_row));
            digits(model_table) = rep(0,n_cols+1);

        } else{

            n_rows 			= ceiling(n_coefs/n_vars_per_row)
            n_cols  		= 1 + 2*n_vars_per_row
            model_table = array("",c(n_rows,n_cols));
            model_table[1,1] = "\\textfn{Score} ="
            model_table[1:n_rows,2:n_cols] = t(array(score_string,dim=c(n_cols-1,n_rows)))

            #put in latex form
            model_table = xtable(as.data.frame(model_table))
            align(model_table)  = c("l","l",rep(c("p{1mm}","l"),n_vars_per_row));
            digits(model_table) = rep(0,n_cols+1);

        }
        return(model_table);
    }
}

get.c50.model = function(model, model_size, fold_index=NULL, max_model_size=NULL){

    if (is.null(max_model_size) || (model_size <= max_model_size)) {
        model = model$output;
        if (method_name == "c50_tree"){
            model = gsub(".*\n\nDecision tree:\n\n","",model);
        } else if (method_name == "c50_rule") {
            model = gsub(".*\n\nRules:\n\n","",model);
        }
        model = gsub("\n\n\nEvaluation on training data .*","",model);
    }
    return(model);
}

texify.variable.names = function(xnames, yname, math_environment = TRUE){
    all_names = list(xnames = xnames, yname = yname);
    if (math_environment){
        for (i in 1:length(all_names)){
            v = all_names[[i]];
            v = gsub("_leq_", " \\\\leq ", v);
            v = gsub("_geq_", " \\\\geq ", v);
            v = gsub("_lt_", " < ", v);
            v = gsub("_gt_", " > ", v);
            v = gsub("_eq_", " = ", v);
            v = gsub("_", "\\\\,", v);
            all_names[[i]] = v;
        }
    } else {
        for (i in 1:length(all_names)){
            v = all_names[[i]];
            v = gsub("_leq_", " $\\leq$", v);
            v = gsub("_geq_", " $\\geq$", v);
            v = gsub("_lt_", " $<$ ", v);
            v = gsub("_gt_", " $>$ ", v);
            v = gsub("_eq_", " $=$ ", v);
            v = gsub("_", " ", v);
            all_names[[i]] = v;
        }
    }

    return(all_names);
}

get.scoring.system.xtable = function(coefs, xnames, yname, include_subtotals = FALSE, risk_score_flag = FALSE){

    #shift intercept to the end of the model
    ind 	= xnames==INTERCEPT_NAME;
    cutoff 	= as.numeric(coefs[ind]);
    xnames 	= xnames[!ind]
    coefs  	= coefs[!ind]

    #remove zero coefficients
    nnz_ind = coefs!=0.0;
    xnames 	= xnames[nnz_ind];
    coefs 	= coefs[nnz_ind];
    n_coefs = length(coefs);

    #group positive and negative coefficients together and sort them by size
    pos_ind = coefs>0;
    pos_coefs = coefs[pos_ind];
    pos_names = xnames[pos_ind];

    pos_coefs = sort(pos_coefs,decreasing=TRUE,index.return=TRUE);
    pos_ind = pos_coefs$ix;
    pos_coefs = pos_coefs$x;
    pos_names = pos_names[pos_ind];

    neg_ind  = coefs<0;
    neg_coefs = coefs[neg_ind];
    neg_names = xnames[neg_ind];
    neg_coefs = sort(neg_coefs,decreasing=TRUE,index.return=TRUE);
    neg_ind = neg_coefs$ix;
    neg_coefs = neg_coefs$x;
    neg_names = neg_names[neg_ind];

    xnames = c(pos_names, neg_names);
    coefs = c(pos_coefs, neg_coefs);

    #create strings
    n_pos = length(pos_coefs)
    n_neg = length(neg_coefs)
    n_int = length(ind)

    if (include_subtotals){

        pos_nums_string = sprintf("%d.",seq(1,n_pos))
        pos_coefs_string = sprintf("$\\textssm{%s}$",pos_names)
        pos_points_string = sprintf("%d points",abs(pos_coefs))
        pos_points_string[abs(pos_coefs)==1] = "1 point"

        if (n_pos > 1){
            pos_user_string 		= c("$\\phantom{+}\\prow{}$", rep("$+\\prow{}$",n_pos-1))
            pos_total_string 		= sprintf("\\small{\\textbf{ADD POINTS FROM ROWS %d-%d}}",1,n_pos)
        } else if (n_pos ==1) {
            pos_user_string 		= "$\\phantom{+}\\prow{}$"
            pos_total_string 		= sprintf("\\small{\\textbf{ADD POINTS FROM ROW %d}}",1)
        }

        neg_nums_string 		= sprintf("%d.",seq(n_pos+1,n_pos+n_neg))
        neg_coefs_string   	= sprintf("$\\textssm{%s}$",neg_names)
        neg_points_string 	= sprintf("%d points",abs(neg_coefs))
        neg_points_string[abs(neg_coefs)==1] = "1 point"

        if (n_neg > 1){
            neg_user_string 		= c("$\\phantom{+}\\prow{}$",rep("$+\\prow{}$",n_neg-1))
            neg_total_string 		= sprintf("\\small{\\textbf{ADD POINTS FROM ROWS %d-%d}}",n_pos+1,n_pos+n_neg)
        } else if (n_neg ==1) {
            neg_user_string 		= "$\\phantom{+}\\prow{}$"
            neg_total_string 		= sprintf("\\small{\\textbf{ADD POINTS FROM ROWS %d}}",n_pos+1)
        }

        if ((n_pos > 0) & (n_neg >0) ){

            nums_column			= c(pos_nums_string,"",neg_nums_string,"","")
            coefs_column		= c(pos_coefs_string,pos_total_string,neg_coefs_string,neg_total_string,"\\small{\\textbf{SUBTRACT TOTAL B FROM TOTAL A}}")
            points_column		= c(pos_points_string,"\\small{\\textbf{TOTAL A}}",neg_points_string,"\\small{\\textbf{TOTAL B}}","\\scorelabel{}")
            user_column 		= c(pos_user_string,"$=\\prow{}$",neg_user_string,"$=\\phantom{\\prow{}}$","$=\\phantom{\\prow{}}$")

            pred_guide 			= sprintf("\\predcell{c}{PREDICT %s IF SCORE $> %d$}", yname,abs(cutoff))
            hline_guide = c(0,n_pos,n_pos+1,n_pos+n_neg+1,n_pos+n_neg+2,n_pos+n_neg+3)

        } else if ((n_pos>0) & (n_neg==0)){

            nums_column			= c(pos_nums_string,"")
            coefs_column 		= c(pos_coefs_string,pos_total_string)
            points_column		= c(pos_points_string,"\\scorelabel{}")
            user_column 		= c(pos_user_string,"$=\\phantom{\\prow{}}$")

            pred_guide 			= sprintf("\\predcell{c}{PREDICT %s IF SCORE $> %d$}", yname,abs(cutoff))

            hline_guide = c(0,n_pos,n_pos+1)

        } else if ((n_pos==0) & (n_neg>0)){

            nums_column			= c(neg_nums_string,"")
            coefs_column 		= c(neg_coefs_string,neg_total_string)
            points_column		= c(neg_points_string,"\\scorelabel{}")
            user_column 		= c(neg_user_string,"$=\\phantom{\\prow{}}$")

            pred_guide 			= sprintf("\\predcell{c}{PREDICT %s if SCORE $< %d$}", yname,-(abs(cutoff)))

            hline_guide = c(0,n_neg,n_neg+1)

        }

    } else if ((n_pos > 0) | (n_neg > 0)){

        if (n_pos>0){
            pos_nums_string   	= sprintf("%d.",seq(1,n_pos))
            pos_coefs_string 	= sprintf("$\\textssm{%s}$",pos_names)
            pos_points_string 	= sprintf("%d points",abs(pos_coefs))
            pos_points_string[abs(pos_coefs)==1] = "1 point"
        } else{
            pos_nums_string = NULL;
            pos_coefs_string = NULL;
            pos_points_string = NULL;
        }

        if (n_neg>0){
            neg_nums_string 	= sprintf("%d.",seq(n_pos+1,n_pos+n_neg))
            neg_coefs_string   	= sprintf("$\\textssm{%s}$",neg_names)
            neg_points_string 	= sprintf("-%d points",abs(neg_coefs))
            neg_points_string[abs(neg_coefs)==1] = "-1 point"
        } else {
            neg_nums_string   	= NULL
            neg_coefs_string   	= NULL
            neg_points_string 	= NULL
        }

        total_string   	= sprintf("\\instruction{%d}{%d}",1,n_pos+n_neg)
        nums_column  		= c(pos_nums_string, neg_nums_string, "")
        coefs_column		= c(pos_coefs_string, neg_coefs_string, total_string)
        points_column		= c(pos_points_string, neg_points_string, "\\scorelabel{}")
        user_column 		= c("$\\phantom{+}\\prow{}$", rep("$+\\prow{}$", n_pos + n_neg - 1), "$=\\phantom{\\prow{}}$");
        pred_guide 			= ifelse(risk_score_flag, "", sprintf("\\predcell{c}{PREDICT %s IF SCORE $> %d$}\n", yname, abs(cutoff)))
        hline_guide         = c(0,n_pos+n_neg,n_pos+n_neg+1)

    } else {

        total_string   	= "all coefficients are 0"
        nums_column  	= ""
        coefs_column	= ""
        points_column	= ""
        user_column 	= ""
        pred_guide 		= ""
        hline_guide     = c(0,1)

    }

    #remove weird characters
    pred_guide 		= gsub("_","\\_",pred_guide,fixed=TRUE)
    coefs_column 	= gsub("_","\\_",coefs_column,fixed=TRUE)

    model_table = cbind(nums_column,coefs_column,points_column,user_column);
    colnames(model_table) = NULL;
    rownames(model_table) = NULL;
    model_table = xtable(as.data.frame(model_table))

    align(model_table)  = c("|","l","|","l"," l "," c ","|"," c ","|")
    digits(model_table) = rep(0, 5);

    #compile output
    scoring_system = list(
        pred_guide = pred_guide,
        xtable = model_table,
        hline_guide = hline_guide,
        size_command = paste0("\\scoringsystem{}\n",pred_guide,"\n\\vspace{0.5em}")
    )

    return(scoring_system)
}

get.regression.score.xtable = function(coefs, xnames, yname, include_subtotals = FALSE, risk_score_flag = FALSE){

    #shift intercept to the end of the model
    ind 	= xnames==INTERCEPT_NAME;
    offset 	= as.numeric(coefs[ind]);
    xnames 	= xnames[!ind]
    coefs  	= coefs[!ind]

    #remove zero coefficients
    nnz_ind = coefs!=0.0;
    xnames 	= xnames[nnz_ind];
    coefs 	= coefs[nnz_ind];
    n_coefs = length(coefs);

    #group positive and negative coefficients together and sort them by size
    pos_ind = coefs>0;
    pos_coefs = coefs[pos_ind];
    pos_names = xnames[pos_ind];

    pos_coefs = sort(pos_coefs,decreasing=TRUE,index.return=TRUE);
    pos_ind = pos_coefs$ix;
    pos_coefs = pos_coefs$x;
    pos_names = pos_names[pos_ind];

    neg_ind  = coefs<0;
    neg_coefs = coefs[neg_ind];
    neg_names = xnames[neg_ind];
    neg_coefs = sort(neg_coefs,decreasing=TRUE,index.return=TRUE);
    neg_ind = neg_coefs$ix;
    neg_coefs = neg_coefs$x;
    neg_names = neg_names[neg_ind];

    xnames = c(pos_names, neg_names);
    coefs = c(pos_coefs, neg_coefs);

    #create strings
    n_pos = length(pos_coefs)
    n_neg = length(neg_coefs)
    n_int = length(ind)

    if ((n_pos > 0) | (n_neg > 0)){

        if (n_pos>0){
            pos_nums_string   	= sprintf("%d.",seq(1,n_pos))
            pos_coefs_string 	= sprintf("$\\textssm{%s}$",pos_names)
            pos_points_string 	= sprintf("%d points",abs(pos_coefs))
            pos_points_string[abs(pos_coefs)==1] = "1 point"
        } else{
            pos_nums_string = NULL;
            pos_coefs_string = NULL;
            pos_points_string = NULL;
        }

        if (n_neg>0){
            neg_nums_string 	= sprintf("%d.",seq(n_pos+1,n_pos+n_neg))
            neg_coefs_string   	= sprintf("$\\textssm{%s}$",neg_names)
            neg_points_string 	= sprintf("-%d points",abs(neg_coefs))
            neg_points_string[abs(neg_coefs)==1] = "-1 point"
        } else {
            neg_nums_string   	= NULL
            neg_coefs_string   	= NULL
            neg_points_string 	= NULL
        }

        offset_nums_string = sprintf("%d.", n_pos+n_neg+1)
        offset_coefs_string = "$\\textssm{Offset}$"
        if (offset > 0){
            offset_points_string = sprintf("%d points",abs(offset))
        } else {
            offset_points_string = sprintf("-%d points",abs(offset))
        }

        total_string   	= sprintf("\\instruction{%d}{%d}",1,n_pos+n_neg)
        nums_column     = c(pos_nums_string, neg_nums_string, offset_nums_string, "")
        coefs_column		= c(pos_coefs_string, neg_coefs_string, offset_coefs_string, total_string)
        points_column		= c(pos_points_string, neg_points_string, offset_points_string, "\\scorelabel{}")
        user_column 		= c("$\\phantom{+}\\prow{}$", rep("$+\\prow{}$", n_pos + n_neg), "$=\\prow{}$");
        pred_guide 			= ""
        hline_guide         = c(0,n_pos+n_neg+1,n_pos+n_neg+2)

    } else {

        total_string   	= "all coefficients are 0"
        nums_column  	= ""
        coefs_column	= ""
        points_column	= ""
        user_column 	= ""
        pred_guide 		= ""
        hline_guide     = c(0,1)

    }

    #remove weird characters
    pred_guide 		= gsub("_","\\_",pred_guide,fixed=TRUE)
    coefs_column 	= gsub("_","\\_",coefs_column,fixed=TRUE)

    model_table = cbind(nums_column,coefs_column,points_column,user_column);
    colnames(model_table) = NULL;
    rownames(model_table) = NULL;
    model_table = xtable(as.data.frame(model_table))
    align(model_table)  = c("|","l","|","l"," l "," c ","|"," c ","|")
    digits(model_table) = rep(0, 5);

    #compile output
    scoring_system = list(
        pred_guide = pred_guide,
        xtable = model_table,
        hline_guide = hline_guide,
        size_command = paste0("\\scoringsystem{}\n")
    )

    return(scoring_system)
}

get.risk.xtable = function(df,
                           lower_risk_threshold = 0.01,
                           upper_risk_threshold = 0.99,
                           score_digits = 1,
                           risk_digits = 0,
                           intercept_value = 0,
                           adjust_to_intercept = TRUE,
                           portrait_orientation = FALSE,
                           score_column_title = "\\scorelabel{}",
                           risk_column_title = "\\risklabel{}"){


    if (adjust_to_intercept){
        if (intercept_value > 0){
            df$score.min = df$score.min + intercept_value;
            df$score.max = df$score.max + intercept_value;
        } else if (intercept_value < 0){
            df$score.min = df$score.min - intercept_value;
            df$score.max = df$score.max - intercept_value;
        }
    }

    df = df %>%
        arrange(risk) %>%
        rowwise() %>%
        mutate(score.has.range = !(score.min == score.max),
               score.is.integer = score.min == round(score.min) && score.max == round(score.max),
               score.min.fixed = min(score.min, score.max),
               score.max.fixed = max(score.min, score.max)) %>%
        select(score.min = score.min.fixed,
               score.max = score.max.fixed,
               risk,
               score.has.range,
               score.is.integer) %>%
        ungroup();


    print.score.column = function(score.min, score.max, score.is.integer, score.has.range, score_digits){
        score_digits = ifelse(score.is.integer, 0, score_digits);
        if (score.has.range){
            return(print.value.range(score.min, score.max, score_digits, range_separator = " to "));
        } else {
            return(print.value(score.min, score_digits));
        }
    }

    print.error = function(mean_err, digits=1, na_string = "-"){
        mean_err = as.numeric(mean_err)
        printed_values = paste0(formatC(100*mean_err, format="f",digits=digits),"$\\%$");
        printed_values[is.na(mean_err)] = na_string
        names(printed_values) = names(mean_err);
        return(printed_values)
    }

    n_top_rows = df %>% filter(risk < lower_risk_threshold) %>% nrow();
    n_bottom_rows = df %>% filter(risk > upper_risk_threshold) %>% nrow();
    n_middle_rows = df %>% filter(risk >= lower_risk_threshold, risk <= upper_risk_threshold) %>% nrow();

    #build risk table
    risk_table = data.frame();

    #middle rows
    if (n_middle_rows > 0){

        middle_rows = df %>%
            filter(risk >= lower_risk_threshold, risk <= upper_risk_threshold) %>%
            rowwise() %>%
            mutate(score_column = print.score.column(score.min, score.max, score.is.integer, score.has.range, score_digits),
                   risk_column = print.error(risk, risk_digits)) %>%
            select(score_column, risk_column) %>%
            ungroup();
        risk_table = bind_rows(risk_table, middle_rows)
    }

    #top rows
    if (n_top_rows == 1){

        top_row = df %>%
            filter(risk < lower_risk_threshold) %>%
            mutate(score_column = print.score.column(score.min, score.max, score.is.integer, score.has.range, score_digits),
                 risk_column = print.error(risk, risk_digits)) %>%
            select(score_column, risk_column);

        risk_table = bind_rows(top_row, risk_table);

    } else if (n_top_rows > 1){

        top_row = df %>%
            filter(risk < lower_risk_threshold) %>%
            summarise(score.min = min(score.min),
                      score.max = max(score.max),
                      risk = lower_risk_threshold) %>%
            ungroup() %>%
            mutate(score.has.range = !(score.min == score.max),
                   score.is.integer = score.min == round(score.min) && score.max == round(score.max),
                   score_column = print.score.column(score.min, score.max, score.is.integer, score.has.range, score_digits),
                   risk_column = paste0("$<$ ", print.error(risk, risk_digits))) %>%
            select(score_column, risk_column);

        risk_table = bind_rows(top_row, risk_table);
    }

    if (n_bottom_rows == 1){

        bottom_row = df %>%
            filter(risk > upper_risk_threshold) %>%
            mutate(score_column = print.score.column(score.min, score.max, score.is.integer, score.has.range, score_digits),
                   risk_column = print.error(risk, risk_digits)) %>%
            select(score_column, risk_column);

        risk_table = bind_rows(risk_table, bottom_row);

    } else if (n_bottom_rows > 1){

        bottom_row = df %>%
            ungroup() %>%
            filter(risk > upper_risk_threshold) %>%
            summarise(score.min = min(score.min),
                      score.max = max(score.max),
                      risk = upper_risk_threshold) %>%
            mutate(score.has.range = !(score.min == score.max),
                   score.is.integer = score.min == round(score.min) && score.max == round(score.max),
                   score_column = print.score.column(score.min, score.max, score.is.integer, score.has.range, score_digits),
                   risk_column = paste0("$>$ ", print.error(risk, risk_digits))) %>%
            select(score_column, risk_column);

        risk_table = bind_rows(risk_table, bottom_row);
    }
    n_scores = nrow(risk_table);

    #create table
    score_field_title = paste0("\\rowcolor{scorecolor}", score_column_title)
    risk_field_title = paste0("\\rowcolor{riskcolor}", risk_column_title)

    risk_xtable = rbind(c(score_field_title, risk_field_title), risk_table);
    colnames(risk_xtable) = NULL;
    rownames(risk_xtable) = NULL;
    if (portrait_orientation){
        risk_xtable = xtable(risk_xtable);
        align(risk_xtable)  = c("|","l","|","c","|", "r","|");
    } else {
        risk_xtable = t(risk_xtable);
        risk_xtable = xtable(risk_xtable);
        align(risk_xtable)  = c("|","l","|","l","|",rep(c("c","|"), n_scores))
    }
    digits(risk_xtable) = 0;

    #return table object
    tbl = list('table' = risk_table,
               'xtable' = risk_xtable,
               'hline_guide' = c(0, 1, nrow(risk_xtable)),
               'size_command' = "\\risktable{}")

    return(tbl);
}


#### Table Creation #####

sanitize.tex = function(x){
    sanitize(x, type = "latex")
}

bold.tex = function(x){
    paste0('{\\bfseries ', x, '}')
}

print.error = function(mean_err, digits=1, na_string = "-"){
    printed_values = paste(formatC(100*mean_err, format="f",digits=digits),"$\\%$",sep="");
    printed_values[is.na(mean_err)] = na_string
    names(printed_values) = names(mean_err);
    return(printed_values)
}

print.mean = function(mean_err, digits=1){
    mean_pm_std = paste(formatC(100*mean_err,format="f",digits=digits),"$\\%$",sep="");
    names(mean_pm_std) = names(mean_err);
    return(mean_pm_std)
}

print.mean.pm.std = function(mean_err,std_err,digits=1){
    mean_err 		= formatC(100*mean_err,format="f",digits=digits);
    std_err 		= formatC(100*std_err,format="f",digits=digits);
    mean_pm_std = sprintf("%s $\\pm$ %s%s",mean_err,std_err,"$\\%$");
    names(mean_pm_std) = names(mean_err);
    return(mean_pm_std)
}

print.mean.pm.std.scalar = function(mean_val,std_val,digits=1){
    mean_val 		= formatC(mean_val,format="f",digits=digits);
    std_val 		= formatC(std_val,format="f",digits=digits);
    mean_pm_std = sprintf("%s $\\pm$ %s",mean_val,std_val);
    names(mean_pm_std) = names(mean_val);
    return(mean_pm_std)
}

print.value = function(val,digits = 1){
    printed_value = paste(formatC(val,format="f",digits=digits),sep="");
    names(printed_value) = names(val);
    return(printed_value)
}

print.value.range = function(lower_value, upper_value, digits = 3, range_separator = " - "){
    value_range = paste(formatC(lower_value,format="f",digits=digits),range_separator,formatC(upper_value,format="f",digits=digits),sep="")
    names(value_range) = names(value_range);
    return(value_range);
}

print.error.range = function(lower_value,upper_value,digits=1){
    mean_range = paste0(formatC(100*lower_value,format="f",digits=digits)," - ",formatC(100*upper_value,format="f",digits=digits),"$\\%$")
    names(mean_range) = names(lower_value);
    return(mean_range);
}

print.mean.range = function(lower_value,upper_value,digits=1){
    mean_range = paste0(formatC(100*lower_value,format="f",digits=digits)," - ",formatC(100*upper_value,format="f",digits=digits),"$\\%$")
    names(mean_range) = names(lower_value);
    return(mean_range);
}

print.model.range = function(lower_value,upper_value){
    model_range = paste(formatC(lower_value,format="d")," - ",formatC(upper_value,format="d"),sep="")
    names(model_range) = names(lower_value);
    return(model_range);
}

print.bfcell.header = function(header){
    header = gsub("\\","\\\\",header,fixed=TRUE)
    header = sprintf("\\bfcell{c}{%s}",header);
    return(header)
}

print.tex.header = function(header){
    header = gsub("\\","\\\\",header,fixed=TRUE)
    header = sprintf("\\begin{tabular}{>{\\bf}c}%s\\end{tabular}",header);
    return(header)
}

make.single.tabular = function(cells){

    if (class(cells)=="data.frame"){
        cells = sapply(cells,function(x) as.character(x))
    }

    tab = paste0("\\cell{c}{",paste(cells,collapse="\\\\"),"}")

    return(tab)
}

make.multi.tabulars = function(top_cells,bottom_cells,top_size="scriptsize",bottom_size="tiny"){
    if (!is.null(top_size)){
        top_cells = paste(paste0("\\",top_size,"{"),top_cells,"}",sep="")
    }
    if (!is.null(bottom_size)){
        bottom_cells = paste(paste0("\\",bottom_size,"{"),bottom_cells,"}",sep="")
    }
    cells = rbind(top_cells,bottom_cells);
    cells = apply(cells,2,function(x) make.single.tabular(x));
    return(cells);
}

cells.to.tabular = function(cell_contents, cell_type = "cell{c}"){

    if (class(cell_contents)=="data.frame"){
        cell_contents = sapply(cell_contents,function(x) as.character(x))
    }

    table_of_cells = paste0("\\", cell_type, "{")

    for (n in 1:length(cell_contents)){
        if (n < length(cell_contents)){
            table_of_cells = paste0(table_of_cells, as.character(cell_contents[n]),"\\\\")
        } else {
            table_of_cells = paste0(table_of_cells, as.character(cell_contents[n]))
        }
    }

    table_of_cells = paste0(table_of_cells, "}")
    return(table_of_cells)
}

#### Post-Processing ####

run.platt.scaling = function(model, data, cv_metric, fold){

    out = data.to.XY(data, cv_metric, fold)
    coefficients = get.coefficients.from.model(model)

    #compute scores
    scores = out$X %*% coefficients

    #run platt glm
    platt_glm = glm(formula(Y ~ score), data = data.frame(score = scores, Y = out$Y, check.names = FALSE), family = binomial, x = FALSE, y = FALSE)

    #fix platt parameters for trivial models
    platt_intercept = platt_glm$coefficients[[1]]
    platt_scale = platt_glm$coefficients[[2]]
    platt_intercept = ifelse(is.na(platt_intercept), 0, platt_intercept)
    platt_scale = ifelse(is.na(platt_scale), 0, platt_scale)

    #store platt model
    model = list(type="platt", coefficients = coefficients, platt_intercept = platt_intercept, platt_scale = platt_scale)
    return(model);
}

#### Metric Computation ####

is.zero = function(x, tol = .Machine$double.eps ^ 0.5){
    return(sapply(x, FUN = function(e) isTRUE(all.equal(e, 0.0, tolerance = tol))))
}

data.to.XY = function(data, cv_metric = "train", fold = 0){
    if (cv_metric == "test"){
        X = data$X_test;
        Y = data$Y_test;
    } else if (cv_metric == "valid"){
        valid_ind = fold == data$folds;
        X = data$X[valid_ind, ];
        Y = data$Y[valid_ind];
    } else if (cv_metric == "valid_train"){
        valid_ind = data$folds != fold
        X = data$X[valid_ind, ];
        Y = data$Y[valid_ind];
    } else {
        X = data$X;
        Y = data$Y;
    }
    Y[Y == -1] = 0

    return(list("X" = X, "Y" = Y))
}

infer.model.type = function(model){

    if (is.list(model)){
        field_names = names(model)
        if (all(c("platt_scale", "platt_intercept", "coefficients") %in% field_names)){
            return("platt")
        }

        if (all(c("score_bins", "risk_df", "coefficients") %in% field_names)){
            return("histogram")
        }
    } else if (is.numeric(model)|is.array(model)){
        return("logit")
    } else {
        stopifnot(FALSE)
    }

}

get.coefficients.from.model = function(m){
    if (is.list(m) && (!is.null(m[["coefficients"]]))){
        coefs = m[["coefficients"]];
    } else {
        coefs = m;
    }
    stopifnot(is.numeric(coefs));
    return(coefs);
}

get.scores.from.model = function(m, X){
    if (is.list(m) && (!is.null(m[["coefficients"]]))){
        coefs = m[["coefficients"]];
    } else {
        coefs = m;
    }
    stopifnot(is.numeric(coefs));
    return(X %*% coefs);
}

get.probabilities.from.model = function(model, X, model_type = NULL){

    if (is.null(model_type)){
        model_type = infer.model.type(model)
    }

    if (model_type == "logit"){
        get.probabilities = get.probabilities.logit
    } else if (model_type == "platt") {
        get.probabilities = get.probabilities.platt
    } else if (model_type == "histogram") {
        get.probabilities = get.probabilities.histogram
    }
    scores = get.scores.from.model(model, X)
    probabilities = get.probabilities(scores, model)
    return(probabilities)
}

get.scores.logit = function(X, model){
    return(X %*% model);
}

get.scores.histogram = function(X, model){
    return(X %*% model$coefficients);
}

get.scores.platt = function(X, model){
    return(X %*% model$coefficients);
}

get.probabilities.logit = function(scores, model){
    return(inv.logit(scores));
}

get.probabilities.histogram = function(scores, model){
    idx = findInterval(x = scores, vec = model$score_bins);
    return(model$risk_df %>% select("risk_pdf") %>% slice(idx) %>% unlist(use.names=FALSE));
}

get.probabilities.platt = function(scores, model){
    return(inv.logit(model$platt_intercept + model$platt_scale * scores))
}

get.predictions = function(X, model, model_type = NULL){

    if (is.null(model_type)){
        model_type = infer.model.type(model)
    }

    if (model_type == "logit"){
        get.scores = get.scores.logit
        get.probabilities = get.probabilities.logit
    } else if (model_type == "platt") {
        get.scores = get.scores.platt
        get.probabilities = get.probabilities.platt
    } else if (model_type == "histogram") {
        get.scores = get.scores.histogram
        get.probabilities = get.probabilities.histogram
    }

    scores = get.scores(X, model)
    probabilities = get.probabilities(scores, model)
    return(list(scores = scores, probabilities = probabilities))

}

get.score.based.metrics = function(model, data, cv_metric = "train", fold = 0, discrete_flag = FALSE){

    out = data.to.XY(data, cv_metric, fold)

    metrics = compute.score.based.metrics(
        scores = get.scores.from.model(model, out$X),
        probabilities = get.probabilities.from.model(model, out$X),
        true_labels = out$Y,
        discrete_flag = discrete_flag) %>%
        mutate(
            cv_metric = cv_metric,
            fold = fold
        )

    return(metrics)

}

compute.score.based.metrics = function(scores, probabilities, true_labels, discrete_flag = FALSE){

    #compute helper
    N_CALIBRATION_ERROR_BINS = 10;
    N = length(scores);
    pos_idx = true_labels == 1;
    neg_idx = !pos_idx;
    N_pos = sum(pos_idx)
    N_neg = N - N_pos;

    #discrete calibration errors
    if (discrete_flag){
        distinct_probs = sort(unique(probabilities));
        calibration_breaks = c(-0.5, distinct_probs);
        calibration_indices = findInterval(x = probabilities, vec = calibration_breaks) - 1;
        #stopifnot(all(calibration_breaks[calibration_indices] == probabilities))
        #stopifnot(!any(calibration_indices==1))
        avg_cal_err_distinct = 0;
        max_cal_err_distinct = 0;
        n_distinct = length(distinct_probs);
        for (b in 1:n_distinct){
            ind = calibration_indices == b;
            if (any(ind, na.rm = TRUE)){
                actual = mean(true_labels[ind]);
                predicted = mean(probabilities[ind]);
                cal_err = abs(actual - predicted);
                max_cal_err_distinct = max(cal_err, max_cal_err_distinct);
                avg_cal_err_distinct = avg_cal_err_distinct + sum(ind) * cal_err;
            }
        }
        avg_cal_err_distinct = avg_cal_err_distinct/N;
    } else {
        n_distinct = NA;
        max_cal_err_distinct = NA;
        avg_cal_err_distinct = NA;
    }

    #continuous calibration errors
    calibration_breaks = seq(0, N_CALIBRATION_ERROR_BINS - 1)/N_CALIBRATION_ERROR_BINS;
    calibration_indices = findInterval(x = probabilities, vec = calibration_breaks);
    avg_cal_err_binned = 0;
    max_cal_err_binned = 0;
    for (b in 1:N_CALIBRATION_ERROR_BINS){
        ind = calibration_indices == b;
        if (any(ind, na.rm = TRUE)){
            actual = mean(true_labels[ind]);
            predicted = mean(probabilities[ind]);
            cal_err = abs(actual - predicted);
            max_cal_err_binned = max(cal_err, max_cal_err_binned);
            avg_cal_err_binned = avg_cal_err_binned + sum(ind) * cal_err;
        }
    }
    avg_cal_err_binned = avg_cal_err_binned/N;

    #AUC
    idx = order(probabilities, decreasing = TRUE);
    TP = cumsum(true_labels[idx]==1);
    FP = cumsum(true_labels[idx]==0);
    ## remove fp & tp for duplicated predictions
    #as duplicated keeps the first occurrence, but we want the last, two rev are used.
    keep_idx = !rev(duplicated(rev(probabilities[idx])));
    TP = c(0, TP[keep_idx]);
    FP = c(0, FP[keep_idx]);
    auc = 0;
    for (k in 2:length(FP)) {
        auc <- auc + abs(FP[k] - FP[k-1]) * (TP[k] + TP[k-1]);
    }
    auc = auc / (2 * N_neg * N_pos);

    #compute logistic loss
    scores[neg_idx] = -scores[neg_idx];
    pos_score_ind = scores > 0;
    neg_score_ind = !pos_score_ind;
    exp_scores_pos = exp(-scores[pos_score_ind]);
    exp_scores_neg = exp(scores[neg_score_ind]);
    mxe = (sum(log1p(exp_scores_pos))+ sum(-scores[neg_score_ind] + log1p(exp_scores_neg)))/N;

    #sanity checks for testing (use ROCR)
    #pred_object = prediction(probabilities, labels = true_labels);
    #auc_rocr = performance(pred_object, measure = "auc")@y.values[[1]];
    #mxe_rocr = performance(pred_object, measure = "mxe")@y.values[[1]];
    #print(sprintf("AUC %1.6f, %1.6f", auc, auc_rocr))
    #print(sprintf("MXE %1.6f, %1.6f", mxe, mxe_rocr))

    return(data.frame(auc = auc,
                      mxe = mxe,
                      n_distinct_scores = n_distinct,
                      max_cal_err_distinct = max_cal_err_distinct,
                      avg_cal_err_distinct = avg_cal_err_distinct,
                      n_bins = N_CALIBRATION_ERROR_BINS,
                      max_cal_err_binned = max_cal_err_binned,
                      avg_cal_err_binned = avg_cal_err_binned,
                      stringsAsFactors = FALSE,
                      check.names = FALSE,
                      check.rows = FALSE));

}

get.roc.plot.data = function(model, data, model_type = "final", cv_metric = "train", fold = 0){

    #model_info has the following columns:
    #
    #data_name: valid dataset in data_dir
    #fold_id: K[XX]N[YY]
    #inner_fold_id: "NONE" or F[VV]K[WW] where VV is between 1 to K
    #fold: numeric between 0 to K
    #print_model_id: to access print_models
    #final_model: TRUE if the model is the final model
    #cv_metric: 'train'/'valid'/'test'
    out = data.to.XY(data, cv_metric, fold)
    X = out$X
    Y = out$Y
    pos_ind = Y == 1;
    neg_ind = !pos_ind

    N = nrow(X);
    N_pos = sum(pos_ind);
    N_neg = N - N_pos;
    weights = as.vector(rep(1.0,N))

    pred = get.predictions(X, model)
    probs = pred$probabilities
    idx = order(probs, decreasing = TRUE);
    predict_pos = Y[idx] == 1
    weights = weights[idx];
    TP = cumsum(weights*predict_pos);
    FP = cumsum(weights*(!predict_pos));

    # remove fp & tp for duplicate values
    # as duplicated keeps the first occurrence, but we want the last, two rev are used.
    keep_idx = !rev(duplicated(rev(probs[idx])));
    TP = c(0, TP[keep_idx]);
    FP = c(0, FP[keep_idx]);

    # add scores and probabilities
    roc_df = data.frame(FP = FP,
                        TP = TP,
                        FPR = FP/N_neg,
                        TPR = TP/N_pos,
                        stringsAsFactors = FALSE) %>%
        mutate(N_pos = N_pos,
               N_neg = N_neg,
               model_type = model_type,
               cv_metric = cv_metric,
               fold = fold) %>%
        arrange(FPR, TPR)

    roc_df = roc_df %>% mutate
    return(roc_df);
}

get.calibration.plot.data = function(model, data, model_type = "final", cv_metric = "train", fold = 0, n_bins = 10, discrete_flag = FALSE){
    #model_info has the following columns:
    #
    #data_name: valid dataset in data_dir
    #fold_id: K[XX]N[YY]
    #inner_fold_id: "NONE" or F[VV]K[WW] where VV is between 1 to K
    #fold: numeric between 0 to K
    #print_model_id: to access print_models
    #final_model: TRUE if the model is the final model
    #cv_metric: 'train'/'valid'/'test'

    out = data.to.XY(data, cv_metric, fold)
    X = out$X
    Y = out$Y
    predictions = get.predictions(X, model)
    probabilities = predictions$probabilities
    scores = predictions$scores
    calib_df = get.calibration.hist.df(scores, probabilities, Y, n_bins, discrete_flag = discrete_flag);
    calib_df = calib_df %>%
        mutate(model_type = model_type,
               cv_metric = cv_metric,
               fold = fold)

    return(calib_df);
}

get.calibration.hist.df = function(scores, probabilities, labels, n_bins = 10, discrete_flag = FALSE){

    idx = order(scores, decreasing = FALSE);
    distinct_scores = unique(scores[idx])
    n_distinct_scores = length(distinct_scores);

    if (discrete_flag && (n_distinct_scores >= 50)){
        discrete_flag = FALSE
    }

    if (discrete_flag){

        distinct_probs = unique(probabilities[idx]);
        n_bins = length(distinct_probs);
        calibration_breaks = c(-0.5, distinct_probs);
        calibration_indices = findInterval(x = probabilities, vec = calibration_breaks) - 1;

        hist_df = data.frame(bin = seq(1, n_bins),
                             bin.left = distinct_probs,
                             bin.right = distinct_probs,
                             discrete_flag = discrete_flag,
                             count = 0,
                             score.min = NA,
                             score.mean = NA,
                             score.max = NA,
                             score.sd = NA,
                             predicted.min = NA,
                             predicted.mean = NA,
                             predicted.max = NA,
                             predicted.sd = NA,
                             actual.min = NA,
                             actual.mean = NA,
                             actual.max = NA,
                             actual.sd = NA);

    } else {

        calibration_breaks = seq(0, n_bins - 1)/n_bins;
        calibration_indices = findInterval(x = probabilities, vec = calibration_breaks);
        n_bins = n_bins;

        hist_df = data.frame(bin = seq(1, n_bins),
                             bin.left = seq(0, n_bins - 1)/n_bins,
                             bin.right = seq(1, n_bins)/n_bins,
                             discrete_flag = discrete_flag,
                             count = 0,
                             score.min = NA,
                             score.mean = NA,
                             score.max = NA,
                             score.sd = NA,
                             predicted.min = NA,
                             predicted.mean = NA,
                             predicted.max = NA,
                             predicted.sd = NA,
                             actual.min = NA,
                             actual.mean = NA,
                             actual.max = NA,
                             actual.sd = NA);
    }

    for (b in 1:n_bins){

        ind = calibration_indices == b

        if (any(ind)){

            binned_scores = scores[ind];
            binned_probabilities = probabilities[ind];
            binned_actual = labels[ind]==1;

            hist_df[b, "count"] = sum(ind);

            hist_df[b, "score.min"] = min(binned_scores);
            hist_df[b, "score.mean"] = mean(binned_scores);
            hist_df[b, "score.max"] = max(binned_scores);
            hist_df[b, "score.sd"] = ifelse(discrete_flag, NA, sd(binned_scores));

            hist_df[b, "predicted.min"] = min(binned_probabilities);
            hist_df[b, "predicted.mean"] = mean(binned_probabilities);
            hist_df[b, "predicted.max"] = max(binned_probabilities);
            hist_df[b, "predicted.sd"] = ifelse(discrete_flag, NA, sd(binned_probabilities));

            hist_df[b, "actual.min"] = min(binned_actual);
            hist_df[b, "actual.mean"] = mean(binned_actual);
            hist_df[b, "actual.max"] = max(binned_actual);
            hist_df[b, "actual.sd"] = ifelse(discrete_flag, NA, sd(binned_actual));
        }
    }
    return(hist_df);
}

collapse.calibration.df = function(df, lower_risk_threshold = 0.01, upper_risk_threshold = 0.99){

    stopifnot(between(lower_risk_threshold, 0, upper_risk_threshold))
    stopifnot(between(upper_risk_threshold, lower_risk_threshold, 1.0))

    # info
    info_df = df %>%
        distinct(model_id, model_type, cv_metric, fold)

    #
    middle_df  = df %>%
        filter(between(predicted.mean, lower_risk_threshold, upper_risk_threshold)) %>%
        group_by(model_id, model_type, cv_metric) %>%
        select(model_id,
               model_type,
               cv_metric,
               predicted.mean,
               actual.mean,
               count,
               score.mean);

    lower_df = df %>%
        filter(predicted.mean < lower_risk_threshold) %>%
        group_by(model_id, model_type, cv_metric) %>%
        summarise(predicted.mean = sum(predicted.mean * count) / sum(count),
                  actual.mean = sum(actual.mean * count) /sum(count),
                  count = sum(count),
                  score.mean = max(score.mean));

    upper_df = df %>%
        filter(predicted.mean > upper_risk_threshold) %>%
        group_by(model_id, model_type, cv_metric) %>%
        summarise(predicted.mean = sum(predicted.mean * count) / sum(count),
                  actual.mean = sum(actual.mean * count) / sum(count),
                  count = sum(count),
                  score.mean = min(score.mean));


    out_df = bind_rows(middle_df, lower_df, upper_df) %>%
        arrange(model_id, cv_metric, model_type, predicted.mean) %>%
        left_join(info_df, by = c("model_id", "model_type", "cv_metric")) %>%
        arrange(fold, predicted.mean) %>%
        mutate(bin = row_number(),
               score.label = "",
               final_model = fold == 0) %>%
        select(model_id, cv_metric, model_type, everything())

    return(out_df)

}