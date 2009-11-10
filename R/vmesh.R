`vmesh` <-
function (infile = NULL, data = NULL, edges = NULL, xfun = "sin(v)*cos(u)", 
    yfun = "sin(v)*sin(u)", zfun = "cos(v)", param1 = "u", param2 = "v", 
    range1 = c(0, 2 * pi), range2 = c(0, pi), size1 = 30, size2 = 30, 
    filename = "out.wrl", cols = "red", scalefac = 4, autoscale = ifelse(is.null(infile), 
        "independent", "equicenter"), lab.axis = c("X-axis", 
        "Y-axis", "Z-axis"), col.axis = "white", showaxis = TRUE, 
    col.lab = "white", col.bg = "black", cex.lab = 1, navigation = "EXAMINE", 
    transparency = 0, fov = 0.785, pos = rep(scalefac + 4, 3), 
    dir = c(0.19, 0.45, 0.87, 2.45), htmlout = NULL, hwidth = 1200, 
    hheight = 800) 
{
    
    # case 1: the user provides an obj-file as input
    if (!is.null(infile)) {
    
        # read the obj-file
        obj <- strsplit(readLines(infile), "\t")
        
        # collect face data
        faces <- which(as.numeric(lapply(obj, function(x) grep("^f", 
            x[1]))) == 1)
            
        # collect node data
        nodes <- which(as.numeric(lapply(obj, function(x) grep("^v ", 
            x[1]))) == 1)
            
        # parse node data
        tmpmat <- lapply(obj[nodes], function(x) strsplit(x, 
            " ")[[1]])            
        redmat <- t(sapply(tmpmat, function(x) as.numeric(x[2:length(x)])))
        data <- redmat[, which(apply(redmat, 2, function(x) !any(is.na(x))))]
        
        # extract edges from faces
        edges_lst <- lapply(obj[faces], function(x) strsplit(x, 
            " ")[[1]])
        edges_filt1 <- lapply(edges_lst, function(y) as.numeric(sapply(y[2:length(y)], 
            function(x) strsplit(x, "/")[[1]][1])))
        edges_filt <- lapply(edges_filt1, function(x) x[which(!is.na(x))])
        
        # split faces with more than 4 edges into smaller faces
        filter <- sapply(edges_filt, length) > 4
        filt <- edges_filt[filter]
        reduced_lst <- NULL
        if (length(filt) > 0) {
            for (j in 1:length(filt)) {
                reduced_lst <- c(reduced_lst, list(filt[[j]][1:4]))
                for (k in seq(4, length(filt[[j]]), 3)) {
                  if (!is.na(filt[[j]][k + 2])) {
                    reduced_lst <- c(reduced_lst, list(c(filt[[j]][k:(k + 
                      2)], filt[[j]][1])))
                  }
                  else if (!is.na(filt[[j]][k + 1])) {
                    reduced_lst <- c(reduced_lst, list(c(filt[[j]][k:(k + 
                      1)], filt[[j]][1], 0)))
                    break
                  }
                  else {
                    break
                  }
                }
            }
        }
        
        # combine edge data
        comb_lst <- c(edges_filt[!filter], reduced_lst)
        filtless <- which(sapply(comb_lst, length) < 4)
        comb_lst[filtless] <- lapply(comb_lst[filtless], function(x) c(x, 
            0))
        edges <- t(sapply(comb_lst, rbind)) - 1
    }
    
    # case 2: a parametric function is used as input
    else if (is.null(data)) {
        
        # check if the required data is available
        if (is.null(xfun) || is.null(yfun) || is.null(zfun)) {
            stop("Either the paramater infile or data or the parameters xfun, yfun and zfun have to be specified.")
        }
        if (is.null(param1) || is.null(param2)) {
            stop("The parameter names param1 and param2 have not been specified")
        }
        
        # initialize variables
        x <- NULL
        y <- NULL
        z <- NULL
        
        smin <- range1[1]
        smax <- range1[2]
        tmin <- range2[1]
        tmax <- range2[2]
        sn <- size1
        tn <- size2
        ds <- (smax - smin)/sn
        dt <- (tmax - tmin)/tn
        
        # compute the data points representing the parametric function
        for (i in seq(smin, (smax - ds/2), ds)) {
            
            for (j in seq(tmin, (tmax - dt/2), dt)) {
            
                eval(parse(text = paste(param1, " <- ", i)))
                eval(parse(text = paste(param2, " <- ", j)))
                x <- c(x, eval(parse(text = xfun)))
                y <- c(y, eval(parse(text = yfun)))
                z <- c(z, eval(parse(text = zfun)))
                eval(parse(text = paste(param1, " <- ", param1, 
                  " + ds")))
                x <- c(x, eval(parse(text = xfun)))
                y <- c(y, eval(parse(text = yfun)))
                z <- c(z, eval(parse(text = zfun)))
                eval(parse(text = paste(param2, " <- ", param2, 
                  " + dt")))
                x <- c(x, eval(parse(text = xfun)))
                y <- c(y, eval(parse(text = yfun)))
                z <- c(z, eval(parse(text = zfun)))
                eval(parse(text = paste(param1, " <- ", param1, 
                  " - ds")))
                x <- c(x, eval(parse(text = xfun)))
                y <- c(y, eval(parse(text = yfun)))
                z <- c(z, eval(parse(text = zfun)))
            }
        }
        
        # combine data
        data <- as.matrix(cbind(x, y, z))
    }
        
    if (ncol(data) != 3) {
        stop("Data matrix does not have 3 columns!")
    }
    
    # apply the chosen scaling method
    scaledat <- function(data) {
        
        diff <- max(data) - min(data)
        if (diff == 0) 
            return(data)
        else return(scalefac * (data - min(data))/(diff))
    }
    
    if (autoscale == "independent") 
        data <- apply(data, 2, scaledat)
    else if (autoscale == "equidist") 
        data <- scalefac * (data/max(data))
    else if (autoscale == "equicenter") 
        data <- scalefac/2 * data/max(data) + scalefac/2
        
    # write VRML header
    write("#VRML V2.0 utf8\n", file = filename, append = FALSE)
    
    # set view point
    write(paste("\nViewpoint {\n\tfieldOfView", fov, "\n\tposition", 
        pos[1], pos[2], pos[3], "\n\torientation", dir[1], dir[2], 
        dir[3], dir[4], "\n\tjump TRUE\n\tdescription \"viewpoint1\"\n}\n", 
        sep = " "), file = filename, append = TRUE)
        
    # set mouse navigation type
    write(paste("\nNavigationInfo { type \"", navigation, "\" }\n", 
        sep = ""), file = filename, append = TRUE)

    # set background color        
    bg_rcol <- (col2rgb(col.bg)/255)[1]
    bg_gcol <- (col2rgb(col.bg)/255)[2]
    bg_bcol <- (col2rgb(col.bg)/255)[3]
    write(paste("Background {\n\t skyColor [\n\t\t ", bg_rcol, 
        bg_gcol, bg_bcol, " \n\t]\n}", sep = " "), file = filename, 
        append = TRUE)
        
    # draw the axes
    if (showaxis) {
        
        lab_rcol <- (col2rgb(col.lab)/255)[1]
        lab_gcol <- (col2rgb(col.lab)/255)[2]
        lab_bcol <- (col2rgb(col.lab)/255)[3]
        
        # x-axis label
        write(paste("Transform {\n\ttranslation ", scalefac + 
            0.5 + nchar(as.character(lab.axis[1])) * 0.12 * cex.lab, 
            " 0 0\n\tscale ", cex.lab * 0.28, cex.lab * 0.28, 
            cex.lab * 0.28, "\n\trotation 0.00000 0.70711 0.70711 3.14159\n\tchildren Shape {\n\t\tappearance Appearance { material Material {diffuseColor ", 
            lab_rcol, lab_gcol, lab_bcol, "  } }\n\tgeometry Text { string \"", 
            lab.axis[1], "\" }\n\t}\n}", sep = " "), file = filename, 
            append = TRUE)
            
	# y-axis label            
        write(paste("Transform {\n\ttranslation 0 ", scalefac + 
            0.5, " 0\n\tscale ", cex.lab * 0.28, cex.lab * 0.28, 
            cex.lab * 0.28, "\n\trotation 0.57735 0.57735 0.57735 2.09440\n\tchildren Shape {\n\t\tappearance Appearance { material Material {diffuseColor ", 
            lab_rcol, lab_gcol, lab_bcol, "  } }\n\tgeometry Text { string \"", 
            lab.axis[2], "\" }\n\t}\n}", sep = " "), file = filename, 
            append = TRUE)
            
        # z-axis label
        write(paste("Transform {\n\ttranslation 0 0 ", scalefac + 
            0.5, "\n\tscale ", cex.lab * 0.28, cex.lab * 0.28, 
            cex.lab * 0.28, "\n\trotation 0.28108 0.67860 0.67860 2.59356\n\tchildren Shape {\n\t\tappearance Appearance { material Material {diffuseColor ", 
            lab_rcol, lab_gcol, lab_bcol, "  } }\n\tgeometry Text { string \"", 
            lab.axis[3], "\" }\n\t}\n}", sep = " "), file = filename, 
            append = TRUE)
            
        ax_rcol <- (col2rgb(col.axis)/255)[1]
        ax_gcol <- (col2rgb(col.axis)/255)[2]
        ax_bcol <- (col2rgb(col.axis)/255)[3]
        
        # draw x-axis
        write(paste("Transform {\n\ttranslation ", 0.5 * scalefac, 
            " 0 0\n\trotation 0 0 1 1.5708\n\tchildren Shape {\n\t\tappearance Appearance { material Material {\n\t\tdiffuseColor ", 
            ax_rcol, ax_gcol, ax_bcol, " } }\n\tgeometry Cylinder { height ", 
            scalefac, " radius 0.04 }\n\t}\n}", sep = " "), file = filename, 
            append = TRUE)
            
        # draw y-axis
        write(paste("Transform {\n\ttranslation 0 ", 0.5 * scalefac, 
            " 0\n\trotation 0 0 1 0\n\tchildren Shape {\n\t\tappearance Appearance { material Material { diffuseColor ", 
            ax_rcol, ax_gcol, ax_bcol, " } }\n\tgeometry Cylinder { height ", 
            scalefac, " radius 0.04 }\n\t}\n}", sep = " "), file = filename, 
            append = TRUE)
            
        # draw z-axis
        write(paste("Transform {\n\ttranslation 0 0 ", 0.5 * 
            scalefac, "\n\trotation 1 0 0 1.5708\n\tchildren Shape {\n\t\tappearance Appearance { material Material { diffuseColor ", 
            ax_rcol, ax_gcol, ax_bcol, " } }\n\tgeometry Cylinder { height ", 
            scalefac, " radius 0.04 }\n\t}\n}", sep = " "), file = filename, 
            append = TRUE)
    }
    
    # write the data point coordinates
    write(paste("Transform {\nscale 1.0 1.0 1.0\n\tchildren [\n\t\tShape {\n\t\t\tappearance Appearance{ material Material{ transparency 0}}\n", 
        sep = " "), file = filename, append = TRUE)
    
    write(paste("\n\t\t\tgeometry IndexedFaceSet {\n\t\t\tcoord DEF\n\tVertexArray Coordinate{\n\tpoint [\n", 
        sep = " "), file = filename, append = TRUE)
    
    for (j in 1:nrow(data)) {
        write(paste(paste(data[j, ], collapse = " "), ",\n", 
            sep = ""), file = filename, append = TRUE)
    }    
    write("]\n}\n", file = filename, append = TRUE)
    
    # write the edges (using coordinate indices)        
    write("coordIndex [\n", file = filename, append = TRUE)
    mat <- NULL
    
    if (!is.null(edges)) {
        mat <- edges
        while (ncol(mat) < 4) {
            mat <- cbind(mat, rep(-1, nrow(mat)))
        }
    }
    else {
        mat <- matrix(1:nrow(data), ncol = 4, byrow = TRUE) - 
            1
    }
    
    for (j in 1:nrow(mat)) {
        write(paste(paste(mat[j, ], collapse = " "), "-1", sep = " "), 
            file = filename, append = TRUE)
    }
    
    
    # write vertex colors
    write(paste("]\n solid FALSE\n colorPerVertex TRUE\n color \nDEF VertexColorArray Color{\ncolor [", 
        sep = ""), file = filename, append = TRUE)
    
    # use rainbow colors if no colors are pre-defined    
    if (!length(cols) || (is.null(cols))) {
        
        cols <- rainbow(nrow(data))
        mat <- sapply(cols, function(x) (col2rgb(x)/255))
        for (j in 1:nrow(data)) {
            write(paste(mat[1, j], mat[2, j], mat[3, j], ",\n", 
                collapse = " "), file = filename, append = TRUE)
        }
    }
    
    # use a single color if the number of colors is less than the number of data rows
    else if ((length(cols) == 1) || (length(cols) < nrow(data))) {
        
        rcol <- (col2rgb(cols[1])/255)[1]
        gcol <- (col2rgb(cols[1])/255)[2]
        bcol <- (col2rgb(cols[1])/255)[3]
        for (j in 1:nrow(data)) {
            write(paste(rcol, gcol, bcol, ",\n", collapse = " "), 
                file = filename, append = TRUE)
        }
    }   
    else {
        
        mat <- sapply(cols, function(x) (col2rgb(x)/255))
        for (j in 1:nrow(data)) {
            write(paste(mat[1, j], mat[2, j], mat[3, j], ",\n", 
                collapse = " "), file = filename, append = TRUE)
        }
    }    
    write(paste("]\n}\n}\n}\n]\n}", sep = ""), file = filename, 
        append = TRUE)
        
    # create HTML output
    if (!is.null(htmlout)) {
        cat("<HTML>", file = htmlout, append = FALSE)
        cat("<HEAD><TITLE>VRMLGen-visualization</TITLE></HEAD><BODY><br>", 
            file = htmlout, append = FALSE)
        cat(paste("<object type=\"x-world/x-vrml\" data=\"", 
            filename, "\" ", sep = ""), file = htmlout, append = TRUE)
        cat(paste("width=\"", hwidth, "\" height=\"", hheight, 
            "\"><br>", sep = ""), file = htmlout, append = TRUE)
        cat(paste("<param name=\"src\" value=\"", filename, "\"><br>", 
            sep = ""), file = htmlout, append = TRUE)
        cat("Your browser cannot display VRML files.<br>Please INSTALL a VRML-plugin or open the file in an external VRML-viewer.</object><br>", 
            file = htmlout, append = TRUE)
        cat("</BODY></HTML>", file = htmlout, append = TRUE)
    }
    
    # show success message
    cat(paste("\nOutput file \"", filename, "\" was generated.\n", 
        sep = ""))
}

