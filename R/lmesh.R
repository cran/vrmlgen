`lmesh` <-
function (xfun = "sin(v)*cos(u)", yfun = "sin(v)*sin(u)", zfun = "cos(v)", 
    param1 = "u", param2 = "v", range1 = c(0, 2 * pi), range2 = c(0, 
        pi), size1 = 30, size2 = 30, filename = "out.m", cols = "red", 
    scalefac = 4, autoscale = "independent", lab.axis = c("X-axis", 
        "Y-axis", "Z-axis"), col.axis = "black", showaxis = TRUE, 
    col.lab = "black", col.bg = "white", cex.lab = 1, ambientlight = 0.5, 
    htmlout = NULL, hwidth = 1200, hheight = 800) 
{
    
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
            eval(parse(text = paste(param1, " <- ", param1, " + ds")))
            x <- c(x, eval(parse(text = xfun)))
            y <- c(y, eval(parse(text = yfun)))
            z <- c(z, eval(parse(text = zfun)))
            eval(parse(text = paste(param2, " <- ", param2, " + dt")))
            x <- c(x, eval(parse(text = xfun)))
            y <- c(y, eval(parse(text = yfun)))
            z <- c(z, eval(parse(text = zfun)))
            eval(parse(text = paste(param1, " <- ", param1, " - ds")))
            x <- c(x, eval(parse(text = xfun)))
            y <- c(y, eval(parse(text = yfun)))
            z <- c(z, eval(parse(text = zfun)))
        }
    }
    
    # combine data
    data <- as.matrix(cbind(x, y, z))
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
        
    # write LiveGraphics3D header
    write("Graphics3D[\n", file = filename, append = FALSE)
    write("{\n", file = filename, append = TRUE)
    
    # initialize color variables
    bg_rcol <- (col2rgb(col.bg)/255)[1]
    bg_gcol <- (col2rgb(col.bg)/255)[2]
    bg_bcol <- (col2rgb(col.bg)/255)[3]
    lab_rcol <- NULL
    lab_gcol <- NULL
    lab_bcol <- NULL
    ax_rcol <- NULL
    ax_gcol <- NULL
    ax_bcol <- NULL
    
    # draw the axes
    if (showaxis) {
        
        lab_rcol <- (col2rgb(col.lab)/255)[1]
        lab_gcol <- (col2rgb(col.lab)/255)[2]
        lab_bcol <- (col2rgb(col.lab)/255)[3]
        
        # x-axis label
        write(paste("RGBColor[", lab_rcol, ",", lab_gcol, ",", 
            lab_bcol, "], Text [ \"", lab.axis[1], "\", {", scalefac + 
                0.5, ",0,0 }],\n", sep = ""), file = filename, 
            append = TRUE)
        
        # y-axis label
        write(paste("RGBColor[", lab_rcol, ",", lab_gcol, ",", 
            lab_bcol, "], Text [ \"", lab.axis[2], "\", {0,", 
            scalefac + 0.5, ",0 }],\n", sep = ""), file = filename, 
            append = TRUE)
        
        # z-axis label
        write(paste("RGBColor[", lab_rcol, ",", lab_gcol, ",", 
            lab_bcol, "], Text [ \"", lab.axis[3], "\", {0, 0,", 
            scalefac + 0.5, " }],\n", sep = ""), file = filename, 
            append = TRUE)
        
        ax_rcol <- (col2rgb(col.axis)/255)[1]
        ax_gcol <- (col2rgb(col.axis)/255)[2]
        ax_bcol <- (col2rgb(col.axis)/255)[3]
        
        # draw x-axis
        write(paste("Thickness[0.01], RGBColor[", ax_rcol, ",", 
            ax_gcol, ",", ax_bcol, "], Line[{{0,0,0},{", scalefac, 
            ",0,0}}],", sep = " "), file = filename, append = TRUE)
            
	# draw y-axis            
        write(paste("Thickness[0.01], RGBColor[", ax_rcol, ",", 
            ax_gcol, ",", ax_bcol, "], Line[{{0,0,0},{0,", scalefac, 
            ",0}}],", sep = " "), file = filename, append = TRUE)
            
        # draw z-axis
        write(paste("Thickness[0.01], RGBColor[", ax_rcol, ",", 
            ax_gcol, ",", ax_bcol, "], Line[{{0,0,0},{0,0,", 
            scalefac, "}}],", sep = " "), file = filename, append = TRUE)
    }
    
    # main loop: iterate over data points
    for (j in 1:nrow(data)) {
    
        x <- data[j, 1]
        y <- data[j, 2]
        z <- data[j, 3]
        
        
        # set current color
        rcol <- NULL
        gcol <- NULL
        bcol <- NULL
        
        if (!length(cols)) {
            rcol <- (col2rgb(rainbow(nrow(data))[j])/255)[1]
            gcol <- (col2rgb(rainbow(nrow(data))[j])/255)[2]
            bcol <- (col2rgb(rainbow(nrow(data))[j])/255)[3]
        }
        else if ((length(cols) == 1) || (length(cols) < nrow(data))) {
            rcol <- (col2rgb(cols[1])/255)[1]
            gcol <- (col2rgb(cols[1])/255)[2]
            bcol <- (col2rgb(cols[1])/255)[3]
        }
        else {
            rcol <- (col2rgb(cols[j])/255)[1]
            gcol <- (col2rgb(cols[j])/255)[2]
            bcol <- (col2rgb(cols[j])/255)[3]
        }
        
        
        # write polygon
        
        # polygon start node
        if (j%%4 == 1) 
            write(paste(" SurfaceColor[RGBColor[", rcol, ",", 
                gcol, ",", bcol, "]], Polygon[{{", x, ",", y, 
                ",", z, "},", sep = ""), file = filename, append = TRUE)
        
        # polygon end node
        else if (j%%4 == 0) 
            write(paste(" {", x, ",", y, ",", z, "}}],", sep = ""), 
                file = filename, append = TRUE)
                
        # polygon inner nodes
        else write(paste(" {", x, ",", y, ",", z, "},", sep = ""), 
            file = filename, append = TRUE)
    }
    
    
    # configure general plot settings (font size, text style, etc.)
    write(paste("\n}, Boxed -> False, Axes -> False, AmbientLight->GrayLevel[", 
        ambientlight, "], TextStyle -> {FontFamily -> \"TimesRoman\", FontSlant ->\"Italic\", FontSize -> ", 
        14 * cex.lab, "}, Lighting -> True, BoxRatios -> Automatic, PlotRange -> All ]\n", 
        sep = ""), file = filename, append = TRUE)
    
    # create HTML output
    if (!is.null(htmlout)) {
        cat("<HTML>", file = htmlout, append = FALSE)
        cat("<HEAD><TITLE>VRMLGen-visualization</TITLE></HEAD><BODY>", 
            file = htmlout, append = TRUE)
        cat(paste("<APPLET ARCHIVE=\"live.jar\" CODE=\"Live.class\" WIDTH=", 
            hwidth, " HEIGHT=", hheight, " ALIGN=LEFT>", sep = ""), 
            file = htmlout, append = TRUE)
        coln <- col2rgb(col.bg)
        cat(paste("<PARAM NAME=\"BGCOLOR\" VALUE=\"", rgb(red = coln[1, 
            ]/255, green = coln[2, ]/255, blue = coln[3, ]/255), 
            "\">", sep = ""), file = htmlout, append = TRUE)
        cat("<PARAM NAME=\"MAGNIFICATION\" VALUE=1.0>", file = htmlout, 
            append = TRUE)
        cat(paste("<PARAM NAME=\"INPUT_FILE\" VALUE=\"", filename, 
            "\">", sep = ""), file = htmlout, append = TRUE)
        cat("</APPLET>", file = htmlout, append = TRUE)
        cat("</HTML>", file = htmlout, append = TRUE)
    }
    
    # show success message
    cat(paste("\nOutput file \"", filename, "\" was generated.\n\nPlease make sure that the output folder contains a copy of the \"live.jar\" file\navailable at: http://www.vis.uni-stuttgart.de/~kraus/LiveGraphics3D/download.html.\n\n", 
        sep = ""))
}

