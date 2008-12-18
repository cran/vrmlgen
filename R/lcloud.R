`lcloud` <-
function (data, labels = rownames(data), filename = "out.m", 
    pointstyle = c("s", "b", "t"), cols = rainbow(length(unique(labels))), 
    scalefac = 4, lab.axis = c("X-axis", "Y-axis", "Z-axis"), 
    col.axis = "black", showaxis = TRUE, col.lab = "black", col.bg = "white", 
    cex.lab = 1, ambientlight = 0.5, htmlout = NULL, hwidth = 1200, 
    hheight = 800) 
{
    data <- as.matrix(data)
    if (ncol(data) != 3) {
        stop("Data matrix does not have 3 columns!")
    }
    numlabels <- NULL
    if (length(labels)) {
        lab <- unique(unlist(labels))
        numlabels <- apply(as.matrix(labels), 1, function(x) match(x, 
            as.matrix(lab)))
    }
    scaledat <- function(data) {
        return(scalefac * (data - min(data))/(max(data) - min(data)))
    }
    data <- apply(data, 2, scaledat)
    write("Graphics3D[\n", file = filename, append = FALSE)
    write("{\n", file = filename, append = TRUE)
    bg_rcol <- (col2rgb(col.bg)/255)[1]
    bg_gcol <- (col2rgb(col.bg)/255)[2]
    bg_bcol <- (col2rgb(col.bg)/255)[3]
    if (length(labels)) {
        cur_height <- scalefac + 1 + cex.lab/5
        for (j in 1:length(unique(labels))) {
            rcol <- (col2rgb(cols[unique(numlabels)[j]])/255)[1]
            gcol <- (col2rgb(cols[unique(numlabels)[j]])/255)[2]
            bcol <- (col2rgb(cols[unique(numlabels)[j]])/255)[3]
            write(paste("RGBColor[", rcol, ",", gcol, ",", bcol, 
                "], Text [ \"", unique(labels)[j], "\", {0,0,", 
                cur_height, " }],\n", sep = ""), file = filename, 
                append = TRUE)
            cur_height <- cur_height + 0.4
        }
    }
    lab_rcol <- NULL
    lab_gcol <- NULL
    lab_bcol <- NULL
    ax_rcol <- NULL
    ax_gcol <- NULL
    ax_bcol <- NULL
    if (showaxis) {
        lab_rcol <- (col2rgb(col.lab)/255)[1]
        lab_gcol <- (col2rgb(col.lab)/255)[2]
        lab_bcol <- (col2rgb(col.lab)/255)[3]
        write(paste("RGBColor[", lab_rcol, ",", lab_gcol, ",", 
            lab_bcol, "], Text [ \"", lab.axis[2], "\", {", scalefac + 
                0.5, ",0,0 }],\n", sep = ""), file = filename, 
            append = TRUE)
        write(paste("RGBColor[", lab_rcol, ",", lab_gcol, ",", 
            lab_bcol, "], Text [ \"", lab.axis[1], "\", {0,", 
            scalefac + 0.5, ",0 }],\n", sep = ""), file = filename, 
            append = TRUE)
        write(paste("RGBColor[", lab_rcol, ",", lab_gcol, ",", 
            lab_bcol, "], Text [ \"", lab.axis[3], "\", {0, 0,", 
            scalefac + 0.5, " }],\n", sep = ""), file = filename, 
            append = TRUE)
        ax_rcol <- (col2rgb(col.axis)/255)[1]
        ax_gcol <- (col2rgb(col.axis)/255)[2]
        ax_bcol <- (col2rgb(col.axis)/255)[3]
        write(paste("Thickness[0.01], RGBColor[", ax_rcol, ",", 
            ax_gcol, ",", ax_bcol, "], Line[{{0,0,0},{", scalefac, 
            ",0,0}}],", sep = " "), file = filename, append = TRUE)
        write(paste("Thickness[0.01], RGBColor[", ax_rcol, ",", 
            ax_gcol, ",", ax_bcol, "], Line[{{0,0,0},{0,", scalefac, 
            ",0}}],", sep = " "), file = filename, append = TRUE)
        write(paste("Thickness[0.01], RGBColor[", ax_rcol, ",", 
            ax_gcol, ",", ax_bcol, "], Line[{{0,0,0},{0,0,", 
            scalefac, "}}],", sep = " "), file = filename, append = TRUE)
    }
    single_pointstyle <- function(pointstyle, rcol, gcol, bcol, 
        filename) {
        if (pointstyle == "t") {
            write(paste("{ SurfaceColor[RGBColor[", rcol, ",", 
                gcol, ",", bcol, "]], Polygon[{{", x, ", ", y, 
                ",", z + 0.06415, "}, {", x + 0.09072222, ",", 
                y, ",", z - 0.096225, "}, {", x - 0.09072222, 
                ",", y + 0.15713333, ",", z - 0.096225, "}}], Polygon[{{", 
                x, ",", y, ",", z + 0.06415, "}, {", x - 0.09072222, 
                ",", y + 0.15713333, ",", z - 0.096225, "}, {", 
                x - 0.09072222, ",", y - 0.15713333, ",", z - 
                  0.096225, "}}], Polygon[{{", x, ",", y, ",", 
                z + 0.06415, "}, {", x - 0.09072222, ",", y - 
                  0.15713333, ",", z - 0.096225, "}, {", x + 
                  0.09072222, ",", y, ",", z - 0.096225, "}}], Polygon[{{", 
                x + 0.09072222, ",", y, ",", z - 0.096225, "}, {", 
                x - 0.09072222, ",", y - 0.15713333, ",", z - 
                  0.096225, "}, {", x - 0.09072222, ",", y + 
                  0.15713333, ",", z - 0.096225, "}}] },\n", 
                sep = ""), file = filename, append = TRUE)
        }
        else if (pointstyle == "s") {
            write(paste("PointSize[", 0.08/scalefac, "], RGBColor[", 
                rcol, ",", gcol, ",", bcol, "],  Point[{", x, 
                ",", y, ",", z, "}],", sep = ""), file = filename, 
                append = TRUE)
        }
        else {
            write(paste("SurfaceColor[RGBColor[", rcol, ",", 
                gcol, ",", bcol, "]], Cuboid[{", x - 0.04, ",", 
                y - 0.04, ",", z - 0.04, "},{", x + 0.04, ",", 
                y + 0.04, ",", z + 0.04, "}],", sep = ""), file = filename, 
                append = TRUE)
        }
    }
    for (j in 1:nrow(data)) {
        x <- data[j, 2]
        y <- data[j, 1]
        z <- data[j, 3]
        rcol <- NULL
        gcol <- NULL
        bcol <- NULL
        if (!length(cols)) {
            rcol <- (col2rgb(rainbow(nrow(data))[j])/255)[1]
            gcol <- (col2rgb(rainbow(nrow(data))[j])/255)[2]
            bcol <- (col2rgb(rainbow(nrow(data))[j])/255)[3]
        }
        else if ((length(cols) == 1) || ((length(cols) < nrow(data)) && 
            !length(labels))) {
            rcol <- (col2rgb(cols[1])/255)[1]
            gcol <- (col2rgb(cols[1])/255)[2]
            bcol <- (col2rgb(cols[1])/255)[3]
        }
        else if (length(labels) && (length(cols) == length(unique(labels)))) {
            rcol <- (col2rgb(cols[numlabels[j]])/255)[1]
            gcol <- (col2rgb(cols[numlabels[j]])/255)[2]
            bcol <- (col2rgb(cols[numlabels[j]])/255)[3]
        }
        else {
            rcol <- (col2rgb(cols[j])/255)[1]
            gcol <- (col2rgb(cols[j])/255)[2]
            bcol <- (col2rgb(cols[j])/255)[3]
        }
        if (length(pointstyle) == 1) {
            single_pointstyle(pointstyle, rcol, gcol, bcol, filename)
        }
        else if (length(pointstyle) >= length(unique(numlabels))) {
            if (length(labels)) {
                stylevec <- c("s", "b", "t")
                curstyle <- stylevec[numlabels[j]]
                if (curstyle == "t") {
                  write(paste("{ SurfaceColor[RGBColor[", rcol, 
                    ",", gcol, ",", bcol, "]], Polygon[{{", x, 
                    ", ", y, ",", z + 0.06415, "}, {", x + 0.09072222, 
                    ",", y, ",", z - 0.096225, "}, {", x - 0.09072222, 
                    ",", y + 0.15713333, ",", z - 0.096225, "}}], Polygon[{{", 
                    x, ",", y, ",", z + 0.06415, "}, {", x - 
                      0.09072222, ",", y + 0.15713333, ",", z - 
                      0.096225, "}, {", x - 0.09072222, ",", 
                    y - 0.15713333, ",", z - 0.096225, "}}], Polygon[{{", 
                    x, ",", y, ",", z + 0.06415, "}, {", x - 
                      0.09072222, ",", y - 0.15713333, ",", z - 
                      0.096225, "}, {", x + 0.09072222, ",", 
                    y, ",", z - 0.096225, "}}], Polygon[{{", 
                    x + 0.09072222, ",", y, ",", z - 0.096225, 
                    "}, {", x - 0.09072222, ",", y - 0.15713333, 
                    ",", z - 0.096225, "}, {", x - 0.09072222, 
                    ",", y + 0.15713333, ",", z - 0.096225, "}}] },\n", 
                    sep = ""), file = filename, append = TRUE)
                }
                else if (curstyle == "s") {
                  write(paste("PointSize[", 0.08/scalefac, "], RGBColor[1, 0, 1],  Point[{", 
                    x, ",", y, ",", z, "}],", sep = ""), file = filename, 
                    append = TRUE)
                }
                else {
                  write(paste("SurfaceColor[RGBColor[", rcol, 
                    ",", gcol, ",", bcol, "]],  Cuboid[{", x - 
                      0.04, ",", y - 0.04, ",", z - 0.04, "},{", 
                    x + 0.04, ",", y + 0.04, ",", z + 0.04, "}],", 
                    sep = ""), file = filename, append = TRUE)
                }
            }
            else {
                single_pointstyle(pointstyle[1], rcol, gcol, 
                  bcol, filename)
            }
        }
        else {
            single_pointstyle(pointstyle[1], rcol, gcol, bcol, 
                filename)
        }
    }
    write(paste("\n}, Boxed -> False, Axes -> False, AmbientLight->GrayLevel[", 
        ambientlight, "], TextStyle -> {FontFamily -> \"TimesRoman\", FontSlant ->\"Italic\", FontSize -> ", 
        14 * cex.lab, "}, Lighting -> True, BoxRatios -> Automatic, PlotRange -> All ]\n", 
        sep = ""), file = filename, append = TRUE)
    if (!is.null(htmlout)) {
        cat("<HTML>", file = htmlout, append = FALSE)
        cat("<HEAD><TITLE>VRMLGen-visualization</TITLE></HEAD><BODY>", 
            file = htmlout, append = TRUE)
        cat(paste("<APPLET ARCHIVE=\"live.jar\" CODE=\"Live.class\" WIDTH=", 
            hwidth, " HEIGHT=", hheight, "ALIGN=LEFT>", sep = ""), 
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
}

