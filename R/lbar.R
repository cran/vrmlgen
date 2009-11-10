`lbar` <-
function (data, row.labels = rownames(data), col.labels = colnames(data), 
    filename = "out.m", space = 0.5, cols = rainbow(length(as.matrix(data))), 
    rcols = NULL, ccols = NULL, scalefac = 4, lab.axis = c("X-axis", 
        "Y-axis", "Z-axis"), col.axis = "black", showaxis = TRUE, 
    col.lab = "white", col.bg = "white", cex.lab = 1, cex.rowlab = 1, 
    cex.collab = 1, ambientlight = 0.5, htmlout = NULL, hwidth = 1200, 
    hheight = 800, showlegend = TRUE) 
{
    data <- as.matrix(data)
    data <- scalefac * (data/max(data))
    write("Graphics3D[\n", file = filename, append = FALSE)
    write("{\n", file = filename, append = TRUE)
    if (!is.null(col.labels) && showlegend) 
        col.labels <- sapply(strsplit(col.labels, " "), function(x) paste(x, 
            collapse = "\n"))
    if (!is.null(row.labels) && showlegend) 
        row.labels <- sapply(strsplit(row.labels, " "), function(x) paste(x, 
            collapse = "\n"))
    bg_rcol <- (col2rgb(col.bg)/255)[1]
    bg_gcol <- (col2rgb(col.bg)/255)[2]
    bg_bcol <- (col2rgb(col.bg)/255)[3]
    write(paste("Background {\n\t skyColor [\n\t\t ", bg_rcol, 
        bg_gcol, bg_bcol, " \n\t]\n}", sep = " "), file = filename, 
        append = TRUE)
    blength <- scalefac/(nrow(data) * (1 + space))
    bwidth <- scalefac/(ncol(data) * (1 + space))
    if (showaxis) {
        lab_rcol <- (col2rgb(col.lab)/255)[1]
        lab_gcol <- (col2rgb(col.lab)/255)[2]
        lab_bcol <- (col2rgb(col.lab)/255)[3]
        write(paste("RGBColor[", lab_rcol, ",", lab_gcol, ",", 
            lab_bcol, "], RGBColor[", lab_rcol, ",", lab_gcol, 
            ",", lab_bcol, "], Text [ \"", lab.axis[2], "\", {", 
            scalefac + 0.5, ",0,0 }],\n", sep = ""), file = filename, 
            append = TRUE)
        write(paste("RGBColor[", lab_rcol, ",", lab_gcol, ",", 
            lab_bcol, "], Text [ \"", lab.axis[3], "\", {0,", 
            scalefac + 0.5, ",0 }],\n", sep = ""), file = filename, 
            append = TRUE)
        write(paste("RGBColor[", lab_rcol, ",", lab_gcol, ",", 
            lab_bcol, "], Text [ \"", lab.axis[1], "\", {0, 0,", 
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
    if (!is.null(row.labels) && showlegend) {
        for (j in 1:length(row.labels)) {
            if (length(rcols) == length(row.labels)) {
                rcol <- (col2rgb(rcols[j])/255)[1]
                gcol <- (col2rgb(rcols[j])/255)[2]
                bcol <- (col2rgb(rcols[j])/255)[3]
            }
            else if (length(rcols) == 1) {
                rcol <- (col2rgb(rcols[1])/255)[1]
                gcol <- (col2rgb(rcols[1])/255)[2]
                bcol <- (col2rgb(rcols[1])/255)[3]
            }
            else {
                rcol <- (col2rgb("black")/255)[1]
                gcol <- (col2rgb("black")/255)[2]
                bcol <- (col2rgb("black")/255)[3]
            }
            cur_xwidth <- j/nrow(data) * scalefac - blength/2
            write(paste("RGBColor[", rcol, ",", gcol, ",", bcol, 
                "], Text [ \"", row.labels[j], "\", {", cur_xwidth, 
                ",", -0.4, ",", scalefac + 0.2, "}],\n", sep = ""), 
                file = filename, append = TRUE)
        }
    }
    if (!is.null(col.labels) && showlegend) {
        for (j in 1:length(col.labels)) {
            if (length(ccols) == length(col.labels)) {
                rcol <- (col2rgb(ccols[j])/255)[1]
                gcol <- (col2rgb(ccols[j])/255)[2]
                bcol <- (col2rgb(ccols[j])/255)[3]
            }
            else if (length(ccols) == 1) {
                rcol <- (col2rgb(ccols[1])/255)[1]
                gcol <- (col2rgb(ccols[1])/255)[2]
                bcol <- (col2rgb(ccols[1])/255)[3]
            }
            else {
                rcol <- (col2rgb("black")/255)[1]
                gcol <- (col2rgb("black")/255)[2]
                bcol <- (col2rgb("black")/255)[3]
            }
            cur_ywidth <- j/ncol(data) * scalefac - bwidth/2
            write(paste("RGBColor[", rcol, ",", gcol, ",", bcol, 
                "], Text [ \"", col.labels[j], "\", {", -0.4, 
                ",", cur_ywidth, ",", scalefac + 0.2, "}],\n", 
                sep = ""), file = filename, append = TRUE)
        }
    }
    bwidth <- bwidth/2
    for (k in 1:ncol(data)) {
        for (j in 1:nrow(data)) {
            x <- j/nrow(data) * scalefac
            y <- k/ncol(data) * scalefac
            z <- data[j, k]
            if (!is.null(ccols)) {
                rcol <- (col2rgb(ccols[k])/255)[1]
                gcol <- (col2rgb(ccols[k])/255)[2]
                bcol <- (col2rgb(ccols[k])/255)[3]
            }
            else if (!is.null(rcols)) {
                rcol <- (col2rgb(rcols[j])/255)[1]
                gcol <- (col2rgb(rcols[j])/255)[2]
                bcol <- (col2rgb(rcols[j])/255)[3]
            }
            else {
                rcol <- (col2rgb(cols[(k - 1) * ncol(data) + 
                  j])/255)[1]
                gcol <- (col2rgb(cols[(k - 1) * ncol(data) + 
                  j])/255)[2]
                bcol <- (col2rgb(cols[(k - 1) * ncol(data) + 
                  j])/255)[3]
            }
            write(paste("SurfaceColor[RGBColor[", rcol, ",", 
                gcol, ",", bcol, "]],  Cuboid[{", x - bwidth, 
                ",", y - bwidth, ",", 0, "},{", x + bwidth, ",", 
                y + bwidth, ",", z, "}],", sep = ""), file = filename, 
                append = TRUE)
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
    cat(paste("\nOutput file \"", filename, "\" was generated.\n", 
        sep = ""))
}

