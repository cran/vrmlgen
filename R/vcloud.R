`vcloud` <-
function (data, labels = rownames(data), filename = "out.wrl", 
    pointstyle = c("s", "b", "c"), cols = rainbow(length(unique(labels))), 
    showdensity = FALSE, scalefac = 4, lab.axis = c("X-axis", 
        "Y-axis", "Z-axis"), col.axis = "white", col.lab = "white", 
    col.bg = "black", cex.lab = 1, navigation = "EXAMINE", transparency = 0, 
    fov = 0.785, pos = rep(scalefac + 4, 3), dir = c(-0.59, 0.77, 
        0.24, 0.99)) 
{
    Data <- as.matrix(data)
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
    write("#VRML V2.0 utf8\n", file = filename, append = FALSE)
    write(paste("\nViewpoint {\n\tfieldOfView", fov, "\n\tposition", 
        pos[1], pos[2], pos[3], "\n\torientation", dir[1], dir[2], 
        dir[3], dir[4], "\n\tjump TRUE\n\tdescription \"viewpoint1\"\n}\n", 
        sep = " "), file = filename, append = TRUE)
    write(paste("\nNavigationInfo { type \"", navigation, "\" }\n", 
        sep = ""), file = filename, append = TRUE)
    bg_rcol <- (col2rgb(col.bg)/255)[1]
    bg_gcol <- (col2rgb(col.bg)/255)[2]
    bg_bcol <- (col2rgb(col.bg)/255)[3]
    write(paste("Background {\n\t skyColor [\n\t\t ", bg_rcol, 
        bg_gcol, bg_bcol, " \n\t]\n}", sep = " "), file = filename, 
        append = TRUE)
    if (length(labels)) {
        cur_height <- scalefac + 1.2
        for (j in 1:length(unique(labels))) {
            rcol <- (col2rgb(cols[unique(numlabels)[j]])/255)[1]
            gcol <- (col2rgb(cols[unique(numlabels)[j]])/255)[2]
            bcol <- (col2rgb(cols[unique(numlabels)[j]])/255)[3]
            write(paste("Transform {\n\ttranslation ", -nchar(as.character(unique(labels)[j])) * 
                0.075, " ", cur_height, " 0\n\tscale 0.36 0.36 0.36\n\trotation 0 1 0 0.7854\n\tchildren Shape {\n\t\tappearance Appearance { material Material {diffuseColor ", 
                rcol, " ", gcol, " ", bcol, "  } }\n\t\tgeometry Text { string \"", 
                unique(labels)[j], "\" }\n\t}\n}", sep = ""), 
                file = filename, append = TRUE)
            cur_height <- cur_height + 0.4
        }
    }
    lab_rcol <- (col2rgb(col.lab)/255)[1]
    lab_gcol <- (col2rgb(col.lab)/255)[2]
    lab_bcol <- (col2rgb(col.lab)/255)[3]
    write(paste("Transform {\n\ttranslation ", scalefac + 0.5, 
        " 0 0\n\tscale ", cex.lab * 0.28, cex.lab * 0.28, cex.lab * 
            0.28, "\n\tchildren Shape {\n\t\tappearance Appearance { material Material {diffuseColor ", 
        lab_rcol, lab_gcol, lab_bcol, "  } }\n\tgeometry Text { string \"", 
        lab.axis[1], "\" }\n\t}\n}", sep = " "), file = filename, 
        append = TRUE)
    write(paste("Transform {\n\ttranslation ", -nchar(as.character(lab.axis[3])) * 
        0.075 * cex.lab, " ", scalefac + 0.5, " 0\n\tscale ", 
        cex.lab * 0.28, cex.lab * 0.28, cex.lab * 0.28, "\n\trotation 0 1 0 0.7854\n\tchildren Shape {\n\t\tappearance Appearance { material Material {diffuseColor ", 
        lab_rcol, lab_gcol, lab_bcol, "  } }\n\tgeometry Text { string \"", 
        lab.axis[3], "\" }\n\t}\n}", sep = " "), file = filename, 
        append = TRUE)
    write(paste("Transform {\n\ttranslation 0 0 ", scalefac + 
        0.5 + nchar(as.character(lab.axis[2])) * 0.12 * cex.lab, 
        "\n\tscale ", cex.lab * 0.28, cex.lab * 0.28, cex.lab * 
            0.28, "\n\trotation 0 1 0 1.5708\n\tchildren Shape {\n\t\tappearance Appearance { material Material {diffuseColor ", 
        lab_rcol, lab_gcol, lab_bcol, "  } }\n\tgeometry Text { string \"", 
        lab.axis[2], "\" }\n\t}\n}", sep = " "), file = filename, 
        append = TRUE)
    ax_rcol <- (col2rgb(col.axis)/255)[1]
    ax_gcol <- (col2rgb(col.axis)/255)[2]
    ax_bcol <- (col2rgb(col.axis)/255)[3]
    write(paste("Transform {\n\ttranslation ", 0.5 * scalefac, 
        " 0 0\n\trotation 0 0 1 1.5708\n\tchildren Shape {\n\t\tappearance Appearance { material Material {\n\t\tdiffuseColor ", 
        ax_rcol, ax_gcol, ax_bcol, " } }\n\tgeometry Cylinder { height ", 
        scalefac, " radius 0.04 }\n\t}\n}", sep = " "), file = filename, 
        append = TRUE)
    write(paste("Transform {\n\ttranslation 0 ", 0.5 * scalefac, 
        " 0\n\trotation 0 0 1 0\n\tchildren Shape {\n\t\tappearance Appearance { material Material { diffuseColor ", 
        ax_rcol, ax_gcol, ax_bcol, " } }\n\tgeometry Cylinder { height ", 
        scalefac, " radius 0.04 }\n\t}\n}", sep = " "), file = filename, 
        append = TRUE)
    write(paste("Transform {\n\ttranslation 0 0 ", 0.5 * scalefac, 
        "\n\trotation 1 0 0 1.5708\n\tchildren Shape {\n\t\tappearance Appearance { material Material { diffuseColor ", 
        ax_rcol, ax_gcol, ax_bcol, " } }\n\tgeometry Cylinder { height ", 
        scalefac, " radius 0.04 }\n\t}\n}", sep = " "), file = filename, 
        append = TRUE)
    for (j in 1:nrow(data)) {
        x <- data[j, 1]
        y <- data[j, 3]
        z <- data[j, 2]
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
        else if (length(labels) && (cols == rainbow(length(unique(labels))))) {
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
            if (pointstyle == "c") 
                write(paste("\nTransform {\n\ttranslation ", 
                  x, " ", y, " ", z, "\n\tchildren Shape {\n\t\tappearance Appearance { material Material { diffuseColor ", 
                  rcol, " ", gcol, " ", bcol, " transparency ", 
                  transparency, " } }\n\tgeometry Cone { bottomRadius 0.08  height 0.08\n\t\tside TRUE}\n\t\t}\n}", 
                  sep = ""), file = filename, append = TRUE)
            else if (pointstyle == "s") 
                write(paste("\nTransform {\n\ttranslation ", 
                  x, " ", y, " ", z, "\n\tchildren Shape {\n\t\tappearance Appearance { material Material { diffuseColor ", 
                  rcol, " ", gcol, " ", bcol, " transparency ", 
                  transparency, "} }\n\tgeometry Sphere { radius 0.08}\n\t\t}\n}", 
                  sep = ""), file = filename, append = TRUE)
            else write(paste("\nTransform {\n\ttranslation ", 
                x, " ", y, " ", z, "\n\tchildren Shape {\n\t\tappearance Appearance { material Material { diffuseColor ", 
                rcol, " ", gcol, " ", bcol, " transparency ", 
                transparency, "} }\n\tgeometry Box { size 0.08 0.08 0.08}\n\t\t}\n}", 
                sep = ""), file = filename, append = TRUE)
        }
        else if (length(pointstyle) >= length(unique(numlabels))) {
            if (length(labels)) {
                stylevec <- c("s", "b", "c")
                curstyle <- stylevec[numlabels[j]]
                if (curstyle == "c") 
                  write(paste("\nTransform {\n\ttranslation ", 
                    x, " ", y, " ", z, "\n\tchildren Shape {\n\t\tappearance Appearance { material Material { diffuseColor ", 
                    rcol, " ", gcol, " ", bcol, " transparency ", 
                    transparency, "} }\n\tgeometry Cone { bottomRadius 0.08  height 0.08\n\t\tside TRUE}\n\t\t}\n}", 
                    sep = ""), file = filename, append = TRUE)
                else if (curstyle == "s") 
                  write(paste("\nTransform {\n\ttranslation ", 
                    x, " ", y, " ", z, "\n\tchildren Shape {\n\t\tappearance Appearance { material Material { diffuseColor ", 
                    rcol, " ", gcol, " ", bcol, " transparency ", 
                    transparency, "} }\n\tgeometry Sphere { radius 0.08}\n\t\t}\n}", 
                    sep = ""), file = filename, append = TRUE)
                else write(paste("\nTransform {\n\ttranslation ", 
                  x, " ", y, " ", z, "\n\tchildren Shape {\n\t\tappearance Appearance { material Material { diffuseColor ", 
                  rcol, " ", gcol, " ", bcol, " transparency ", 
                  transparency, "} }\n\tgeometry Box { size 0.08 0.08 0.08}\n\t\t}\n}", 
                  sep = ""), file = filename, append = TRUE)
            }
            else {
                if (pointstyle[1] == "c") 
                  write(paste("Transform {\n\ttranslation ", 
                    x, " ", y, " ", z, "\n\tchildren Shape {\n\t\tappearance Appearance { material Material { diffuseColor ", 
                    rcol, " ", gcol, " ", bcol, " transparency ", 
                    transparency, "} }\n\tgeometry Cone { bottomRadius 0.08  height 0.08\n\t\tside TRUE}\n\t\t}\n}", 
                    sep = ""), file = filename, append = TRUE)
                else if (pointstyle[1] == "s") 
                  write(paste("Transform {\n\ttranslation ", 
                    x, " ", y, " ", z, "\n\tchildren Shape {\n\t\tappearance Appearance { material Material { diffuseColor ", 
                    rcol, " ", gcol, " ", bcol, " transparency ", 
                    transparency, "} }\n\tgeometry Sphere { radius 0.08}\n\t\t}\n}", 
                    sep = ""), file = filename, append = TRUE)
                else write(paste("Transform {\n\ttranslation ", 
                  x, " ", y, " ", z, "\n\tchildren Shape {\n\t\tappearance Appearance { material Material { diffuseColor ", 
                  rcol, " ", gcol, " ", bcol, " transparency ", 
                  transparency, "} }\n\tgeometry Box { size 0.08 0.08 0.08}\n\t\t}\n}", 
                  sep = ""), file = filename, append = TRUE)
            }
        }
        else {
            if (length(labels)) {
                if (pointstyle[1] == "c") 
                  write(paste("\nTransform {\n\ttranslation ", 
                    x, " ", y, " ", z, "\n\tchildren Shape {\n\t\tappearance Appearance { material Material { diffuseColor ", 
                    rcol, " ", gcol, " ", bcol, " transparency ", 
                    transparency, "} }\n\tgeometry Cone { bottomRadius 0.08  height 0.08\n\t\tside TRUE}\n\t\t}\n}\n]\n}\n]\n}", 
                    sep = ""), file = filename, append = TRUE)
                else if (pointstyle[1] == "s") 
                  write(paste("\nTransform {\n\ttranslation ", 
                    x, " ", y, " ", z, "\n\tchildren Shape {\n\t\tappearance Appearance { material Material { diffuseColor ", 
                    rcol, " ", gcol, " ", bcol, " transparency ", 
                    transparency, "} }\n\tgeometry Sphere { radius 0.08}\n\t\t}\n}", 
                    sep = ""), file = filename, append = TRUE)
                else write(paste("\nTransform {\n\ttranslation ", 
                  x, " ", y, " ", z, "\n\tchildren Shape {\n\t\tappearance Appearance { material Material { diffuseColor ", 
                  rcol, " ", gcol, " ", bcol, " transparency ", 
                  transparency, "} }\n\tgeometry Box { size 0.08 0.08 0.08}\n\t\t}\n}", 
                  sep = ""), file = filename, append = TRUE)
            }
            else {
                if (pointstyle[1] == "c") 
                  write(paste("Transform {\n\ttranslation ", 
                    x, " ", y, " ", z, "\n\tchildren Shape {\n\t\tappearance Appearance { material Material { diffuseColor ", 
                    rcol, " ", gcol, " ", bcol, " transparency ", 
                    transparency, "} }\n\tgeometry Cone { bottomRadius 0.08  height 0.08\n\t\tside TRUE}\n\t\t}\n}", 
                    sep = ""), file = filename, append = TRUE)
                else if (pointstyle[1] == "s") 
                  write(paste("Transform {\n\ttranslation ", 
                    x, " ", y, " ", z, "\n\tchildren Shape {\n\t\tappearance Appearance { material Material { diffuseColor ", 
                    rcol, " ", gcol, " ", bcol, " transparency ", 
                    transparency, "} }\n\tgeometry Sphere { radius 0.08}\n\t\t}\n}", 
                    sep = ""), file = filename, append = TRUE)
                else write(paste("Transform {\n\ttranslation ", 
                  x, " ", y, " ", z, "\n\tchildren Shape {\n\t\tappearance Appearance { material Material { diffuseColor ", 
                  rcol, " ", gcol, " ", bcol, " transparency ", 
                  transparency, "} }\n\tgeometry Box { size 0.08 0.08 0.08}\n\t\t}\n}", 
                  sep = ""), file = filename, append = TRUE)
            }
        }
    }
    write("\n", file = filename, append = TRUE)
    if (showdensity) 
        est <- .vdense(data, filename)
}

