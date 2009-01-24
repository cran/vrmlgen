`vbar` <-
function (data, row.labels = rownames(data), col.labels = colnames(data), 
    filename = "out.wrl", space = 0.5, cols = rainbow(length(data)), 
    scalefac = 4, lab.axis = c("X-axis", "Y-axis", "Z-axis"), 
    col.axis = "white", col.lab = "white", col.bg = "black", 
    cex.lab = 1, cex.rowlab = 1, cex.collab = 1, navigation = "EXAMINE", 
    fov = 0.785, pos = rep(scalefac + 4, 3), dir = c(-0.59, 0.77, 
        0.24, 0.99)) 
{
    data <- as.matrix(data)
    data <- scalefac * (data/max(data))
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
    blength <- scalefac/(nrow(data) * (1 + space))
    bwidth <- scalefac/(ncol(data) * (1 + space))
    if (!is.null(row.labels)) {
        for (j in 1:length(row.labels)) {
            rcol <- (col2rgb(col.axis)/255)[1]
            gcol <- (col2rgb(col.axis)/255)[2]
            bcol <- (col2rgb(col.axis)/255)[3]
            cur_xwidth <- j/nrow(data) * scalefac - blength/2
            write(paste("Transform {\n\ttranslation ", cur_xwidth, 
                scalefac + 0.2, -0.4, "\n\tscale ", cex.rowlab * 
                  0.28, cex.rowlab * 0.28, cex.rowlab * 0.28, 
                "\n\tchildren Shape {\n\t\tappearance Appearance { material Material {diffuseColor ", 
                rcol, " ", gcol, " ", bcol, "  } }\n\t\tgeometry Text { string \"", 
                row.labels[j], "\" }\n\t}\n}", sep = " "), file = filename, 
                append = TRUE)
        }
    }
    if (!is.null(col.labels)) {
        for (j in 1:length(col.labels)) {
            rcol <- (col2rgb(col.axis)/255)[1]
            gcol <- (col2rgb(col.axis)/255)[2]
            bcol <- (col2rgb(col.axis)/255)[3]
            cur_ywidth <- j/ncol(data) * scalefac - bwidth/2
            write(paste("Transform {\n\ttranslation ", -0.4, 
                scalefac + 0.2, cur_ywidth, "\n\tscale ", cex.collab * 
                  0.28, cex.collab * 0.28, cex.collab * 0.28, 
                "\n\trotation 0 1 0 1.5708\n\tchildren Shape {\n\t\tappearance Appearance { material Material {diffuseColor ", 
                rcol, " ", gcol, " ", bcol, "  } }\n\t\tgeometry Text { string \"", 
                col.labels[j], "\" }\n\t}\n}", sep = " "), file = filename, 
                append = TRUE)
        }
    }
    for (k in 1:ncol(data)) {
        for (j in 1:nrow(data)) {
            x <- j/nrow(data) * scalefac
            z <- k/ncol(data) * scalefac
            y <- data[j, k]/2
            rcol <- (col2rgb(cols[(k - 1) * ncol(data) + j])/255)[1]
            gcol <- (col2rgb(cols[(k - 1) * ncol(data) + j])/255)[2]
            bcol <- (col2rgb(cols[(k - 1) * ncol(data) + j])/255)[3]
            write(paste("Transform {\n\ttranslation ", x, y, 
                z, "\n\tchildren Shape {\n\t\tappearance Appearance { material Material { diffuseColor ", 
                rcol, gcol, bcol, " } }\n\tgeometry Box { size ", 
                blength, data[j, k], bwidth, "}\n\t\t}\n}", sep = " "), 
                file = filename, append = TRUE)
        }
    }
    write("\n", file = filename, append = TRUE)
}

