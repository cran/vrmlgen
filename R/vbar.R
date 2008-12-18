`vbar` <-
function (data, row.labels = rownames(data), col.labels = colnames(data), 
    metalabels = NULL, filename = "out.wrl", space = 0.5, cols = rainbow(length(as.matrix(data))), 
    scalefac = 4, lab.axis = c("X-axis", "Y-axis", "Z-axis"), 
    col.axis = "white", showaxis = TRUE, col.lab = "white", col.bg = "black", 
    cex.lab = 1, cex.rowlab = 1, cex.collab = 1, navigation = "EXAMINE", 
    fov = 0.785, pos = rep(scalefac + 4, 3), dir = c(-0.59, 0.77, 
        0.24, 0.99), htmlout = NULL, hwidth = 1200, hheight = 800) 
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
    popuptext <- TRUE
    if (is.null(metalabels)) {
        popuptext <- FALSE
    }
    if (popuptext) 
        write(paste("\n\nPROTO PopupText [\n\teventIn MFString showThis\n\texposedField SFNode fontStyle FontStyle {\n\t\t\t\tjustify [ \"MIDDLE\", \"MIDDLE\" ] }\n\texposedField SFNode appearance Appearance { material Material { } } \n\tfield SFString default \"\" \n\tfield MFString stringlist [ \"\" ] \n", 
            paste(paste("eventIn SFBool over", 0:(length(metalabels) - 
                1), "\n\t", sep = "", collapse = ""), sep = "", 
                collapse = ""), "\t] { \nGroup { children [ \n\tDEF POPIT Script { \n\t\tfield SFString defstring IS default \n\t\tfield MFString list IS stringlist \n\t\tfield MFString strout [ \"\" ] ", 
            paste(paste("\n eventIn SFBool over", 0:(length(metalabels) - 
                1), " IS over", 0:(length(metalabels) - 1), sep = "", 
                collapse = "")), "\n\t\teventOut MFString string_changed \n\t\turl [ \"javascript: \n\t\tfunction evtnum(num, value) \n\t\t{ \n\t\tif (value && (num < list.length)) \n\t\t\tstrout[0] = list[num]; \n\t\telse \n\t\t\tstrout[0] = defstring; \n\t\tstring_changed = strout; \n\t\t} \n \n\t", 
            paste(paste("\tfunction over", 0:(length(metalabels) - 
                1), "(v, t) { evtnum(", 0:(length(metalabels) - 
                1), ", v); } \n", sep = "", collapse = "")), 
            "\t\", \n\t\t\"Popup.class\"] \n\t} \n\t \n\tTransform { \n\ttranslation 0 ", 
            scalefac, " 0 \n\trotation 0 1 0 0.7854\n\t \n\tchildren Shape { \n\t\tappearance IS appearance \n\tgeometry DEF POPUP Text { \n\t\tstring \"\" \n\t\tset_string IS showThis \n\t\tfontStyle IS fontStyle \n\t\t} \n\t} \n\t} \n] } \nROUTE POPIT.string_changed TO POPUP.set_string \n} \n \n \nGroup { \nchildren DEF POP PopupText { \n\t\t#default \"Nothing selected\" \n\t\tstringlist [ ", 
            paste(paste("\"", metalabels[1:(length(metalabels) - 
                1)], "\",", sep = "", collapse = " ")), "\"", 
            metalabels[length(metalabels)], "\" ] \n\t} \n} \n\n", 
            sep = "", collapse = ""), file = filename, append = TRUE)
    bg_rcol <- (col2rgb(col.bg)/255)[1]
    bg_gcol <- (col2rgb(col.bg)/255)[2]
    bg_bcol <- (col2rgb(col.bg)/255)[3]
    write(paste("Background {\n\t skyColor [\n\t\t ", bg_rcol, 
        bg_gcol, bg_bcol, " \n\t]\n}", sep = " "), file = filename, 
        append = TRUE)
    if (showaxis) {
        lab_rcol <- (col2rgb(col.lab)/255)[1]
        lab_gcol <- (col2rgb(col.lab)/255)[2]
        lab_bcol <- (col2rgb(col.lab)/255)[3]
        write(paste("Transform {\n\ttranslation ", scalefac + 
            0.5, " 0 0\n\tscale ", cex.lab * 0.28, cex.lab * 
            0.28, cex.lab * 0.28, "\n\tchildren Shape {\n\t\tappearance Appearance { material Material {diffuseColor ", 
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
        write(paste("Transform {\n\ttranslation 0 0 ", 0.5 * 
            scalefac, "\n\trotation 1 0 0 1.5708\n\tchildren Shape {\n\t\tappearance Appearance { material Material { diffuseColor ", 
            ax_rcol, ax_gcol, ax_bcol, " } }\n\tgeometry Cylinder { height ", 
            scalefac, " radius 0.04 }\n\t}\n}", sep = " "), file = filename, 
            append = TRUE)
    }
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
    popup_txt_str <- ""
    popup_txt_str2 <- ""
    for (k in 1:ncol(data)) {
        for (j in 1:nrow(data)) {
            x <- j/nrow(data) * scalefac
            z <- k/ncol(data) * scalefac
            y <- data[j, k]/2
            rcol <- (col2rgb(cols[(k - 1) * ncol(data) + j])/255)[1]
            gcol <- (col2rgb(cols[(k - 1) * ncol(data) + j])/255)[2]
            bcol <- (col2rgb(cols[(k - 1) * ncol(data) + j])/255)[3]
            if (popuptext) {
                popup_txt_str <- paste("Group {\n  children [\n    DEF Schalter", 
                  (k - 1) * ncol(data) + j - 1, " TouchSensor { }", 
                  sep = "", collapse = "")
                popup_txt_str2 <- "\n]\n}"
            }
            write(paste(popup_txt_str, "Transform {\n\ttranslation ", 
                x, y, z, "\n\tchildren Shape {\n\t\tappearance Appearance { material Material { diffuseColor ", 
                rcol, gcol, bcol, " } }\n\tgeometry Box { size ", 
                blength, data[j, k], bwidth, "}\n\t\t}\n}", popup_txt_str2, 
                sep = " "), file = filename, append = TRUE)
        }
    }
    write("\n", file = filename, append = TRUE)
    if (popuptext) 
        write(paste("ROUTE Schalter", 0:(length(metalabels) - 
            1), ".isOver TO POP.over", 0:(length(metalabels) - 
            1), "\n", sep = "", collapse = ""), file = filename, 
            append = TRUE)
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
}

