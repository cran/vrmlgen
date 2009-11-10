`vbar` <-
function (data, row.labels = rownames(data), col.labels = colnames(data), 
    metalabels = NULL, filename = "out.wrl", space = 0.5, cols = rainbow(length(as.matrix(data))), 
    rcols = NULL, ccols = NULL, scalefac = 4, lab.axis = c("X-axis", 
        "Y-axis", "Z-axis"), col.axis = "white", showaxis = TRUE, 
    col.lab = "white", col.bg = "black", cex.lab = 1, cex.rowlab = 1, 
    cex.collab = 1, navigation = "EXAMINE", fov = 0.785, pos = rep(scalefac + 
        4, 3), dir = c(0.19, 0.45, 0.87, 2.45), htmlout = NULL, 
    hwidth = 1200, hheight = 800, showlegend = TRUE) 
{
    
    data <- as.matrix(data)
    
    # scale the data to fit within the axes ranges
    data <- scalefac * (data/max(data))
    
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
        
    # configure popup-text for metalabels        
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
            "\t\", \n\t\t\"Popup.class\"] \n\t} \n\t \n\tTransform { \n\ttranslation 0 0 ", 
            scalefac, " \n\trotation 0.28108 0.67860 0.67860 2.59356\n\t \n\tchildren Shape { \n\t\tappearance IS appearance \n\tgeometry DEF POPUP Text { \n\t\tstring \"\" \n\t\tset_string IS showThis \n\t\tfontStyle IS fontStyle \n\t\t} \n\t} \n\t} \n] } \nROUTE POPIT.string_changed TO POPUP.set_string \n} \n \n \nGroup { \nchildren DEF POP PopupText { \n\t\t#default \"Nothing selected\" \n\t\tstringlist [ ", 
            paste(paste("\"", metalabels[1:(length(metalabels) - 
                1)], "\",", sep = "", collapse = " ")), "\"", 
            metalabels[length(metalabels)], "\" ] \n\t} \n} \n\n", 
            sep = "", collapse = ""), file = filename, append = TRUE)
            

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
    
    
    # calculate bar-length and -width (account for scaling and spacing)
    blength <- scalefac/(nrow(data) * (1 + space))
    bwidth <- scalefac/(ncol(data) * (1 + space))
    
    # draw row-labels
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
            
            write(paste("Transform {\n\ttranslation ", cur_xwidth, 
                -0.4, scalefac + 0.2, "\n\tscale ", cex.rowlab * 
                  0.28, cex.rowlab * 0.28, cex.rowlab * 0.28, 
                "\n\trotation 0.00000 0.70711 0.70711 3.14159\n\tchildren Shape {\n\t\tappearance Appearance { material Material {diffuseColor ", 
                rcol, " ", gcol, " ", bcol, "  } }\n\t\tgeometry Text { string \"", 
                row.labels[j], "\" }\n\t}\n}", sep = " "), file = filename, 
                append = TRUE)
        }
    }
    
    # draw column-labels
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
            
            write(paste("Transform {\n\ttranslation ", -0.4, 
                cur_ywidth, scalefac + 0.2, "\n\tscale ", cex.collab * 
                  0.28, cex.collab * 0.28, cex.collab * 0.28, 
                "\n\trotation 0.57735 0.57735 0.57735 2.09440\n\tchildren Shape {\n\t\tappearance Appearance { material Material {diffuseColor ", 
                rcol, " ", gcol, " ", bcol, "  } }\n\t\tgeometry Text { string \"", 
                col.labels[j], "\" }\n\t}\n}", sep = " "), file = filename, 
                append = TRUE)
        }
    }
    
    # initialize color and meta-label variables
    popup_txt_str <- ""
    popup_txt_str2 <- ""
    
    rcol <- NULL
    gcol <- NULL
    bcol <- NULL
    
    # set RGB-colors
    if (length(cols) >= (ncol(data) * nrow(data))) {
        rcols <- sapply(cols, function(x) (col2rgb(x)/255)[1])
        gcols <- sapply(cols, function(x) (col2rgb(x)/255)[2])
        bcols <- sapply(cols, function(x) (col2rgb(x)/255)[3])
    }
    else {
        if (length(cols) >= 1) {
            rcol <- (col2rgb(cols[1])/255)[1]
            gcol <- (col2rgb(cols[1])/255)[2]
            bcol <- (col2rgb(cols[1])/255)[3]
        }
        else {
            rcol <- (col2rgb("lightblue")/255)[1]
            gcol <- (col2rgb("lightblue")/255)[2]
            bcol <- (col2rgb("lightblue")/255)[3]
        }
    }
    
    
    # main loop: iterate over data points
    counter <- 0
    for (j in 1:nrow(data)) {
    
        for (k in 1:ncol(data)) {
            
            x <- j/nrow(data) * scalefac
            y <- k/ncol(data) * scalefac
            z <- data[j, k]
            
            counter <- counter + 1
            # set current color
            if (!is.null(rcols)) {
                rcol <- rcols[counter]
                gcol <- gcols[counter]
                bcol <- bcols[counter]
            }
            
            # set meta-label touch sensor
            if (popuptext) {
                popup_txt_str <- paste("Group {\n  children [\n    DEF Schalter", 
                  (k - 1) * ncol(data) + j - 1, " TouchSensor { }", 
                  sep = "", collapse = "")
                popup_txt_str2 <- "\n]\n}"
            }
            
            # draw data point
            write(paste(popup_txt_str, "Transform {\n\ttranslation ", 
                x, y, z/2, "\n\tchildren Shape {\n\t\tappearance Appearance { material Material { diffuseColor ", 
                rcol, gcol, bcol, " } }\n\tgeometry Box { size ", 
                blength, bwidth, z, "}\n\t\t}\n}", popup_txt_str2, 
                sep = " "), file = filename, append = TRUE)
        }
    }
    
    write("\n", file = filename, append = TRUE)
    
    
    # configure interactive popup-text
    if (popuptext) 
        write(paste("ROUTE Schalter", 0:(length(metalabels) - 
            1), ".isOver TO POP.over", 0:(length(metalabels) - 
            1), "\n", sep = "", collapse = ""), file = filename, 
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

