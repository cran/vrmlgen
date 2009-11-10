`vcloud` <-
function (data, labels = rownames(data), metalabels = NULL, filename = "out.wrl", 
    pointstyle = c("s", "b", "c"), cols = rainbow(length(unique(labels))), 
    showdensity = FALSE, scalefac = 4, autoscale = "independent", 
    lab.axis = c("X-axis", "Y-axis", "Z-axis"), col.axis = "white", 
    showaxis = TRUE, col.lab = "white", col.bg = "black", cex.lab = 1, 
    navigation = "EXAMINE", transparency = 0, fov = 0.785, pos = rep(scalefac + 
        4, 3), dir = c(0.19, 0.45, 0.87, 2.45), htmlout = NULL, 
    hwidth = 1200, hheight = 800, showlegend = TRUE) 
{
    
    data <- as.matrix(data)
    if (ncol(data) != 3) {
        stop("Data matrix does not have 3 columns!")
    }
    
    # identify unique set of row labels
    numlabels <- NULL
    if (length(labels)) {
        lab <- unique(unlist(labels))
        numlabels <- apply(as.matrix(labels), 1, function(x) match(x, 
            as.matrix(lab)))
    }
    
    # check if metalabels are available
    popuptext <- TRUE
    if (is.null(metalabels)) {
        popuptext <- FALSE
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
    if (popuptext) 
        write(paste("\n\nPROTO PopupText [\n\teventIn MFString showThis\n\texposedField SFNode fontStyle FontStyle {\n\t\t\t\tjustify [ \"MIDDLE\", \"MIDDLE\" ] }\n\texposedField SFNode appearance Appearance { material Material { } } \n\tfield SFString default \"\" \n\tfield MFString stringlist [ \"\" ] \n", 
            paste(paste("eventIn SFBool over", 0:(nrow(data) - 
                1), "\n\t", sep = "", collapse = ""), sep = "", 
                collapse = ""), "\t] { \nGroup { children [ \n\tDEF POPIT Script { \n\t\tfield SFString defstring IS default \n\t\tfield MFString list IS stringlist \n\t\tfield MFString strout [ \"\" ] ", 
            paste(paste("\n eventIn SFBool over", 0:(nrow(data) - 
                1), " IS over", 0:(nrow(data) - 1), sep = "", 
                collapse = "")), "\n\t\teventOut MFString string_changed \n\t\turl [ \"javascript: \n\t\tfunction evtnum(num, value) \n\t\t{ \n\t\tif (value && (num < list.length)) \n\t\t\tstrout[0] = list[num]; \n\t\telse \n\t\t\tstrout[0] = defstring; \n\t\tstring_changed = strout; \n\t\t} \n \n\t", 
            paste(paste("\tfunction over", 0:(nrow(data) - 1), 
                "(v, t) { evtnum(", 0:(nrow(data) - 1), ", v); } \n", 
                sep = "", collapse = "")), "\t\", \n\t\t\"Popup.class\"] \n\t} \n\t \n\tTransform { \n\ttranslation 0 0 ", 
            scalefac, " \n\trotation 0.28108 0.67860 0.67860 2.59356\n\t \n\tchildren Shape { \n\t\tappearance IS appearance \n\tgeometry DEF POPUP Text { \n\t\tstring \"\" \n\t\tset_string IS showThis \n\t\tfontStyle IS fontStyle \n\t\t} \n\t} \n\t} \n] } \nROUTE POPIT.string_changed TO POPUP.set_string \n} \n \n \nGroup { \nchildren DEF POP PopupText { \n\t\t#default \"Nothing selected\" \n\t\tstringlist [ ", 
            paste(paste("\"", metalabels[1:(nrow(data) - 1)], 
                "\",", sep = "", collapse = " ")), "\"", metalabels[nrow(data)], 
            "\" ] \n\t} \n} \n\n"), file = filename, append = TRUE)
   
    # set background color
    bg_rcol <- (col2rgb(col.bg)/255)[1]
    bg_gcol <- (col2rgb(col.bg)/255)[2]
    bg_bcol <- (col2rgb(col.bg)/255)[3]
    write(paste("Background {\n\t skyColor [\n\t\t ", bg_rcol, 
        bg_gcol, bg_bcol, " \n\t]\n}", sep = " "), file = filename, 
        append = TRUE)
    
    # draw the plot legend    
    if (length(labels) && showlegend) {
        cur_height <- scalefac + 1.2
        for (j in 1:length(unique(labels))) {
            rcol <- (col2rgb(cols[unique(numlabels)[j]])/255)[1]
            gcol <- (col2rgb(cols[unique(numlabels)[j]])/255)[2]
            bcol <- (col2rgb(cols[unique(numlabels)[j]])/255)[3]
            write(paste("Transform {\n\tcenter 0 0 0 \n\ttranslation 0 0 ", 
                cur_height, "\n\tscale 0.36 0.36 0.36\n\trotation 0.28108 0.67860 0.67860 2.59356\n\tchildren Shape {\n\t\tappearance Appearance { material Material {diffuseColor ", 
                rcol, " ", gcol, " ", bcol, "  } }\n\t\tgeometry Text { string \"", 
                unique(labels)[j], "\" }\n\t}\n}", sep = ""), 
                file = filename, append = TRUE)
            cur_height <- cur_height + 0.4
        }
    }
    
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
    
    popup_txt_str <- ""
    popup_txt_str2 <- ""
    
    # drawing a single point with a given point-style
    single_pointstyle <- function(pointstyle, x, y, z, rcol, 
        gcol, bcol, popup_txt_str, popup_txt_str2, filename) {
        
        # cone style
        if (pointstyle == "c") 
            write(paste(popup_txt_str, "\nTransform {\n\ttranslation ", 
                x, " ", y, " ", z, "\n\trotation 1 0 0 1.5708\n\tchildren Shape {\n\t\tappearance Appearance { material Material { diffuseColor ", 
                rcol, " ", gcol, " ", bcol, " transparency ", 
                transparency, " } }\n\tgeometry Cone { bottomRadius 0.08  height 0.08\n\t\tside TRUE}\n\t\t}\n}", 
                popup_txt_str2, sep = ""), file = filename, append = TRUE)
        
        # sphere style
        else if (pointstyle == "s") 
            write(paste(popup_txt_str, "\nTransform {\n\ttranslation ", 
                x, " ", y, " ", z, "\n\tchildren Shape {\n\t\tappearance Appearance { material Material { diffuseColor ", 
                rcol, " ", gcol, " ", bcol, " transparency ", 
                transparency, "} }\n\tgeometry Sphere { radius 0.08}\n\t\t}\n}", 
                popup_txt_str2, sep = ""), file = filename, append = TRUE)
        
        # box style
        else write(paste(popup_txt_str, "\nTransform {\n\ttranslation ", 
            x, " ", y, " ", z, "\n\tchildren Shape {\n\t\tappearance Appearance { material Material { diffuseColor ", 
            rcol, " ", gcol, " ", bcol, " transparency ", transparency, 
            "} }\n\tgeometry Box { size 0.08 0.08 0.08}\n\t\t}\n}", 
            popup_txt_str2, sep = ""), file = filename, append = TRUE)
    }
    
    # main loop: iterate over data points
    for (j in 1:nrow(data)) {
        
        x <- data[j, 1]
        y <- data[j, 2]
        z <- data[j, 3]
                
        # set data point color
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
        
        # configure popup-text for meta-labels        
        if (popuptext) {
            popup_txt_str <- paste("Group {\n  children [\n    DEF Schalter", 
                j - 1, " TouchSensor { }", sep = "", collapse = "")
            popup_txt_str2 <- "\n]\n}"
        }
        
        # draw data point with given point-style
        if (length(pointstyle) == 1) {
            
            single_pointstyle(pointstyle, x, y, z, rcol, gcol, 
                bcol, popup_txt_str, popup_txt_str2, filename)
        }
        else if (length(pointstyle) >= length(unique(numlabels))) {
            
            if (length(labels)) {
                
                stylevec <- c("s", "b", "c")
                curstyle <- stylevec[numlabels[j]]
                single_pointstyle(curstyle, x, y, z, rcol, gcol, 
                  bcol, popup_txt_str, popup_txt_str2, filename)
            }
            else {
                
                popup_txt_str <- ""
                popup_txt_str2 <- ""
                single_pointstyle(pointstyle[1], x, y, z, rcol, 
                  gcol, bcol, popup_txt_str, popup_txt_str2, 
                  filename)
            }
        }
        else {
            
            if (length(labels)) {
                
                single_pointstyle(pointstyle[1], x, y, z, rcol, 
                  gcol, bcol, popup_txt_str, popup_txt_str2, 
                  filename)
            }
            else {
                
                popup_txt_str <- ""
                popup_txt_str2 <- ""
                single_pointstyle(pointstyle[1], x, y, z, rcol, 
                  gcol, bcol, popup_txt_str, popup_txt_str2, 
                  filename)
            }
        }
    }
        
    write("\n", file = filename, append = TRUE)
    
    # configure interactive popup-text
    if (popuptext) 
        write(paste("ROUTE Schalter", 0:(nrow(data) - 1), ".isOver TO POP.over", 
            0:(nrow(data) - 1), "\n", sep = "", collapse = ""), 
            file = filename, append = TRUE)
    
    # draw density estimation contour surfaces
    if (showdensity) 
        est <- .vdense(data, filename)
        
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

