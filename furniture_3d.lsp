
;    Furniture, generates basic furniture,  by user input of width and depth (and height in some cases)
;    Copyright (C) 2018 octobre  Joern Rettweiler
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <https://www.gnu.org/licenses/>.



(defun c:AC-Furniture (/ entities set2d furniture_dim)







	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;;;;;;;;;;;;BEGIN FUNCITON DEFINITION;;;;;;;;;;;;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun geom_2d_rectang (/ pt_1 pt_2 pt_3 pt_4)
;points will be generated anti clockwise

    (setq pt_0 '(0 0 0))

    (setq pt_1 (list width 0 0))
    (setq pt_2 (list width depth 0))
    (setq pt_3 (list 0 depth 0))
    (setq pt_4 '(0 0 0))




    (setq pts_2d_geom (list pt_0 pt_1 pt_2 pt_3 pt_4))


    (command ".-layer" "_set" layer_geom2d "")


    (LWPoly pts_2d_geom)

  )


  (defun geom_2d_workingarea ()
;generates working area default values references to ASR (Arbeitstaettenrichtlinie / Common German Labor Facility Guide )

    (if (= furniture_typ 5)
      (progn  ; then predicate

        (setq depth_inv -1000)
      )
      (
        setq depth_inv (* width -0.5)
      )
    )

    (setq pt_0 (list 0 0 0))
    (setq pt_1 (list 0 depth_inv 0))
    (setq pt_2 (list width depth_inv 0))
    (setq pt_3 (list width 0 0))




    (setq pts_workarea (list pt_0 pt_1 pt_2 pt_3))

;generating geom and bind geom to layer

    (command ".-layer" "_set" layer_geom2d_wa "")
    (LWPoly pts_workarea)


;(print "pts Workspace")
;(print pts_workarea)


  )

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun LWPoly (pts)
    (entmakex (append (list
                        (cons 0 "LWPOLYLINE")
                        (cons 100 "AcDbPolyline")
                        (cons 100 "AcDbEntity")
                        (cons 90 (length pts))
                        (cons 70 0)   ;close 1 open 0
                      )
                (mapcar (function (lambda (p) (cons 10 p))) pts)
              )

    )




    (setq entity (entlast))
    (ssadd entity set2d)
    
    ;testing 
    (setq blockelements (list ))
    (cons blockelements (entlast))
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun shelf_heights ()
    ; Add attribut with height

    (setq txtsize 80)
    (setq pt (list (/ width 2) (- (* depth 0.6) (* txtsize 3)) 0))
    (setq furniture_height_cm 600)
    (setq furniture_height_cm (getint "Enter furniture heigt[cm]"))




    (setq att_furniture_height (strcat (rtos furniture_height_cm) "[cm]"))
    (attdef att_furniture_height pt 2 txtsize)
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun geom_2d_swingdoor ()
    ;wallthickness


    (defun swing_door ()
      (setq radius (- (/ width 2) wallthickness))
      (entmake
        (list
          (cons 0 "ARC")
          (cons 100 "AcDbArc")
          (cons 100 "AcDbCircle")
          (cons 10 center_circle)
          (cons 40 radius)
          (cons 50 start_ang)
          (cons 51 end_ang)

        )

      )

      (ssadd (entlast) set2d)
    )

;set raidus

;left swing door arc
    (setq center_circle (list (- width wallthickness) 0 0))
    (setq start_ang (* pi (/ -180 180.0)))
    (setq end_ang (* pi (/ -90 180.0)))
    (swing_door)

;draw line of swing door
    (setq pt_1 (list wallthickness 0 0))
    (setq pt_2 (list wallthickness (+ depth_inv wallthickness) 0))
    (setq pts_door (list pt_1 pt_2))
    (LWPoly pts_door)


;right swing door arc
    (setq center_circle (list wallthickness 0 0))

    (setq start_ang (* pi (/ -90 180.0)))
    (setq end_ang (* pi (/ 0 180.0)))
    (swing_door)

;draw line of swing door
    (setq pt_1 (list (- width wallthickness) 0 0))
    (setq pt_2 (list (- width wallthickness) (+ depth_inv wallthickness) 0))
    (setq pts_door (list pt_1 pt_2))
    (LWPoly pts_door)










  )


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun layermake (layername layercolor)
    (entmake
      (list
        (cons 0 "LAYER")
        (cons 100 "AcDbSymbolTableRecord")
        (cons 100 "AcDbLayerTableRecord")
        (cons 70 0)
        (cons 2 layername)
        (cons 6 "Continuous")
        (cons 62 layercolor)
      )
    )
  )

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun attdef (defval pt flag height)

    (command ".-layer" "_set" layer_att "")

    (entmakex (list (cons 0 "ATTDEF")
                    (cons 10 pt)      ;point
                    (cons 11 pt)      ;point
                    (cons 40 height)  ;textheight
                    (cons 1 defval)   ;default value
                    (cons 3 "none")
                    (cons 2 defval)   ;tag value
                    (cons 70 flag)
                    (cons 72 1)       ; horizontal justification
                    (cons 73 1)       ;vertical justification
              )
    )
    ;  (setq entities (append (entlast)))


    (setq entitiy_geom2d (entlast))


    (ssadd entitiy_geom2d set2d)
  )

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun attdata ()
    (setq txtsize 80)
    (setq pt (list (/ width 2) (* depth 0.6) 0))
    (attdef furniture_name pt 2 txtsize)

;if-condition because of round table
    (if (> depth 0)
      (setq furniture_dim (strcat (rtos width_cm) "x" (rtos depth_cm)))
      (setq furniture_dim (strcat "Ø" (rtos width_cm)))
    )

    (print furniture_dim)


    (setq pt (list (/ width 2) (- (* depth 0.6) (* txtsize 1.5)) 0))


    (attdef furniture_dim pt 2 txtsize)


  )

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun rect2d (pt width_rectangle depth_rectangle)


    (setq pt_input_x (nth 0 pt))
    (setq pt_input_y (nth 1 pt))


    (setq pt1 pt)
    (setq pt2 (list (+ pt_input_x width_rectangle) pt_input_y 0))

    (setq pt3 (list (+ pt_input_x width_rectangle) (+ pt_input_y depth_rectangle) 0))
    (setq pt4 (list pt_input_x (+ pt_input_y depth_rectangle) 0))

    (print "pt_inmput_x")
    (print pt_input_x)

    (print "pt4 XXX")
    (print pt4)

    (setq pts_foot (list pt1 pt2 pt3 pt4 pt1))
    (print "pts food")
    (print pts_foot)

    (LWPoly pts_foot)


;(setq entitiy_geom2d(entlast))
;(ssadd  entitiy_geom2d set2d)
  )


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun geom3d_extrusion ())




  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun geom2d_desk_foot ()


    (defun geom_food_2d (foot_k-factor)
    ;generate 2d foot abstractions


;generate outline rectangle

      (setq foot_width 50)


      (setq dist_margin (+ 25 foot_k-factor))
      (setq dist_margin_y 25)

;(if (foot_k-factor > 0 )

      (setq base_x (+ 25 foot_k-factor))

; (setq base_x (+ 25 foot_k-factor))


(if (= furniture_typ 11)
  (setq depth depth_desk_2)
)



      (setq pt_1 (list base_x dist_margin_y 0))

      (setq pt_2 (list (+ base_x foot_width) dist_margin_y 0))

      (setq pt_3 (list (+ base_x foot_width) (- depth dist_margin_y) 0))

      (setq pt_4 (list base_x (- depth dist_margin_y) 0))

      (setq pts_foot (list pt_1 pt_2 pt_3 pt_4 pt_1))





      (command ".-layer" "_set" layer_geom2d "")
      (LWPoly pts_foot)

;generate toe line
      (setq pt_5 (list base_x (+ dist_margin_y foot_width) 0))
      (setq pt_6 (list (+ base_x foot_width) (+ dist_margin_y foot_width) 0))
      (setq pts_toe_south (list pt_5 pt_6))
      (LWPoly pts_toe_south)


      (setq pt_5 (list base_x (- depth dist_margin_y foot_width) 0))

      (setq pt_6 (list (+ base_x foot_width) (- depth dist_margin_y foot_width) 0))

      (setq pts_toe_north (list pt_5 pt_6))
      (LWPoly pts_toe_north)



    )


;first foot
    (geom_food_2d 0)

;second foot


    (setq temp (- width foot_width (* 2 dist_margin)))


    (geom_food_2d temp)




  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun geom_3d_table ()
    ;generate table foot and table plate as 3d polysolid
    (setq dist_margin 50)
    (setq setfood (ssadd))

;Points for table foot
    (setq 2d_foot_1 (list dist_margin dist_margin 0))
    (setq 2d_foot_2 (list (- width dist_margin dist_margin) dist_margin 0))
    (setq 2d_foot_3 (list (- width dist_margin dist_margin) (- depth dist_margin dist_margin) 0))
    (setq 2d_foot_4 (list dist_margin (- depth dist_margin dist_margin) 0))

;generate 2d_projection
    (rect2d 2d_foot_1 dist_margin dist_margin)
    (ssadd (entlast) setfood)

    (rect2d 2d_foot_2 dist_margin dist_margin)
    (ssadd (entlast) setfood)

    (rect2d 2d_foot_3 dist_margin dist_margin)
    (ssadd (entlast) setfood)

    (rect2d 2d_foot_4 dist_margin dist_margin)
    (ssadd (entlast) setfood)



  )


  (defun geom2d_sideboard ()
    ;generates opening arrows in workarea/ function area of furniture

    (setq
      x_arrow_line 150
      y_arrow_line -100
      width_upperline 0.7
      width_line (* width width_upperline)
      width_lowerline (- width width_line)
    )

;upper line
    (setq pt_1 (list x_arrow_line y_arrow_line 0))
    (setq pt_2 (list width_line y_arrow_line 0))

    (setq pts (list pt_1 pt_2))

    (LWPoly pts)


;lower line
    (setq pt_3x (- width x_arrow_line))
    (setq pt_3 (list width_lowerline (* 2 y_arrow_line) 0))
    (setq pt_4 (list pt_3x (* 2 y_arrow_line) 0))

    (setq pts (list pt_3 pt_4))
    (LWPoly pts)


;arrow
    (defun sideboard_arrow (insert_arrow_pt orientation)
      (if (= orientation 0)
        (setq
          arrow_width 70
          arrow_depth 50
        )
        (setq
          arrow_width -70
          arrow_depth -50
        )

      )


      (setq
        arrow_max_x (- (car insert_arrow_pt) arrow_width)
        arrow_pt1_y (+ (cadr insert_arrow_pt) (* 0.5 arrow_depth))
        arrow_pt2_y (- (cadr insert_arrow_pt) (* 0.5 arrow_depth))

        arrow_pt1 (list (car insert_arrow_pt) arrow_pt1_y 0)
        arrow_pt2 (list (car insert_arrow_pt) arrow_pt2_y 0)
        arrow_pt3 (list arrow_max_x (cadr insert_arrow_pt) 0)

        pts (list arrow_pt1 arrow_pt2 arrow_pt3 arrow_pt1)
      )

      (LWPoly pts)

      (print arrow_max_x)
    )
    ;(print arrow_max_x)


    (print "__________________________________________________")
    (sideboard_arrow pt_1 0)
    (sideboard_arrow pt_4 1)
    (print "__________________________________________________")


  )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun geom_2d_table_round ()



    (setq
      cirlce_center (list (/ width 2) 0 0)
      cirlce_radius (* 0.5 width)
    )



    (command ".-layer" "_set" layer_geom2d "")

    (entmake
      (list
        (cons 0 "CIRCLE")
        ;(cons 100 "AcDbSymbolTableRecord")
        (cons 100 "AcDbCircle")
        (cons 10 cirlce_center)
        (cons 40 cirlce_radius)


      )
    )

;add geom to entity list
    (setq entity (entlast))
    (ssadd entity set2d)

  )
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (defun geom_2d_table_trapez ()
    (setq 
      angle_table 30
      radians (* pi (/ angle_table 180.0))
    )
            
    (setq distance_margin (* depth (/ (sin radians)(cos radians))  ))
    
    
    (setq 
      pt_1 (list 0 0 0)
      
      pt_2 (list width 0 0)
      
      pt_3x (- width distance_margin)
      pt_3 (list pt_3x depth 0)
      
      pt_4 (list distance_margin depth 0)
  
      
      pts (list pt_1 pt_2 pt_3 pt_4 pt_1)
  )  
      
    (command ".-layer" "_set" layer_geom2d "")
    (LWPOLY pts)
      
    )    
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun geom_2d_desk_l ()
    
    (setq 
      radius 500
      width_desk_2 (getint "Enter Width 2 in [cm] ")
      width_desk_2 (* 10 width_desk_2)
      depth_desk_2 (getint "Enter depth 2 in [cm]")
      depth_desk_2 (* 10 depth_desk_2)
      
    )
    
      (setq 
        pt_1 (list 0 0 0)
        pt_2 (list width 0 0  )
        pt_3 (list width depth_desk_2 0)
        pt_4  (list (+ width_desk_2 radius) depth_desk_2 0)             
        
        pt_5 (list width_desk_2 (+ depth_desk_2 radius))
        pt_6 (list width_desk_2 depth 0)
        pt_7 (list 0 depth 0)
        
        ; pt_5 will be erased if i've understand the arc dxf group code ....
        pts (list pt_5 pt_6 pt_7 pt_1 pt_2 pt_3 pt_4 pt_5)
        
        arc_center (list (+ width_desk_2 radius) (+ depth_desk_2 radius))
        
        )
    
    (LWPOLY pts)
    
    ;ridiculous arc group code
;    (entmake
;      (list
;        (cons 0 "ARC")
;        (cons 100 "AcDbCircle")
;        (cons 100 "AcDbArc")
;        (cons 10 arc_center)
;        (cons 40 radius)
;        (cons 50 180)
;        (cons 51 90)
;      )
;    )
    
    
    ;add geom to entity list
    (setq entity (entlast))
    (ssadd entity set2d)
    
      )
    
    
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun block_build_insert_shelfs ()
    (setq furniture_blockname (strcat furniture_name "_" (rtos width_cm) "x" (rtos depth_cm) "_" (rtos furniture_height_cm)))

    (setq blockbasepoint (list 0 0 0))

  )

  (defun block_build_insert_tables ()
    (setq furniture_blockname (strcat furniture_name "_" (rtos width_cm) "x" (rtos depth_cm)))

    (setq blockbasepoint (list 0 0 0))

  )



  (defun misc ()
    (setq pt (list (/ width 2) (* depth 0.6) 0))
    (attdef furniture_name pt 1 80)

    (attdata)
  )
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;;;;;;;;;;;;END FUNCITON DEFINITION;;;;;;;;;;;;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;variables
  (setq set2d (ssadd))


  (setq wallthickness 20)



  ;layer name
  (setq layer_geom2d "AC-Geom2d")
  (setq layer_geom2d_wa "AC-Working_area")
  (setq layer_att "AC-Text")


  ;layer generation:
  (layermake layer_geom2d 1)          ;1=red 4=cyan
  (layermake layer_geom2d_wa 4)       ;1=red 4=cyan
  (layermake layer_att 255)           ;255 = white

	;get basic values and scale them to millimiter
  (setq width_cm (getreal "\Enter furniture WIDTH [cm]: "))
  (setq depth_cm (getreal "\nEnter furniture DEPTH [cm]: "))

  (setq width (* width_cm 10))
  (setq depth (* depth_cm 10))



  (setq furniture_typ (getint "1=filling cabinet 2=Shelf 3=sideboard 4=table, 5=desk, 6=filling cabinet UP, 11=L-Desk, 15=Trapez table"))
  ;7=sideboard UP,8 shelf UP, 9=Sofa, 10=Bed, 11=L-Desk, 12=Drawer Cabinet, 13=Pedestral Mobile, 14=locker, 15=Trapez table"

  (cond
    ((= furniture_typ 1)
        (setq furniture_name "Filling_Cabinet")

        (geom_2d_rectang)
        (shelf_heights)

        (geom_2d_workingarea)
        (geom_2d_swingdoor)


        (misc)
        (block_build_insert_shelfs)

    )

    ((= furniture_typ 2)
        (setq furniture_name "Shelf")
        (geom_2d_rectang)
        (shelf_heights)


        (geom_2d_workingarea)



        (misc)
        (block_build_insert_shelfs)

    )

    ((= furniture_typ 3)
        (setq furniture_name "Sideboard")
        (geom_2d_rectang)
        (shelf_heights)

        (geom_2d_workingarea)
        (geom2d_sideboard)


        (misc)
        (block_build_insert_shelfs)

    )
    ((= furniture_typ 4)
        (setq furniture_name "Table")



      (cond
        ((= depth_cm 0)
            (
              geom_2d_table_round
            )
        )
        (t
          (geom_2d_rectang)
          (geom_3d_table)
        )
      )

      (misc)
      (block_build_insert_tables)

    )
    ((= furniture_typ 5)

        (setq furniture_name "Desk")
        (geom_2d_rectang)

        (geom_2d_workingarea)

        (geom2d_desk_foot)


        (misc)
        (block_build_insert_tables)

    )

    ((= furniture_typ 6)
        (setq furniture_name "Filling Cabinet UP")
        (geom_2d_rectang)
        (shelf_heights)

        (geom_2d_workingarea)
        (geom_2d_swingdoor)


        (misc)
        (block_build_insert_shelfs)

    )
    
        ((= furniture_typ 15)
        (setq furniture_name "Table_Trapezoid")
        (geom_2d_table_trapez)
        



        (misc)
        (block_build_insert_tables)

    )
      ((= furniture_typ 11)

        (setq furniture_name "Desk")
        (geom_2d_desk_l)

        ;(geom_2d_workingarea)

        (geom2d_desk_foot)


        (misc)
        (block_build_insert_tables)

    )
  
  
  
  )








;; Comming Soon 
;;(entmake
;;      (list
;;            (cons 0 "BLOCK")
;;            (cons 100 "AcDbEntity")
;;            (cons 100 "AcDbBlockBegin")
;;            (cons 102  blockelements )
;;
;;            (cons 2 furniture_blockname)
;;            (cons 70 2)
;;            (cons 10 (list 0 0 0)) ;basepoint
;;
;;)
;)


 (command "._block" furniture_blockname blockbasepoint set2d "")

  (setq insert_pt (getpoint "Pick insert Point!"))


  

  (entmake
    (list
      (cons 0 "INSERT")
      (cons 100 "acdbentity")
      (cons 100 "acdbblockreference")
      (cons 2 furniture_blockname)
      (cons 10 insert_pt)

    )
  )

)
