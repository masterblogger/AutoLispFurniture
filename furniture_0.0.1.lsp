
;future features" Generates simply 2d & 3D rectangular furniture elments
;future features"Generates simply 2d & 3D rectangular furniture elments
 ;
 ; GNU GENERAL PUBLIC LICENSE Version 3
 ;
 ; Version 0.0.1
 ;
 ; by Joern Rettweiler, 2018 septembre 08
 ;
 ;
 ; Tested with brainless computation
 ;
 ; NO WARRANTY

 ; load this script with following command "appload"

 ;



(defun c:AC-Furniture ( / entities set2d furniture_dim)


;(setq entities (list))




	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;;;;;;;;;;;;BEGIN FUNCITON DEFINITION;;;;;;;;;;;;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(defun geom_2d_rectang ( / pt_1 pt_2 pt_3 pt_4)
;points will be generated anti clockwise

	(setq pt_0  '(0 0 0))

	(setq pt_1 (list width 0 0))
	(setq pt_2 (list  width depth 0 ))
	(setq pt_3 (list 0 depth 0))
	(setq pt_4  '(0 0 0))




(setq pts_2d_geom (list pt_0 pt_1 pt_2 pt_3 pt_4))


(command ".-layer" "_set" layer_geom2d "" )
(LWPoly pts_2d_geom)

	)


(defun geom_2d_workingarea ()
;generates working area default values references to ASR (Arbeitstaettenrichtlinie / Common German Labor Facility Guide )
	(setq depth_inv -1000)

	(setq pt_0 (list 0 0 0))
	(setq pt_1 (list 0 depth_inv 0))
	(setq pt_2 (list width depth_inv 0))
	(setq pt_3 (list width 0 0 ))




	(setq pts_workarea (list pt_0 pt_1 pt_2 pt_3))

	;generating geom and bind geom to layer

	(command ".-layer" "_set" layer_geom2d_wa "" )
	(LWPoly pts_workarea)

  
  (print "pts Workpsace")
  (print pts_workarea)
  
  
)

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun LWPoly (pts)
 (entmakex (append (list
	 												(cons 0 "LWPOLYLINE")
												 	(cons 100 "AcDbPolyline")
												 	(cons 100 "AcDbEntity")
                         	(cons 90 (length pts))
                         	(cons 70 0)) ;close 1 open 0
                   (mapcar (function (lambda (p) (cons 10 p))) pts)
          )

  )
  ;(setq entities (append list entlast entlast))
  ;(setq entities (append entities entlast))
  ;(setq entities (entlast))
  (setq entities (cons entities entlast ))
  (setq entity (entlast) )
  (print "_____________________")
  (print "_____________________")
  (print entity)
  (print "_____________________")
  (print "_____________________")
  
  (setq ent_2d (append ent_2d '(entity)))
  
  
  

  
  
  (ssadd  entity set2d)
  

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

(command ".-layer" "_set" layer_att "" )

 (entmakex (list (cons 0 "ATTDEF")
                 (cons 10   pt) ;point
								 (cons 11   pt) ;point
                 (cons 40  height) ;textheight
                 (cons 1   defval) ;default value
                 (cons 3 "none")
                 (cons 2   defval) ;tag value
								 (cons 70 flag)
								 (cons 72 1) ; horizontal justification
								 (cons 73 1) ;vertical justification
					 ))
          ;  (setq entities (append (entlast)))
 
 
 (setq entitiy_geom2d (entlast))
 
 
  (ssadd  entitiy_geom2d set2d)
 )

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun attdata ()
	(setq txtsize 180)
	(setq pt (list (/ width  2) (* depth 0.6) 0 ))
	(attdef furniture_name pt 2 txtsize)

	(setq furniture_dim (strcat (rtos width_cm) "x" (rtos depth_cm )))
	
	(print furniture_dim)
  

	(setq pt (list (/ width  2) (- (* depth 0.6) (* txtsize 1.5)) 0 ))
	
  
  (attdef furniture_dim pt 2 txtsize)
	

)

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (defun rect2d (pt width depth )
    
    
    (setq pt_input_x (nth 0 pt))
    (setq pt_input_y (nth 1 pt))
    
    
    (setq pt1 pt)
    (setq pt2 (list (+ pt_input_x width) pt_input_y 0 ))
    
    (setq pt3 (list (+ pt_input_x width) (+ pt_input_y depth) 0 ))
    (setq pt4 (list pt_input_x (+ pt_input_y depth) 0))
    
    (print "pt_inmput_x")
    (print pt_input_x)
    
    (print "pt4 XXX")
    (print pt4)
    
    (setq pts_foot (list pt1 pt2 pt3 pt4))
    (print "pts food")
    (print pts_foot)
    
    ;(LWPoly pts_foot)
  )
   
   
   (rect2d '(5 15 0) 5 10)
  
  
  
  (defun geom_3d_table ()
    
    
  )

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;;;;;;;;;;;;END FUNCITON DEFINITION;;;;;;;;;;;;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;variables
  (setq set2d (ssadd))
  (setq ent2d '())



  ;layer name
  (setq layer_geom2d "AC-Geom2d")
  (setq layer_geom2d_wa "AC-Working_area")
  (setq layer_att "AC-Text")
  
  
  ;layer generation:
  (layermake layer_geom2d 1) ;1=red 4=cyan
  (layermake layer_geom2d_wa 4) ;1=red 4=cyan
  (layermake layer_att 255) ;255 = white

	;get basic values and scale them to millimiter
	(setq width_cm (getreal "\Enter furniture WIDTH [cm]: "))
	(setq depth_cm (getreal "\nEnter furniture DEPTH [cm]: "))

	(setq width (* width_cm 10))
	(setq depth (* depth_cm 10))



	(setq furniture_typ (getint "1=filling cabinet 2=Shelf 3=sideboard 4=table, 5=desk, 6=filling cabinet UP"))
  ;7=sideboard UP,8 shelf UP, 9=Sofa, 10=Bed, 11=L-Desk, 12=Drawer Cabinet, 13=Pedestral Mobile, 14=locker, 15=Trapez table"
	
	(cond
		((= furniture_typ 1)
			(setq furniture_name "Filling_Cabinet")
      (geom_2d_rectang)

		)

		((= furniture_typ 2)
			(setq furniture_name "Shelf")
      (geom_2d_rectang)

		)

		((= furniture_typ 3)
			(setq furniture_name "Sideboard")
      (geom_2d_rectang)

		)
		((= furniture_typ 4)
			(setq furniture_name "Table")
      (geom_2d_rectang)

		)
		((= furniture_typ 5)

			(setq furniture_name "Desk")
			(geom_2d_rectang)
			(geom_2d_workingarea)

		)

		((= furniture_typ 6)
      (setq furniture_name "Filling Cabinet UP")
			(geom_2d_rectang)

		)
)

(setq pt (list (/ width  2) (* depth 0.6) 0 ))
(attdef furniture_name pt 1 180)

(attdata)
; 	;



(setq furniture_dim (strcat furniture_name (rtos width_cm) "x" (rtos depth_cm )))
(setq blockbasepoint (list 0 0 0))

(setq loopbreaker 0)
(setq leng_ent (length '(entities)))

(while  (<= loopbreaker leng_ent)
  (setq loopbreaker (+ loopbreaker  1))
  (print "loop")
  (print loopbreaker)
)







; Comming Soon 
;(entmake
;      (list
;            (cons 0 "BLOCK")
;            (cons 100 "AcDbEntity")
;            (cons 102            entities )
;            (cons 100 "AcDbBlockBegin")
;            (cons 2 "blockname")
;            (cons 70 2)
;            (cons 10 (list 0 0 0)) ;basepoint
;
;)
;)

(command "._block" furniture_dim blockbasepoint set2d  "" )

(setq insert_pt (getpoint "Pick insert Point!"))


  (command "._insert" furniture_dim  "_Scale" 1 insert_pt 0 "")
  ;
  ;(entmake
  ;    (list
  ;          (cons 100 "acdbentity")
  ;          (cons 100 "acdbblockreference")
  ;          (cons 2 furniture_dim)
  ;          (cons 10 blockbasepoint)
  ;          
  ;    )
  ;)

)
