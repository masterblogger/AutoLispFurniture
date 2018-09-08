
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



(defun c:AC-Furniture ()







	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;;;;;;;;;;;;BEGIN FUNCITON DEFINITION;;;;;;;;;;;;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(defun geom_2d_rectang ()
;points will be generated anti clockwise

	(setq pt_0  '(0 0 0))

	(setq pt_1 (list width 0 0))
	(setq pt_2 (list  width depth 0 ))
	(setq pt_3 (list 0 depth 0))
	(setq pt_4  '(0 0 0))




(setq pts_2d_geom (list pt_0 pt_1 pt_2 pt_3 pt_4))

(layermake "AC-Geom2d" 1) ;1=red 4=cyan
(command ".-layer" "_set" "AC-Geom2d" "" )
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
	(layermake "AC-Working_area" 4) ;1=red 4=cyan
	(command ".-layer" "_set" "AC-Working_area" "" )
	(LWPoly pts_workarea)
	;(print (length pts_workarea))
)

(defun LWPoly (pts)
 (entmakex (append (list
	 												(cons 0 "LWPOLYLINE")
												 	(cons 100 "AcDbPolyline")
												 	(cons 100 "AcDbEntity")
                         	(cons 90 (length pts))
                         	(cons 70 0)) ;close 1 open 0
                   (mapcar (function (lambda (p) (cons 10 p))) pts))))


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


(defun attdef (defval pt flag height)


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
					 )))


(defun attdata ()
	(setq txtsize 180)
	(setq pt (list (/ width  2) (* depth 0.6) 0 ))
	(attdef furniture_name pt 1 txtsize)

	(setq furniture_dim (strcat (rtos width_cm) " x " (rtos depth_cm )))
	(print  "this is furniture dim")
	(print furniture_dim)



	(setq pt (list (/ width  2) (- (* depth 0.6) (* txtsize 1.5)) 0 ))
	(attdef furniture_dim pt 1 txtsize)
	(print "blub")

)

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;;;;;;;;;;;;END FUNCITON DEFINITION;;;;;;;;;;;;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	;get basic values and scale them to millimiter
	(setq width_cm (getreal "\Enter furniture WIDTH [cm]: "))
	(setq depth_cm (getreal "\nEnter furniture DEPTH [cm]: "))

	(setq width (* width_cm 10))
	(setq depth (* depth_cm 10))



	(setq furniture_typ (getint "1=filling cabinet 2=Shelf 3=sideboard 4=table, 5=desk, 6=filling cabinet UP, 7=sideboard UP,8 shelf UP, 9=Sofa, 10=Bed, 11=L-Desk, 12=Drawer Cabinet, 13=Pedestral Mobile, 14=locker, 15=Trapez table"))

		;(geom_2d_rectang)
		;(geom_2d_workingarea)
	;
	(cond
		((= furniture_typ 1)
			(geom_2d_rectang)

		)

		((= furniture_typ 2)
			(geom_2d_rectang)

		)

		((= furniture_typ 3)
			(geom_2d_rectang)

		)
		((= furniture_typ 4)
			(geom_2d_rectang)

		)
		((= furniture_typ 5)

			(setq furniture_name "Desk")
			(geom_2d_rectang)
			(geom_2d_workingarea)

		)

		((= furniture_typ 6)
			(geom_2d_rectang)

		)
)

(setq pt (list (/ width  2) (* depth 0.6) 0 ))
(attdef furniture_name pt 1 180)

(attdata)
; 	;


)
