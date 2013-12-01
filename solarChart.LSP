(defun c:solarhr ()
;; LISP CODE PLOTTING SOLAR CHART (hour lines portion)
  ;; 
;; TO BE RUN IN AUTOCAD GRAPHIC MODE
;; with acad file SolarCalculator.dwg
;; DOES NOT DRAW THE BASIC CONCENTRIC CIRCLES

  (graphscr)
  (SETVAR "OSMODE" 0)
  (setq someText (CAL "asin(1)"))
  ;; Convert value in radians to degrees
  (defun R2D (radVal)
    (* 180.0 (/ radVal pi))

  )
  ;; Convert value in degrees to radianss
  (defun D2R (degVal)
    (* pi (/ degVal 180.0))
  )
  ;; asine function
  (defun asine (angVal)
    (distof
      (rtos (cal (strcat "asin(" (rtos angVal 2 3) ")")) 2 3)
    )
    ;;OUTPUT IN DEGREES
  )
  ;; acos function
  (defun acosine (angVal)
    (distof
      (rtos (cal (strcat "acos(" (rtos angVal 2 3) ")")) 2 3)
    )
    ;;OUTPUT IN DEGREES
  )
  ;; STARTING INPUT MODULE
 ;(setq NDY (getreal "Please Enter the New Day Number: "))
 (setq latval (D2R (getreal "Please Enter the Latitude value: ")))
  ;;(setq	RADIUSVAL
					;(getreal "Please Enter the Radius of solar chart: "))

  ;(setq NDY 200)
  ;(setq latval (D2R 28))
  (setq RADIUSVAL 50)
  ;(setq PT (getpoint "\nClick on a point to begin text: "))
  ;(setq PTT PT)
  ;;(setq HT (getdist "\nGive height of text: "))
  (setq HT 2.5)
  ;(SETQ dumb (ASINE -0.133541))
			; Main calculation
  ;;loading initial values
  (setq nexPt (list 0 0 0))
  (setq counter 1
	header 0)
  (setq timeval 5.00)
  (SETQ PT1 (list 0 0))
  (SETQ PT2 (list 0 -50))
(command "_pline"  PT1 PT2 "")
  (setvar "CMDECHO" 0)

    ;(setq PT (list (+ (car PT) (* (* 50 HT) mloop)) (cadr PT)))
    ;(setq PTT (list (+ (car PTT) (* (* 50 HT) mloop)) (cadr PT)))
  
  (setq timeval 5)
  (while (<= timeval 19)
    
    (setq NDY 173)
  (while (<= NDY 356)
    (setq inipt nexPt)
    (setq hraval (D2R (* 15 (- timeval 12))))
					;done in radians
					;latval is already in radians AND IS FIXED FOR WHOLE CHART
    (setq decval (D2R (* 23.450 (sin (D2R (* 0.986 (+ 284 NDY)))))))
					;decval is already in radians
					;get the altitude IN degrees
    ;;COMPUTING altval
    (setq altval1 (+ (* (sin decval) (sin latval))
		     (* (cos decval) (* (cos latval) (cos hraval)))
		  )
    )
    (setq altval (asine altval1))
					;(princ altval)
    ;;altval is now in degrees
					;convert degrees to equivalent length
    (setq ALTlen (/ (* RADIUSVAL (cos (D2R altval)))
		    (+ 1 (sin (D2R altval)))
		 )
    )
    		;ALTLEN IS NOW IN METER
    ;get the azimuth IN degrees
    ;;COMPUTING azival
    (setq azival2 (/ (- (* (cos latval) (sin decval)) (* (cos decval) (* (sin latval) (cos hraval)))) (cos (D2R altval))))
    (setq azival2 (acosine azival2))
    (setq azival1 (/ (* (cos decval) (sin hraval)) (cos (D2R altval))))
    (setq azival (acosine azival1))
;    (setq azival (- azival 90))
    ;;AZIVAL IS NOW IN DEGREES
    ;check for noon
    	(if (> timeval 12)
	  (setq azival2 (- 360 azival2))
	  (setq azival2 azival2)
	  )
    ;convert the points to string
    (setq ALTlenX (rtos ALTlen 2 3))
    (setq azivalX (rtos (- azival2 90) 2 3))
    (setq nexPt (strcat ALTlenX "<" azivalX))
    ;(COMMAND "_point" nexPt)
;    (command "_pline" iniPt nexPt "")
        (command "_pline"  iniPt nexPt "")
    ;(setq iniPt nexPt)
    	(setq nowText0 (rtos (- 270 azival) 2 3)
	      nowText0 (rtos (+ 90 azival))
	      nowText1 (rtos altval 2 3)
	      nowText2 (rtos timeval 2 3)
	      ;nowText2 (strcat nowText2 "_" (rtos azival2))
	      )

    (setq counter (+ counter 1))
    ;(setq timeval (+ 0.25 timeval))
    (setq PT (list (car PT) (- (cadr PT) (* 2 HT))))
    ;store variables for drawing table
		(setq NDY (+ 3 NDY))
    )
  
    			(setq timeval (+ 1 timeval))
    )
  (SETVAR "OSMODE" 14847)
  (setvar "CMDECHO" 1)
  (princ)
  

)