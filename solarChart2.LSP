(defun c:solar ()
;; LISP CODE PLOTTING SOLAR CHART
;; TO BE RUN IN AUTOCAD GRAPHIC MODE
;; with acad file SolarCalculator#.dwg where # is version number
;; DOES NOT DRAW THE BASIC CONCENTRIC CIRCLES

  (graphscr)
  (SETVAR "OSMODE" 0)
  ;(setvar "ORTHOMODE" 0)
  ;(setq someText (CAL "asin(1)"))
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
 (setq NDY (getreal "Please Enter the New Day Number: "))
 (setq latval (D2R (getreal "Please Enter the Latitude value: ")))
  ;;(setq	RADIUSVAL
					;(getreal "Please Enter the Radius of solar chart: "))

  ;(setq NDY 200)
  ;(setq latval (D2R 28))
  (setq RADIUSVAL 50)
  (setq PT (getpoint "\nClick on a point to begin text: "))
  (setq PTT PT)
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
  
  (while (< timeval 19)
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
    (command "_text" "J" "C" PT HT "" nowText2)
    (setq PT1 (list (+ (car PT) (* HT 10)) (cadr PT))
	  PT2 (list (+ (car PT) (* HT 20)) (cadr PT))
	  )
    (setq ln1 (list (- (car PT) (* HT 5)) (- (cadr PT) (/ HT 2)))
	  ln2 (list (+ (car PT2) (* HT 5)) (- (cadr PT2) (/ HT 2)))
	  )
    (command "_line" ln1 ln2 "")
    (command "_text" "j" "c" PT1 HT "" nowText1)
    (command "_text" "j" "c" PT2 HT "" nowText0)
    	(while (= header 0)
		;drawing header
	  	(command "_text" "J" "C" (list (+ (car PT) (* HT 0)) (+ (* 2 HT) (cadr PT))) HT "" "Time")
	      	(command "_text" "j" "c" (list (+ (car PT) (* HT 10)) (+ (* 2 HT) (cadr PT))) HT "" "Altitude")
    		(command "_text" "j" "c" (list (+ (car PT) (* HT 20)) (+ (* 2 HT) (cadr PT))) HT "" "Azimuth")
	  (setq header 1)
	  )
    (setq counter (+ counter 1))
    (setq timeval (+ 0.25 timeval))
    (setq PT (list (car PT) (- (cadr PT) (* 2 HT))))
    ;store variables for drawing table
  )
  ;draw the table header and vertical lines
  (setq tl1 (list (- (car PTT) (* HT 5)) (+ (cadr PTT) (* HT 1.5))))
  (setq PTT (list (+ (car PTT) (* HT 25)) (+ (cadr PTT) (* HT 1.5))))
  (command "_line" PTT tl1 "")
  (setq tl1v (list (- (car ln1) (* HT 0)) (+ (cadr PTT) (* HT 2)))
  	PTTv (list (+ (car ln2) (* HT 0)) (+ (cadr PTT) (* HT 2)))
	)
  (command "_line" PTTv ln2 "")
  (command "_line" tl1v ln1 "")
  (setq vl1 (list (- (car PTT) (* HT 10)) (+ (cadr PTT) (* HT 2)))
	vl2 (list (- (car PTT) (* HT 10)) (+ (cadr ln2) (* HT 0)))
	vl3 (list (- (car PTT) (* HT 20)) (+ (cadr PTT) (* HT 2)))
	vl4 (list (- (car PTT) (* HT 20)) (+ (cadr ln2) (* HT 0)))
	)
  (command "_line" PTTv tl1v "")
  (command "_line" vl1 vl2 "")
  (command "_line" vl3 vl4 "")
  ;(command "_line" vl1 vl3 "")
  (SETVAR "OSMODE" 14847)
  (setvar "CMDECHO" 1)
  (setvar "ORTHOMODE" 1)
  (princ)

)