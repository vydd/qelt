;;;; qelt.lisp

(in-package #:qelt)

;;; UTILS

(defparameter *palette*
  ;; https://coolors.co/app/5bc0eb-fde74c-9bc53d-c3423f-404e4d
  `#(,(hex-to-color "#5bc0eb")
     ,(hex-to-color "#f4e74c")
     ,(hex-to-color "#9bc53d")
     ,(hex-to-color "#c3423f")
     ,(hex-to-color "#404e4d")))

(defvar *font-face*)

(defun color (idx)
  (aref *palette* (mod idx (array-dimension *palette* 0))))

(defun pen (idx)
  (typecase idx
    (number (make-pen :fill (color idx)))
    (t (make-pen :fill (color (position idx #(:blue :yellow :green :red)))))))

(defun shape (idx)
  (case idx
    ((0 :cross)
     (with-current-matrix
       (polygon 24 43.5 43.5 43.5 43.5 24 56.5 24 56.5 43.5 76
		43.5 76 56.5 56.5 56.5 56.5 76 43.5 76 43.5 56.5 24 56.5)))
    ((1 :square)
     (with-current-matrix
       (rotate 45 50 50)
       (rect 24 24 53.5 53.5)))
    ((2 :triangle)
     (with-current-matrix
       (translate -5 -5)
       (ngon 3 45 45 41 41 45)))
    ((3 :circle)
     (circle 50 50 28))))

(defun make-animator (easing from to steps setter destroyer)
  (let ((step 0))
    (lambda ()
      (funcall setter (alexandria:lerp
		       (funcall easing (/ step steps)) from to))
      (incf step)
      (when (> step steps)
	(funcall setter to)
	(funcall destroyer)))))

;;; RING

(defclass ring ()
  ((state :initform 0)
   (angle :initform 0)
   (animator :initform (lambda ()))))

(defclass color-ring (ring) ())
(defclass shape-ring (ring) ())

(defmethod animate ((instance ring))
  (funcall (slot-value instance 'animator)))

(defun draw-ring-with-visuals (angle left fdraw)
  (with-current-matrix
    (with-pen (make-pen :stroke (color 4) :weight -5)
      (translate left 300)
      (with-pen (make-pen :fill (color 4))
	(circle 0 0 10))
      (scale 0.8)
      (rotate (+ 45 angle))
      (dotimes (i 4)
	(with-current-matrix
	  (translate -4 -4)
	  (funcall fdraw i))
	(rotate 90)))))

(defmethod draw ((instance color-ring) &key &allow-other-keys)
  (draw-ring-with-visuals
   (slot-value instance 'angle) 220
   (lambda (i) (with-pen (pen i) (shape :circle)))))

(defmethod draw ((instance shape-ring) &key &allow-other-keys)
  (draw-ring-with-visuals
   (slot-value instance 'angle) 580
   (lambda (i)
     (when (= i 2)
       (translate 6 6))
     (shape i))))

(defmethod state ((instance color-ring))
  (aref #(:red :green :yellow :blue) (slot-value instance 'state)))


(defmethod state ((instance shape-ring))
  (aref #(:square :cross :circle :triangle) (slot-value instance 'state)))

(defmethod move ((instance ring) direction)
  (with-slots (state angle animator) instance
    (let ((prev-state state))
      (setf state (mod (+ state direction) 4)
	    animator (make-animator #'ease:out-circ
				    0 90 20
				    (lambda (x) (setf angle (+ (* 90 prev-state) (* direction x))))
				    (lambda () (setf animator (lambda ()))))))))

;;; BELT

(defclass belt ()
  ((items :initform (make-array 8 :initial-element nil))
   (translation :initform 0)
   (animator :initform (lambda ()))))

(let ((colors #(:red :blue :green :yellow))
      (shapes #(:circle :square :triangle :cross)))
  (defun generate-item (invalid)
    (loop for item = invalid
       then (list (aref colors (random 4))
		  (aref shapes (random 4)))
       if (not (and (eql (first item) (first invalid))
		    (eql (second item) (second invalid))))
       return item)))

(defmethod move ((instance belt) invalid-next-item)
  (with-slots (items animator translation) instance
    (setf animator (make-animator #'ease:out-circ
				  0 100 12
				  (lambda (x) (setf translation x))
				  (lambda () (setf animator (lambda ())))))
    (dotimes (i (- (array-dimension items 0) 1))
      (setf (aref items i) (aref items (+ i 1))))
    (setf (aref items (- (array-dimension items 0) 1)) (generate-item invalid-next-item))))

(defmethod animate ((instance belt))
  (funcall (slot-value instance 'animator)))

(defmethod draw ((instance belt) &key &allow-other-keys)
  (with-slots (items translation) instance
    (with-current-matrix
      (translate 0 (- translation))
      (loop for (color shape) across items
	 do (progn
	      (with-current-matrix
		(translate 400 0)
		(rotate 225)
		(translate -50 -50)
		(when (and color shape)
		  (with-pen (pen color)
		    (shape shape))))
	      (translate 0 100))))))

(defmethod reset ((instance belt))
  (setf (slot-value instance 'items) (make-array 8 :initial-element nil)))

;;; SCANNER

(defclass scanner ()
  ((fill :initform 0)
   (belt :initarg :belt)
   (game)
   (color-ring :initarg :color-ring)
   (shape-ring :initarg :shape-ring)))

(defmethod draw ((instance scanner) &key &allow-other-keys)
  (with-slots (fill) instance
    (with-pen (make-pen :stroke (gray 0.92))
      (rect 320 250 160 100))
    (with-pen (make-pen :fill (gray 0.92))
      (rect 320 (- 350 fill) 160 fill))))

(defmethod reset ((instance scanner))
  (setf (slot-value instance 'fill) 0))

(defmethod move ((instance scanner) fill)
  (incf (slot-value instance 'fill) fill)
  (<= (slot-value instance 'fill) 100))

(defmethod scan ((instance scanner))
  (aref (slot-value (slot-value instance 'belt) 'items) 4))

(defmethod selection ((instance scanner))
  (list (state (slot-value instance 'color-ring))
	(state (slot-value instance 'shape-ring))))

;;; GAME

(defclass game ()
  ((belt :initarg :belt)
   (scanner :initarg :scanner)
   (level :initform 0)
   (score :initform 0)
   (state :initform :initial) ;; :initial, :starting, :running, :lost
   (time :initform 0)))

(defmethod tick ((instance game))
  (with-slots (belt scanner level score running time) instance
    (incf time)
    (when (zerop (mod time (max 1 (- 6 (truncate level 2)))))
      (unless (move scanner 1)
	(end instance)))
    (when (zerop (mod time (max 1 (- 60 (* level 5)))))
      (move belt (selection scanner)))))

(defmethod action ((instance game))
  (with-slots (scanner score state time belt level) instance
    (setf state :running)
    (let ((scan (scan scanner))
	  (select (selection scanner)))
      (if (and (eql (first scan) (first select))
	       (eql (second scan) (second select)))
	  (progn
	    (incf score)
	    (when (member score '(5 10 20 50 100 250 500 1000 5000 10000))
	      (incf level))
	    (move belt (selection scanner))
	    (setf time 0)
	    (reset scanner))
	  (end instance)))))

(defmethod start ((instance game))
  (with-slots (state score) instance
    (setf state :starting
	  score 0)))

(defmethod end ((instance game))
  (with-slots (belt scanner level state time) instance
      (setf state :lost
	    time 0
	    level 0)
      (reset belt)
      (reset scanner)))

(defsketch qelt
    ((title "QELT") (width 800) (height 600)
     (color-ring (make-instance 'color-ring))
     (shape-ring (make-instance 'shape-ring))
     (belt (make-instance 'belt))
     (scanner (make-instance 'scanner :belt belt :color-ring color-ring :shape-ring shape-ring))
     (game (make-instance 'game :belt belt :scanner scanner)))
  (background +white+)
  (case (slot-value game 'state)
    (:initial
     (with-font (make-font :face *font-face* :size 80 :color (color 4))
       (text title 300 200))
     (with-font (make-font :face *font-face* :size 40 :color (color 4))
       (text "PRESS SPACE TO START" 184 290))
     (with-font (make-font :face *font-face* :size 20 :color (color 4))
       (text "(c) Danilo Vidovic (vydd)" 286 440)
       (text "made during Spring Lisp Game Jam 2016" 218 460)))
    ((:starting :running)
     (when (eql (slot-value game 'state) :starting)
       (with-font (make-font :face *font-face* :size 16 :color (color 4))
	 (text "PRESS SPACE WHEN YOU MATCH SHAPE AND COLOR" 208 143)))
     (with-font (make-font :face *font-face* :size 30 :color (color 4))
       (text title 700 540)
       (text (format nil "SCORE~6d" (slot-value game 'score)) 30 540))
     (with-font (make-font :face *font-face* :size 16 :color (color 4))
       (text "D - F" 203 400)
       (text "J - K" 564 400))
     (tick game)
     (animate color-ring)
     (animate shape-ring)
     (animate belt)
     (draw color-ring)
     (draw shape-ring)
     (draw scanner)
     (draw belt))
    (:lost
     (with-font (make-font :face *font-face* :size 80 :color (color 4))
       (text (format nil "SCORE~8d" (slot-value game 'score)) 172 200))
     (with-font (make-font :face *font-face* :size 40 :color (color 4))
       (text "PRESS R TO RESTART" 205 290)))))

(defmethod setup ((instance qelt) &key &allow-other-keys)
  (setf *font-face* (load-resource (sketch::relative-path "HKGrotesk-Regular.otf" 'qelt))))

(defmethod kit.sdl2:keyboard-event ((instance qelt) state timestamp repeat-p keysym)
  (declare (ignorable timestamp repeat-p keysym))
  (with-slots (color-ring shape-ring scanner game) instance
    (when (eql state :keydown)
      (case (slot-value game 'state)
	(:initial
	 (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-space)
	   (start game)))
	((:starting :running)
	 (cond ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-d) (move color-ring -1))
	       ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-f) (move color-ring 1))
	       ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-j) (move shape-ring -1))
	       ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-k) (move shape-ring 1))
	       ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-space) (action game))))
	(:lost
	 (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-r)
	   (start game)))))))

(defun qelt ()
  (make-instance 'qelt))
