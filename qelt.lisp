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

(defun c (idx)
  (aref *palette* (mod idx (array-dimension *palette* 0))))

(defun p (idx)
  (typecase idx
    (number (make-pen :fill (c idx)))
    (t (make-pen :fill (c (position idx #(:blue :yellow :green :red)))))))

(defun s (idx)
  (case idx
    ((0 :cross)
     (with-current-matrix
       (translate 50 50)
       (polygon -26 -6.5 -6.5 -6.5 -6.5 -26 6.5 -26 6.5 -6.5 26
		-6.5 26 6.5 6.5 6.5 6.5 26 -6.5 26 -6.5 6.5 -26 6.5)))
    ((1 :square)
     (with-current-matrix
       (rotate 45 50 50)
       (rect 22.5 22.5 55 55)))
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

(defmethod ring-animate ((instance ring))
  (funcall (slot-value instance 'animator)))

(defun draw-with-visuals (angle left fdraw)
  (with-current-matrix
    (with-pen (make-pen :stroke (c 4) :weight -5)
      (translate left 300)
      (with-pen (make-pen :fill (c 4))
	(circle 0 0 10))
      (scale 0.8)
      (rotate (+ 45 angle))
      (dotimes (i 4)
	(with-current-matrix
	  (translate -4 -4)
	  (funcall fdraw i))
	(rotate 90)))))

(defmethod ring-draw ((instance ring) (visuals (eql :colors)))
  (draw-with-visuals (slot-value instance 'angle) 220
		     (lambda (i) (with-pen (p i) (s :circle)))))

(defmethod ring-draw ((instance ring) (visuals (eql :shapes)))
  (draw-with-visuals (slot-value instance 'angle) 580
		     (lambda (i) (s i))))

(defmethod ring-state ((instance ring) (visuals (eql :colors)))
  (aref #(:red :green :yellow :blue) (slot-value instance 'state)))


(defmethod ring-state ((instance ring) (visuals (eql :shapes)))
  (aref #(:square :cross :circle :triangle) (slot-value instance 'state)))

(defmethod ring-rotate ((instance ring) direction)
  (with-slots (state angle animator) instance
    (let ((prev-state state))
      (setf state (mod (+ state direction) 4)
	    animator (make-animator #'ease:out-circ
				    0 90 20
				    (lambda (x) (setf angle (+ (* 90 prev-state) (* direction x))))
				    (lambda () (setf animator (lambda ()))))))))

;;; BELT

(defclass belt ()
  ((items :initform (make-array 8 :initial-contents
				'((:green :cross)
				  (:blue :square)
				  (:red :triangle)
				  (:yellow :square)
				  (:blue :circle)
				  (:green :triangle)
				  (:red :circle)
				  (:blue :square))))
   (translation :initform 0)
   (animator :initform (lambda ()))))

(defmethod belt-move ((instance belt))
  (with-slots (items animator translation) instance
    (setf animator (make-animator #'ease:out-circ
				  0 100 10
				  (lambda (x) (setf translation x))
				  (lambda () (setf animator (lambda ())))))
    (dotimes (i (- (array-dimension items 0) 1))
      (setf (aref items i) (aref items (+ i 1))))
    (setf (aref items (- (array-dimension items 0) 1)) (list (random 4) (random 4)))))

(defmethod belt-animate ((instance belt))
  (funcall (slot-value instance 'animator)))


(defmethod draw ((instance belt) &key &allow-other-keys)
  (with-slots (items translation) instance
    (with-current-matrix
      (translate 0 (- translation))
      (loop for (color shape) across items
	 if (and color shape)
	 do (progn
	      (with-current-matrix
		(translate 400 0)
		(rotate 225)
		(translate -50 -50)
		(with-pen (p color)
		  (s shape)))
	      (translate 0 100))))))

;;; GAME

(defsketch qelt
    ((title "QELT") (width 800) (height 600)
     (color-ring (make-instance 'ring))
     (shape-ring (make-instance 'ring))
     (belt (make-instance 'belt))
     (frames 0))
  (incf frames)
  (when (zerop (mod frames 60))
    (belt-move belt))
  (background +white+)
  (with-font (make-font :face (load-resource "~/HKGrotesk-Regular.otf")
			:size 30
			:color (c 4))
    (text title 700 540)
    (text (format nil "SCORE~6d" 1) 30 540))
  (with-font (make-font :face (load-resource "~/HKGrotesk-Regular.otf")
			:size 16
			:color (c 4))
    (text "A - D" 202 400)
    (text "LEFT - RIGHT" 535 400))
  (ring-animate color-ring)
  (ring-animate shape-ring)
  (belt-animate belt)
  (ring-draw color-ring :colors)
  (ring-draw shape-ring :shapes)
  (draw-scanner (mod (truncate frames 2) 100))
  (draw belt))

(defmethod kit.sdl2:keyboard-event ((instance qelt) state timestamp repeat-p keysym)
  (declare (ignorable timestamp repeat-p keysym))
  (with-slots (color-ring shape-ring belt) instance
      (when (eql state :keydown)
	(cond ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-a) (ring-rotate color-ring -1))
	      ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-d) (ring-rotate color-ring 1))
	      ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-left) (ring-rotate shape-ring -1))
	      ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-right) (ring-rotate shape-ring 1))))))


(defun draw-scanner (height)
  ;; (with-pen (make-pen :stroke (c 4) :weight 2)
  ;;   (line 320 250 480 250)
  ;;   (line 320 350 480 350))
  (with-pen (make-pen :stroke (gray 0.85 0.5))
      (rect 320 250 160 100))
  (with-pen (make-pen :fill (gray 0.85 0.5))
    (rect 320 (- 350 height) 160 height)))

(defun qelt ()
  (make-instance 'qelt))
