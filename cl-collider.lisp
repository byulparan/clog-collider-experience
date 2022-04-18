(ql:quickload '(:cl-collider :sc-extensions :clog))

(defpackage #:clog-collider
  (:shadowing-import-from :clog :pattern)
  (:use #:cl #:sc #:sc-extensions #:clog)
  (:export #:main))

(in-package :clog-collider)

(unless (typep *s* 'sc::external-server)
  (setf *s* (make-external-server "localhost" :port 57140))
  (server-boot *s*))


(defvar *theme-skin* (rgb 50 100 160))

(defvar *running-p* nil)

(defvar *synth-volume* .0)

(defvar *transport-cell* nil)
(defvar *sequence-obj* nil)
(defvar *sequence-data* (loop repeat 5 collect (make-list 8)))
(defvar *buffers* (loop repeat 5 collect (buffer-alloc 8)))

(defvar *vu-meter* nil)
(defvar *bpm* 60.0)

(defun container (body)
  (let* ((div (create-div body)))
    (setf (width div) "100%"
	  (height div) 800)
    (setf (display div) :flex
	  (flex-direction div) :row
	  (flex-wrap div) :wrap)
    div))

(defun controller (parent title)
  (let* ((section (create-div parent)))
    (setf (display section) :flex
	  (flex-direction section) :column
	  (justify-content section) :space-around)
    (set-border section "1px" :solid *theme-skin*)
    (set-margin section "10px" "10px" "10px" "10px")
    (setf (width section) 400)
    (let* ((title-div (create-div section))
	   (content-div (create-div section)))
      (setf (width title-div) "100%"
	    (height title-div) "15%")
      (setf (width content-div) "100%"
	    (height content-div) "85%")
      (let* ((title (create-child title-div (format nil "<h3> ~a </h3>" title))))
	(setf (color title) *theme-skin*)
	(set-margin title "10px" 0 0 "10px"))
      content-div)))


(defun display-vu (vu)
  ;;(set-margin vu 0 0 0 "300px")
  (setf (width vu) 40 (height vu) 140)
  (set-border vu "1px" :solid *theme-skin*)
  (setf (border-radius vu) "4px")
  (setf (background-color vu) "white")
  (setf (display vu) :flex
	(flex-direction vu) :row
	(justify-content vu) :space-around
	(align-items vu) :flex-end)
  (let* ((vu1 (create-div vu))
	 (vu2 (create-div vu)))
    (setf *vu-meter* (list vu1 vu2))
    (dolist (v (list vu1 vu2))
      (setf (width v) 15 (height v) 0)
      (setf (background-color v) *theme-skin*))))

(defun system-controller (parent)
  (let* ((div (controller parent "System")))
    (setf (display div) :flex)
    (let* ((left (create-div div))
	   (right (create-div div)))
      (setf (display left) :flex
	    (width left) "80%"
	    (flex-direction left) :column
	    (flex-wrap left) :wrap)
      (let* ((toggle-start (create-button left :class "button" :content "start / stop"))
	     (clear-button (create-button left :class "button" :content "clear sequence"))
	     (label (create-label left :content (format nil "bpm: ~a" *bpm*)))
	     (range (create-child left (format nil "<input type='range' min='40' max='160' step='1' value='~a' </input>" *bpm*)
				  :clog-type 'clog-form-element))
	     (vu (create-button right)))
	(setf (width toggle-start) 100
	      (height toggle-start) 40)
	(setf (width clear-button) 100
	      (height clear-button) 20)
	(setf (width range) 100)
	(set-margin label "20px" 0 0 "10px")
	(set-margin range 0 0 0 "10px")
	(setf (color label) *theme-skin*)
	(display-vu vu)
	(set-on-mouse-down toggle-start (lambda (obj opt)
					  (if *running-p* (progn (proxy :metro nil)
								 (ctrl :pad :gain .0 :fade 4.0))
					    (progn
					      (metro *bpm*)
					      (ctrl :pad :gain (* .8 *synth-volume*) :fade 4.0)
					      (add-reply-responder
					       "/input-meter"
					       (lambda (id v v1-peak v1-rms v2-peak v2-rms)
						 (declare (ignorable id v v1-peak v1-rms v2-peak v2-rms))
						 (setf (height (first *vu-meter*)) (* v1-peak 120.0)
						       (height (second *vu-meter*)) (* v2-peak 120.0))))
					      (proxy :vumeter
						(send-peak-rms.ar (in.ar 0 2) 20.0 3 "/input-meter")
						:to 0
						:pos :tail)))
					  (setf *running-p* (not *running-p*))))
	(set-on-mouse-down clear-button (lambda (obj opt)
					  (loop for b in *buffers* do (buffer-zero b))
					  (loop for obj in *sequence-obj*
						do (setf (hiddenp obj) t))
					  (setf *sequence-data* (loop repeat 5 collect (make-list 8)))))
	(set-on-event range "input" (lambda (obj)
				      (let* ((v (parse-float:parse-float (value obj))))
					(when (is-playing-p :metro)
					  (ctrl :metro :bpm v))
					(setf *bpm* v)
					(setf (text label) (format nil "bpm: ~a" v)))))))))

;; ================================================================================
;; synth
;; 

(defun synth-controller (parent)
  (let* ((div (controller parent "Synth"))
	 (volume (create-div div))
	 (buttons (create-div div)))
    (let* ((label (create-label volume :content "pad volume"))
	   (range (create-child volume "<input type='range' min='0.0' max='1.0' step='0.001' </input>"
				:clog-type 'clog-form-element)))
      (setf (color label) *theme-skin*)
      (set-margin label 0 0 0 "10px")
      (setf (background-color range) :red)
      (setf (value range) *synth-volume*)
      (set-on-event range "input" (lambda (obj)
				    (let* ((v (parse-float:parse-float (value obj))))
				      (ctrl :pad :gain (* .8 v))
				      (setf *synth-volume* v))))
      (set-margin range "20px" 0 "20px" "10px"))
    (loop for name in (list "chord 1" "chord 2" "chord 3"
			    "chord 4" "chord 5" "chord 6")
	  for chord in '((48 56 63 69) (45 55 64 71) (41 57 64 73)
			 (48 59 65 76) (43 59 67 75) (41 59 68 79))
	  collect (let* ((btn (create-button buttons :class "button" :content name))
			 (chord chord))
		    (setf (width btn) 100 (height btn) 20)
		    (set-on-click btn (lambda (obj)
					(proxy-ctrl :pad :note chord :gain *synth-volume* :fade 4.0)))))))

;; ================================================================================
;; sequencer
;; 

(defun sequence-contoller (parent)
  (setf *transport-cell* nil
	*sequence-obj* nil)
  (let* ((div (controller parent "Sequencer")))
    (setf (display div) :flex
	  (flex-direction div) :column
	  (justify-content div) :space-around)
    (dotimes (i 6)
      (let* ((sequnce-line (create-div div)))
	(setf (display sequnce-line) :flex
	      (justify-content sequnce-line) :space-around
	      (align-items sequnce-line) :center)
	(setf (height sequnce-line) "10%")
	(dotimes (j 8)
	  (let* ((cell (create-div sequnce-line))
		 (core (create-div cell)))
	    (if (= i 0) (push core *transport-cell*)
	      (push core *sequence-obj*))
	    (setf (display cell) :flex
		  (justify-content cell) :space-around
		  (align-items cell) :center)
	    (setf (width cell) 15 (height cell) 15)
	    (setf (width core) 10 (height core) 10)
	    (set-border cell "1px" :solid *theme-skin*)
	    (setf (background-color core) *theme-skin*)
	    (setf (hiddenp core) t)
	    (unless (= i 0)
	      (setf (border-radius cell) "50%")
	      (setf (border-radius core) "50%")
	      (let* ((buffer (nth (- i 1) *buffers*))
		     (index (- i 1))
		     (count j))
		(setf (hiddenp core) (not (nth count (nth index *sequence-data*))))
		(set-on-click cell (lambda (obj)
				     (setf (hiddenp core) (not (hiddenp core)))
				     (setf (nth count (nth index *sequence-data*))
				       (not (nth count (nth index *sequence-data*))))
				     (buffer-set buffer count (if (not (hiddenp core)) 1.0 0.0))))))))))
    (setf *transport-cell* (reverse *transport-cell*))
    div))


(defun set-button-css (div width height)
  (setf (color div) *theme-skin*)
  (setf (background-color div) :white)
  (setf (width div) width)
  (setf (height div) height)
  (set-border div "1px" :solid *theme-skin*)
  (setf (border-radius div) "5px")
  (set-margin div "10px" 0 0 "10px"))




(defun on-new-window (body)
  (setf (title (html-document body)) "Lisp GUI")
  (let* ((sb (create-style-block body)))
    (add-style sb :class "button" `(("color" ,*theme-skin*)
				    ("background-color" :white)
				    ("border" ,(format nil "1px solid ~a" *theme-skin*))
				    ("border-radius" "5px")
				    ("margin" "10px 0 0 10px")))
    (add-style sb :class "button:active" `(("color" :red)
					   ("border" ,(format nil "1px solid ~a" :red)))))
  (setf (color (create-child body "<h1> CLOG + CL-COLLIDER </h1>")) *theme-skin*)
  (set-border (create-hr body) :1px :solid *theme-skin*)
  (let* ((container (container body)))
    (system-controller container)
    (synth-controller container)
    (sequence-contoller container)
    (controller container "Effect")
    (run body)))

(defun main ()
  (clog:initialize 'on-new-window)
  (proxy-handle :transport
      (list (tr 4) (mod (cnt 4) 8))
    (lambda (count)
      (dotimes (i 8)
	(setf (hiddenp (nth i *transport-cell*)) (not (= i count))))))
  (proxy :pad
    (with-controls ((note '(48 56 63 69)))
      (mapcar #'mix (sin-osc.ar (+ (list 0 2) (list (midicps note))) 0 .1)))
    :gain .0)
  (proxy :drum
    (let* ((trigs (loop for i from 0 below 5
			collect (* (tr 4) (buf-rd.kr 1 (nth i *buffers*) (mod (cnt 4) 8))))))
      (pan2.ar (+ (sin-osc.ar (t-line.kr 160 80 .1 (nth 0 trigs)) 0 (env.kr .0 .2 .1 (nth 0 trigs) .4))
		  (hpf.ar (white-noise.ar (env.kr .0 (t-rand.kr .04 .2 (nth 1 trigs)) .0 (nth 1 trigs) .1)) 400)
		  (bpf.ar (white-noise.ar (env.kr .0 .1 .0 (nth 2 trigs) .2))
			  (+ 100.0 (t-line.kr 2000 100 .06 (nth 2 trigs))))
		  (sin-osc.ar 400 0 (env.kr .0 .04 .0 (nth 3 trigs) .2))
		  (sin-osc.ar 1800 0 (env.kr .0 .04 .0 (nth 4 trigs) .2)))))))





  
  


