(in-package stumpwm)

;; (setf *debug-level* 2)

(defparameter *tabbar-margin* 1)
(defparameter *tabbar-border-width* 1)
(defparameter *tabbar-foreground-color* "gray73")
(defparameter *tabbar-background-color* "gray40")
(defparameter *tabbar-border-color* "gray73")
(defparameter *tabbar-active-background-color* "gray60")
(defparameter *tabbar-active-foreground-color* "gray98")
(defparameter *tabbar-active-border-color* "gray98")

(defparameter *tabbar-current-tabbar* nil)

(defclass tabbar ()
  ((item-alist :initform nil 			 ;((item-window item-string))
               :reader tabbar-item-alist)
   (window :initform nil :reader tabbar-window)
   (gcontext :initform nil :reader tabbar-gc)
   (width :initarg :width :initform 16 :accessor tabbar-width)
   (height :initarg :height :initform 16 :accessor tabbar-height)
   (item-width :initarg :item-width :initform 0 :accessor item-width)
   (item-height :initarg :item-height :initform (- 16 *tabbar-margin*)  :accessor item-height)
   (geometry-changed-p :initform t :accessor tabbar-geometry-changed-p))
  (:documentation "A simple tab bar."))

(defclass tabbar-tab ()
  ((window :initarg :window :reader tabbar-tab-window)
   (stumpwm-window :initarg :stumpwm-window :reader tabbar-tab-stumpwm-window)
   (gcontext :initarg :gcontext :accessor tabbar-tab-gc)   
   (width :initarg :width :initform 16 :accessor tabbar-tab-width)
   (height :initarg :height :initform 16 :accessor tabbar-tab-height))
  (:documentation "A tab in a tab bar"))

(defun create-tabbar-tab (parent-window gcontext stumpwm-window)
  "Create an instance of the tabbar-bar class.
PARENT-WINDOW - xlib parent window
GCONTEXT - xlib GCONTEXT used to draw this tab
STUMPWM-WINDOW - instance of the STUMPWM:WINDOW class to draw"
  (make-instance
   'tabbar-tab
   :window 
   (xlib:create-window
    :parent       parent-window
    :x            0 ;temporary value
    :y            0 ;temporary value
    :width        16 ;temporary value
    :height       16 ;temporary value
    :border-width *tabbar-border-width*
    :border       (xlib:gcontext-foreground gcontext)
    :background   (xlib:gcontext-background gcontext)
    :event-mask   (xlib:make-event-mask :exposure
                                        :leave-window
                                        :button-press))
   :gcontext gcontext
   :stumpwm-window stumpwm-window))

(defmethod tabbar-bar-refresh ((self tabbar-tab))
  (with-slots (width height gcontext stumpwm-window window) self
    (let* ((string         (window-title stumpwm-window))
           (font           (xlib:gcontext-font gcontext))
           (baseline-y     (xlib:font-ascent font))
           (width          (xlib:text-extents font string))
           (drawable-width (xlib:drawable-width window)))
      (dformat 2 "text width = ~d drawable width = ~d~%"
               width drawable-width)
      (xlib:draw-image-glyphs
       window gcontext
       (+ (/ (- drawable-width width) 2)
          *tabbar-margin*)			;start x
       (+ baseline-y *tabbar-margin*)	;start y
       string))))
      

(defmethod tabbar-bar-reposition ((self tabbar-tab) x y w h)
  (with-slots (window width height) self
    (xlib:with-state (window)
      (setf (xlib:drawable-height window) h
            height h
            (xlib:drawable-width  window) w
            width w
            (xlib:drawable-x      window) x
            (xlib:drawable-y      window) y))))


(defun create-tabbar (parent-window text-color background-color text-font)
  (let ((new-tabbar (make-instance 'tabbar)))
    (with-slots (window gcontext) new-tabbar      
      (setf gcontext ;; Create menu graphics context      
            (xlib:create-gcontext :drawable   parent-window
                                  :foreground text-color
                                  :background background-color
                                  :font       text-font)
            window ;; Create tabbar main window
            (xlib:create-window
             :parent            parent-window
             :class             :input-output
             :x                 0	;temporary value
             :y                 0	;temporary value
             :width             16	;temporary value
             :height            16	;temporary value
             ;;             :border-width      *tabbar-border-width*
             :border            text-color
             :background        background-color
             :save-under        :on
             :override-redirect :on ;override window mgr when positioning
             :event-mask        (xlib:make-event-mask :leave-window :exposure))))
    (setf *tabbar-current-tabbar* new-tabbar)
    (dformat 2 "Tabbar created: ~s~%" (tabbar-window new-tabbar))
    new-tabbar))

(defun find-tabbar-by-window (xwin)
  (when (and *tabbar-current-tabbar*
             (or 
              (eq (tabbar-window *tabbar-current-tabbar*) xwin)
              (find xwin (tabbar-item-alist *tabbar-current-tabbar*) :key #'car)))
                   
    *tabbar-current-tabbar*))


(defmethod tabbar-set-item-alist ((self tabbar) &rest item-strings)
  ;; ;; Assume the new items will change the tabbar's width and height
  (setf (tabbar-geometry-changed-p self) t)
  (with-slots (item-alist) self
    ;; Destroy any existing item windows
    (dolist (item item-alist)
      (destroy-window (first item)))
    ;; Add (item-window item-string) elements to item-alist
    (setf item-alist
          (loop for item in item-strings
                collect
                (list 
                 (xlib:create-window
                  :parent       (tabbar-window self)
                  :x            0 ;temporary value
                  :y            0 ;temporary value
                  :width        16 ;temporary value
                  :height       16 ;temporary value
                  :border-width *tabbar-border-width*
;;                  :border       (xlib:gcontext-foreground (tabbar-gc self))
                  :background   (xlib:gcontext-background (tabbar-gc self))
                  :event-mask   (xlib:make-event-mask :exposure
                                                      :leave-window
                                                      :button-press))
                 item)))))

(defmethod tabbar-recompute-geometry ((self tabbar))
  "Recompute the geometry of tabbar and its items"
  (with-slots (window
               width
               height
               item-height
               item-width
               item-alist
               gcontext
               geometry-changed-p)
      self
    (when geometry-changed-p
      (let* ((tabbar-font (xlib:gcontext-font gcontext))
             (nitems      (length item-alist)))
        ;; -- ascent (i.e. 12) -----
        ;;                              /\
        ;;                             /--\
        ;; -- baseline -------------  /    \
        ;; -- descent (i.e. 4) -----
        (setf width (head-width (current-head))
              item-height (+ (xlib:font-ascent tabbar-font)
                             (xlib:font-descent tabbar-font)
                             (* 2 *tabbar-margin* ))
              item-width  (if (= 0 nitems) 0
                              (/ width nitems)))
                                        ;        )
        (dformat 2 "font-ascend ~d font-descent ~d item-height ~d"
                 (xlib:font-ascent tabbar-font)
                 (xlib:font-descent tabbar-font)
                 item-height)
        (xlib:with-state (window)
          (setf (xlib:drawable-x      window) 0
                ;; TODO: if we are positioned on top, adjust modeline height here, if present          
                (xlib:drawable-y      window) 16
                (xlib:drawable-width  window) width
                (xlib:drawable-height window) (+ item-height
                                                 (* 2 *tabbar-border-width*))))
        (loop for (w s) in item-alist
              with x-offset = 0
              with x-step = (/ width nitems)
              do
                 (xlib:with-state (w)
                   (setf (xlib:drawable-height w) item-height                                               
                         (xlib:drawable-width  w) (- item-width
                                                     (* 2 *tabbar-border-width*))
                         (xlib:drawable-x      w) x-offset
                         (xlib:drawable-y      w) 0)
                   (dformat 2 "x-offset ~d " x-offset)
                   (incf x-offset x-step)))
        ;; map window
        (xlib:map-window window)
        ;; Map all item windows      
        (xlib:map-subwindows window)
        ;; save item geometry
        (setf geometry-changed-p nil)))))

(defmethod tabbar-refresh ((self tabbar))
  "Draw the tabbar"
  (with-slots (window
               item-height
               item-width
               item-alist
               gcontext)
      self
    (loop for (w s) in item-alist
          with box-margin = *tabbar-margin*
          with baseline-y = (xlib:font-ascent (xlib:gcontext-font gcontext))
          for width = (xlib:text-extents (xlib:gcontext-font gcontext) s)
          for drawable-width = (xlib:drawable-width w)
          do
             (dformat 2 "text width = ~d drawable width = ~d~%"
                      width drawable-width)
             (xlib:draw-image-glyphs
              w gcontext
              (+ (/ (- drawable-width width) 2)
                 box-margin)			;start x
              (+ baseline-y box-margin)	;start y
              s))))


(defun tabbar-start (&optional (font-name "terminus-bold-16"))
  (when-let (windows (mapcar #'window-title (group-windows (current-group))))
    (let* (
           (display *display*)
           (xscreen (stumpwm::screen-number (stumpwm::current-screen)))
           (fg-color  (stumpwm::alloc-color (stumpwm::current-screen) *tabbar-foreground-color*))
           (bg-color  (stumpwm::alloc-color (stumpwm::current-screen) *tabbar-background-color*))
           (nice-font (xlib:OPEN-FONT display font-name))
           ;; Create a tab bar as a child of the root window.
           (tabbar (create-tabbar (xlib:screen-root xscreen)
                                  fg-color bg-color nice-font)))
      (apply #'tabbar-set-item-alist tabbar windows)
      (tabbar-recompute-geometry tabbar)
      (tabbar-refresh tabbar)
      ;; Bedevil the user until he picks a nice programming language
      (unwind-protect
           ;; (loop
           ;;  ;; Determine the current root window position of the pointer
           ;;  (multiple-value-bind (x y) (QUERY-POINTER (SCREEN-ROOT screen))

           ;;    (let ((choice (menu-choose a-menu x y)))
           ;;      (when (string-equal "Lisp" choice)
           ;;        (return)))))
           (progn
             )
        ;; (xlib:CLOSE-DISPLAY display)
        nil
        )
      tabbar)))

(defmethod tabbar-close ((self tabbar))
  (xlib:unmap-subwindows (tabbar-window self))
  (xlib:unmap-window (tabbar-window self))
  (xlib:destroy-window (tabbar-window self)))


(defcommand tabbar-show ()
    ()
  "Show tabbar."
  (when *tabbar-current-tabbar*
    (tabbar-close *tabbar-current-tabbar*)
    (setf *tabbar-current-tabbar* nil))
  (tabbar-start))


(defcommand tabbar-hide ()
    ()
  "Show tabbar."
  (when *tabbar-current-tabbar*
    (tabbar-close *tabbar-current-tabbar*)
    (setf *tabbar-current-tabbar* nil)))
