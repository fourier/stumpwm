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
(defparameter *tabbar-font-name* "terminus-bold-16")

(defparameter *tabbar-current-tabbar* nil)

(defclass tabbar ()
  ((tabs :initform nil
         :reader tabbar-tabs)
   (window :initform nil :reader tabbar-window)
   (active-gcontext :initform nil :reader tabbar-active-gc)
   (gcontext :initform nil :reader tabbar-gc)
   (width :initarg :width :initform 16 :accessor tabbar-width)
   (height :initarg :height :initform 16 :accessor tabbar-height)
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

(defmethod tabbar-tab-refresh ((self tabbar-tab))
  "Draw the contents of the tab"
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
       (+ (round (/ (- drawable-width width) 2))
          *tabbar-margin*)			;start x
       (+ baseline-y *tabbar-margin*)	;start y
       string))))
      

(defmethod tabbar-tab-reposition ((self tabbar-tab) x y w h)
  "Set size and position of the tab"
  (with-slots (window width height) self
    (xlib:with-state (window)
      (setf (xlib:drawable-height window) h
            height h
            (xlib:drawable-width  window) w
            width w
            (xlib:drawable-x      window) x
            (xlib:drawable-y      window) y))))


(defun create-tabbar (parent-window)
  (let ((new-tabbar (make-instance 'tabbar))
        (text-font (xlib:open-font *display* *tabbar-font-name*))
        (fg-color (alloc-color (current-screen)
                               *tabbar-foreground-color*))
        (bg-color (alloc-color (current-screen)
                               *tabbar-background-color*))
        (active-fg-color (alloc-color (current-screen)
                               *tabbar-active-foreground-color*))
        (active-bg-color (alloc-color (current-screen)
                               *tabbar-active-background-color*)))
    (with-slots (gcontext active-gcontext window) new-tabbar
      (setf gcontext ;; Create passive tab and window graphics context      
            (xlib:create-gcontext :drawable   parent-window
                                  :foreground fg-color
                                  :background bg-color
                                  :font       text-font)
            active-gcontext
            (xlib:create-gcontext :drawable   parent-window
                                  :foreground active-fg-color
                                  :background active-bg-color
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
             :border            fg-color
             :background        bg-color
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
              (find xwin (tabbar-tabs *tabbar-current-tabbar*) :key #'tabbar-tab-window)))
    *tabbar-current-tabbar*))

(defun update-tabbar ()
  (when *tabbar-current-tabbar*
    (tabbar-recreate-tabs *tabbar-current-tabbar*)
    (tabbar-recompute-geometry *tabbar-current-tabbar*)
    (tabbar-refresh *tabbar-current-tabbar*)))

(defmethod tabbar-recreate-tabs ((self tabbar))
  ;; ;; Assume the new items will change the tabbar's width and height
  (setf (tabbar-geometry-changed-p self) t)
  (with-slots (tabs) self
    ;; Destroy any existing item windows
    (dolist (tab tabs)
      (xlib:destroy-window (tabbar-tab-window tab)))
    ;; get the list of the windows in current group
    (when-let (windows
               (sort
                (copy-list (group-windows (current-group)))
                #'< :key #'window-number))
      ;; Create corresponding tabs
      (setf tabs
            (loop for w in windows
                  collect
                  (create-tabbar-tab (tabbar-window self)
                                     (if (eq w (current-window))
                                         (tabbar-active-gc self)
                                         (tabbar-gc self))
                                     w)))))
  (values))

(defmethod tabbar-recompute-geometry ((self tabbar))
  "Recompute the geometry of tabbar and its items"
  (with-slots (window
               width
               height
               tabs
               gcontext
               geometry-changed-p)
      self
    (when geometry-changed-p
      (let* ((tabbar-font (xlib:gcontext-font gcontext))
             item-height
             item-width
             (nitems      (length tabs)))
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
                              (round (/ width nitems))))
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
        (loop for tab in tabs
              with x-offset = 0
              with x-step = (round (/ width nitems))
              do
                 (tabbar-tab-reposition
                  tab
                  x-offset
                  0
                  (- item-width
                     (* 2 *tabbar-border-width*))
                  item-height)
                 (dformat 2 "x-offset ~d " x-offset)
                 (incf x-offset x-step))
        ;; map window
        (xlib:map-window window)
        ;; Map all item windows      
        (xlib:map-subwindows window)
        ;; save item geometry
        (setf geometry-changed-p nil)))))

(defmethod tabbar-refresh ((self tabbar))
  "Draw the tabbar"
  (dolist (tab (tabbar-tabs self))
    (tabbar-tab-refresh tab)))

(defun tabbar-start ()
  (let* ((xscreen (stumpwm::screen-number (stumpwm::current-screen)))
         ;; Create a tab bar as a child of the root window.
         (tabbar (create-tabbar (xlib:screen-root xscreen))))
      (tabbar-recreate-tabs tabbar)
      (tabbar-recompute-geometry tabbar)
      (tabbar-refresh tabbar)
      tabbar))

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

(defcommand tabbar-toggle ()
    ()
  "Toggle tabbar."
  (if *tabbar-current-tabbar*
      (tabbar-hide)
      (tabbar-show)))
