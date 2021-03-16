(in-package stumpwm)

;; (setf *debug-level* 2)
;; (compile-file "events.lisp")
;; (compile-file "mode-line.lisp")
;; (compile-file "tile-group.lisp")
;; (compile-file "window.lisp")

(defparameter *tabbar-margin* 1)
(defparameter *tabbar-border-width* 1)
(defparameter *tabbar-foreground-color* "gray73")
(defparameter *tabbar-background-color* "gray40")
(defparameter *tabbar-border-color* "gray73")
(defparameter *tabbar-active-background-color* "gray60")
(defparameter *tabbar-active-foreground-color* "gray98")
(defparameter *tabbar-active-border-color* "gray98")
(defparameter *tabbar-font-name* "terminus-bold-16")
(defparameter *tabbar-position* :top
  "Position of the tab bar - :top or :bottom of the screen")

(defparameter *tabbar-list-tabbars* nil
  "List of the tab bars. There should be 1 tabbar per screen.")
(defparameter *tabbar-instances* nil)

;;; Tabbar tab class definition and functions

(defclass tabbar-tab ()
  ((window :initarg :window :reader tabbar-tab-window)
   (stumpwm-window :initarg :stumpwm-window :reader tabbar-tab-stumpwm-window)
   (gcontext :initarg :gcontext :accessor tabbar-tab-gc)
   (active-gcontext :initarg :active-gcontext :accessor tabbar-tab-active-gc)
   (width :initarg :width :initform 16 :accessor tabbar-tab-width)
   (height :initarg :height :initform 16 :accessor tabbar-tab-height))
  (:documentation "A tab in a tab bar"))

(defun create-tabbar-tab (parent-window gc active-gc stumpwm-window)
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
    :border       (xlib:gcontext-foreground gc)
    :background   (xlib:gcontext-background gc)
    :event-mask   (xlib:make-event-mask :exposure
                                        :leave-window
                                        :button-press))
   :active-gcontext active-gc
   :gcontext gc
   :stumpwm-window stumpwm-window))

(defmethod tabbar-tab-refresh ((self tabbar-tab))
  "Draw the contents of the tab"
  (with-slots (width height gcontext active-gcontext stumpwm-window window) self
    (let* ((active-p (eq (current-window) stumpwm-window))
           (gc             (if active-p active-gcontext gcontext))
           (string         (window-name stumpwm-window))
           (font           (xlib:gcontext-font gc))
           (baseline-y     (xlib:font-ascent font))
           (drawable-width (xlib:drawable-width window))
           (new-string     (string-trim-to-fit string font
                                               (+ drawable-width
                                                  (* 2 *tabbar-margin*))))
           (width          (xlib:text-extents font new-string)))
      (dformat 2 "refresh tab ~s [~a]~a~%" new-string (if active-p "active" "passive") self)
      (dformat 2 "text width = ~d drawable width = ~d~%"
               width drawable-width)
      ;; set the border and background
      (xlib:with-state (window)
        (setf (xlib:window-background window) (xlib:gcontext-background gc)
              (xlib:window-border     window) (xlib:gcontext-foreground gc)))
      ;; clear the prevoius text
      (xlib:clear-area window)
      ;; now draw the text
      (xlib:draw-image-glyphs
       window gc
       (+ (round (/ (- drawable-width width) 2))
          *tabbar-margin*)			; start x
       (+ baseline-y *tabbar-margin*)	; start y
       new-string))))

(defun string-trim-to-fit (string font desired-width)
  "Replace suffix of the given STRING with dots (...)
to fit to DESIRED-WIDTH pixels when rendered with a FONT provided"
  (let ((width (xlib:text-extents font string))
        (len (length string)))
    (dformat 2 "string: ~a width ~a desired ~a~%"
             string width desired-width)
    (if (or (< width desired-width)     ; string fits
            (string= string "..."))     ; nothing to trim
        string                          ; just return the string
        (string-trim-to-fit
         ;; replace last 4 characters with "..."
         (concatenate 'string
                      (subseq string 0 (- len 4))
                      "...")
         font
         desired-width))))

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

;;; Tabbar class definition and functions

(defclass tabbar ()
  ((tabs :initform nil
         :reader tabbar-tabs)
   (window :initform nil :reader tabbar-window)
   (screen :initarg :screen :reader tabbar-screen)
   (head :initarg :head :reader tabbar-head)
   (visible-p :accessor tabbar-visible-p :initform t)
   (active-gcontext :initform nil :reader tabbar-active-gc)
   (gcontext :initform nil :reader tabbar-gc)
   (width :initarg :width :initform 16 :accessor tabbar-width)
   (height :initarg :height :initform 16 :accessor tabbar-height)
   (position :initarg :position :initform :top :reader tabbar-position))
  (:documentation "A simple tab bar."))

(defmethod (setf tabbar-head) (head (self tabbar))
  "Setter for the tabbar head. Refreshes tabbar contents"
  (when head
    (setf (slot-value self 'head) head)
    (update-tabbar self)))

(defmethod (setf tabbar-position) (position (self tabbar))
  "Setter for the tabbar head. Refreshes tabbar contents"
  (setf (slot-value self 'position) position)
  (update-tabbar self)
  ;; change windows layout
  (dolist (group (screen-groups (tabbar-screen self)))
    (group-sync-head group (tabbar-head self))))

(defun create-tabbar (parent-window screen)
  (let ((new-tabbar
          (make-instance 'tabbar
                         :head (current-head)
                         :screen screen
                         :position *tabbar-position*))
        (text-font (xlib:open-font *display* *tabbar-font-name*))
        (fg-color (alloc-color screen
                               *tabbar-foreground-color*))
        (bg-color (alloc-color screen
                               *tabbar-background-color*))
        (active-fg-color (alloc-color screen
                                      *tabbar-active-foreground-color*))
        (active-bg-color (alloc-color screen
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
             :background        bg-color
             :save-under        :off
             :override-redirect :on ;override window mgr when positioning
             :event-mask        (xlib:make-event-mask :leave-window :exposure))))
    (dformat 2 "Tabbar created: ~s~%" (tabbar-window new-tabbar))
    (push new-tabbar *tabbar-instances*)
    new-tabbar))


(defmethod tabbar-recreate-tabs ((self tabbar))
  ;; Assume the new items will change the tabbar's width and height
  (with-slots (tabs) self
    ;; get the list of the windows in current group
    (let ((windows
            (sort-windows (screen-current-group
                           (tabbar-screen self))))
          ;; list of windows on tabs
          (tab-windows
            (mapcar #'tabbar-tab-stumpwm-window tabs)))
      ;; only destroy tabs when the window list has been changed
      (dformat 2 "should recreate tabs? ~a~%" (not (equal windows tab-windows)))
      (unless (equal windows tab-windows)
        ;; Destroy any existing item windows
        (dolist (tab tabs)
          (xlib:destroy-window (tabbar-tab-window tab)))
        ;; Create corresponding tabs
        (setf tabs
              (loop for w in windows
                    collect
                    (create-tabbar-tab (tabbar-window self)
                                       (tabbar-gc self)
                                       (tabbar-active-gc self)
                                       w))))
      ;; decide if we want to recompute geometry and show
      ;; the tab bar
      (setf (tabbar-visible-p self) (when tabs t))))
  (values))

(defmethod tabbar-recompute-geometry ((self tabbar))
  "Recompute the geometry of tabbar and its items"
  (with-slots (window
               width
               height
               tabs
               gcontext)
      self
    (when (tabbar-visible-p self)
      (setf width (head-width (tabbar-head self)))
      (let* ((ml (head-mode-line (tabbar-head self)))
             ;; check if mode line is visible
             (modeline-visible-p (and ml
                                      (not
                                       (eq (mode-line-mode ml)
                                           :hidden))))
             ;; offset to add/subtract from tabbar position
             ;; corresponding to modeline height 
             (y-offset (if (and modeline-visible-p
                                (eq (mode-line-position ml)
                                    (tabbar-position self)))
                           ;; only if the mode line and tabbar
                           ;; are on the same side of the screen
                           (xlib:with-state
                               ((mode-line-window ml))
                             (+ (xlib:drawable-height
                                 (mode-line-window ml))
                                (* 2 (xlib:drawable-border-width (mode-line-window ml)))))
                           0))
             (tabbar-font (xlib:gcontext-font gcontext))
             (nitems      (length tabs))
             ;; -- ascent (i.e. 12) -----
             ;;                              /\
             ;;                             /--\
             ;; -- baseline -------------  /    \
             ;; -- descent (i.e. 4) -----
             (item-height
               (+ (xlib:font-ascent tabbar-font)
                  (xlib:font-descent tabbar-font)
                  (* 2 *tabbar-margin* )))
             (item-width
               (if (= 0 nitems) 0
                   (round (/ width nitems))))
             (x-pos (head-x (tabbar-head self)))
             (y-pos (if (eq (tabbar-position self) :top)
                        (+ (head-y (tabbar-head self)) y-offset)
                        (- (+ (head-y (tabbar-head self))
                              (head-height (tabbar-head self)))
                           (* 2 *tabbar-border-width*)
                           y-offset
                           item-height))))
        (setf height (+ item-height (* 2 *tabbar-border-width*)))
        (dformat 2 "y-offset ~d h: ~d w: ~d x: ~d y: ~d~%"
                 y-offset height width x-pos y-pos)
        (xlib:with-state (window)
          (setf (xlib:drawable-x      window) x-pos
                (xlib:drawable-y      window) y-pos
                (xlib:drawable-width  window) width
                (xlib:drawable-height window) height))
        (loop for tab in tabs
              with x-offset = 0
              with x-step = (round (/ width nitems))
              for tab-width = (- item-width
                                 (* 2 *tabbar-border-width*))
              do
                 (tabbar-tab-reposition
                  tab
                  x-offset
                  0
                  tab-width
                  item-height)
                 (dformat 2 "tab: x-offset ~d w: ~d h: ~d~%" x-offset tab-width item-height)
                 (incf x-offset x-step))))))

(defmethod tabbar-refresh ((self tabbar))
  "Draw the tabbar"
  (when (tabbar-visible-p self)
    (with-slots (tabbar-gc tabbar-active-gc tabs) self
      (dolist (tab (tabbar-tabs self))
        (setf (tabbar-tab-gc tab)
              (if (eq (tabbar-tab-stumpwm-window tab)
                      (current-window))
                  (tabbar-active-gc self)
                  (tabbar-gc self)))
        ;; FIXME: active/passive borders and background
      (tabbar-tab-refresh tab)))))

(defmethod tabbar-show ((self tabbar))
  "Show/hide tabbar depending on a visible-p flag"
  (if (tabbar-visible-p self)
      (progn
        ;; map main windows
        (xlib:map-window (tabbar-window self))
        ;; Map all item windows      
        (xlib:map-subwindows (tabbar-window self))
        ;; show window contents
        (tabbar-refresh self))
      (xlib:unmap-window (tabbar-window self))))
   
;;; Used by other modules of StumpWM

(defun find-tabbar-by-window (xwin)
  "Find tabbar the X11 window XWIN belongs to
(either main window or tab)"
  (find-if (lambda (tb)
             (or
              (eq (tabbar-window tb) xwin)
              (find xwin (tabbar-tabs tb)
                    :key #'tabbar-tab-window)))
           *tabbar-list-tabbars*))

(defun screen-tabbar (screen)
  "Find tabbar for the StumpWM SCREEN provided.
Only 1 tabbar per screen"
  (find screen *tabbar-list-tabbars* :key #'tabbar-screen))

(defun head-tabbar (head)
  "Return the tabbar for the StumpWM HEAD provided if
it is displayed on this head"
  (find head *tabbar-list-tabbars* :key #'tabbar-head))

(defun update-all-tabbars ()
  "Update all tabbars on all screens"
  (dolist (tb *tabbar-list-tabbars*)
    (update-tabbar tb)))

(defmethod update-tabbar ((self tabbar))
  "Update tabbar on a current head/screen.
There could only be one tabbar per screen"
  (tabbar-recreate-tabs self)
  (tabbar-recompute-geometry self)
  (tabbar-refresh self)
  (tabbar-show self))

(defmethod tabbar-handle-click-on-window ((self tabbar) window)
  "Handle click event on a tab bar. WINDOW is a window
(either tabbar or one of its tabs to receive event"
  (dformat 2 "tabbar-handle-click-on-window~%")
  (when-let* ((found-tab
               (find-if (lambda (tab)
                          (eq window (tabbar-tab-window tab)))
                        (tabbar-tabs self)))
              (win (tabbar-tab-stumpwm-window found-tab)))
    (raise-window win)
    (focus-window win)
    (tabbar-refresh self)))

;;; Create/Destroy/Toggle

(defun tabbar-create (&optional (screen (current-screen)))
  (let* ((xscreen (screen-number screen))
         ;; Create a tab bar as a child of the root window.
         (tabbar (create-tabbar (xlib:screen-root xscreen)
                                screen)))
    (tabbar-recreate-tabs tabbar)
    (tabbar-recompute-geometry tabbar)
    (tabbar-show tabbar)
    (push tabbar *tabbar-list-tabbars*)    
    tabbar))

(defmethod tabbar-destroy ((self tabbar))
  (xlib:unmap-subwindows (tabbar-window self))
  (xlib:unmap-window (tabbar-window self))
  (dolist (w (tabbar-tabs self))
    (xlib:destroy-window (tabbar-tab-window w)))
  (xlib:destroy-window (tabbar-window self))
  (xlib:free-gcontext (tabbar-gc self))
  (xlib:free-gcontext (tabbar-active-gc self))
  (setf *tabbar-list-tabbars* (remove self *tabbar-list-tabbars*))
  (setf *tabbar-instances* (remove self *tabbar-list-tabbars*))
  )

(defun tabbar-toggle (screen)
  (let ((tb (screen-tabbar screen)))
    (if tb
        (tabbar-destroy tb)
        (tabbar-show (setf tb (tabbar-create screen))))
    (dolist (group (screen-groups screen))
      (group-sync-head group (tabbar-head tb)))))


;;; Commands

(defcommand toggle-tabbar ()
    ()
  "Toggle tabbar for current screen"
  (tabbar-toggle (current-screen)))

