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

(defparameter *tabbar-list-tabbars* nil
  "List of the tab bars. There should be 1 tabbar per screen.")


;;; Tabbar tab class definition and functions

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
    (let* ((string         (window-name stumpwm-window))
           (font           (xlib:gcontext-font gcontext))
           (baseline-y     (xlib:font-ascent font))
           (drawable-width (xlib:drawable-width window))
           (new-string     (string-trim-to-fit string font
                                               (+ drawable-width
                                                  (* 2 *tabbar-margin*))))
           (width          (xlib:text-extents font new-string)))
      (dformat 2 "text width = ~d drawable width = ~d~%"
               width drawable-width)
      (xlib:draw-image-glyphs
       window gcontext
       (+ (round (/ (- drawable-width width) 2))
          *tabbar-margin*)			;start x
       (+ baseline-y *tabbar-margin*)	;start y
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
   (geometry-changed-p :initform t :accessor tabbar-geometry-changed-p))
  (:documentation "A simple tab bar."))

(defmethod (setf tabbar-head) (head (self tabbar))
  "Setter for the tabbar head. Refreshes tabbar contents"
  (when head
    (setf (slot-value self 'head) head)
    (update-tabbar)))

(defun create-tabbar (parent-window screen)
  (let ((new-tabbar
          (make-instance 'tabbar
                         :head (current-head)
                         :screen screen))
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
             ;;             :border-width      *tabbar-border-width*
             :border            fg-color
             :background        bg-color
             :save-under        :on
             :override-redirect :on ;override window mgr when positioning
             :event-mask        (xlib:make-event-mask :leave-window :exposure))))
    (dformat 2 "Tabbar created: ~s~%" (tabbar-window new-tabbar))
    new-tabbar))


(defmethod tabbar-recreate-tabs ((self tabbar))
  ;; ;; Assume the new items will change the tabbar's width and height
  (setf (tabbar-geometry-changed-p self) t)
  (with-slots (tabs) self
    ;; Destroy any existing item windows
    (dolist (tab tabs)
      (xlib:destroy-window (tabbar-tab-window tab)))
    ;; get the list of the windows in current group
    (when-let (windows
               (sort-windows (current-group)))
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
        (setf width (head-width (tabbar-head self))
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
          (setf (xlib:drawable-x      window) (head-x (tabbar-head self))
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
        ;; save item geometry
        (setf geometry-changed-p nil)))))

(defmethod tabbar-refresh ((self tabbar))
  "Draw the tabbar"
  (when (tabbar-visible-p self)
    (dolist (tab (tabbar-tabs self))
      (tabbar-tab-refresh tab))))

(defmethod tabbar-show ((self tabbar))
  "Show/hide tabbar depending on a visible-p flag"
  (if (tabbar-visible-p self)
      (progn
        ;; map main windows
        (xlib:map-window (tabbar-window self))
        ;; Map all item windows      
        (xlib:map-subwindows (tabbar-window self)))
      (xlib:unmap-window (tabbar-window self))))
   
;;; Used by other modules of StumpWM

(defun find-tabbar-by-window (xwin)
  (find-if (lambda (tb)
             (or
              (eq (tabbar-window tb) xwin)
              (find xwin (tabbar-tabs tb)
                    :key #'tabbar-tab-window)))
           *tabbar-list-tabbars*))

(defun screen-tabbar (screen)
  (find screen *tabbar-list-tabbars* :key #'tabbar-screen))

(defun update-tabbar (&optional (screen (current-screen)))
  "Update tabbar on a current head/screen.
There could only be one tabbar per screen"
  (when-let (tb (screen-tabbar screen))
    (tabbar-recreate-tabs tb)
    (tabbar-recompute-geometry tb)
    (tabbar-refresh tb)
    (tabbar-show tb)))

(defmethod tabbar-handle-click-on-window ((self tabbar) window)
  "Handle click event on a tab bar. WINDOW is a window
(either tabbar or one of its tabs to receive event"
  (when-let* ((found-tab
               (find-if (lambda (tab)
                          (eq window (tabbar-tab-window tab)))
                        (tabbar-tabs self)))
              (win (tabbar-tab-stumpwm-window found-tab)))
    (raise-window win)
    (focus-window win)
    ;; todo: solve this. need to supply screen
    (update-tabbar)))

;;; Create/Destroy/Toggle

(defun tabbar-create (&optional (screen (current-screen)))
  (let* ((xscreen (screen-number screen))
         ;; Create a tab bar as a child of the root window.
         (tabbar (create-tabbar (xlib:screen-root xscreen)
                                screen)))
    (tabbar-recreate-tabs tabbar)
    (tabbar-recompute-geometry tabbar)
    (tabbar-refresh tabbar)
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
  (setf *tabbar-list-tabbars* (remove self *tabbar-list-tabbars*)))

(defun tabbar-toggle (screen)
  (let ((tb (screen-tabbar screen)))
    (cond ((and tb (tabbar-visible-p tb))
           (setf (tabbar-visible-p tb) nil))
          ((and tb (not (tabbar-visible-p tb)))
           (setf (tabbar-visible-p tb) t))
          (t (setf tb (tabbar-create screen))))
    (tabbar-show tb)))

;;; Commands

(defcommand toggle-tabbar ()
    ()
  "Toggle tabbar for current screen"
  (tabbar-toggle (current-screen)))

