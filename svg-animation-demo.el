(require 'esxml)
(require 'pdata)

(defvar svg-animation-demo-interval (/ 1.0 50))

(defvar svg-animation-demo-state (pdata-create))
(pdata-add-records svg-animation-demo-state
  '(:x 0 :y 0 :type static :layer foreground
       :frames 10.0 :frame 0 :msecs 500
       :beg-x 0 :beg-y 0
       :end-x 240 :end-y 0)
  '(:type board :layer background))
(cl-loop for i from 0 to 3 do
         (pdata-add-record svg-animation-demo-state
           `(:x ,i :y 0 :type background-tile :layer background)))

(defun svg-animation-demo-render-state (plist)
  (let ((board-color "#aaaaaa")
        (tile-color "#cccccc")
        (tile-size 64)
        (padding 16)
        (offset 12)
        (roundness 8)
        (type (plist-get plist :type))
        (frame (plist-get plist :frame))
        (frames (plist-get plist :frames))
        (beg-x (plist-get plist :beg-x))
        (beg-y (plist-get plist :beg-y))
        (end-x (plist-get plist :end-x))
        (end-y (plist-get plist :end-y))
        (x (plist-get plist :x))
        (y (plist-get plist :y)))
    (sxml-to-xml
     (cond
      ((eq type 'board)
       `(rect
         (@ (width ,(number-to-string (+ (* 4 tile-size) (* 5 padding))))
            (height ,(number-to-string (+ (* 1 tile-size) (* 2 padding))))
            (x ,(number-to-string offset))
            (y ,(number-to-string offset))
            (rx ,(number-to-string roundness))
            (fill ,board-color))))
      ((eq type 'background-tile)
       `(rect
         (@ (width ,(number-to-string tile-size))
            (height ,(number-to-string tile-size))
            (x ,(number-to-string
                 (+ (* x tile-size)
                    (* (1+ x) padding) offset)))
            (y ,(number-to-string
                 (+ (* y tile-size)
                    (* (1+ y) padding) offset)))
            (rx ,(number-to-string (/ roundness 2)))
            (fill ,tile-color))))
      ((eq type 'static)
       `(rect
         (@ (width ,(number-to-string tile-size))
            (height ,(number-to-string tile-size))
            (x ,(number-to-string
                 (+ (* x tile-size)
                    (* (1+ x) padding) offset)))
            (y ,(number-to-string
                 (+ (* y tile-size)
                    (* (1+ y) padding) offset)))
            (rx ,(number-to-string (/ roundness 2)))
            (fill "#0000aa"))))
      ((eq type 'moving)
       `(rect
         (@ (width ,(number-to-string tile-size))
            (height ,(number-to-string tile-size))
            (x ,(number-to-string
                 (+ padding offset
                    (mod (floor (* (/ frame frames) (- end-x beg-x)))
                         (abs (- end-x beg-x))))))
            (y ,(number-to-string
                 (+ padding offset
                    (mod (floor (* (/ frame frames) (- end-y beg-y)))
                         (abs (- end-y beg-y))))))
            (rx ,(number-to-string (/ roundness 2)))
            (fill "#aa0000"))))
      (t nil)))))

(defun svg-animation-demo-create-svg ()
  (sxml-to-xml
   `(svg
     (@ (xmlns "http://www.w3.org/2000/svg") (width "360") (height "120"))
     ,@(cl-loop for item in '(background foreground animated) nconc
                `(g ,@(cl-loop for plist in
                               (pdata-select svg-animation-demo-state
                                             `(:layer ,item))
                               collect
                               (svg-animation-demo-render-state plist)))))))

(defun svg-animation-demo-init ()
  (interactive)
  (pdata-update svg-animation-demo-state '(:layer foreground)
    :type 'static :frame 0)
  (svg-animation-demo-redraw))

(defun svg-animation-demo-redraw ()
  (interactive)
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert-image (create-image (svg-animation-demo-create-svg) 'svg t))
  (insert "\n")
  (setq buffer-read-only t))

(define-derived-mode svg-animation-demo-mode special-mode "SVG Animation Demo"
  "A SVG animation demo."
  (with-current-buffer (get-buffer "*svg animation demo*")
    (buffer-disable-undo)
    (svg-animation-demo-init)
    (svg-animation-demo-redraw)))

(defun svg-animation-demo ()
  "Start the animation demo"
  (interactive)
  (switch-to-buffer "*svg animation demo*")
  (svg-animation-demo-mode)
  (goto-char (point-max)))

(defun svg-animation-demo-move ()
  (interactive)
  (let ((direction
         (cond ((pdata-select-at svg-animation-demo-state
                                 '(:layer foreground :x 0 :y 0))
                'left-to-right)
               ((pdata-select-at svg-animation-demo-state
                                 '(:layer foreground :x 3 :y 0))
                'right-to-left)
               (t 'unknown))))
    (cond
     ((eq direction 'left-to-right)
      (pdata-update svg-animation-demo-state '(:layer foreground :x 0 :y 0)
       :frame 1 :beg-x 0 :beg-y 0 :end-x 240 :end-y 0 :type 'moving))
     ((eq direction 'right-to-left)
      (pdata-update svg-animation-demo-state '(:layer foreground :x 3 :y 0)
       :frame 1 :beg-x 240 :beg-y 0 :end-x 0 :end-y 0 :type 'moving)))
    (while (<= (pdata-select-at svg-animation-demo-state
                               '(:layer foreground) :frame)
              (pdata-select-at svg-animation-demo-state
                               '(:layer foreground) :frames))
      (sit-for svg-animation-demo-interval)
      (svg-animation-demo-redraw)
      (pdata-update svg-animation-demo-state '(:type moving)
        :frame (1+ (pdata-select-at svg-animation-demo-state
                                    '(:type moving) :frame))))
    (cond
     ((eq direction 'left-to-right)
      (pdata-update svg-animation-demo-state '(:type moving) :x 3 :y 0))
     ((eq direction 'right-to-left)
      (pdata-update svg-animation-demo-state '(:type moving) :x 0 :y 0)))
    (pdata-update svg-animation-demo-state '(:type moving)
      :type 'static :frame 0)
    (svg-animation-demo-redraw)))

(define-key svg-animation-demo-mode-map (kbd "g") 'svg-animation-demo-redraw)
(define-key svg-animation-demo-mode-map (kbd "i") 'svg-animation-demo-init)
(define-key svg-animation-demo-mode-map (kbd "m") 'svg-animation-demo-move)
