(in-package #:nodgui-test)

(defun get-current-time-seconds ()
  (float (/ (get-internal-real-time) internal-time-units-per-second)))

(defun get-thread-by-name (thread-name)
  (loop for thread in (sb-thread:list-all-threads)
        when (string= thread-name (sb-thread:thread-name thread))
        do (return-from get-thread-by-name thread)))

(defun destroy-thread-by-name (thread-name)
  (let ((thread (get-thread-by-name thread-name)))
    (when thread
      (sb-thread:terminate-thread thread))))

(defun make-quit-button ()
  (let ((button (make-instance 'button :text "QUIT" :command (lambda () (exit-nodgui)))))
    (grid button 0 2)
    button))

(defmacro with-nodgui-thread (&body body)
  `(sb-thread:make-thread
    (lambda ()
      (with-nodgui ()
        (wm-title *tk* "Window")
        ,@body))))

(defun kj-to-kcal ()
  (with-nodgui-thread ()
    (wm-title *tk* "kj to kcal")
    (let* ((content (make-instance 'frame))
           (kj-entry (make-instance 'entry :master content :width 12))
           (kcal-label (make-instance 'label :master content :text (format nil "0 kj == 0 kcal")))
           (btn-calculate (make-instance 'button :master content :text "Calculate" :command (lambda () (let ((kj-value (or (ignore-errors (parse-integer (text kj-entry))) 0)))
                                                                                                         (setf (text kcal-label) (format nil "~a kj == ~a kcal" kj-value (float (/ kj-value 4.184)))))))))
      (grid content 0 0 :sticky "nsew")
      (grid kj-entry 0 0 :sticky "nw")
      (grid kcal-label 1 0)
      (grid btn-calculate 2 0)
      )))

(defun event-loop-example ()
  (with-nodgui-thread
    (wm-title *tk* "event-example.lisp")
    (let ((my-style (make-instance 'style))
          (interrupt nil)
          (button (make-instance 'button :text "Start!"))
          (label (make-instance 'label :text "No Answer"))
          (progressbar (make-instance 'progressbar :orientation :horizontal
                                                   :mode :determinate
                                                   :maximum 2000
                                                   :length 100)))

      (defparameter cool-style my-style)
      (grid button 0 1 :padx 5 :pady 5)
      (grid label 0 0 :padx 5 :pady 5)
      (grid progressbar 1 0 :padx 5 :pady 5)

      (labels ((start ()
                 (setf (text button) "Stop!"
                       (command button) #'stop)
                 (setf (text label) "Working ...")
                 (setf interrupt nil)
                 (after 1 #'next))
               (stop ()
                 (setf interrupt t))
               (next (&optional (count 0))
                 (configure progressbar :value count)
                 (if interrupt                                  ; 1
                     (result "")
                     (nodgui:after 1                                   ; 2
                                   #'(lambda ()
                                       (if (= count 2000)
                                           (result 42)
                                           (next (+ 1 count)))))))
               (result (answer)
                 (configure progressbar :value 0)
                 (setf (text button) "Start!"
                       (command button) #'start)
                 (setf (text label)
                       (if (numberp answer)
                           (format nil "Answer: ~a" answer)
                           "No answer"))))
        (setf (command button) #'start)))))

(defun paint-example ()
  (with-nodgui-thread
    (let* ((last-x 0)
           (last-y 0)
           (colour :blue)
           (canvas (make-instance 'canvas :width 500 :height 400 :background :gray75))  ; 1
           (add-line #'(lambda (x y)
                         (configure (make-line canvas (list last-x last-y x y))         ; 2
                                    :fill colour
                                    :width 5
                                    :tag "currentline")                                 ; 3
                         (setf last-x x
                               last-y y))))

      (grid canvas 0 0 :sticky "news")
      (grid-columnconfigure *tk* 0 :weight 1)
      (grid-rowconfigure *tk* 0 :weight 1)

      (bind canvas "<1>" #'(lambda (evt) (setf last-x (event-x evt)                     ; 4
                                               last-y (event-y evt))))
      (bind canvas "<B1-Motion>"                                                        ; 5
            #'(lambda (evt) (funcall add-line (event-x evt) (event-y evt))
                (tag-configure canvas "currentline" :width (+ 5 (* 5 (sin (get-current-time-seconds)))))))
      (bind canvas "<B1-ButtonRelease>"                                                 ; 6
            #'(lambda (evt) (tag-configure canvas "currentline" :width 1)))

      ;; add three rectangles, and option to change colour
      (let ((r (make-rectangle canvas 10 10 30 30)))
        (configure r :fill :red)
        (bind r "<1>" #'(lambda (evt) (setf colour :red))))                             ; 7
      (let ((r (make-rectangle canvas 10 35 30 55)))
        (configure r :fill :blue)
        (bind r "<1>" #'(lambda (evt) (setf colour :blue))))
      (let ((r (make-rectangle canvas 10 60 30 80)))
        (configure r :fill :black)
        (bind r "<1>" #'(lambda (evt) (setf colour :black)))))))

(defun image-selection-example ()
  (with-nodgui-thread ()
    (wm-title *tk* "image selection")
    (let ((content (make-instance 'frame)))
      (configure content :padding "3 3 12 12")
      (grid content 0 0 :sticky "nsew")
      (grid-columnconfigure *tk* 0 :weight 1)
      (grid-rowconfigure *tk* 0 :weight 1)

      (let* ((image-file "3.jpg")
             (my-cool-image (make-image image-file 64 64))
             (my-image-label (make-instance 'label :master content :image my-cool-image)))
        (grid my-image-label 0 0)
        (grid (make-instance 'button :master content :text "Choose image" :command (lambda ()
                                                                                     (let ((result (get-open-file :title "Choose an image file")))
                                                                                       (when result
                                                                                         (configure my-image-label :image (make-image result 64 64))))))
              0 1)))))

(defun label-example ()
  (sb-thread:make-thread
   (lambda ()
     (with-nodgui ()
       (unwind-protect
            (progn
              (wm-title *tk* "labels and images")
              (let* ((label-1 (make-instance 'label :text "Simple text label"))     ; 1
                     (image (make-image "3.jpg" 64 64))                ; 2
                     (label-2 (make-instance 'label :image image))                  ; 3
                     (label-3 (make-instance 'label                                 ; 4
                                             :font "Helvetica 14 bold"
                                             :text (format nil "Some text~&on multiple~&lines")))
                     (label-4 (make-instance 'label :image image :text "gaba")) ; 5
                     (label-5 (make-instance 'label :image image :text "Tcl Logo"))
                     (label-6 (make-instance 'label :image image :text "ghouls"))
                     (text "Dad... I'm a... I'm not a crazed gunman dad, I'm an assassin. Well the difference being ones a job and the other's mental sickness!")
                     (label-7 (make-instance 'label :text text :wraplength 300)))   ; 6

                (configure label-4 :compound :bottom)                               ; 7
                (configure label-5 :compound :center)
                (configure label-6 :compound :top)
                
                (grid label-1 0 0)
                (grid label-2 0 1)
                (grid label-3 0 2)
                (grid label-4 1 0)
                ;;(grid label-5 1 1)
                (grid label-6 1 2)
                (grid label-7 2 0 :columnspan 3 :sticky "news")))
         (exit-nodgui))
))))

(defun main ()
  (with-nodgui-thread ()
    (wm-title *tk* "Feet to Metres") ; 1
    (let ((content (make-instance 'frame))) ; 2
      (configure content :padding "3 3 12 12") ; 3
      (grid content 0 0 :sticky "nsew")
      (grid-columnconfigure *tk* 0 :weight 1)
      (grid-rowconfigure *tk* 0 :weight 1)

      (let* ((feet-entry (make-instance 'entry :master content :width 60)) ; 4
             (metres-label (make-instance 'label :master content :text "" :font "Courier a"))
             (quit-button (make-quit-button)))
        (flet ((calculate () ; 5
                 (let ((feet (ignore-errors (eval (read-from-string (format nil "(progn (in-package :nodgui-test) ~a)"(text feet-entry)))))))
                   (setf (text metres-label)
                         (if (numberp feet)
                             (/ (round (* 0.3048 feet 10000.0)) 10000.0)
                             "Input not a valid number")))))
                                        ; top row has the entry widget and explanatory label to its right
          (grid feet-entry 1 2 :sticky "we" :padx 5 :pady 5) ; 6
          (grid (make-instance 'label :master content :text "feet")
                1 3 :sticky "w" :padx 5 :pady 5)
                                        ; middle row has three labels
          (grid (make-instance 'label :master content :text "is equivalent to")
                2 1 :sticky "e" :padx 5 :pady 5)
          (grid metres-label 2 2 :sticky "we" :padx 5 :pady 5)
          (grid (make-instance 'label :master content :text "metres")
                2 3 :sticky "w" :padx 5 :pady 5)
                                        ; last row has the button on right
          (grid (make-instance 'button :master content ; 7
                                       :text "Calculate"
                                       :command #'calculate)
                3 3 :sticky "w" :padx 5 :pady 5)

          (focus feet-entry) ; 8
          (bind *tk* "<Return>" (lambda (evt) (calculate))))))      ))
