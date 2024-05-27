(in-package #:nodgui-test)


(defun main ()
  (with-nodgui ()
    (wm-title *tk* "Feet to Metres")                                          ; 1
    (let ((content (make-instance 'frame)))                                   ; 2
      (configure content :padding "3 3 12 12")                                ; 3
      (grid content 0 0 :sticky "nsew")
      (grid-columnconfigure *tk* 0 :weight 1)
      (grid-rowconfigure *tk* 0 :weight 1)

      (let* ((feet-entry (make-instance 'entry :master content :width 7))     ; 4
             (metres-label (make-instance 'label :master content :text "")))
        (flet ((calculate ()                                                  ; 5
                 (let ((feet (eval (read-from-string (text feet-entry)))))
                   (setf (text metres-label)
                         (if (numberp feet)
                             (/ (round (* 0.3048 feet 10000.0)) 10000.0)
                             "Input not a valid number")))))
                                        ; top row has the entry widget and explanatory label to its right
          (grid feet-entry 1 2 :sticky "we" :padx 5 :pady 5)                  ; 6
          (grid (make-instance 'label :master content :text "feet")
                1 3 :sticky "w" :padx 5 :pady 5)
                                        ; middle row has three labels
          (grid (make-instance 'label :master content :text "is equivalent to")
                2 1 :sticky "e" :padx 5 :pady 5)
          (grid metres-label 2 2 :sticky "we" :padx 5 :pady 5)
          (grid (make-instance 'label :master content :text "metres")
                2 3 :sticky "w" :padx 5 :pady 5)
                                        ; last row has the button on right
          (grid (make-instance 'button :master content                        ; 7
                                       :text "Calculate"
                                       :command #'calculate)
                3 3 :sticky "w" :padx 5 :pady 5)

          (focus feet-entry)                                                  ; 8
          (bind *tk* "<Return>" (lambda (evt) (calculate))))))      ))
