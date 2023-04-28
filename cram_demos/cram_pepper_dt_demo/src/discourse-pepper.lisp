(in-package :pepper-dt-demo)

(defvar *onto* nil)
(defvar *list-greeting* nil)
(defvar *list-task* nil)
(defvar *list-explanation* nil)
(defvar *list-please-take* nil)
(defvar *list-wrong-cube* nil)
(defvar *list-end* nil)
(defvar *list-help* nil)
(defvar *list-congrate* nil)
(defvar *list-cancel* nil)
(defvar *list-scan* nil)
(defvar *list-rescan* nil)
(defvar *list-discover* nil)
(defvar *list-waiting* nil)
(defvar *list-point-good-cube* nil)
(defvar *list-no-cube-take* nil)
(defvar *list-continue* nil)
(defvar *list-designate-cube* nil)
(defvar *list-not-understand* nil)
(defvar *list-question* nil)
(defvar *list-impossible-take* nil)

; (defun init-onto (onto)
;     (setq *onto* onto)
;     (init-list-discourse)
;     (princ (hello)))

(defun init-list-discourse (&optional (word ""))
 (setq *list-greeting* ; tell greet
     (list "Hello i am pepper" "Hello my name is pepper"))
 (setq *list-task* ;tell inform task
           (list "We will do a task together" "I hope you are willing to help me" "I count on you to help me"))
 (setq *list-explanation* ;tell explain task
            (list (format nil "I'm going to ask you to remove ~a  cubes and put them in the pink box." word) (format nil "I'll describe you ~a  cubes to put in the pink box" word ) (format nil "We'll put ~a  the cubes in the pink box. I'll describe them to you" word)))
 (setq *list-please-take* ;ask request task
           (list (format nil "Can you remove ~a" word) (format nil "Remove ~a" word) (format nil "Take ~a" word)))
 (setq *list-wrong-cube* ;tell inform wrong-cube
           (list "I think you picked the wrong cube." "It seems to me that it is not this cube" "I wasn't thinking about this cube"))
 (setq *list-end* ;tell end
          (list "Congratulations to you! Thank you for helping me. See you soon" "Great! Thanks for helping me. See you soon" "Well done  we did it. Thank you and see you soon"))
 (setq *list-help* ;ask help
            (list (format nil "~a can you help us?" word) (format nil "can ~a you help us?" word))) 
 (setq *list-congrate* ;tell congrate
            (list "Bravo it was the one" "Very good  it was this cube" "super it was him"))
 (setq *list-cancel* ;tell cancel
            (list "We will finish another time"))
 (setq *list-scan* ;tell inform scan
            (list "Give me some time to look at what's in the boxes" "Let's look at the objects there."))
 (setq *list-rescan* ;tell inform rescan
            (list "Thank you  I will check what has moved" "Let's see what has moved"))
 (setq *list-discover* ;tell inform discover
            (list (format nil "I saw a ~a" word) (format nil "There is a ~a" word) (format nil "Oh  a ~a" word)))
 (setq *list-waiting* ;tell inform wait
            (list "I am waiting for you to take it" "I think the cube is still there")) ;;instead of did you take it
 (setq *list-point-good-cube* ;tell inform good-cube
            (list "I was thinking more like this one" "I wanted to talk about this cube"))
 (setq *list-no-cube-take* ;tell inform no-cube
            (list "For me the cube is always here" "I have the impression that the cube is still here"))
 (setq *list-continue* ;tell continue
             (list "Let's go to the next one" "Let's go on"))
 (setq *list-designate-cube* ;tell inform designate-cube
             (list "I think you mean this one" "You want to talk about this cube" "Here is the cube you are talking about"))
 (setq *list-not-understand* ;tell inform not-understand
     (list "I didn't quite understand"  "I'm not sure I understood"  "Sorry  can you repeat that"))
 (setq *list-question* ;ask question
     (list "Which one do you want to enter " "Is that what you mean " "Do you mean  "))
 (setq *list-impossible-take* ;tell inform impossible-take
     (list "I can't catch anything but I'll show it to you"  "I'll show it to you instead because I can't catch it"  "Since I can't catch it  I'll show it to you")))  
      
(defun say-hello ()
 (let ((n (+ 1 (random (length *list-greeting*)))))
      (nth (- n 1) *list-greeting*)))

(defun say-task ()
 (let ((n (+ 1 (random (length *list-task*)))))
      (nth (- n 1) *list-task*)))

(defun say-wrong-cube ()
       (let ((n (+ 1 (random (length *list-wrong-cube*)))))
        (nth (- n 1) *list-wrong-cube*)))

(defun say-goodbye ()
    (let ((n (+ 1 (random (length *list-end*)))))
     (nth (- n 1) *list-end*)))

(defun say-continue-task ()
    (let ((n (+ 1 (random (length *list-continue*)))))
     (nth (- n 1) *list-continue*)))

(defun say-congrate ()
    (let ((n (+ 1 (random (length *list-congrate*)))))
     (nth (- n 1) *list-congrate*)))

(defun say-cancel ()
    (let ((n (+ 1 (random (length *list-cancel*)))))
     (nth (- n 1) *list-cancel*)))

(defun say-scan ()
    (let ((n (+ 1 (random (length *list-scan*)))))
     (nth (- n 1) *list-scan*)))

(defun say-rescan ()
    (let ((n (+ 1 (random (length *list-rescan*)))))
     (nth (- n 1) *list-rescan*)))

(defun say-waiting ()
    (let ((n (+ 1 (random (length *list-waiting*)))))
     (nth (- n 1) *list-waiting*)))

(defun say-point-good-cube ()
    (let ((n (+ 1 (random (length *list-point-good-cube*)))))
     (nth (- n 1) *list-point-good-cube*)))

(defun say-no-cube-take ()
    (let ((n (+ 1 (random (length *list-no-cube-take*)))))
     (nth (- n 1) *list-no-cube-take*)))

(defun say-designate-cube ()
    (let ((n (+ 1 (random (length *list-designate-cube*)))))
     (nth (- n 1) *list-designate-cube*)))

(defun say-not-understand ()
    (let ((n (+ 1 (random (length *list-not-understand*)))))
     (nth (- n 1) *list-not-understand*)))

(defun say-question ()
    (let ((n (+ 1 (random (length *list-question*)))))
     (nth (- n 1) *list-question*)))

(defun say-impossible-take ()
    (let ((n (+ 1 (random (length *list-impossible-take*)))))
     (nth (- n 1) *list-impossible-take*)))

(defun say-discover-cube (cube)
         (let ((?word cube))
          (init-list-discourse ?word)
          (let ((n (+ 1 (random (length *list-discover*)))))
           (nth (- n 1) *list-discover*))))
 
(defun say-help (name)
    (let ((?word name))
     (init-list-discourse ?word)
     (let ((n (+ 1 (random (length *list-help*)))))
      (nth (- n 1) *list-help*))))

(defun say-explain (nb)
     (let ((?word nb))
      (init-list-discourse ?word)
      (let ((n (+ 1 (random (length *list-explanation*)))))
       (nth (- n 1) *list-explanation*))))

(defun say-take-cube (cube)
    (let ((?word cube))
     (init-list-discourse ?word)
     (let ((n (+ 1 (random (length *list-please-take*)))))
      (nth (- n 1) *list-please-take*))))

(defun take-cube-sentence ()
 (let ((cube (dt::get-current-cube-verbalization)))
  (say-take-cube cube)))
