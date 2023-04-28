(in-package :agin)

(defparameter *count* 0)
(defparameter *mvt-duration* 3)
(defparameter *agent-state* "in-progress")
(defparameter *agent-goal* nil)

                  
;;demo

(defun init-dt-demo ()

;;set goal/event to WaitingHuman1

 (occasions-events:on-event (make-instance 'dt-event-detecting-human))
 (roslisp:loop-at-most-every 1  (occasions-events:on-event (make-instance 'monitor))))


(defun reset-goal ()

;;set goal/event to WaitingHuman1
 (occasions-events:on-event (make-instance 'dt-event-detecting-human)))

;;agent's states:
  
(defun pepper-state (state)
  (ecase state
             (:ready 0)
             (:scanning 1)
             (:in-progress 2)
             (:in-help 3)
             (:no-activity 4)
             (:listening 5))) 


; (defun agent-goal (goal)
;   (ecase goal
;              (:waiting-human-1 0)
;              (:look-at-cube 1)
;              (:waiting-human 2)
;              (:look-at-exp 3)
;              (:look-at-cube-2 4)
;              (:idle 5))) 


;;event classes for goals

(defclass monitor (occasions-events:event) ())


(defclass dt-event-detecting-human (occasions-events:event) ())


(defclass dt-event-instructing-human (occasions-events:event) ())


(defclass dt-event-waiting-for-human (occasions-events:event) ())


(defclass dt-event-looking-at-cube (occasions-events:event) ())

;; event classes for cube takes

(defclass dt-event-detecting-wrong-cube-take (occasions-events:event)
 ((no-cube-taken
   :initarg :no-take 
   :reader no-take
   :initform nil)))


(defclass dt-event-detecting-good-cube-take  (occasions-events:event) ())


;;event-handlers

(defmethod occasions-events:on-event ((occasions-events:event monitor))
 (write-line "I am monitoring.")
 (cond ((string= *agent-state* "in-progress")
        (progn 
         (setf *count* (+ *count* 1))
        
         (ecase *agent-goal*
          (:wait-for-human1 (cond ((>= *count* (+ *mvt-duration* 2))
                                   (progn 
                                          (setf *count* 0)
                                          ;;(setf *agent-goal* :look-at-cube1)
                                          (occasions-events:on-event (make-instance 'dt-event-instructing-human))))))
          (:look-at-cube1 (cond ((>= *count* (+ *mvt-duration* 2))
                                 (progn 
                                        (setf *count* 0)
                                        ;;(setf *agent-goal* :wait-for-human2) 
                                        (occasions-events:on-event (make-instance 'dt-event-waiting-for-human))))))
                                        
                                        
          (:wait-for-human2 (cond ((>= *count* (+ *mvt-duration* 2))
                                   (progn 
                                          (setf *count* 0)
                                          ;;(setf *agent-goal* :look-at-cube2)  
                                          (occasions-events:on-event (make-instance 'dt-event-instructing-human)))))))))
               

  ((reset-goal))))


(defmethod occasions-events:on-event ((occasions-events:event dt-event-detecting-human))
 (write-line "Current event: detecting-human and current goal: :wait-for-human1")
;  (cond ((>= *count* (+ *mvt-duration* 2))
;         (progn 
;                (setf *count* 0)
; ;;(perform lookobject)
;          (princ "looking now at cube1")
;          (occasions-events:on-event (make-instance 'looking-at-cube1))))))
 (setf *agent-goal* :wait-for-human1))

(defun greet-human (&key ((:greeting-speech ?sentence)) ((:greeting-place ?place)) &allow-other-keys)

        (defun greet-human (&key ((:greeting-speech ?sentence)) ((:greeting-place ?place)) &allow-other-keys)

         (exe:perform (desig:an action (type moving) (:to ?place)))
         (exe:perform (desig:an action (type speaking) (:speech ?sentence))))

 (defun move-to (&key ((:to ?place)) &allow-other-keys)
     (let ((?x (float (cl-transforms:x ?place) 1.0)) 
           (?y (float (cl-transforms:y ?place) 1.0)))
      (pm-execute 'pepper-navigation  (desig:a motion (type moving) (:x-val ?x) (:y-val ?y)))))
 (exe:perform (desig:an action (type speaking) (:speech ?sentence))))

(defun move-to (&key ((:to ?place)) &allow-other-keys)
    (let ((?x (float (cl-transforms:x ?place) 1.0)) 
          (?y (float (cl-transforms:y ?place) 1.0)))
     (pm-execute 'pepper-navigation  (desig:a motion (type moving) (:x-val ?x) (:y-val ?y)))))

(defmethod occasions-events:on-event ((occasions-events:event dt-event-instructing-human))
 (write-line "Current event: instructing-human and current goal: :look-at-cube1")
 (exe:perform (desig:an action (type looking) (:at ?object)))
;  (cond ((>= *count* (+ *mvt-duration* 2))
;         (progn 
;                (setf *count* 0)
; ;;(perform lookobject)
;          (occasions-events:on-event (make-instance 'waiting-for-human2))))))
 (setf *agent-goal* :look-at-cube1))

(defmethod occasions-events:on-event ((occasions-events:event dt-event-waiting-for-human))
 (write-line "Current event: waiting-for-human and current goal: :wait-for-human2")
;  (cond ((>= *count* (+ *mvt-duration* 2))
;         (progn 
;                (setf *count* 0)
; ;;(perform lookobject)
;          (occasions-events:on-event (make-instance 'waiting-for-human2))))))
 (setf *agent-goal* :wait-for-human2))


; (defmethod occasions-events:on-event ((occasions-events:event waiting-for-human2))
;  (write-line "I am waiting now for human2")
;  (cond 
;   ((eql *count* *mvt-duration*))
;    ;(perform sayWaiting))
   
;   ((>= *count* (+ *mvt-duration* 2))
;   ;; (perform lookObject)
;    (occasions-events:on-event (make-instance 'looking-at-cube2)))))


; (defmethod occasions-events:on-event ((occasions-events:event looking-at-cube2))
;  (write-line "I am looking now at cube2")
;  (cond ((>= *count* (+ *mvt-duration* 2))
;         (progn 
;          (setf *count* 0)
; ;;(perform lookobject)))
;          (occasions-events:on-event (make-instance 'wrong-cube-take :no-take t))))))


; ; ;; event-handlers for cube takes

; (defmethod occasions-events:on-event ((occasions-events:event wrong-cube-take))
; ;;(perform lookAt)
;  (cond 
;   ((eql (no-take event) t))))
;    ;(perfom sayNoCubeTake))
   
;    ;(perform sayWaiting))
   
;  ;; ((perform sayWrongCubeTake))
;   ;; (perform lookObject)
;   ;; (perform lookExperimentator)
;   ;; (perform pointAt)
; ;;   (perform sayhelp)
;   ;; (occasions-events:on-event (make-instance 'looking-at-cube2)))))