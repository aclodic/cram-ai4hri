(in-package :pepper-dt-demo)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar ?target nil)

(defun init-pepper ()
  ;;(start-ros-node "cram_pepper_demo")
  ;;(init-ros-dt)
  (init-list-discourse))
  ;; (init-ros-pepper)
  ;; (init-ros-dt-pepper))



(defun return-sentence (?intention  &optional (?word nil))
  (ecase ?intention
   (:greet (setf ?sentence (say-hello)))
   (:explain (setf ?sentence (say-explain ?word)))
   (:ask-to-take (setf ?sentence (say-take-cube ?word)))
   (:explain-wrong-cube (setf ?sentence (say-wrong-cube)))
   (:end (setf ?sentence (say-goodbye)))
   (:congrate (setf ?sentence (say-congrate)))
   (:cancel (setf ?sentence (say-cancel)))
   (:inform-scan (setf ?sentence (say-scan)))
   (:inform-rescan (setf ?sentence (say-rescan)))
   (:inform-discover (setf ?sentence (say-discover-cube ?word)))
   (:continue-task (setf ?sentence (say-continue-task)))
   (:impossible-take (setf ?sentence (say-impossible-take)))
   (:ask-for-help (setf ?sentence (say-help ?word)))
   (:question (setf ?sentence (say-question)))
   (:not-understand (setf ?sentence (say-not-understand)))
   (:designate-cube (setf ?sentence (say-designate-cube)))
   (:inform-no-cube (setf ?sentence (say-no-cube-take)))
   (:waiting (setf ?sentence (say-waiting)))
   (:inform-good-cube (setf ?sentence (say-point-good-cube))))
   
 (values ?sentence))


(defun demo ()
 (setq ?target (desig:an object
                (type object)
                (name "cube_BBTG")))
 (let((?targettest ?target))
   (princ ?targettest)
   (top-level
     (with-process-modules-running (pepper-navigation)
       (let ((target (desig:a motion (type looking) (target ?targettest))))
          (pm-execute 'pepper-navigation target))))))


(defun demo-moveto ()
 (let((?x -0.25) (?y 0))
   (top-level
     (with-process-modules-running (pepper-navigation)
       (let ((target (desig:a motion (type moving) (x-val ?x) (y-val ?y))))
          (pm-execute 'pepper-navigation target))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun demo-greet ()
 (let ((?sentence (take-cube-sentence)) (?place (cl-transforms:make-3d-vector 0.25 0.0 0)))
  (top-level
    (with-process-modules-running (pepper-navigation pepper-communication)
      (exe:perform (desig:an action (type greeting) (:greeting-speech ?sentence) (:greeting-place ?place)))))))


(defun greet-human (&key ((:greeting-speech ?sentence)) ((:greeting-place ?place)) &allow-other-keys)

        (exe:perform (desig:an action (type moving) (:to ?place)))
       (exe:perform (desig:an action (type speaking) (:speech ?sentence))))

(defun move-to (&key ((:to ?place)) &allow-other-keys)
    (let ((?x (float (cl-transforms:x ?place) 1.0)) 
          (?y (float (cl-transforms:y ?place) 1.0)))
     (pm-execute 'pepper-navigation  (desig:a motion (type moving) (:x-val ?x) (:y-val ?y)))))
        
(defun speak (&key ((:speech ?sentence)) &allow-other-keys)
;  (let ((?text-to-speak ?sentence))
;       (print ?text-to-speak)
      (pm-execute 'pepper-communication (desig:a motion (type speaking) (:sentence ?sentence))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init-dt ()
 (start-ros-node "cram_pepper_demo_ros_node")
 (pepper-ll::init-tf)
 (init-pepper)
 ;;(init-dt-pepper)
 ;;(init-list-discourse)
 ;;(agin::add-human
 (agin::init-dt "pepper" "eng")  
 (agin::construct-you-agent-desig "human_0"))
  ;;(princ ">>>changing context")(terpri)
 ;;(change-context)
 ;;(i-restart))
 
 



(defun i-scanning ()
 (pepper-ll::look-at)
 (check-onto))


(defun i-restart ()
 (i-scanning)
 (agin::update-cube-list)
 (princ "---> RESTART")(terpri)
 (princ agin::*cubes*)(terpri))

(defun i-perform-dt-step ()

 (let ((response-disambiguate-srv (agin::disambiguate-cube agin::*current-cube*)))
      (setq *sparql-result* (msg-slot-value response-disambiguate-srv :SPARQLRESULT))
      (setq *ambiguous* (msg-slot-value response-disambiguate-srv :AMBIGUOUS)))
 (let ((response-verbalize-srv (agin::call-verbalize-srv *sparql-result*)))
      (setq *verbalization* (msg-slot-value response-verbalize-srv :VERBALIZATION)))
                                
 (say (say-take-cube *verbalization*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







;;demo listening

; (defun demo-greet ()
;  (let ((?sentence (say-hello)) (?place (cl-transforms:make-3d-vector 0.25 0.0 0)))
;   (top-level
;     (with-process-modules-running (pepper-navigation pepper-communication)
;       (exe:perform (desig:an action (type listening) (:understand ?interaction-desig)))))))


;;;;;; demo point at

; (defun speak (&key ((:speech ?sentence)) &allow-other-keys)
; ;  (let ((?text-to-speak ?sentence))
; ;       (print ?text-to-speak)
;       (pm-execute 'pepper-communication (desig:a motion (type speaking) (:sentence ?sentence))))


; (defun point (&key ((:at ?object-designator)) ((:arm ?selected-arm)) &allow-other-keys)
;  (let ((object ?object-designator) (arm ?selected-arm))
;       (exe:perform (desig:an action (type pointing) (:at object) (:arm arm)))))


(defun test-mo ()
 (top-level
      (with-process-modules-running (pepper-navigation)
       (pm-execute 'pepper-navigation  (desig:a motion (type testing))))))

(defun test-int ()
 (top-level
      (with-process-modules-running (process-input)
       (pm-execute 'process-input (desig:an interaction (type understanding) (:input "the cube") (:context nil))))))

(defun test-int-c ()
 (let ((?ctx (agin::reset-context)))
  (write-line (format nil "the context is:~a" ?ctx))
  (top-level
       (with-process-modules-running (process-input)
        (pm-execute 'process-input (desig:an interaction (type understanding) (:input "the cube") (:context ?ctx)))))))


(defun test-you-int ()
 (agin::reset-context)
 (write-line "what is your agent name?")
 (let ((?agent-name (read)))
  (write-line "what do you want?")
  (let ((?message (read)))
   (top-level
        (with-process-modules-running (listening)
         (pm-execute 'listening (desig:an interaction (type receiving) (:from-agent ?agent-name) (:with-content ?message))))))))


(defparameter *count* 0)
(defparameter *mvt-duration* 3)
(defparameter *agent-state* "in-progress")

(defparameter *agent-intention* nil)
(defparameter *agent-goal* nil)
(defparameter *current-cube* (desig:an object
                              (type object)
                              (name agin::*current-cube*)))
(defparameter *human* "human_0")
                  
;;demo

; (defun demo ()
;  (setq ?target (desig:an object
;                 (type object)
;                 (name "cube_BBTG")))
;  (let((?targettest ?target))
;    (princ ?targettest)
;    (top-level
;      (with-process-modules-running (pepper-navigation)
;        (let ((target (desig:a motion (type looking) (target ?targettest))))
;           (pm-execute 'pepper-navigation target))))))

;  (defun speak (&key ((:speech ?sentence)) &allow-other-keys)
; ;  (let ((?text-to-speak ?sentence))
; ;       (print ?text-to-speak)
;       (pm-execute 'pepper-communication (desig:a motion (type speaking) (:sentence ?sentence))))         

;;dt actions
(defun look-at (&key ((:at ?object-desig)) &allow-other-keys)
 (write-line "Performing Action : Looking")
 (let ((?object ?object-desig))
  (pm-execute 'pepper-navigation  (desig:a motion (type looking) (:target ?object)))))

(defun wait-for-agent (&key ((:for ?agent-desig)) &allow-other-keys)
 (let ((?agent ?agent-desig))
  (write-line (format nil "Performing Action : Waiting for ~a" ?agent))))

(defun point (&key ((:at ?object-desig)) &allow-other-keys)
 (write-line "Performing Action : Pointing")
 (let ((?object ?object-desig))
  (pm-execute 'pepper-manipulation  (desig:a motion (type pointing) (:target ?object)))))

(defun start-dt-demo ()
 (agin::update-cube-list)
 (agin::select-current-cube)
 (agin::construct-you-agent-desig "human_0")
 (setf *agent-intention* :greet)
 (top-level
      (with-process-modules-running (pepper-navigation pepper-manipulation pepper-communication)
       (exe:perform (desig:an action (type performing) (:task "dt"))))))

(defun perform-dt-task (&key ((:task ?dt)) &allow-other-keys)
 (occasions-events:on-event (make-instance 'dt-event-detecting-human :you-agent agin::*you-agent-desig*))
 (roslisp:loop-at-most-every 1 
  (occasions-events:on-event (make-instance 'dt-event-play-demo))))

  


(defun reset-goal ()

;;set goal/event to WaitingHuman1
 (occasions-events:on-event (make-instance 'dt-event-detecting-human :you-agent agin::*you-agent-desig*)))

;;event classes for goals

(defclass dt-event-monitor (occasions-events:event)  
 ((agent-to-monitor
   :initarg :you-agent
   :reader you-agent
   :initform nil)
  (the-current-cube
    :initarg :current-cube
    :reader current-cube
    :initform nil)))

(defclass dt-event-play-demo (occasions-events:event) ())
(defclass dt-event-greet-agent (occasions-events:event) ())
(defclass dt-event-instruct-agent (occasions-events:event) ())
(defclass dt-event-request-agent (occasions-events:event) ())
(defclass dt-event-end-demo (occasions-events:event) ())
  ; ((the-current-cube
  ;    :initarg :current-cube
  ;    :reader current-cube
  ;    :initform nil)))
  



(defclass dt-event-detecting-human (occasions-events:event)  
 ((detected-agent
   :initarg :you-agent
   :reader you-agent
   :initform nil)))


(defclass dt-event-instructing-human (occasions-events:event) 
 ((the-current-cube
    :initarg :current-cube
    :reader current-cube
    :initform nil)))



;;event-handlers

(defmethod occasions-events:on-event ((occasions-events:event dt-event-greet-agent))
  (write-line "Current event: greeting agent and current intention: :greet")
 (interaction-tell :greet *human*)
 (setf *agent-intention* :inform))

(defmethod occasions-events:on-event ((occasions-events:event dt-event-instruct-agent))
  (write-line "Current event: instructing agent and current intention: :inform")
  (interaction-tell :inform *human* :task)
 (interaction-tell :explain *human* :task 1)
 (setf *agent-intention* :request))

(defmethod occasions-events:on-event ((occasions-events:event dt-event-request-agent))
  (write-line "Current event: requesting agent to remove cube and current intention: :request")
 (interaction-ask :request *human* :take-cube)
 (setf *agent-intention* :monitor)
 (setf *agent-goal* :wait-for-human1))

(defmethod occasions-events:on-event ((occasions-events:event dt-event-end-demo))
 (write-line "Current event: greeting agent and current intention: :greet")
 (interaction-tell :end *human*) (values nil))
 


(defmethod occasions-events:on-event ((occasions-events:event dt-event-play-demo))
 (write-line "Playing dt demo.")       
 (ecase *agent-intention*
  (:greet (occasions-events:on-event (make-instance 'dt-event-greet-agent)))
  (:inform (occasions-events:on-event (make-instance 'dt-event-instruct-agent)))
  (:request  (occasions-events:on-event (make-instance 'dt-event-request-agent)))
  (:monitor (occasions-events:on-event (make-instance 'dt-event-monitor :you-agent agin::*you-agent-desig*)))
  (:end (occasions-events:on-event (make-instance 'dt-event-end-demo)))))
 

(defmethod occasions-events:on-event ((occasions-events:event dt-event-monitor))
 (write-line "I am monitoring.")
 (cond ((string= *agent-state* "in-progress")
        (progn 
         (setf *count* (+ *count* 1))
        
         (ecase *agent-goal*
          (:wait-for-human1 (cond ((>= *count* (+ *mvt-duration* 2))
                                   (progn 
                                    (write-line "Current goal: wait-for-human1")
                                    (setf *count* 0)
                                          ;;(setf *agent-goal* :look-at-cube1)
                                          ; (occasions-events:on-event (make-instance 'dt-event-instructing-human :current-cube *current-cube*))
                                    (let ((?you-agent (you-agent occasions-events:event)))(princ ?you-agent)
                                     (exe:perform (desig:an action (type waiting) (:for ?you-agent))))
                                    (setf *agent-goal* :look-at-cube1)))))
                                          
          (:look-at-cube1 (cond ((>= *count* (+ *mvt-duration* 2))
                                 (progn 
                                        (write-line "Current goal: look-at-cub1")
                                        (setf *count* 0)

                                        (let ((?current-cube *current-cube*))
                                        ;;(current-cube occasions-events:event)))
                                        (princ ?current-cube)
                                         (exe:perform (desig:an action (type looking) (:at ?current-cube))))
                                        (setf *agent-goal* :wait-for-human2)))))
                                
                                                                               ;;(setf *agent-goal* :wait-for-human2) 
                                                                               ;;(occasions-events:on-event (make-instance 'dt-event-waiting-for-human)))                                                                                            
                                        
          (:wait-for-human2 (cond ((>= *count* (+ *mvt-duration* 2))
                                   (progn 
                                    (write-line "Current goal: wait-for-human2")
                                    (setf *count* 0)
                                    (interaction-tell :inform *human* :waiting)
                                    (let ((?you-agent (you-agent occasions-events:event)))(princ ?you-agent)
                                     (exe:perform (desig:an action (type waiting) (:for ?you-agent))))
                                    (setf *agent-goal* :check-cube-take)))))

                                          ;;(setf *agent-goal* :look-at-cube2)  
                                          ; (occasions-events:on-event (make-instance 'dt-event-instructing-human)))))))))
          (:check-cube-take (cond ((>= *count* (+ *mvt-duration* 2))
                                   (progn 
                                    (write-line "Current goal: check-cube-take")
                                    (setf *count* 0)
                                    (cond 
                                     ((eql (agin::b-cube-taken agin::*current-cube*) nil)

                                      (interaction-tell :congrate *human*)
                                      (setf *agent-intention* :end))

                                     ((progn 
                                       (interaction-tell :inform *human* :wrong-cube)
                                       (interaction-tell :inform *human* :good-cube)
                                       (let ((?current-cube *current-cube*))
                                        ;;(current-cube occasions-events:event)))
                                        (princ ?current-cube)
                                        (exe:perform (desig:an action (type pointing) (:at ?current-cube))))
                                         
                                       (setf *agent-goal* :wait-for-human1)))))))))))

       ((reset-goal))))


; (defmethod occasions-events:on-event ((occasions-events:event dt-event-detecting-human))
;  (write-line "Current event: detecting-human and current goal: :wait-for-human1")
;  (let ((?you-agent (you-agent occasions-events:event)))(princ ?you-agent)
;   (exe:perform (desig:an action (type waiting) (:for ?you-agent))))
;  (setf *agent-goal* :wait-for-human1))

; (defmethod occasions-events:on-event ((occasions-events:event dt-event-instructing-human))
;  (write-line "Current event: instructing-human and current goal: :look-at-cube1")
;  (let ((?current-cube (current-cube occasions-events:event)))(princ ?current-cube)
;   (exe:perform (desig:an action (type looking) (:at ?current-cube))))
;  (setf *agent-goal* :look-at-cube1))

; (defmethod occasions-events:on-event ((occasions-events:event dt-event-waiting-for-human))
;  (write-line "Current event: waiting-for-human and current goal: :wait-for-human2")
;  (interaction-tell :inform *human* :waiting)
;  (setf *agent-goal* :wait-for-human2))


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