(in-package :pepper-dt-demo)

;;navigation
(def-process-module pepper-navigation (motion-designator)
  (roslisp:ros-info (pepper-process-modules)
                    "pepper navigation invoked with motion designator `~a'."
                    motion-designator)
  (destructuring-bind (command motion) (reference motion-designator)
    (ecase command
     (look
             ( let (
                    (target (pepper-looking-motion-target motion)))
                   ;; (arm (pepper-looking-motion-arm motion))
               
              (let ((my-target-type (desig:desig-prop-value target :type)))

               (cond 
                ((string= my-target-type
                      :object)

                 (let ((obj (desig:desig-prop-value target :name)))
                    (print obj)
                    (pepper-ll::look-object obj)))
             
                ((princ "no object to look at"))))))      
             
     (move-to
            (let ((x  (pepper-moving-motion-x-val  motion))
                  (y  (pepper-moving-motion-y-val  motion)))
              (pepper-ll::call-move-to-srv x y))))))  


 ;;manipulation
(def-process-module pepper-manipulation (motion-designator)
  (roslisp:ros-info (pepper-process-modules)
                    "pepper navigation invoked with motion designator `~a'."
                    motion-designator)
  (destructuring-bind (command motion) (reference motion-designator)
    (ecase command
     (point
             ( let (
                    (target (pepper-manipulation-motion-target motion)))
                   ;; (arm (pepper-looking-motion-arm motion))
               
              (let ((my-target-type (desig:desig-prop-value target :type)))

               (cond 
                ((string= my-target-type
                      :object)

                 (let ((obj (desig:desig-prop-value target :name)))
                    (print obj)
                    (pepper-ll::point-at-object obj)))
             
                ((princ "no object to look at")))))))))


  ;;commmunication           
(def-process-module pepper-communication (motion-designator)
 (roslisp:ros-info (pepper-process-modules)
                   "pepper communication invoked with motion designator `~a'."
                   motion-designator)
 (destructuring-bind (command motion) (reference motion-designator)
   (ecase command         
    (speak
            
           (let ((sentence  (pepper-communication-motion-sentence motion)))
            (print sentence)
            (pepper-ll::call-say-srv sentence))))))                  


(def-process-module listening (interaction-designator)
 (roslisp:ros-info (pepper-process-modules)
                   "processing input invoked with interaction designator `~a'."
                   interaction-designator)
 (destructuring-bind (command interaction) (reference interaction-designator)

   (ecase command

    (understand       
     (let ((input (understand-interaction-input interaction))
           (context (understand-interaction-context interaction)))
      (write-line "I am trying to understand your sentence")
      (cond ((eql context nil) (agin::understand-call input)) 
       ((agin::merge-query-call (nth 0 (agin::understand-call input)) context nil)))))
      
      
    
    (receive-input      
     (let ((agent-name (receive-interaction-from-agent interaction))
           (content (receive-interaction-with-content interaction)))
      (agin::construct-you-interaction-designator agent-name content))))))
      
      
       
              

(def-process-module telling (interaction-designator)
 (roslisp:ros-info (pepper-process-modules)
                   "processing input invoked with interaction designator `~a'."
                   interaction-designator)
 (destructuring-bind (command interaction) (reference interaction-designator)

   (ecase command

    (reply

      (agin::reply-to-msg))
      
    (tell (agin::construct-me-interaction-designator (say-hello) "greeting" "human_0" "estop")))))
       
      
      
       
              

; (def-process-module listen (interaction-designator)
;  (roslisp:ros-info (pepper-process-modules)
;                    "processing input invoked with interaction designator `~a'."
;                    interaction-designator)
;  (destructuring-bind (command interaction) (reference interaction-designator)

;    (ecase command

;     (understand       
;      (let ((input (understand-interaction-input interaction))
;            (context (understand-interaction-context interaction)))
;       (write-line "I am trying to understand your sentence")
;       (cond ((eql context nil) (agin::understand-call input)) 
;       ((agin::merge-query-call (nth 0 (agin::understand-call input)) context nil))
      
;       )))
    
;     (receive-input      
;      (let ((agent-name (receive-interaction-from-agent interaction))
;            (content (receive-interaction-with-content interaction)))
;       (agin::construct-you-interaction-designator agent-name content)
      
;       ))
;       ) 
;       ))     