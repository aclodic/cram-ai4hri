(in-package :agin)

(defparameter *you-interact-desig* nil)
(defparameter *me-interact-desig* nil)

(defvar *point-at-msg* "point at the blue cube with a circle")
(defvar *reply-msg* nil)
(defvar *reply-state* nil)

(defun construct-you-interaction-designator (agent-name message)
 (let ((?message message)(result-msg (construct-message-desig message)) (?from-agent-desig (construct-you-agent-desig agent-name))) 
      (let ((?from-msg-desig (nth 0 result-msg))) 
       (setf *reply-msg* (nth 1 result-msg)) 
       (setf *reply-state* (nth 2 result-msg))
       (setf *you-interact-desig*
        (desig:an interaction (type :receiving) 
                              (:from-agent ?from-agent-desig)
                              (:with-content ?message) 
                              (:from-msg ?from-msg-desig)))
       (multiple-value-list (values *you-interact-desig* *reply-msg* *reply-state*)))))

; (defun construct-me-interaction-designator (message)
;  (let ((?message message))
;   (cond ((eql *you-interact-desig* nil)
;          (write-line "There is nothing to reply to"))
;         ((progn 
;           (let ((?to-agent-desig (desig-prop-value *you-interact-desig* :from-agent)) 
;                 (?to-msg-desig (desig-prop-value *you-interact-desig* :from-msg)))
;            (setf *me-interact-desig*
;             (desig:an interaction (type :replying) 
;                                   (:to ?to-agent-desig)
;                                   (:with-content ?message) 
;                                   (:to-msg ?to-msg-desig)))))))))

(defun construct-me-interaction-designator (message intention topic agent)
 (let ((?message message) (?intention intention) (?agent-name agent) (?topic topic))
;   (cond ((eql *you-interact-desig* nil)
;          (write-line "There is nothing to reply to"))
;         ((progn 
      (let ((?to-agent-desig (construct-you-agent-desig ?agent-name)))
          ;; (?to-agent-desig (desig-prop-value *you-interact-desig* :from-agent))) 
          ;;  (?to-msg-desig (desig-prop-value *you-interact-desig* :from-msg)))
       (setf *me-interact-desig*
        (desig:an interaction (type ?intention)
                              (:about ?topic)
                              (:to ?to-agent-desig)
                              (:with-content ?message)))))
 (write-line (format nil "The current me-interaction-designator is: ~a" *me-interact-desig*)))
                                ;;  ))) 
                                 ;; (:to-msg ?to-msg-desig)))))))))
; (defun get-reply-msg-state (agent-name message)

;    (let ((you-desig (construct-you-interaction-designator agent-name message)))
;     (let ((reply-msg (nth 1 you-desig)) (reply-state (nth 2 you-desig))))

(defun demo-conversation ()
 (reset-context)
 (write-line "what is your agent name?")
 (let ((agent-name (read)))
  (write-line "what do you want?")
  (let ((message (read)))
   (let ((you-desig (construct-you-interaction-designator agent-name message)))
    (let ((reply-msg (nth 1 you-desig)) (reply-state (nth 2 you-desig)))
     (let ((me-desig (construct-me-interaction-designator reply-msg "test" "testhum" "estop")))
      (values (nth 0 you-desig) me-desig)
      (cond ((eql reply-state nil)
             (progn 
              (write-line reply-msg)
              (let ((reply (read)))
               (let ((you-desig (construct-you-interaction-designator agent-name reply)))
                (let ((reply-msg (nth 1 you-desig)) (reply-state (nth 2 you-desig)))
                 (let ((me-desig (construct-me-interaction-designator reply-msg "test" "testhum" "estop")))
                  (values (nth 0 you-desig) me-desig))))))))))))))
            
(defun reply-to-msg ()
     (let ((me-desig (construct-me-interaction-designator *reply-state* "test" "testhum" "estop")))))
    ;   (cond ((eql *reply-state* nil)
    ;          (progn 
    ;           (write-line *reply-msg*)
    ;           (let ((reply (read)))
    ;            (let ((you-desig (construct-you-interaction-designator agent-name reply)))
    ;             (let ((reply-msg (nth 1 you-desig)) (reply-state (nth 2 you-desig)))
    ;              (let ((me-desig (construct-me-interaction-designator reply-msg)))
    ;               (values (nth 0 you-desig) me-desig)))))))))))))
            
         
                             