(in-package :dt)


(defparameter object-property nil)
(defparameter property-value nil)
(defparameter object-of-interest nil)
(defparameter *communicated-action* nil)
(defparameter *communicated-object* nil)
(defparameter *communicated-sparql-query* nil)
(defvar type-set nil)
(defvar *question* "")



;;understand service

; (defun understand-call (message)
;  "Call the KSP understand service"
;  (call-get-srv message))

(defun get-sparql-query (message)
 (msg-slot-value (understand-call message) :SPARQLQUERY))

(defun get-action (message)
 (msg-slot-value (understand-call message) :ACTION))

 ;;merge service 

; (defun merge-query-call (message)
;  "Call the KSP merge service"
;  (call-merge-srv (get-sparql-query message) (get-context) nil))

(defun merge-query-call (query context partial)
 "Call the KSP merge service"
 ;;(let ((merge-query-response (call-merge-srv query context partial)))
  (msg-slot-value (call-merge-srv query context partial) :MERGED_QUERY))

; (defun get-merged-query (message)
;  (msg-slot-value (merge-query-call message) :MERGED_QUERY))

;;sparql service
;;(defun sparql-call (query))

(defun disambiguate-matches (matches merge-sparql)
 (set-new-context merge-sparql)
 (setf *question-part* nil)
 (setf *question* "")
 (let ((new-context merge-sparql))

     (loop for match in (coerce matches 'list) do
         (let ((dis-resp (disambiguate-call match new-context)))
          (let ((sparql-result (nth 0 dis-resp)) (ambiguous (nth 1 dis-resp)))
              (write-line (format nil "Sparql result: ~a" sparql-result))

              (cond
                  ((not (coerce ambiguous 'list))
                   (setq *match-sparql* sparql-result))
                  ((values "not understand")))
              (write-line (format nil "sparql : ~a" *match-sparql*))   
              (setq *match-sparql* (merge-query-call *match-sparql* new-context t))
              (setq *question-part* (verbalization-call *match-sparql*))
              (write-line  (format nil "Question part: ~a" *question-part*))))

         (cond
             ((string= *question* "")
              (setq *question*  *question-part*))
             ((setq *question*  (concatenate 'string *question* ", or, " *question-part*)))))
     (let ((message (format nil "Do you mean: ~a?" *question*)))
      (write-line message)
      ;;(construct-me-interaction-designator message)
      (values *question*))))
 
 
(defun verbalization-call (query)
 (msg-slot-value (call-verbalize-srv query) :VERBALIZATION))

(defun check-sparql-matches (matches)
  (cond
             ((> (length matches) 1)
              (progn
                   (let ((message "I am not sure what you are speaking about. ")) 
                    (write-line message)
                   ;; (construct-me-interaction-designator message)
                    (multiple-value-list (values "multiple matches" message)))))

             ((= (length matches) 1)
              (progn
                    (let ((message "I think I understand what you want. ")) 
                     (write-line message)
                     ;;(construct-me-interaction-designator message)
                     (multiple-value-list (values "one match" message)))))
            
             ((progn
                   (let ((message "I do not have knowledge about this. ")) 
                    (write-line message)
                    ;;(construct-me-interaction-designator message)
                    (multiple-value-list (values "no match" message)))))))
             
 
; (defun check-sparql-matches (matches)
;   (cond
;              ((> (length matches) 1)
;               (progn
;                   (write-line "I am not sure of what you are speaking about. I need to disambiguate your input")
;                   (values "multiple matches")))

;              ((= (length matches) 1)
;               (progn
;                   (write-line "I think I understand what you want")
;                   (values "one match")))
            
;              ((progn
;                   (write-line "I dont have knowledge about this.")
;                   (values "no match")))))
            


(defun sparql-matches-call (query)
 (msg-slot-value (call-sparql-srv query) :RESULTS))



(defun disambiguate-call (match query)
  (let ((response-disambiguate-srv (call-disambiguate-srv match query)))
       (let ((sparql-result (msg-slot-value response-disambiguate-srv :SPARQLRESULT))
             (ambiguous (msg-slot-value response-disambiguate-srv :AMBIGUOUS)))
        (multiple-value-list (values sparql-result ambiguous)))))

; (defun understand-response (message) 
;  (multiple-value-bind
;   (sparql-query communicated-action)
;   (understand-call message) 
;   (list sparql-query communicated-action)))

(defun disambiguate-response (match query) 
 (multiple-value-bind
  (sparql-result ambiguous)
  (disambiguate-call match query) 
  (list sparql-result ambiguous)))
; (defun understand-response (message &optional (context nil)) 
;  (multiple-value-bind
;   (communicated-sparql-query communicated-action communicated-object)
;   (understand-message message context) 
;   (list communicated-sparql-query communicated-action communicated-object)))

(defun understand-call (message)
 (let ((response-understand-srv (call-understand-srv message)))

  (let ((sparql-query (msg-slot-value response-understand-srv :SPARQLQUERY))
        (communicated-action (msg-slot-value response-understand-srv :ACTION)))
        
   (multiple-value-list (values sparql-query communicated-action)))))

; (defun understand-message (message &optional (context nil))
;  (let ((response-understand-srv (understand-call message)))
;   (setf *communicated-sparql-query* (msg-slot-value response-understand-srv :SPARQLQUERY))
;   (setf *communicated-action* (msg-slot-value response-understand-srv :ACTION)))
;  (setf *communicated-object* (get-object message "?0" context))
        
;  (values *communicated-sparql-query* *communicated-action* *communicated-object*))

