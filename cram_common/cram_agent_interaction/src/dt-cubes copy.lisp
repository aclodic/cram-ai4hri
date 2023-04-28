(in-package :agin)

(defvar *result* nil)
(defvar *state* nil)
(defvar *ctx-designate* (vector (format nil "?0 isAbove ~a" *table*)
                                "?0 isInContainer ?1" "?1 isA VisibleDtBox"))
(defvar *action* nil)
(defvar *sparql* nil)
(defvar *question* nil)
(defvar *merge-sparql* nil)
(defvar *matches* nil)
(defvar *new-context* nil)
(defvar *sparql-result* nil)
(defvar *new-context* nil)
(defvar *ambiguous* nil)
(defvar *match-sparql* nil)
(defvar *question-part* nil)
(defvar *cheat-list* (vector "?0 isAbove table_1" "?0 isInContainer ?a" "?a isA VisibleDtBox"))
(defvar *no-cheat-list* (vector "?0 isAbove table_1"))
(defvar base-facts-triples nil)

(defun designate-cube (sentences)
 (setf *table* "table_1")
    (setq *ctx-designate* (vector (format nil "?0 isAbove ~a" *table*)
                                "?0 isInContainer ?1" "?1 isA VisibleDtBox"))

    (let ((response-understand-srv (call-understand-srv sentences)))
        (setq *action* (msg-slot-value response-understand-srv :ACTION))
        (setq *sparql* (msg-slot-value response-understand-srv :SPARQLQUERY)))           
    (let ((response-merge-srv (call-merge-srv *sparql* *ctx-designate* nil)))
        (setq *merge-sparql* (msg-slot-value response-merge-srv :MERGED_QUERY)))
    (let ((response-sparql-srv (call-sparql-srv *merge-sparql*)))
         (setq *matches* (msg-slot-value response-sparql-srv :RESULTS)))
     (princ (format nil "these are the matched sparql ~a" *matches*))(terpri)
    (cond
        ((> (length *matches*) 1)
         (progn
             (terpri)(princ "I am not sure of what you are speaking about...")(terpri)
             (setq *new-context* *merge-sparql*) 
             (setq *question* ""))
         (terpri)(princ "new context set:")(terpri)(princ *new-context*)
                (loop for match in (coerce *matches* 'list) do
                        (terpri)(princ (format nil "match found: ~a" match))
                        (let ((response-disambiguate-srv (call-disambiguate-srv match *new-context*)))
                            (setq *sparql-result* (msg-slot-value response-disambiguate-srv :SPARQLRESULT))
                            (setq *ambiguous* (msg-slot-value response-disambiguate-srv :AMBIGUOUS)))
                        (terpri)(princ "Sparql result: ")(terpri) (princ *sparql-result*)
                        (cond
                            ((not (coerce *ambiguous*  'list))
                             (setq *match-sparql* *sparql-result*))
                            ((progn
                                (setq *result* "not understand")
                                (princ *result*)
                                (values *state* *result* *action*))))
                        (terpri)(princ "sparql : ")(princ *match-sparql*)(terpri)(princ "the context is:")
                            (terpri)(princ *new-context*)
                            (let ((response-merge-match-srv (call-merge-srv *sparql-result* *new-context* t)))
                                (setq *match-sparql* (msg-slot-value response-merge-match-srv :MERGED_QUERY)))
                                (princ (format nil "this is the matched sparql ~a" *match-sparql*))(terpri)
                            (let ((response-verbalize-srv (call-verbalize-srv *match-sparql*)))
                                (setq *question-part* (msg-slot-value response-verbalize-srv :VERBALIZATION)))
                            (princ "Question part : ")(princ *question-part*)(terpri)
                        (cond
                            ((string= *question* "")
                             (setq *question*  *question-part*))
                            ((setq *question*  (concatenate 'string *question* ", or, " *question-part*)))))
            (princ "Do you mean : ") (princ *question*)(princ "?")
            (setq *ctx-designate* *new-context*)
            (values *state* *question* *action*))

        ((= (length *matches*) 1)
         (progn
             (terpri)(princ "cube find:")(terpri)(princ *matches*)
             (setq *ctx-designate* nil)
             (setq *state* t)
             (values *state* *matches* *action*)))

        ((progn
            (setq *result* "not understand")
            (princ *result*) (terpri)
            (setq *state* nil)
            (values *state* *result* *action*)))))


(defun disambiguate-cube (cube)
  "Function to call the Disambgiuate service to disambiguate cube."
  (setq base-facts-triples nil)
  (handler-case
    (progn
        (loop for fact in 
            (coerce *cheat-list* 'list) do 
                (setq base-facts-triples (append base-facts-triples (list (get-triplet fact)))))
       (let ((symbol-table (roslisp:make-msg "knowledge_sharing_planner_msgs/SymbolTable"   :individuals (vector cube) :symbols (vector "0"))))
            (princ base-facts) 
            (let ((response (call-service *disambiguate-srv*
                             'knowledge_sharing_planner_msgs-srv:Disambiguation 
                             :ontology *robot-name*
                             :symbol_table symbol-table
                             :individual cube
                             :baseFacts base-facts-triples)))
                             
             
             (princ "resp disambiguate-cube :")
             (princ response))))
       
    (roslisp::ros-rpc-error () 
     (princ "Service call failed:") (princ *disambiguate-srv*)))) 

;;assume cubes are ordered
(defun select-current-cube ()
    (cond 
        ((> (cubes-length *cubes*) 0)
         (progn 
          (setf *current-cube* (nth 0 (split-sequence:SPLIT-SEQUENCE #\Space *cubes*)))
          (princ (format nil "current cube is: ~a" *current-cube*))
          (values t)))
        
        ((values nil))))
        
        
; (defun order-cubes ()
;     (let ((tmp-cubes (sort '*cubes*  :key (value-cube cube))))
;         (setf *cubes* tmp-cubes)))
    

; (defun value-cube (cube-name)
;     (let ((response-disambiguate-srv (call-disambiguate-srv match new-context)))
;          (let ((sparql-result (roslisp:msg-slot-value response-disambiguate-srv :SPARQLRESULT))
;                (ambiguous (roslisp:msg-slot-value response-disambiguate-srv :AMBIGUOUS))))))
;         ;;    (if ((eql (length ambiguous) 0))
;         ;;     (values (length sparql-result))
;         ;;     (expt 2 63))
(defparameter relations nil)  

(defvar *test-var* (vector "?0 isA Cube" "?0 hasColor blue" "?0 hasGraphicalEntity ?1" "?1 isA Triangle" "?1 hasColor green"))

(defvar *test-fact*  "?0 hasColor blue")
(defun get-relations-of-one-entity (facts entity)
  (setq relations nil)

  (loop for fact in 
            (coerce facts 'list) do
           (let ((from (get-from fact)))
            (cond ((string= from entity)
                   (progn
                    (setq relations (append relations (list (get-relation fact)))))))))
  (values relations))
 

(defun get-relation (fact)
    (let ((match (split-sequence:SPLIT-SEQUENCE #\Space fact)))
       (let ((relation (second match)))
        (values relation))))


(defun get-from (fact)
 (let ((match (split-sequence:SPLIT-SEQUENCE #\Space fact)))
      (let ((from (car match)))
       (values from))))

(defun get-on (fact)
 (let ((match (split-sequence:SPLIT-SEQUENCE #\Space fact)))
      (let ((on (car (last match))))
       (values on))))

(defun get-triplet (fact)
    (let ((match (split-sequence:SPLIT-SEQUENCE #\Space fact)))
       (let ((triplet (roslisp:make-msg "knowledge_sharing_planner_msgs/Triplet"
                       :from (car match)
                       :relation (second match)
                       :on (car (last match)))))
        (terpri)            
        (princ "triplet :")
        (terpri)
        (princ triplet)
        (values triplet))))
         
(defun cubes-length (list-of-cubes)
 (length (split-sequence:SPLIT-SEQUENCE #\Space list-of-cubes)))


(defun update-cube-list ()
    (cond 
       ((eq *cheat* t)
        (progn 
             (let ((cubes (onto::get-from "isAbove" "table_1" "Cube")))
              (princ (format nil "list cubes : ~a" cubes))(terpri)
              (setf *cubes* "")
              (loop for cube in (coerce cubes 'list) do
                      (let ((boxes (onto::get-from "containerHasIn" cube "VisibleDtBox")))
                       (princ boxes)(terpri)
                       (princ (length (coerce boxes 'list)))(terpri)
                       (cond ((> (length (coerce boxes 'list)) 0)
                              (progn
                               (princ (setf *cubes* (concatenate 'string (format nil "~{~A~^, ~} " (list cube)) *cubes*)))(terpri)))))))))
                               ;;(princ "cubes:")(terpri)
                              ;; (princ cube)(terpri)
                               ;;(princ *cubes*)(terpri)
                               
                               
                               

       ((setf *cubes* (onto::get-from "isAbove" "table_1" "Cube"))))      
    (princ (setf *nb-cubes* (- (cubes-length *cubes*) 1))))
  
(defun reset ()
    (setf *current-cube* nil)
    (setf *cubes* nil)
    (setf *nb-cubes* 0))


(defun set-ontology ()
    (cond 
       ((eql *cheat* t)
        (progn
            (setf onto (onto::get-onto *robot-name*))))
           ;; (setf onto (onto::set-lang lang)))))
   ;; (progn
       ((setf onto (onto::get-onto "human_0")))))
        ;; (setf onto (onto::set-lang lang))))

(defun change-context ()
    (princ "--> Change ctx")(terpri)
    (setf *cheat* (not *cheat*))
    (princ (format nil "*cheat* : ~a" *cheat*))(terpri)
     (princ ">>>setting ontology called again")(terpri)
    (set-ontology)
    (princ ">>>updating cube list")(terpri)
    (update-cube-list))


; (defun perform-dt-step ()

;  (let ((response-disambiguate-srv (disambiguate-cube *current-cube*)))
;                             (setq *sparql-result* (msg-slot-value response-disambiguate-srv :SPARQLRESULT))
;                             (setq *ambiguous* (msg-slot-value response-disambiguate-srv :AMBIGUOUS)))
;  (let ((response-verbalize-srv (call-verbalize-srv *sparql-result* )))
;                                 (setq *verbalization* (msg-slot-value response-verbalize-srv :VERBALIZATION))))

