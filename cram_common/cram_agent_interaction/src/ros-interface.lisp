(in-package :agin)

(defvar *disambiguate-srv* nil "ROS service to disambgiuate")
(defvar *verbalize-srv* nil "ROS service to verbalize")
(defvar *understand-srv* nil "ROS service to understand")
(defvar *merge-srv* nil "ROS service to merge")
(defvar *sparql-srv* nil "ROS service to sparql")
(defvar base-facts-triple nil)
;; (defvar *mementar-sub* nil "ROS sub")
;;(defvar *mementar-cb-value* (make-fluent :name :mementar-cb-value) "")

(defun init-ros-dt ()

  (setf *disambiguate-srv* "/KSP/disambiguate")
  (setf *understand-srv* "/KSP/understand")
  (setf *merge-srv* "/KSP/merge")
  (setf *sparql-srv* "/ontologenius/sparql/pepper")
  (setf *verbalize-srv* "/KSP/verbalize"))
 ;; (setf *mementar-sub* (subscribe (concatenate 'string "/mementar/occasions/" *robot-name*) "mementar/MementarOccasion"  #'mementar-cb))     


;; (defun mementar-cb (msg)
;;   "Callback for mementar. Called by the mementar topic subscriber."
;;   (cond ((eql (roslisp:msg-slot-value msg :id) id-mementar-callback) 
;;     (progn 
;;       (setf *cube* )))
;;   msg))

;;verbalize
(defun call-verbalize-srv (sparql)
  (call-service *verbalize-srv* 'knowledge_sharing_planner_msgs-srv:Verbalization :sparqlQuery
                sparql))

;;sparql_ontology
(defun call-sparql-srv (merge-sparql)
  "Function to call the sparql-srv service."
  (princ merge-sparql)
  (terpri)
  ;;(cond 
  ;; ((typep merge-sparql 'list)
  (setq str_sparql (format nil
                           "~{~A~^, ~}"
                           (coerce merge-sparql 'list)))
  ;; ))
  ;;(setq str_sparql merge-sparql)

  (princ str_sparql)
  (terpri)
  (princ "ontology results: ")
  (terpri)
  (princ (call-service *sparql-srv* 'ontologenius-srv:OntologeniusSparqlService :query
                       str_sparql)))
;;merge
(defun call-merge-srv (query ctx partial)
  "Function to call the KSP-merge service."
  (princ "Waiting for KSP merge")
  (terpri)
  (princ (format nil "query: ~a" query))
  (terpri)
  (princ (format nil "ctx: ~a" ctx))
  (terpri)
  (princ "merge response: ")
  (terpri)
  (princ (call-service *merge-srv* 'knowledge_sharing_planner_msgs-srv:Merge :base_query
                       query :context_query ctx
                       :partial partial)))

(defun call-understand-srv (sentence)
  "Function to call the KSP-understand service."
  (princ "Waiting for KSP Understand")
  (terpri)
  (princ "understand response: ")
  (terpri)
  (princ (call-service *understand-srv* 'knowledge_sharing_planner_msgs-srv:Understand :verbalization
                       sentence)))


(defun create-symbol-table (match)
  (roslisp:make-msg "knowledge_sharing_planner_msgs/SymbolTable"
                    :symbols (msg-slot-value match 'ONTOLOGENIUS-MSG:NAMES)
                    :individuals (msg-slot-value match 'ONTOLOGENIUS-MSG:VALUES)))

(defun call-disambiguate-srv (match ctx)
  "Function to call the Disambgiuate service."
  (handler-case
    (progn
       (loop for fact in (coerce ctx 'list) do 
           (setq base-facts-triple (append base-facts-triple (list (get-triplet fact)))))
       (let  ((response (call-service *disambiguate-srv*
                         'knowledge_sharing_planner_msgs-srv:Disambiguation 
                         :ontology *robot-name*
                         :symbol_table (create-symbol-table match)
                         :individual(svref (msg-slot-value match 'ONTOLOGENIUS-MSG:VALUES) 0)
                         :baseFacts base-facts-triple)))
             (setq base-facts-triple nil)
             (princ "resp disambiguate :")
             (princ response)))  

    (roslisp::ros-rpc-error () 
     (princ "Service call failed:") (princ *disambgiuate-srv*)))) 

; (defun call-disambiguate-srv (match ctx)
;   "Function to call the Disambgiuate service."
;   (handler-case
;     (progn
;        (let  ((response (call-service *disambiguate-srv*
;                          'knowledge_sharing_planner_msgs-srv:Disambiguation 
;                          :ontology *robot-name*
;                          :symbol_table (create-symbol-table match)
;                          :individual(svref (msg-slot-value match 'ONTOLOGENIUS-MSG:VALUES) 0)
;                          :baseFacts  (loop for fact in (coerce ctx 'list) do 
;                                            (append (get-triplet fact))))))
;              (princ "resp disambiguate :")
;              (princ response)))
;              ;;(values (roslisp:msg-slot-value response :ambiguous) (roslisp:msg-slot-value response :sparqlResult))))
       
;     (roslisp::ros-rpc-error () 
    ; (princ "Service call failed:") (princ *disambgiuate-srv*)))) 

;; (defun call-disambiguate-cube-srv (cube)
;;   "Function to call the DisamibguateCube service."

;;    (handler-case 
;;         (progn
;;           (call-service *disambiguate-srv*
;;                 'knowledge_sharing_planner_msgs-srv:Disambiguation :ontology
;;                 "pepper"
;;                 (:symbol_table individuals) cube
;;                 (:symbol_table symbols) "0"
;;                 :individual cube
;;                 )
                
;;             (let
;;                 ((response (call-client-srv action param)))
;;                 (let ((response-code (roslisp:msg-slot-value response :code)))
                
;;                 (eql response-code 0))))

;;         (roslisp::ros-rpc-error () 
;;             (cond
;;                 ((eql *verbose* t)
;;                     (let ((error-message (concatenate 'string "Failure to call ontologenius/" *name* )))
;;                             (print error-message)))

;;                 ((setf *client-srv* (concatenate 'string "ontologenius/" *name* ))))
;;   (call-service *disambiguate-srv*
;;                 'knowledge_sharing_planner_msgs-srv:Disambiguation :ontology
;;                 "pepper"
;;                 :symbol_table (create-symbol-table match):individual
;;                 (svref (msg-slot-value match 'ONTOLOGENIUS-MSG:VALUES)
;;                        0)
;;                 ;;   (loop for n in (coerce match 'list) do (princ n) (return n))
;;                 )) 
