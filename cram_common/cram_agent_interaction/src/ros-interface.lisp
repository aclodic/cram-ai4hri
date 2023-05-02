(in-package :agin)

(defvar *disambiguate-srv* nil "ROS service to disambgiuate")
(defvar *verbalize-srv* nil "ROS service to verbalize")
(defvar *understand-srv* nil "ROS service to understand")
(defvar *merge-srv* nil "ROS service to merge")
(defvar *sparql-srv* nil "ROS service to sparql")
(defvar base-facts-triple nil)

(defun init-ros-dt ()
  (setf *disambiguate-srv* "/KSP/disambiguate")
  (setf *understand-srv* "/KSP/understand")
  (setf *merge-srv* "/KSP/merge")
  (setf *sparql-srv* "/ontologenius/sparql/pepper")
  (setf *verbalize-srv* "/KSP/verbalize"))

;;verbalize
(defun call-verbalize-srv (sparql)
  (call-service *verbalize-srv* 'knowledge_sharing_planner_msgs-srv:Verbalization :sparqlQuery
                sparql))

;;sparql_ontology
(defun call-sparql-srv (merge-sparql)
  "Function to call the sparql-srv service."
  (princ merge-sparql)
  (terpri)
  (setq str_sparql (format nil
                           "~{~A~^, ~}"
                           (coerce merge-sparql 'list)))
  (princ str_sparql)
  (terpri)
  (princ (call-service *sparql-srv* 'ontologenius-srv:OntologeniusSparqlService :query
                       str_sparql)))
;;merge
(defun call-merge-srv (query ctx partial)
  "Function to call the KSP-merge service."
  (write-line "Waiting for KSP MergeQueryService")
  (write-line (format nil "query: ~a , ctx: ~a" query ctx))
  (princ (call-service *merge-srv* 'knowledge_sharing_planner_msgs-srv:Merge :base_query
                       query :context_query ctx
                       :partial partial)))

(defun call-understand-srv (sentence)
  "Function to call the KSP-understand service."
  (write-line "Waiting for KSP UnderstandService")
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