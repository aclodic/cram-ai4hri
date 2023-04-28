(in-package :onto)

(defclass ontology-manipulator ()
    (
        (onto-name
         :initarg :onto-name
         :initform "")
        
        (service-name
         :initarg :service-name
         :initform "ontologenius/sparql")

        individual-client
        action-client))
        
        
    

(defmethod initialize-instance :after ((onto-man ontology-manipulator) &key)
    "Constructs a ROS client linked to the service name(str)."

    (let ((onto-name (slot-value onto-man 'onto-name)) (service-name (slot-value onto-man 'service-name)))
         (setf (slot-value onto-man 'individual-client)
             (make-instance 'individual-client :individual-name onto-name))
         (setf (slot-value onto-man 'action-client)
             (make-instance 'action-client :action-name onto-name))
                        
        (cond 
            ((not (string= onto-name ""))
             (setq service-name (concatenate 'string service-name "/" onto-name))))))

               ;; (princ service-name)
               ;; (princ *onto-man*)
        ;;(roslisp:wait-for-service service-name)
        ;;(initialize-instance onto-man)
    
    


;; (defun init-onto-manlogy-man (&optional (name ""))
;;     "Constructs an ontology manipulator with.
;;         Can be used in a multi-onto-manlogy mode by specifying the name of the ontology name(str). For classic use, do not specify the ontology name name."
;;     (init-indiv-client name)
;;     (init-action-client name)
;;     (let ((service-name "ontologenius/sparql"))
;;         (cond 
;;             ((not (string= name ""))
;;                 (setq service-name (concatenate 'string  "/" name))))

;;         (roslisp:wait-for-service
;;             service-name)
;;     )
;; )

(defun nb-onto-man ()
    "Gives the total number (int) of service calls from all ROS clients instances since the last reset"

    (nb))


(defun reset-nb-onto-man ()
    "Reset the call counter for all instances of ROS clients."

    (reset-nb))


(defun close-onto-man()
    "Same as the ActionClient closing function. Link all the concepts loaded from files and the Internet. Before closing an ontology, exploration requests are not allowed.
    Returns False if the service call fails."
    (initialize-instance onto-man)
    (close-onto))


(defun set-verbose-onto-man (verbose)
    "Same as the ActionClient closing function. Link all the concepts loaded from files and the Internet. Before closing an ontology, exploration requests are not allowed.
    Returns False if the service call fails."

    (set-verbose verbose))
