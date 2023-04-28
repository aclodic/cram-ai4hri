(in-package :onto)

(defclass individual-client  (onto-client)
    (
        (client-name
         :initarg :client-name
         :initform "individual/")

        (individual-name
         :initarg :individual-name
         :initform "")))
        
    

(defparameter individual-client nil)

(defmethod initialize-instance :after ((client individual-client) &key)
    "Constructs an class client.Can be used in a multi-ontology mode by specifying the name of the ontology name(str).
    For classic use, name should be defined as"

    (let ((individual-name (slot-value client 'individual-name))
          (client-name (slot-value client 'client-name)))
    
        (cond 
            ((string= individual-name "")
             (setf individual-client (make-instance 'onto-client :client-name client-name)))

            ((setf individual-client (make-instance 'onto-client :client-name 
                                      (concatenate 'string client-name individual-name)))))

        (initialize-instance individual-client)))
    


;; (defun init-ind-client(name)
;;     "Constructs an class client.
;;         Can be used in a multi-ontology mode by specifying the name of the ontology name(str).
;;         For classic use, name should be defined as"
;;     (cond 
;;             ((string= name "")
;;                 (init-onto-client "individual"))
            
;;             ((init-onto-client (concatenate 'string "individual/" name)))

;;                 ))

(defun get-on (indiv-name property name &optional (selector ""))
    "Gives all the individuals (str[]) pointed by the property property(str) and applied to the individual name(str).
    The optional selector(str) parameter can be set to only get results inheriting from the selector class.
    The default value '' represents no restriction on the result."
    (let ((ind-client (make-instance 'individual-client :individual-name indiv-name)))
     (let ((param (concatenate 'string name ":" property)))
         (cond 
             ((not (string= selector ""))
              (setq param (concatenate 'string param " -s " selector))))
         (call "getOn" param))))
            

(defun get-from (indiv-name property name &optional (selector ""))
    "Gives all the individuals (str[]) having the given property property(str) and pointing to the individual name(str).
    The optional selector(str) parameter can be set to only get results inheriting from the selector class.
    The default value '' represents no restriction on the result."
    (let ((ind-client (make-instance 'individual-client :individual-name indiv-name)))
     (let ((param (concatenate 'string name ":" property)))
         (cond 
             ((not (string= selector ""))
              (setq param (concatenate 'string param " -s " selector))))
             (call "getFrom" param))))


(defun get-with (indiv-from indiv-to &optional (selector "") (depth -1))
    "Gives all the properties (str[]) linking the individual indiv_from(str) to the individual indiv_to(str).
    The optional selector(str) parameter can be set to only get results inheriting from the selector property.
    The default value '' represents no restriction on the result.
    The optional depth(int) parameter can be set to limit tree propagation to a specific value.
    The default value -1 represents no propagation limitation."
    
    (let ((param (concatenate 'string indiv-from ":" indiv-to)))
        (cond 
            ((not (string= selector ""))
             (setq param (concatenate 'string param " -s " selector)))
            
            ((>= depth 0)
             (setq param (concatenate 'string param " -d " (write-to-string depth)))))
                

        (call  "getWith" param)))


(defun get-related-from (property)
    "Gives all the individuals (str[]) possessing the property property(str)."

    (call  "getRelatedFrom" property))


(defun get-related-on (property)
    "Gives all the individuals (str[]) pointed to by the property property(str)."

    (call  "getRelatedOn" property))


(defun get-related-with (name)
    "Gives all the individuals (str[]) having a property pointing to the individual name(str)."

    (call  "getRelatedWith" name))


(defun get-relation-from (name &optional (depth -1))
    "Gives all the properties (str[]) applied to the individual name(str).
    The optional depth(int) parameter can be set to limit tree propagation to a specific value.
    The default value -1 represents no propagation limitation."
    
    (let ((param name))
        (cond 
            ((>= depth 0)
             (setq param (concatenate 'string param " -d " (write-to-string depth)))))
                

        (call "getRelationFrom" param)))



(defun get-relation-on (name &optional (depth -1))
    "Gives all the properties (str[]) going to the individual name(str).
    The optional depth(int) parameter can be set to limit tree propagation to a specific value.
    The default value -1 represents no propagation limitation."
    
    (let ((param name))
        (cond 
            ((>= depth 0)
             (setq param (concatenate 'string param " -d " (write-to-string depth)))))
                

        (call  "getRelationOn" param)))



(defun get-relation-with (name)
    "Gives all the individuals (str[]) having a property pointing to the individual name(str)."

    (call  "getRelationWith" name))


(defun get-domain-of (name &optional (depth -1) (selector ""))
    "Gives all the properties (str[]) for which the individual name(str) is part of the domain.
    The optional selector(str) parameter can be set to only get results inheriting from the selector property.
    The default value '' represents no restriction on the result.
    The optional depth(int) parameter can be set to limit tree propagation of the individual to a specific value.
    The default value -1 represents no propagation limitation."

    (let ((param name))
        (cond 
            ((not (string= selector ""))
             (setq param (concatenate 'string param " -s " selector)))
            
            ((>= depth 0)
             (setq param (concatenate 'string param " -d " (write-to-string depth)))))
                

        (call  "getDomainOf" param)))


(defun get-range-of (name &optional (depth -1) (selector ""))
    "Gives all the properties (str[]) for which the individual name(str) is part of the range.
    The optional selector(str) parameter can be set to only get results inheriting from the selector property.
    The default value '' represents no restriction on the result.
    The optional depth(int) parameter can be set to limit tree propagation of the individual to a specific value.
    The default value -1 represents no propagation limitation."

    (let ((param name))
        (cond 
            ((not (string= selector ""))
             (setq param (concatenate 'string param " -s " selector)))
            
            ((>= depth 0)
             (setq param (concatenate 'string param " -d " (write-to-string depth)))))
                

        (call  "getRangeOf" param)))



(defun get-type (name)
    "Gives all the individuals (str[]) of the type of the given class name(str)."

    (call  "getType" name))


(defun get-same (name)
    "Gives all the individuals (str[]) that are defined as being identical to the individual name(str)."

    (call  "getSame" name))


(defun get-distincts (name)
    "Gives all the defined individuals (str[]) as being distinct from the individual name(str)."

    (call  "getDistincts" name))
