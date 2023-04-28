(in-package :onto)

(defclass onto-client (client-base)
    (
        (client-name
         :initarg :client-name
         :initform (error "Must supply a service name."))))
        
;;(defparameter nil)

(defmethod initialize-instance :after ((client onto-client) &key)
    "Constructs an ontology client linked to the service ontologenius/name(str)."
    (let ((client-name (slot-value client 'client-name)))
        (let ((onto-client (make-instance 'client-base :client-name client-name))))))

       ;; (initialize-instance onto-client)))
    


;; (defun init-()
;;     "Constructs a manager client.
;;     Can only be used in a multi-ontology mode."
;;     (initialize-instance *onto-client*)

;; )

;; (defun init-(name)
;;     "Constructs an ontology client linked to the service ontologenius/name(str)."
;;     (init-client name)
;; )

(defun return-param (name take-id)

    (let ((param name))
        (cond 
            ((eql take-id nil)
             (setq param (concatenate 'string param " -i false"))))))


(defun get-up (name &optional (depth -1) (selector ""))
    "Gives all concepts below the one given in the parameter: name(str).
    The optional depth(int) parameter can be set to limit tree propagation to a specific value.
    The default value -1 represents no propagation limitation.
    The optional selector(str) parameter can be set to only get results inheriting from the selector(str) concept.
    The default value '' represents no restriction on the result."

    (let ((param name))
        (cond 
            ((not (string= selector ""))
             (setq param (concatenate 'string param " -s " selector)))
            
            ((>= depth 0)
             (setq param (concatenate 'string param " -d " (write-to-string depth)))))
                

        (call "getUp" param)))


(defun is-A (name base-class)
    "Return true if the concept name(str) is or inherits of the concept base_class(str).
    This function corresponds to checking if class_base is part of the result of the function getUp applies to the concept name."
    
    (let ((res (get-up  name -1 base-class)))
        (cond 
            ((eq (length res) 0)
             (values nil))
            
            ((not (eq (length res) 0)) ;;else actually
             (values t)))))


(defun get-name (name &optional (take-id t))
    "Gives one of the label (str) of the concept name(str) that is not muted.
    The default take_id(bool) argument can be set to False if you do not want to
    consider the concept identifier as a possible default name.
    The result of this function depends on the setting of the working language."
    ;; (let ((param name))
    ;;     (cond 
    ;;         ((eql take-id nil)
    ;;             (setq param (concatenate 'string param " -i false"))))

    (call-str "getName" (return-param name take-id)))


(defun get-names (name &optional (take-id t))
    "Gives all the labels (str[]) of the concept name(str) excepted the muted ones.
    The default take_id(bool) argument can be set to False if you do not want to
    consider the concept identifier as a possible default name.
    The result of this function depends on the setting of the working language."
    ;; (let ((param name))
    ;;     (cond 
    ;;         ((eql take-id nil)
    ;;             (setq param (concatenate 'string param " -i false"))))

    (call "getNames" (return-param name take-id)))


(defun get-every-names (name &optional (take-id t))
    "Gives all the labels (str[]) of the concept name(str) even the muted ones.
    The default take_id(bool) argument can be set to False if you do not want to
    consider the concept identifier as a possible default name.
    The result of this function depends on the setting of the working language."
    ;; (let ((param name))
    ;;     (cond 
    ;;         ((eql take-id nil)
    ;;             (setq param (concatenate 'string param " -i false"))))

    (call "getEveryNames" (return-param name take-id)))


(defun find-concept (name &optional (take-id t))
    "Gives all the concepts (str[]) having for label name(str).
    The default take_id(bool) argument can be set to False if you do not want
    to consider the concept identifier as a possible default name.
    The result of this function depends on the setting of the working language."
    ;; (let ((param name))
    ;;     (cond 
    ;;         ((eql take-id nil)
    ;;             (setq param (concatenate 'string param " -i false"))))

    (call "find" (return-param name take-id)))


(defun find-sub-concept (name &optional (take-id t))
    "Gives all the concepts (str[]) having for label a subset of name(str).
    The default take_id(bool) argument can be set to false if you do not want to
    consider the concept identifier as a possible default name.
    The result of this function depends on the setting of the working language."
    ;; (let ((param name))
    ;;     (cond 
    ;;         ((eql take-id nil)
    ;;             (setq param (concatenate 'string param " -i false"))))

    (call "findSub" (return-param name take-id)))


(defun find-regex (name &optional (take-id t))
    "Give all concepts (str[]) with a label matching the regular expression regex(str).
    The default take_id(bool) argument can be set to false if you do not want to consider
    the concept identifier as a possible default name.
    The result of this function depends on the setting of the working language."
    ;; (let ((param name))
    ;;     (cond 
    ;;         ((eql take-id nil)
    ;;             (setq param (concatenate 'string param " -i false"))))

    (call "findRegex" (return-param name take-id)))


(defun find-fuzzy (name &optional (threshold 0.5) (take-id t))
    "Give all the names of concepts (str[]) with the lowest
    edit distance with parameter name(str).
    The default take_id(bool) argument can be set to false if you do not want to
    consider the concept identifier as a possible default name.
    The result of this function depends on the setting of the working language and
    does not correspond to the concept identifiers but to other labels known by ontologenius.
    The minimum editing distance can be set with the threshold(double) parameter.
    This value corresponds to the number of changes to be made to pass from one
    word to another divided by the length of the comparison word"

    (let ((param (concatenate 'string name " -t" (write-to-string threshold))))
        (cond 
            ((eql take-id nil)
             (setq param (concatenate 'string param " -i false"))))

        (call "findFuzzy" param)))


(defun exist (name)
    "Returns True if the concept name(str) exists in the subpart of the ontology 
    managed by the client (i.e. class, individuals, object properties, data properties)."

    (not (string= (call-str "exist" name) "")))
