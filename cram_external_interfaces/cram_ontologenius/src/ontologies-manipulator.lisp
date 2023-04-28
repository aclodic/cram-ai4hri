(in-package :onto)

(defparameter *manipulators* (make-hash-table))

;; (defclass ontologies-manipulator (manager-client)
;;     (
;;         (manipulators)
;;         )
;;     )

(defparameter *onto-man* nil)
;     (make-instance 'ontology-manipulator :onto-name "pepper"))


; (defparameter *action-man*
;                 (make-instance 'action-client :action-name "pepper"))

(defun init-ontologies-man ()
    "Constructs a manipulator for several instances of ontologies."

    (init-manager))


(defun wait-init (&optional (timeout -1))
    "Wait for ontologenius services to be advertised and available for. Blocks until it is.
    timeout(int) is the amount of time to wait for before timing out.
    If timeout is -1 (default), waits until the node is shutdown."

    (cond 
        ((not (eql timeout -1))
        
         (roslisp:wait-for-service "ontologenius/manage" timeout))
        
        
        ((roslisp:wait-for-service "ontologenius/manage"))))

(defun add-onto (name)
    "Creates a new instance of ontologyManipulator identified by the name name(str).
    Returns False if the creation fails. Returns True even if the instance already exists."

    (let ((*onto-man* (make-instance 'ontology-manipulator :onto-name name)))

        (cond ((gethash name *manipulators*)
            (values t))

            ((cond ((eq (add-inst-onto name) nil)
                   (values nil))

                ((progn 

                  (setf (gethash name *manipulators*) *onto-man*)
                  (values t))))))))

(defun copy-onto (dest-name src-name)
    "Creates a new instance of ontologyManipulator identified by the name dest_name(str) that manipulates a copy of 
    the ontology handled by the ontologyManipulator src_name(str).
    Returns False if the copy fails. Returns True even if the instance already exists."

    (let ((*onto-man* (make-instance 'ontology-manipulator :onto-name name)))

        (if (gethash dest-name *manipulators*)
            (values t)
                
            (if (eq (copy-inst-onto dest-name src-name) nil)
                (values nil)

                (progn 
                    
                 (setf (gethash dest-name *manipulators*) *onto-man*)
                 (values t))))))

(defun delete-onto (name)
    "Deletes the instance of ontologyManipulator identified by the name name(str).
    Returns False deletion fails. Returns True even if the instance does not exist."

    (let ((*onto-man* (make-instance 'ontology-manipulator :onto-name name)))

        (if (not (gethash name *manipulators*))
            (values t)
                
            (if (eq (delete-inst-onto name) nil)
                (values nil)

                (progn 
                    
                 (delete (gethash name *manipulators*) *onto-man*)
                 (values t))))))

(defun get-onto (name)
    "Returns an OntologyManipulator object instance named name(str).
    Returns None if no OntologyManipulator instance is named name."

   ;; (let ((*onto-man* (make-instance 'ontology-manipulator :onto-name name)))
    ; (princ name)(terpri)
    ; (princ *manipulators*)(terpri)
   (cond ((not (gethash name *manipulators*))
            (princ ">name is not in onto-man")(terpri)
            (values nil))
            
             ((gethash name *manipulators*)
             (princ ">name is in onto-man")(terpri))))
   ;; (princ *manipulators*)(terpri)
        ; (if (not (gethash name *manipulators*))
        ;     (values nil)
                
        ;     ; (if (eq (delete-inst-onto name) nil)
        ;     ;     (values nil)

        ;     ;     (progn 
                    
        ;          (gethash name *manipulators*)))

(defun set-verbose-onto (verbose)
    "If verbose(bool) is set to True, the clients will post messages about
    the failure to call the services and about their restoration."
    (set-verbose verbose))