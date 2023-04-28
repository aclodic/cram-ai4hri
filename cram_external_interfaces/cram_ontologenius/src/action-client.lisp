(in-package :onto)

(defclass action-client (client-base)
    (
        (client-name
        :initarg :client-name
        :initform "actions/")

        (action-name
        :initarg :action-name
        :initform "")
        )
    )

(defparameter action-client nil)

(defmethod initialize-instance :after ((client action-client) &key)
    "Constructs an action client.
    Can be used in a multi-ontology mode by specifying the name of the ontology name(str).
    For classic use, name(str) should be defined as ''"

    (let ((action-name (slot-value client 'action-name))
            (client-name (slot-value client 'client-name)))
    
        (cond 
            ((string= action-name "")
                (setf action-client (make-instance 'client-base :client-name client-name)))

            ((setf action-client (make-instance 'client-base :client-name 
            (concatenate 'string client-name action-name)))))

        (initialize-instance action-client))
    
)

;; (defun init-action-client(name)
;;     "Constructs an action client.
;;         Can be used in a multi-ontology mode by specifying the name of the ontology name(str).
;;         For classic use, name(str) should be defined as ''"
;;     (cond 
;;             ((string= name "")
;;                 (init-client "actions"))
            
;;             ((init-client (concatenate 'string "actions/" name)))

;;                 ))

(defun close-onto ()
    "Link all the concepts loaded from files and the Internet.
    Before closing an ontology, exploration requests are not allowed.
    Returns False if the service call fails."
     (initialize-instance action-client)
     (princ action-client)
    (call-nr "close" "")
)

(defun save-onto (path)
    "Saves the current ontology in the absolute path(str) path.
    The path(str) parameter must be of the form: my/path/to/ontology.owl
    Returns False if the service call fails."

    (call-nr "save" path)
)

(defun export-onto (path)
    "Exports the current modification tree in the absolute path(str) path.
    The path(str) parameter must be of the form: my/path/to/file.xml
    This function has no effect on non copied ontologies.
    Returns False if the service call fails."

    (call-nr "export" path)
)

(defun set-lang (lang)
    "Sets the language of work lang(str).
    Returns False if the service call fails."

    (call-nr "setLang" lang)
)

(defun get-lang ()
    "Return the working language (str)."

    (call-str "getLang" "")
)

(defun add-action (uri)
    "Load an ontology file (.owl) stored at uri(str) from the internet.
    The Close function should be called after all the desired files have been loaded.
    Returns False if the service call fails."

    (call-nr "add" uri)
)

(defun fadd (file)
    "Load an ontology file (.owl) stored at file(str) from your local computer.
    The Close function should be called after all the desired files have been loaded.
    Returns False if the service call fails."

(call-nr "fadd" file)
)

(defun reset-onto ()
    "Unload all the knowledge previously loaded or learned.
    Returns False if the service call fails."

(call-nr "reset" "")
)