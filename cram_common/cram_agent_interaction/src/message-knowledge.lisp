(in-package :dt)

(defparameter *request-msg-desig* (desig:a message (type :request)))

(defun construct-message-desig (sentences)
 (let ((u-resp (understand-call sentences)))
  (let ((query (nth 0 u-resp)) (?communicated-action (nth 1 u-resp)))
   (let ((desig-results (designate-object query))) 
    (let ((?communicated-object (nth 2 desig-results))
          (reply-msg (nth 3 desig-results))
          (state (nth 0 desig-results)))
     (cond ((not (string= ?communicated-action ""))
            (setf *request-msg-desig* (copy-designator *request-msg-desig*
                                       :new-description `((:communicated-action ,?communicated-action))))))
     (cond ((not (eql ?communicated-object nil))      
            (setf *request-msg-desig* (copy-designator *request-msg-desig*
                                       :new-description `((:communicated-object ,?communicated-object))))))
     (multiple-value-list (values *request-msg-desig* reply-msg state)))))))
                     