(in-package :agin)

(defparameter *you-agent-desig* nil)

(defun construct-you-agent-desig (agent-name)
 (let ((agent-role 
 (svref (onto::get-on agent-name "hasRole" agent-name) 0)
 ))
  (let ((?agent-name agent-name) (?agent-role agent-role))
   (setf *you-agent-desig*
    (desig:an agent (type :human) 
                    (:name ?agent-name) 
                    (:role ?agent-role))))))
                         
(defun get-role-of-you-agent ()
 (cond ((eql *you-agent-desig* nil)
        (write-line "No further agent was defined"))
       ((progn 
         (let ((agent-role (desig-prop-value *you-agent-desig* :role)))
          (values agent-role))))))

