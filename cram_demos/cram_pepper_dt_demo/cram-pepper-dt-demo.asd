(defsystem cram-pepper-dt-demo
  :depends-on (
               :roslisp
               :actionlib_msgs-msg
               :actionlib
               :geometry_msgs-msg
               :cl-transforms
               :cl-transforms-stamped
               :cl-tf
               :cl-tf2
               :cram-tf
               :cram-language
               :cram-designators 
               :cram-prolog
               :cram-process-modules 
               :cram-language-designator-support
               :cram-executive 
               :cram-cloud-logger
               :nao_interaction_msgs-srv
               :exp_pepper-msg
           ;;:cram-common-failure
               :resource_management_msgs-msg
               :knowledge_sharing_planner_msgs-msg
               :knowledge_sharing_planner_msgs-srv
               :mementar-msg
               :ontologenius-srv
               :ontologenius-msg
               :pepper_head_manager_msgs-msg
               :cram-ontologenius
               :cram-directory-task
               :cram-occasions-events
               ) 
             

  :components
  ((:module "src"
            :components
            (
              (:file "package")
              (:file "discourse-pepper" :depends-on ("package"))
              (:file "interaction-designators" :depends-on ("package"))
              (:file "motion-designators" :depends-on ("package"))
              (:file "action-designators" :depends-on ("package"))
              
              (:file "process-modules" :depends-on ("package"
                                                    "motion-designators"
                                                    "interaction-designators"))   
              (:file "interaction-actions" :depends-on ("package"
                                                    "motion-designators"
                                                    "interaction-designators"
                                                    "process-modules"))
              (:file "demo-test" :depends-on ("package" "discourse-pepper"
                                                   "process-modules" "dt-pepper" "interaction-actions"))))))  
             
  

