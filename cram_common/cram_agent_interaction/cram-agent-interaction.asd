(defsystem cram-agent-interaction
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
               :cram-utilities 
               :cram-cloud-logger
               :ontologenius-srv
               :ontologenius-msg
               :resource_management_msgs-msg
               :knowledge_sharing_planner_msgs-msg
               :knowledge_sharing_planner_msgs-srv
              ;; :mementar-msg
               :pepper_head_manager_msgs-msg
               :cram-ontologenius
               :cram-occasions-events)
              
            

  :components
  ((:module "src"
            :components
            (
              (:file "package")
              (:file "ros-interface" :depends-on ("package"))
              (:file "dt-init" :depends-on ("package" "ros-interface"))
              (:file "dt-cubes" :depends-on ("package" "dt-init"))
              (:file "understand" :depends-on ("package" "dt-cubes"))
              (:file "agent-knowledge" :depends-on ("package"))
              (:file "object-knowledge" :depends-on ("package" "understand"))
              (:file "message-knowledge" :depends-on ("package" "object-knowledge"))
              (:file "interaction-knowledge" :depends-on ("package" "agent-knowledge" "message-knowledge"))))))
                                                