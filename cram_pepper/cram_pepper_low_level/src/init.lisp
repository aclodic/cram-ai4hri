(in-package :pepper-ll)

(defun init-pepper-ros-control ()

(setf *prio-pub-arm-right* (advertise 
   "/pepper_arm_manager_right/set_priorities"
    "resource_management_msgs/PrioritiesSetter"))

(setf *prio-pub-arm-left* (advertise 
  "/pepper_arm_manager_left/set_priorities"
   "resource_management_msgs/PrioritiesSetter"))

 (setf *point-at-right-pub* (advertise 
  "/pepper_arm_manager_right/social/pepper_arm_manager_msgs1_PrioritizedPoint"
    "pepper_head_manager_msgs/PrioritizedPoint"))

 (setf *point-at-left-pub* (advertise 
  "/pepper_arm_manager_left/social/pepper_arm_manager_msgs1_PrioritizedPoint"
    "pepper_head_manager_msgs/PrioritizedPoint"))

(setf *prio-pub* (advertise 
   "/pepper_head_manager/set_priorities"
    "resource_management_msgs/PrioritiesSetter"))

(setf *env-point-pub* (advertise 
  "/pepper_head_manager/env_monitoring/pepper_head_manager_msgs1_PrioritizedPoint"
   "pepper_head_manager_msgs/PrioritizedPoint"))

(setf *speaking-point-pub* (advertise 
    "/pepper_head_manager/speaking/pepper_head_manager_msgs1_PrioritizedPoint"
        "pepper_head_manager_msgs/PrioritizedPoint"))
        
(init-speaking-buffer)


 )
