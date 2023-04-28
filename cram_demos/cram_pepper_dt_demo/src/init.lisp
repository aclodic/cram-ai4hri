(in-package :pepper-dt-demo)


(defun init-demo ()
(init-ros-pepper)
(init-onto-communication)
)

(defun init-ros-pepper()
 (start-ros-node "cram_pepper_dt_demo_ros_node")
 (pepper-ll::init-tf)
  (init-list-discourse))

(defun init-onto-communication ()
 (agin::init-dt "pepper" "eng")  
 (agin::construct-you-agent-desig "human_0"))
 
 