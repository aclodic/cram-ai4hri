(in-package :pepper-dt-demo)


(defun init-demo ()
 (init-ros-pepper)
 (pepper-ll::init-pepper-ros-control) 
 (init-onto-communication))


(defun init-ros-pepper()
 (start-ros-node "cram_pepper_dt_demo_ros_node")
 (pepper-ll::init-tf)
 (init-list-discourse))

(defun init-onto-communication ()
 (agin::init-dt "pepper" "eng")  
 (agin::construct-you-agent-desig "human_0"))
 
 ;;
; Waiting for action server timed out.
;    [Condition of type CL-TRANSFORMS-STAMPED:TIMEOUT-ERROR]

; Restarts:
;  0: [RETRY] Retry SLIME REPL evaluation request.
;  1: [*ABORT] Return to SLIME's top level.
;  2: [ABORT] abort thread (#<THREAD "repl-thread" RUNNING {10092C9BC3}>)

; Backtrace:
;   0: ((:METHOD INITIALIZE-INSTANCE :AFTER (CL-TF2:BUFFER-CLIENT)) #<CL-TF2:BUFFER-CLIENT {101E2C4C83}> :TIMEOUT 10.0) [fast-method]
;   1: ((LAMBDA ()))