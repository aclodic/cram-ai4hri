(in-package :pepper-dt-demo)

;; (defstruct understand-motion
;;   "Represents motion types"

;; )

;; (def-fact-group process-input-motion-designators (motion-grounding)
;; (<- (desig:motion-grounding ?desig (understand ?motion))
;;     (desig-prop ?desig (:type :understanding))
;;     (lisp-fun make-understand-motion ?motion))    
;;     )


;;navigation and scanning

(defstruct pepper-looking-motion
  "Represents a looking motion"
  target
 ;; arm
)

(defstruct pepper-moving-motion
  "Represents a moving motion"
  x-val
  y-val
)

(defstruct pepper-scanning-motion
  "Represents a moving motion"
  x
  y
  z
)

(def-fact-group pepper-navigation-motion-designators (motion-grounding)
(<- (desig:motion-grounding ?desig (look ?motion))
    (desig-prop ?desig (:type :looking))
    (desig-prop ?desig (:target ?target))
    ;;(desig-prop ?desig (:arm ?arm))
    (lisp-fun make-pepper-looking-motion :target ?target ?motion))

(<- (desig:motion-grounding ?desig (move-to ?motion))
    (desig-prop ?desig (:type :moving))
    (desig-prop ?desig (:x-val ?x-val))
    (desig-prop ?desig (:y-val ?y-val))
    (lisp-fun make-pepper-moving-motion :x-val ?x-val :y-val ?y-val ?motion))
    
(<- (desig:motion-grounding ?desig (scan ?motion))
    (desig-prop ?desig (:type :scanning))
    (desig-prop ?desig (:x ?x))
    (desig-prop ?desig (:y ?y))
    (desig-prop ?desig (:z ?z))
    (lisp-fun make-pepper-scanning-motion :x ?x :y ?y :z ?z ?motion)) 

(<- (desig:motion-grounding ?desig (test ?motion))
    (desig-prop ?desig (:type :testing))
    (lisp-fun make-pepper-scanning-motion ?motion))     
    )
;;communication 

(defstruct pepper-communication-motion
  "Represents a speaking motion"
  sentence
)

(def-fact-group pepper-communication-motion-designators (motion-grounding)  
  (<- (desig:motion-grounding ?desig (speak ?motion))
      (desig-prop ?desig (:type :speaking))
      (desig-prop ?desig (:sentence ?sentence))
      (lisp-fun make-pepper-communication-motion :sentence ?sentence ?motion)
      ))

;;manipulation 

(defstruct pepper-manipulation-motion
  "Represents a speaking motion"
  target
)

(def-fact-group pepper-manipulation-motion-designators (motion-grounding)  
  (<- (desig:motion-grounding ?desig (point ?motion))
      (desig-prop ?desig (:type  :pointing))
      (desig-prop ?desig (:target ?target))
      (lisp-fun make-pepper-manipulation-motion :target ?target ?motion)
      ))
  