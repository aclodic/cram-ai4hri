(in-package :pepper-dt-demo)

(def-fact-group pepper-action-designators (action-grounding) 

;;peforming-dt-task
(<- (desig:action-grounding ?desig (perform-dt-task  ?desig))
      (desig-prop ?desig (:type  :performing))
      (desig-prop ?desig (:task ?dt))
      )

;;waiting-for-human
(<- (desig:action-grounding ?desig (wait-for-agent ?desig))
      (desig-prop ?desig (:type  :waiting))
      (desig-prop ?desig (:for ?you-agent-desig))
      )

;;listen-to-interaction 
(<- (desig:action-grounding ?desig (listen-to-interaction  ?desig))
      (desig-prop ?desig (:type  :listening)) ;
      (desig-prop ?desig (:understand ?interaction-desig))
      )

;;listen-and-reply
(<- (desig:action-grounding ?desig (listen-and-reply  ?desig))
      (desig-prop ?desig (:type  :listening-and-replying)) 
      (desig-prop ?desig (:receive ?you-interaction-desig))
      (desig-prop ?desig (:reply ?me-interaction-desig))
      ) 

;;telling
(<- (desig:action-grounding ?desig (tell ?desig))
      (desig-prop ?desig (:type  :telling)) 
      (desig-prop ?desig (:intention ?intention))
      (desig-prop ?desig (:topic ?topic))
      (desig-prop ?desig (:details ?info))
      (desig-prop ?desig (:to-agent ?agent))
      ) 

;;asking
(<- (desig:action-grounding ?desig (ask ?desig))
      (desig-prop ?desig (:type  :asking)) 
      (desig-prop ?desig (:intention ?intention))
      (desig-prop ?desig (:topic ?topic))
      (desig-prop ?desig (:to-agent ?agent))
      )  

;;greet-human
  (<- (desig:action-grounding ?desig (greet-human ?desig))
      (desig-prop ?desig (:type  :greeting))
      (desig-prop ?desig (:greeting-speech ?sentence))
      (desig-prop ?desig (:greeting-place ?place))
      ) 


;;looking at
  (<- (desig:action-grounding ?desig (look-at ?desig))
      (desig-prop ?desig (:type  :looking))
      (desig-prop ?desig (:at ?object-desig))
      ;;(desig-prop ?desig (:arm ?arm))
      ) 
;;moving-to   
  (<- (desig:action-grounding ?desig (move-to ?desig))
      (desig-prop ?desig (:type  :moving))
      (desig-prop ?desig (:to ?place)))

;;speaking
 (<- (desig:action-grounding ?desig (speak ?desig))
      (desig-prop ?desig (:type  :speaking))
      (desig-prop ?desig (:speech ?sentence)))

;;scanning
  (<- (desig:action-grounding ?desig (scan ?desig))
      (desig-prop ?desig (:type  :scanning))
      (desig-prop ?desig (:zone ?zone)))

;;pointing at
  (<- (desig:action-grounding ?desig (point ?desig))
      (desig-prop ?desig (:type  :pointing))
      (desig-prop ?desig (:at ?object-designator))
      )
      ) 