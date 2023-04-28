(in-package :pepper-dt-demo)



(defstruct understand-interaction
  "Represents ........"
  input
  context
)

(defstruct receive-interaction
  "Represents ........"
  from-agent
  with-content
)

(defstruct tell-interaction
  "Represents ........"
  to-agent
  with-content
  with-intention
)

(defstruct inform-interaction
  "Represents ........"
  about
)

;;listening
(def-fact-group listening-interaction-designators (interaction-grounding)
(<- (desig:interaction-grounding ?desig (understand ?interaction))
    (desig-prop ?desig (:type :understanding))
    (desig-prop ?desig (:input ?input))
    (desig-prop ?desig (:context ?context))
    (lisp-fun make-understand-interaction :input ?input :context ?context ?interaction))    
   
(<- (desig:interaction-grounding ?desig (receive-input ?interaction))
    (desig-prop ?desig (:type :receiving))
    (desig-prop ?desig (:from-agent ?agent-name))
    (desig-prop ?desig (:with-content ?message))
    (lisp-fun make-receive-interaction :from-agent ?agent-name :with-content ?message ?interaction))    
    )

;;listening
(def-fact-group explain-interaction-designators (interaction-grounding)
(<- (desig:interaction-grounding ?desig (understand ?interaction))
    (desig-prop ?desig (:type :understanding))
    (desig-prop ?desig (:input ?input))
    (desig-prop ?desig (:context ?context))
    (lisp-fun make-understand-interaction :input ?input :context ?context ?interaction))    
   
(<- (desig:interaction-grounding ?desig (receive-input ?interaction))
    (desig-prop ?desig (:type :receiving))
    (desig-prop ?desig (:from-agent ?agent-name))
    (desig-prop ?desig (:with-content ?message))
    (lisp-fun make-receive-interaction :from-agent ?agent-name :with-content ?message ?interaction))    
    )

;;telling
(def-fact-group telling-interaction-designators (interaction-grounding)
(<- (desig:interaction-grounding ?desig (reply ?interaction))
    (desig-prop ?desig (:type :replying))
    (desig-prop ?desig (:to-agent ?agent-name))
    (desig-prop ?desig (:with-content ?message))
    (desig-prop ?desig (:to-msg ?to-msg))
    (lisp-fun make-reply-interaction :to-agent ?agent-name :with-content ?message :to-msg ?to-msg ?interaction))

(<- (desig:interaction-grounding ?desig (clarify ?interaction))
    (desig-prop ?desig (:type :clarifying))
    (desig-prop ?desig (:to-agent ?agent-name))
    (desig-prop ?desig (:with-content ?message))
    (desig-prop ?desig (:to-msg ?to-msg))
    (lisp-fun make-clarify-interaction :to-agent ?agent-name :with-content ?message :to-msg ?to-msg ?interaction)) 

(<- (desig:interaction-grounding ?desig (explain ?interaction))
    (desig-prop ?desig (:type :explaining))
    (desig-prop ?desig (:to-agent ?agent-name))
    (desig-prop ?desig (:with-content ?message))
    ;;(desig-prop ?desig (:to-msg ?to-msg))
    (lisp-fun make-explain-interaction :to-agent ?agent-name :with-content ?message ?interaction)) 

(<- (desig:interaction-grounding ?desig (tell ?interaction))
    (desig-prop ?desig (:type :telling))
    (desig-prop ?desig (:with-intention ?intention))
    (desig-prop ?desig (:to-agent ?agent-name))
    (desig-prop ?desig (:with-content ?message))
    ;;(desig-prop ?desig (:to-msg ?to-msg))
    (lisp-fun make-tell-interaction :to-agent ?agent-name :with-content ?message :with-intention ?intention ?interaction))

(<- (desig:interaction-grounding ?desig (inform ?interaction))
    (desig-prop ?desig (:type :informing))
    (desig-prop ?desig (:about ?topic))
    ; (desig-prop ?desig (:to-agent ?agent-name))
    ; (desig-prop ?desig (:with-content ?message))
    ;;(desig-prop ?desig (:to-msg ?to-msg))
    (lisp-fun make-inform-interaction :about ?topic ?interaction))
; (<- (desig:interaction-grounding ?desig (greet ?interaction))
;     (desig-prop ?desig (:type :greeting))
;     (desig-prop ?desig (:to-agent ?agent-name))
;     (desig-prop ?desig (:with-content ?message))
;     (lisp-fun make-greet-interaction :to-agent ?agent-name :with-content ?message ?interaction))     

; (<- (desig:interaction-grounding ?desig (congrate ?interaction))
;     (desig-prop ?desig (:type :congrating))
;     (desig-prop ?desig (:to-agent ?agent-name))
;     (desig-prop ?desig (:with-content ?message))
;     ;;(desig-prop ?desig (:to-msg ?to-msg))
;     (lisp-fun make-greet-interaction :to-agent ?agent-name :with-content ?message ?interaction)) 

    )    

 ;;asking  
(def-fact-group asking-interaction-designators (interaction-grounding)
(<- (desig:interaction-grounding ?desig (demand ?interaction))
    (desig-prop ?desig (:type :demanding))
    (desig-prop ?desig (:to-agent ?agent-name))
    (desig-prop ?desig (:with-content ?message))
    (desig-prop ?desig (:to-msg ?to-msg))
    (lisp-fun make-deman-interaction :to-agent ?agent-name :with-content ?message :to-msg ?to-msg ?interaction))

(<- (desig:interaction-grounding ?desig (question ?interaction))
    (desig-prop ?desig (:type :questioning))
    (desig-prop ?desig (:to-agent ?agent-name))
    (desig-prop ?desig (:with-content ?message))
    (desig-prop ?desig (:to-msg ?to-msg))
    (lisp-fun make-receive-interaction :to-agent ?agent-name :with-content ?message :to-msg ?to-msg ?interaction))

(<- (desig:interaction-grounding ?desig (request ?interaction))
    (desig-prop ?desig (:type :requesting))
    (desig-prop ?desig (:to-agent ?agent-name))
    (desig-prop ?desig (:with-content ?message))
    (desig-prop ?desig (:with-msg ?to-msg))
    (lisp-fun make-request-interaction :to-agent ?agent-name :with-content ?message :with-msg ?with-msg ?interaction))          
    )  

; (def-fact-group process-input-interaction-designators (interaction-grounding)
; (<- (desig:interaction-grounding ?desig (receive ?interaction))
;     (desig-prop ?desig (:type :receiving))
;     (desig-prop ?desig (:from-agent ?from-agent-desig))
;     (desig-prop ?desig (:with-content ?message))
;     (desig-prop ?desig (:from-msg ?from-msg-desig))
;     (lisp-fun make-receive-interaction :from-agent ?from-agent-desig :with-content ?message :from-msg ?from-msg-desig interaction))    
;     )

; (def-fact-group process-input-interaction-designators (interaction-grounding)
; (<- (desig:interaction-grounding ?desig (receive ?interaction))
;     (desig-prop ?desig (:type :receiving))
;     (desig-prop ?desig (:from ?agent-desig))
;     (desig-prop ?desig (:content ?msg-desig))
;     (desig-prop ?desig (:context ?context))
;     (desig-prop ?desig (:ontology ?ontology))
;     (lisp-fun make-receive-interaction :from ?agent-desig :content ?msg-desig :context ?context :ontology ?ontology ?interaction))    
;     )
;; (def-fact-group process-input-interaction-designators (interaction-grounding)
;; (<- (desig:interaction-grounding ?desig (understand-w-ctx ?interaction))
;;     (desig-prop ?desig (:type :clarifying))
;;     (desig-prop ?desig (:input ?input))
;;     (desig-prop ?desig (:context ?context))
;;     (lisp-fun make-understand-interaction :input ?input :context ?context ?interaction))    
;;     )

;; (defstruct reply-interaction
;;   "Represents ........"
;;   message
;; )

;; (def-fact-group process-message-interaction-designators (interaction-grounding)
;; (<- (desig:interaction-grounding ?desig (reply ?interaction))
;;     (desig-prop ?desig (:type :replying))
;;     (desig-prop ?desig (:message ?message))
;;     (lisp-fun make-reply-interaction :message ?message ?interaction))    
;;     )