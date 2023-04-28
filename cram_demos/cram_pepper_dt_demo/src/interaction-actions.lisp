
(in-package :pepper-dt-demo)

(defun inform-about-topic (?topic &optional (?info nil))
  (ecase ?topic
   (:task (setf ?sentence (say-task)))
   (:wrong-cube (setf ?sentence (say-wrong-cube)))
   (:scan (setf ?sentence (say-scan)))
   (:rescan (setf ?sentence (say-rescan)))
   (:discover (setf ?sentence (say-discover-cube ?info)))
   (:impossible-take (setf ?sentence (say-impossible-take)))
   (:not-understand (setf ?sentence (say-not-understand)))
   (:designate-cube (setf ?sentence (say-designate-cube)))
   (:no-cube-take (setf ?sentence (say-no-cube-take)))
   (:waiting (setf ?sentence (say-waiting)))
   (:good-cube (setf ?sentence (say-point-good-cube))))  
 (values ?sentence))

(defun explain-topic (?topic &optional (?info nil))
  (ecase ?topic
   (:task (setf ?sentence (say-explain ?info))))
 (values ?sentence))

(defun request-topic (?topic &optional (?info nil))
  (ecase ?topic
   (:take-cube (setf ?sentence (take-cube-sentence)))
   (:help (setf ?sentence (say-help ?info)))
   (:answers (setf ?sentence (say-question)))
 (values ?sentence)))

(defun tell-sentence (?intention  &optional (?topic nil) (?info nil) )
  (ecase ?intention
   (:greet (setf ?sentence (say-hello)))
   (:end (setf ?sentence (say-goodbye)))
   (:congrate (setf ?sentence (say-congrate)))
   (:cancel (setf ?sentence (say-cancel)))
   (:continue-task (setf ?sentence (say-continue-task)))
   (:inform (setf ?sentence (inform-about-topic ?topic ?info)))
   (:explain (setf ?sentence (explain-topic ?topic ?info))))
 (values ?sentence))

(defun ask-sentence (?intention  &optional (?info nil) (?topic nil)) ;;
  (ecase ?intention
   (:request (setf ?sentence (request-topic :take-cube ))))  ;;justofrnow
 (values ?sentence))

(defun interaction-tell (intention agent &optional (topic nil) (details nil))
 (let ((?intention intention) (?agent agent) (?topic topic) (?info details))
       (exe:perform (desig:an action (type telling) (:intention ?intention) (:topic ?topic) (:details ?info) (:to-agent ?agent)))))

(defun interaction-ask (intention agent &optional (topic nil))
 (let ((?intention intention) (?agent agent) (?topic topic))
       (exe:perform (desig:an action (type asking) (:intention ?intention) (:topic ?topic)(:to-agent ?agent)))))

(defun ask (&key ((:intention ?intention)) ((:to-agent ?agent)) ((:topic ?topic))  &allow-other-keys)
 (let ((?type ?intention) (?to-agent ?agent) (?about ?topic))
  (let ((?message (ask-sentence ?type :?topic ?about)))
   (agin::construct-me-interaction-designator ?message ?type ?about ?to-agent)
   (pm-execute 'pepper-communication (desig:a motion (type speaking) (:sentence ?message))))))

(defun tell (&key ((:intention ?intention)) ((:to-agent ?agent)) ((:topic ?topic)) ((:details ?info)) &allow-other-keys)
 (let ((?type ?intention) (?to-agent ?agent) (?about ?topic) (?word ?info))
  (let ((?message (tell-sentence ?type ?about ?word )))
   (agin::construct-me-interaction-designator ?message ?type ?about ?to-agent)
   (pm-execute 'pepper-communication (desig:a motion (type speaking) (:sentence ?message))))))