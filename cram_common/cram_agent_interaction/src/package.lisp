(defpackage :cram-agent-interaction
  (:nicknames :agin)
  (:use :cpl
        :roslisp
        :cl-transforms
        :cram-designators
        :cram-process-modules
        :cram-language-designator-support)
  (:import-from :cram-prolog :def-fact-group :<- :lisp-fun)
  )
