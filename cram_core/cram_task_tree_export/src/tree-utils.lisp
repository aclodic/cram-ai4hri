;;;
;;; Copyright (c) 2022, Arthur Niedzwiecki <aniedz@cs.uni-bremen.de>
;;;
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;
;;;     * Redistributions of source code must retain the above copyright
;;;       notice, this list of conditions and the following disclaimer.
;;;     * Redistributions in binary form must reproduce the above copyright
;;;       notice, this list of conditions and the following disclaimer in the
;;;       documentation and/or other materials provided with the distribution.
;;;     * Neither the name of the Intelligent Autonomous Systems Group/
;;;       Technische Universitaet Muenchen nor the names of its contributors
;;;       may be used to endorse or promote products derived from this software
;;;       without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(in-package :tt-export)

(defun get-task-tree (&optional (tree-name :top-level))
  "Returns the task-tree object."
  (gethash tree-name cpl-impl::*top-level-task-trees*))

(defun node-children (node)
  "Returns all children of the given node, only including successful executions"
  (remove-if-not #'node-valid
                 (mapcar 'cdr (cpl-impl:task-tree-node-children node))))

(defun node-valid (node)
  "Checks if given node execution :SUCCEEDED.
Moves step-by-step down through slots 'task', 'status' and 'value'"
  (let ((slinky (cpl-impl:task-tree-node-code node)))
    (and (setf slinky (slot-value slinky 'task))
         (setf slinky (slot-value slinky 'status))
         (setf slinky (slot-value slinky 'value))
         (eq slinky :SUCCEEDED))))

(defun node->designator (node)
  "Provides a node's designator, if it has any."
  (when (cpl-impl:task-tree-node-code node)
    (car (slot-value (cpl-impl:task-tree-node-code node) 'parameters))))

