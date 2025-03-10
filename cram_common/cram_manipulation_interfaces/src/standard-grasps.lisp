;;;
;;; Copyright (c) 2018, Gayane Kazhoyan <kazhoyan@cs.uni-bremen.de>
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
;;;     * Neither the name of the Institute for Artificial Intelligence/
;;;       Universitaet Bremen nor the names of its contributors may be used to
;;;       endorse or promote products derived from this software without
;;;       specific prior written permission.
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

(in-package :cram-manipulation-interfaces)

(defparameter *sin-pi/4* (sin (/ pi 4)))
(defparameter *-sin-pi/4* (- *sin-pi/4*))
(defparameter *sin-pi/6* 0.5)
(defparameter *-sin-pi/6* -0.5)
(defparameter *sin-pi/3* (sin (/ pi 3)))
(defparameter *-sin-pi/3* (- *sin-pi/3*))

;; back / front
(defparameter *x-across-z-grasp-rotation*
  '(( 0  0 -1)
    (-1  0  0)
    ( 0  1  0)))
(defparameter *x-across-z-grasp-rotation-2*
  '((0  0 -1)
    (1  0  0)
    (0 -1  0)))
(defparameter *-x-across-z-grasp-rotation*
  '((0 0 1)
    (1 0 0)
    (0 1 0)))
(defparameter *-x-across-z-grasp-rotation-2*
  '(( 0  0  1)
    (-1  0  0)
    ( 0 -1  0)))
(defparameter *x-across-y-grasp-rotation*
  '(( 0  0 -1)
    ( 0 -1  0)
    (-1  0  0)))
(defparameter *x-across-y-30-deg-grasp-rotation*
  `((,*sin-pi/6*   0  ,*-sin-pi/3*)
    (0            -1  0)
    (,*-sin-pi/3*  0  ,*-sin-pi/6*)))
(defparameter *x-across-y-24-deg-grasp-rotation*
  `((,(sin (/ pi 7.5))   0  ,(- (sin (/ pi 2.73))))
    (0            -1  0)
    (,(- (sin (/ pi 2.73)))  0  ,(- (sin (/ pi 7.5))))))
(defparameter *-x-across-y-grasp-rotation*
  '(( 0  0  1)
    ( 0  1  0)
    (-1  0  0)))
(defparameter *-x-across-y-flipped-grasp-rotation*
  '((0  0  1)
    (0 -1  0)
    (1  0  0)))

;; side
(defparameter *y-across-z-grasp-rotation*
  '((1  0  0)
    (0  0 -1)
    (0  1  0)))
(defparameter *-y-across-z-grasp-rotation*
  '((-1 0 0)
    ( 0 0 1)
    ( 0 1 0)))
(defparameter *y-across-z-flipped-grasp-rotation*
  '((-1  0  0)
    ( 0  0 -1)
    ( 0 -1  0)))
(defparameter *-y-across-z-flipped-grasp-rotation*
  '((1  0 0)
    (0  0 1)
    (0 -1 0)))
(defparameter *y-across-x-grasp-rotation*
  '((0   1  0)
    (0   0  -1)
    (-1  0  0)))
(defparameter *-y-across-x-grasp-rotation*
  '((0  -1  0)
    (0   0  1)
    (-1  0  0)))

;; top
(defparameter *z-across-x-grasp-rotation*
  '((0  1  0)
    (1  0  0)
    (0  0 -1)))
(defparameter *z-across-y-grasp-rotation*
  '((1  0  0)
    (0 -1  0)
    (0  0 -1)))
(defparameter *z-diagonal-grasp-rotation*
  `((,*-sin-pi/4* ,*sin-pi/4*  0)
    (,*sin-pi/4*  ,*sin-pi/4*  0)
    (0            0           -1)))

;; bottom
(defparameter *-z-across-x-grasp-rotation*
  '((0 -1  0)
    (1  0  0)
    (0  0  1)))
