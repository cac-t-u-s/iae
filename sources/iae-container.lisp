;============================================================================
; OM-IAE (ISMM Audio Engine)
; Author: Jean Bresson, Diemo Schwarz
;============================================================================
;
;   This program is free software. For information on usage 
;   and redistribution, see the "LICENSE" file in this distribution.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;
;============================================================================
; File author: J. Bresson
;============================================================================

;;=======================================
;; IAE GRANULMAR-SYNTH CONTAINER
;;=======================================

(in-package :iae)


;;;============================================
;;; DATA-STREAM ACTIONS FOR IAE-CONTAINER
;;;============================================

(defclass! IAE-grain (om::data-frame)
   ((om::date :accessor om::date :initarg :date :initform 0 :documentation "date/time of the grain")
    (source :accessor source :initarg :source :initform 0 :documentation "source num inside IAE")
    (pos :accessor pos :initarg :pos :initform 0 :documentation "position in source")
    (duration :accessor duration :initarg :duration :initform 100 :documentation "duration of the grain"))
   (:documentation "A granular-synthesis request for IAE, based on source/position data."))

(defclass! IAE-request (om::data-frame)
   ((om::date :accessor om::date :initarg :date :initform 0 :documentation "date/time of the grain")
    (descriptor :accessor descriptor :initarg :descriptor :initform 0 :documentation "the descriptor number inside IAE/pipo")
    (value :accessor value :initarg :value :initform 0.0 :documentation "the value of the descriptor")
    (weight :accessor weight :initarg :weight :initform 1.0 :documentation "the weight of the descriptor")
    (duration :accessor duration :initarg :duration :initform 100 :documentation "duration of the grain"))
   (:documentation "A granular-synthesis request for IAE, based on sound descritor value and weight.

<descriptor>, <value>, and <weight> can be single values or lists (of the same length!).
"))


;;; utils to generate random grains / requests
(defun make-IAE-grains (n &key (nsources 1) (maxpos 2500) (durtot 10000) (mindur 100) (maxdur 600))
  (sort 
   (loop for i from 1 to n collect
         (make-instance 'IAE-grain :date (random durtot)
                        :source (random (1- nsources))
                        :pos (random maxpos)
                        :duration (+ mindur (random (- maxdur mindur)))))
   '< :key 'om::date))

(defun gen-random-requests (n &key (descriptor 0) (minval 100) (maxval 1000) (durtot 10000) (mindur 50) (maxdur 300))
  (sort 
   (loop for i from 1 to n collect
         (make-instance 'IAE-request :date (random durtot)
                        :descriptor descriptor
                        :value (+ minval (random (- maxval minval)))
                        :duration (+ mindur (random (- maxdur mindur)))))
   '< :key 'om::date))



;;;============================================
;;; IAE-CONTAINER
;;;============================================

(defclass! IAE-container (om::om-cleanup-mixin om::data-stream)
 ((iae :accessor iae :initarg iae :initform nil)
  (grains :accessor grains :initarg :grains :initform nil :documentation "a list of timed-requests for granular synthesis")
  (max-dur :accessor max-dur :initform 10000 :documentation "max duration fo the audio output buffer [ms]")
  (buffer-player :accessor buffer-player :initform nil) ;;;
  )
 (:default-initargs :default-frame-type 'IAE-grain)
 (:documentation "IAE-container is a container for granular synthesis events that are computed dynamically from an IAE object") 
 )

(defmethod om::additional-class-attributes ((self iae::IAE-container)) '(iae::max-dur))
(defmethod om::data-stream-frames-slot ((self iae::IAE-container)) 'iae::grains)

(defmethod om::play-obj? ((self iae::IAE-container)) t)
(defmethod om::get-obj-dur ((self iae::IAE-container)) (iae::max-dur self))


(defmethod om::om-init-instance :after ((self iae::IAE-container) &optional initargs)
  
  (when (iae self)
    
    (om::om-print-dbg "Initializing IAE-container for ~A" (list self) "OM-IAE")
    
    (let* ((sr (iae::samplerate (iae self)))
           (size (round (* (iae::max-dur self) sr) 1000))
           (nch (iae::channels (iae self))))
    
      (om::set-object-time-window self 100)
      
      (let ((audio-buffer (fli::allocate-foreign-object  
                           :type :pointer :nelems nch
                           :initial-contents (loop for c from 1 to nch
                                                   collect
                                                   (fli::allocate-foreign-object :type :float :nelems size :initial-element 0.0)))))
        
        (setf (iae::buffer-player self) 
              (om::make-player-from-buffer audio-buffer size nch sr))
        
        ))))


(defmethod om::om-cleanup ((self iae::IAE-container))
  (when (iae::buffer-player self) 
    (om::free-buffer-player (iae::buffer-player self)))
  )


;;;=================
;;; OM API/Editors 
;;;=================

(defmethod om::item-duration ((self iae::IAE-grain)) (iae::duration self))

(defmethod om::data-frame-text-description ((self iae::IAE-grain)) 
  `("IAE GRAIN:" ,(format nil "~D in source ~A" (iae::pos self) (iae::source self))))
(defmethod om::data-frame-text-description ((self iae::IAE-request)) 
  `("IAE REQUEST:" ,(format nil "desc. ~A = ~D" (iae::descriptor self) (iae::value self))))

(defmethod om::y-range-for-object ((self iae::IAE-Container)) '(-1000 3000))

(defmethod om::get-frame-color ((self iae::IAE-grain)) 
  (oa::om-make-color-alpha (om::get-midi-channel-color (1+ (iae::source self))) 0.5))

(defmethod om::get-frame-posy ((self iae::IAE-grain)) 
  (+ 50 (iae::pos self)))

(defmethod om::get-frame-sizey ((self iae::IAE-grain)) 
  (or (getf (om::attributes self) :posy)
      (setf (getf (om::attributes self) :posy) (+ 200 (om::om-random -50 50)))))

(defmethod om::get-frame-color ((self iae::IAE-request)) 
  (om::om-make-color-alpha (om::get-midi-channel-color (1+ (iae::descriptor self))) 0.5))

(defmethod om::get-frame-posy ((self iae::IAE-request)) 
  (iae::value self))

(defmethod om::get-frame-sizey ((self iae::IAE-request)) 
  (or (getf (om::attributes self) :posy)
      (setf (getf (om::attributes self) :posy) (+ 200 (om::om-random -50 50)))))



;;;===============================
;;; AUDIO RENDERING
;;;===============================

;;; add an audio grain in the IAE-container's buffer-player by copying from an existing audio buffer (coming out of IAE synth)
(defmethod iae-add-grain ((iae-c iae-container) (snd om::om-internal-sound) (dur integer) (at integer))
  (when (iae::buffer-player iae-c)
    (let* ((bp (iae::buffer-player iae-c))
           (iae (iae iae-c))
           (nch (iae::channels iae))
           (pos (round (* at (om::bp-sample-rate bp)) 1000))
           (size (round (* dur (om::bp-sample-rate bp)) 1000))
           (max-size (om::bp-size bp)))
      (dotimes (c nch)
        (dotimes (i size)
          (unless (>= i max-size)
            (setf (fli:dereference 
                   (fli:dereference (om::bp-buffer bp) :index c :type :pointer)
                   :index (+ pos i) :type :float)
                  (+ (fli:dereference 
                      (fli:dereference (om::bp-buffer bp) :index c :type :pointer) 
                      :index (+ pos i) :type :float)
                     (fli:dereference 
                      (fli:dereference (om::om-sound-buffer-ptr (om::buffer snd)) :index c :type :pointer)
                      :index i :type :float)))
            ))))))


(defmethod make-grain-from-frame ((self iae::IAE-Container) (frame iae::IAE-grain))
  (when (iae self)
    (iae::iae-synth (iae self) (source frame) (iae::pos frame) (iae::duration frame))))

(defmethod make-grain-from-frame ((self iae::IAE-Container) (frame iae::IAE-request))
 (when (iae self)
   (iae::iae-synth-desc (iae self) (iae::descriptor frame) (iae::value frame) (iae::weight frame) (iae::duration frame))))



(defmethod om::get-action-list-for-play ((object iae::IAE-Container) interval &optional parent)
  (om::external-player-actions object interval parent))

;;; This is the action performed when we "play" an IAE object
(defmethod om::get-computation-list-for-play ((object iae::IAE-Container) &optional interval)
  (loop for frame in (remove-if #'(lambda (date) (or (< date (car interval)) (>= date (cadr interval))))
                                (om::data-stream-get-frames object) 
                                :key 'om::date)
        do (iae-add-grain object
                          (make-grain-from-frame object frame)
                          (duration frame) (om::date frame))
        )
  nil)


(defmethod iae-reset ((self iae::IAE-Container))
  (when (iae::buffer-player self)
    (dotimes (c (om::bp-channels (iae::buffer-player self)))
      (dotimes (i (om::bp-size (iae::buffer-player self)))
        (setf (fli:dereference 
               (fli:dereference (om::bp-buffer (iae::buffer-player self)) :index c :type :pointer) 
               :index i :type :float)
              0.0)))))

(defmethod om::player-play-object ((self om::scheduler) (object iae::IAE-Container) caller &key parent interval)
  (declare (ignore parent))
  (let ((bp (iae::buffer-player object)))
    (if bp
      (om::start-buffer-player bp 
                               :start-frame (if (car interval)
                                                (round (* (car interval) (/ (om::bp-sample-rate bp) 1000.0)))
                                              (or (car interval) 0)))
      (om::om-beep-msg "No BP initialized for IAE-Container!"))
    (call-next-method)))

(defmethod om::player-stop-object ((self om::scheduler) (object iae::IAE-Container))
  (let ((current-state (om::state self)))
    (if (iae::buffer-player object)
        (om::stop-buffer-player (iae::buffer-player object))
      (om::om-beep-msg "No BP initialized for IAE-Container!"))
    (unless (eq current-state :stop) 
      (iae-reset object))
    (call-next-method)))

(defmethod om::player-pause-object ((self om::scheduler) (object iae::IAE-Container))
  (if (iae::buffer-player object)
      (om::pause-buffer-player (iae::buffer-player object))
    (om::om-beep-msg "No BP initialized for IAE-Container!"))
  (call-next-method))

(defmethod player-continue-object ((self om::scheduler) (object iae::IAE-Container))
  (if (iae::buffer-player object)
      (om::continue-buffer-player (iae::buffer-player object))
    (om::om-beep-msg "No BP initialized for IAE-Container!"))
  (call-next-method))

(defmethod om::set-object-time ((self iae::IAE-Container) time) 
  (iae-reset self)
  (when (iae::buffer-player self)
    (om::jump-to-time (iae::buffer-player self) time))
  (call-next-method))



