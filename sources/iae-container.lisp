;============================================================================
; OM# IAE (ISMM Audio Engine)
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
   (duration :accessor duration :initarg :duration :initform 100 :documentation "duration of the grain")
   (gain :accessor gain :initform 1.0 :documentation "gain of the grain")
   (attack :accessor attack :initform 10 :documentation "attack time (ms)")
   (release :accessor release :initform 10 :documentation "release time (ms)")
   (outputgains :accessor outputgains :initform nil :documentation "list of gains for the IAE output channels")
   (outputdelays :accessor outputdelays :initform nil :documentation "list of delays for the IAE output channels")
   (iae-params :accessor iae-params :initform nil :documentation "a list of (name value(s)) for other IAE parameters")
   )
  (:documentation "A granular-synthesis request for IAE, based on source/position data."))

;;; IAE request's pos and source are "hidden" dans get determined by IAE.knn
(defclass! IAE-request (IAE-grain)
  ((om::date :accessor om::date :initarg :date :initform 0 :documentation "date/time of the grain")
   (descriptor :accessor descriptor :initarg :descriptor :initform 0 :documentation "the descriptor number inside IAE/pipo")
   (value :accessor value :initarg :value :initform 0.0 :documentation "the value of the descriptor")
   (weight :accessor weight :initarg :weight :initform 1.0 :documentation "the weight of the descriptor")
   (duration :accessor duration :initarg :duration :initform 100 :documentation "duration of the grain")
   )
  (:documentation "A granular-synthesis request for IAE, based on sound descritor value and weight.

<descriptor>, <value>, and <weight> can be single values or lists (of the same length!).
"))

(defmethod om::additional-class-attributes ((self IAE-grain))
  '(gain attack release outputgains outputdelays iae-params))

(defmethod om::item-get-duration ((self IAE-grain)) (duration self))


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
  ((iae-obj :accessor iae-obj :initarg :iae-obj :initform nil)
   (grains :initarg :grains :initform nil :documentation "a list of timed-requests for granular synthesis")
   (iae-params :accessor iae-params :initarg :iae-params :initform nil :documentation "a list of (name value(s)) for global IAE parameters")

   (max-dur :accessor max-dur :initform 10000 :documentation "max duration fo the audio output buffer [ms]")
   (value-ranges :accessor value-ranges :initform nil :documentation "ranges for internal descriptor values")
   (buffer-player :accessor buffer-player :initform nil)
   )
  (:default-initargs :default-frame-type 'IAE-grain)
  (:documentation "IAE-container is a container for granular synthesis events that are computed dynamically from an IAE object")
  )

(defmethod om::additional-class-attributes ((self IAE-container)) '(max-dur))

(defmethod om::play-obj? ((self IAE-container)) t)
(defmethod om::get-obj-dur ((self IAE-container)) (max-dur self))


(defmethod initialize-instance ((self IAE-container) &rest initargs)
  (call-next-method)
  (om::data-stream-set-frames self (slot-value self 'grains))
  (setf (slot-value self 'grains) nil)
  self)

(defmethod grains ((self IAE-container))
  (om::data-stream-get-frames self))


(defmethod om::om-init-instance :after ((self IAE-container) &optional initargs)

  (when (iae-obj self)

    (om::om-print-dbg "Initializing IAE-container for ~A" (list self) "OM-IAE")

    (let* ((sr (samplerate (iae-obj self)))
           (size (round (* (max-dur self) sr) 1000))
           (nch (channels (iae-obj self))))

      (let ((audio-buffer (fli::allocate-foreign-object
                           :type :pointer :nelems nch
                           :initial-contents (loop for c from 1 to nch
                                                   collect
                                                   (fli::allocate-foreign-object :type :float :nelems size :initial-element 0.0)))))

        (setf (buffer-player self)
              (om::make-player-from-buffer audio-buffer size nch sr))

        )))

  (set-value-ranges self)
  )


(defmethod om::om-cleanup ((self IAE-container))
  (when (buffer-player self)
    (om::free-buffer-player (buffer-player self))))


;;; by convention descriptor #-1 is just the position in source
(defmethod set-value-ranges ((self IAE-container))

  (loop for grain in (grains self) do

        (if (typep grain 'IAE-request)

            ;;; else: IAE-request
            (loop for d in (om::list! (descriptor grain))
                  for v in (om::list! (value grain)) do

                  (let ((pos (position d (value-ranges self) :key #'car)))
                    (if pos

                        (setf (car (cadr (nth pos (value-ranges self))))
                              (min v (car (cadr (nth pos (value-ranges self)))))
                              (cadr (cadr (nth pos (value-ranges self))))
                              (max v (cadr (cadr (nth pos (value-ranges self))))))

                      (push (list d (list v v))
                            (value-ranges self))))
                  )

          ;;; get pos-range
          (let ((pos (position -1 (value-ranges self) :key #'car)))
            (if pos

                (setf (car (cadr (nth pos (value-ranges self))))
                      (min (pos grain) (car (cadr (nth pos (value-ranges self)))))
                      (cadr (cadr (nth pos (value-ranges self))))
                      (max (pos grain) (cadr (cadr (nth pos (value-ranges self))))))

              (push (list -1 (list (pos grain) (pos grain)))
                    (value-ranges self))))

          )))


;;;=================
;;; OM API/Editors
;;;=================

(defmethod om::data-frame-text-description ((self IAE-grain))
  `("IAE GRAIN:" ,(format nil "~D in source ~A" (pos self) (source self))))

(defmethod om::data-frame-text-description ((self IAE-request))
  `("IAE REQUEST:" ,(format nil "desc. ~A : ~D" (descriptor self) (value self))))


;;; (0 100) is the reference range
(defmethod om::y-range-for-object ((self IAE-Container)) '(-10 110))

#|
  (let ((reference-descriptor
  (let* ((posy-list (loop for item in (grains self) collect (om::get-frame-posy item)))
         (posy-min (list-min posy-list))
         (posy-max (list-max posy-list))
         (margin (if (= posy-max posy-min) 10
                   (* (min 100 (- posy-max posy-min) 0.1)))))

    (list (- posy-min margin) (+ posy-max margin))))
|#

;;; GRAPHICAL ATTRIBUTES FOR GRANULAR GRAINS

(defmethod om::get-frame-color ((self IAE-grain))
  (oa::om-make-color-alpha (om::get-midi-channel-color (1+ (source self))) 0.5))

(defmethod om::get-frame-sizey ((self IAE-grain)) 5)

(defmethod om::get-frame-posy ((self IAE-grain)) (pos self))


(defmethod om::get-frame-area ((frame IAE-grain) editor)
  (let ((panel (om::active-panel editor))
        (container (om::object-value editor)))

    (values ;; x
            (om::x-to-pix panel (om::date frame))
            ;; y
            (- (om::h panel)
               (om::y-to-pix panel (* 100 (/ (om::get-frame-posy frame)
                                             (or (second (second (find -1 (value-ranges container) :key #'car)))
                                                 (max-dur container))))))
            ;; w
            (max 3 (om::dx-to-dpix panel (om::get-frame-graphic-duration frame)))
            ;; h
            (min -3 (om::dy-to-dpix panel (- (om::get-frame-sizey frame))))  ;; !! upwards
            )))


;;; SPECIFIC GRAPHICAL ATTRIBUTES FOR DESCRIPTOR GRAINS

(defmethod om::get-frame-color ((self IAE-request))
  (om::om-make-color-alpha (om::get-midi-channel-color (1+ (car (om::list! (descriptor self))))) 0.5))

(defmethod om::get-frame-posy ((self IAE-request))
  (car (om::list! (value self))))


(defmethod om::get-frame-area ((frame IAE-request) editor)
  (let ((panel (om::active-panel editor))
        (container (om::object-value editor)))

    (values ;; x
            (om::x-to-pix panel (om::date frame))
            ;; y
            (let* ((range-y (cadr (find (car (om::list! (descriptor frame)))
                                        (value-ranges container) :key #'car)))
                   (min-y (first range-y))
                   (max-y (second range-y)))

              (if (= min-y max-y) (setf max-y (1+ min-y))) ;; avoid /0

              (- (om::h panel)
                 (om::y-to-pix panel (* 100  (/ (- (om::get-frame-posy frame) min-y)
                                                (- max-y min-y))))))
            ;; w
            (max 3 (om::dx-to-dpix panel (om::get-frame-graphic-duration frame)))
            ;; h
            (min -3 (om::dy-to-dpix panel (- (om::get-frame-sizey frame))))  ;; !! upwards
            )))


;;;===============================
;;; AUDIO RENDERING
;;;===============================

;;; add an audio grain in the IAE-container's buffer-player by copying from an existing audio buffer (coming out of IAE-synth)
(defmethod iae-add-grain ((iae-c iae-container) (snd om::om-internal-sound) (at integer))
  (when (buffer-player iae-c)
    (let* ((bp (buffer-player iae-c))
           (iae (iae-obj iae-c))
           (nch (channels iae))
           (pos (round (* at (om::bp-sample-rate bp)) 1000))
           (size (om::n-samples snd))
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


(defmethod make-grain-from-frame ((self IAE-Container) (frame IAE-grain))
  (when (iae-obj self)
    (iae-synth (iae-obj self)
               (source frame) (pos frame) (duration frame)
               :gain (gain frame) :attack (attack frame) :release (release frame)
               :outputgains (outputgains frame) :outputdelays (outputdelays frame)
               :other-iae-params (iae-params frame))))

(defmethod make-grain-from-frame ((self IAE-Container) (frame IAE-request))
  (when (iae-obj self)
    (iae-synth-desc (iae-obj self)
                    (descriptor frame) (value frame) (weight frame) (duration frame)
                    :gain (gain frame) :attack (attack frame) :release (release frame)
                    :outputgains (outputgains frame) :outputdelays (outputdelays frame)
                    :other-iae-params (iae-params frame))))


;;; VIRTUAL RUN / DUMP:
(om::defmethod! iae-dump ((self IAE-Container))

  :doc "Generates a sound from the IAE-Containers contents and settings"

  (let* ((iae (iae-obj self))
         (nch (channels iae))
         (sr (samplerate iae))
         (size (round (* (max-dur self) sr) 1000))
         (init-buffer (om::bp-buffer (buffer-player self)))
         (out-buffer (om::make-audio-buffer nch size :float)))

    (loop for frame in (om::data-stream-get-frames self)
          do

          (make-iae-param-calls (iaeengine-ptr iae) (iae-params self)) ;; "global params"

          (iae-add-grain
           self
           (make-grain-from-frame self frame)
           (om::date frame))
          )

    (dotimes (ch nch)
      (dotimes (smp size)
        (setf (cffi::mem-aref (cffi::mem-aref out-buffer :pointer ch) :float smp)
              (cffi::mem-aref (cffi::mem-aref init-buffer :pointer ch) :float smp))))

    (make-instance 'sound
                   :buffer (om::make-om-sound-buffer-GC :ptr out-buffer :nch nch)
                   :n-samples size
                   :n-channels nch
                   :sample-rate sr
                   :smpl-type :float)
    ))


;;; PLAYER:
;;; reports actions to audio player
(defmethod om::get-action-list-for-play ((object IAE-Container) interval &optional parent)
  (om::external-player-actions object interval parent))


;;; This is the action performed when we "play" an IAE object
(defmethod om::get-computation-list-for-play ((object IAE-Container) &optional interval)

  (if (iae-obj object)

      (loop for frame in (remove-if #'(lambda (date) (and interval (or (< date (car interval)) (>= date (cadr interval)))))
                                    (om::data-stream-get-frames object)
                                    :key 'om::date)
            collect
            (list
             0
             (om::date frame)
             #'(lambda ()

                 (make-iae-param-calls (iaeengine-ptr (iae-obj object)) (iae-params object)) ;; "global params"

                 (iae-add-grain
                  object
                  (make-grain-from-frame object frame)
                  (om::date frame))
                 )
             ))

    (progn
      (om::om-print "Error playing IAE-container: no IAE engine loaded!")
      nil)
    ))


(defmethod iae-reset ((self IAE-Container))
  (when (buffer-player self)
    (dotimes (c (om::bp-channels (buffer-player self)))
      (dotimes (i (om::bp-size (buffer-player self)))
        (setf (fli:dereference
               (fli:dereference (om::bp-buffer (buffer-player self)) :index c :type :pointer)
               :index i :type :float)
              0.0)))))


(defmethod iae-reset-interval ((self IAE-Container) from-ms to-ms)
  (when (buffer-player self)

    (let* ((sr (om::bp-sample-rate (buffer-player self)))
           (from (if from-ms (round (* from-ms sr) 1000) 0))
           (to (if to-ms (round (* to-ms sr) 1000) (1- (om::bp-size (buffer-player self))))))

      (dotimes (c (om::bp-channels (buffer-player self)))
        (loop for i from from to to do
              (setf (fli:dereference
                     (fli:dereference (om::bp-buffer (buffer-player self)) :index c :type :pointer)
                     :index i :type :float)
                    0.0)))
      )))


(defmethod om::player-play-object ((self om::scheduler) (object IAE-Container) caller &key parent interval)
  (declare (ignore parent))
  (let ((bp (buffer-player object)))

    (om::set-object-time-window object 100)

    (om::lock-edit object)

    (if bp
        (om::start-buffer-player bp
                                 :start-frame (if (car interval)
                                                  (round (* (car interval) (/ (om::bp-sample-rate bp) 1000.0)))
                                                (or (car interval) 0)))
      (om::om-beep-msg "No audio output buffer initialized for IAE-Container!"))
    (call-next-method)))

(defmethod om::player-stop-object ((self om::scheduler) (object IAE-Container))

  (let ((current-state (om::state self)))

    (if (buffer-player object)
        (om::stop-buffer-player (buffer-player object))
      (om::om-beep-msg "No audio output buffer initialized for IAE-Container!"))

    (unless (eq current-state :stop)
      (iae-reset object))

    (om::unlock-edit object)

    (call-next-method)))

(defmethod om::player-pause-object ((self om::scheduler) (object IAE-Container))
  (if (buffer-player object)
      (om::pause-buffer-player (buffer-player object))
    (om::om-beep-msg "No audio output buffer initialized for IAE-Container!"))
  (call-next-method))

(defmethod om::player-continue-object ((self om::scheduler) (object IAE-Container))
  (if (buffer-player object)
      (om::continue-buffer-player (buffer-player object))
    (om::om-beep-msg "No audio output buffer initialized for IAE-Container!"))
  (call-next-method))

(defmethod om::set-object-time ((self IAE-Container) time)
  (iae-reset self)
  (when (buffer-player self)
    (om::jump-to-time (buffer-player self) time))
  (call-next-method))

(defmethod om::set-time-callback ((self IAE-Container) time)
  (when (buffer-player self)
    (om::jump-to-time (buffer-player self) time))
  (call-next-method))

(defmethod om::reschedule-callback ((self IAE-Container) interval)
  (declare (ignore interval))
  (iae-reset self))
