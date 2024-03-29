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
;; Main class and functions
;;=======================================

(in-package :iae)

;;;==================================
;;; DESCRIPTORS
;;;==================================

(defvar *all-ircam-descriptiors*
  '("SignalZeroCrossingRate"
    "TotalEnergy"
    "SpectralCentroid"
    "SpectralSpread"
    "SpectralSkewness"
    "SpectralKurtosis"
    "SpectralRolloff"
    "SpectralVariation"
    "SpectralDecrease"
    "Loudness"
    "RelativeSpecificLoudness"
    "PerceptualTristimulus"
    "PerceptualOddToEvenRatio"
    "Sharpness"
    "Spread"
    "SpectralFlatness"
    "SpectralCrest"
    "SpectralSlope"
    "Chroma"
    "MFCC"
    "PerceptualSpectralDeviation"
    "PerceptualSpectralCentroid"
    "PerceptualSpectralSpread"
    "PerceptualSpectralSkewness"
    "PerceptualSpectralKurtosis"
    "PerceptualSpectralRolloff"
    "PerceptualSpectralVariation"
    "PerceptualSpectralDecrease"
    "PerceptualSpectralSlope"
    "FundamentalFrequency"
    "Inharmonicity"
    "HarmonicEnergy"
    "NoiseEnergy"
    "Noisiness"
    "HarmonicTristimulus"
    "HarmonicOddToEvenRatio"
    "HarmonicSpectralDeviation"
    "HarmonicSpectralCentroid"
    "HarmonicSpectralSpread"
    "HarmonicSpectralSkewness"
    "HarmonicSpectralKurtosis"
    "HarmonicSpectralVariation"
    "HarmonicSpectralDecrease"
    "HarmonicSpectralSlope"
    "HarmonicSpectralRolloff"))

;;; a reduced list of descriptors that is computed by default
(defparameter *default-ircamdescriptors*
  '("TotalEnergy"
    "FundamentalFrequency"
    "SpectralCentroid"
    "Loudness"
    "Sharpness"
    "Spread"
    "HarmonicEnergy"
    "Inharmonicity"
    "Noisiness"))


(om::defmethod! ircam-descriptors-names ()
  :doc "Returns the list of all available IrcamDescriptors.

Use items of this list to instancitate the :pipo-module attribute of IAE."
  *all-ircam-descriptiors*)


;;;==================================
;;; IAE
;;;==================================

(defclass! IAE (om::om-cleanup-mixin)
  ((iaeengine-ptr :accessor iaeengine-ptr :initform nil)
   (sounds :initarg :sounds :accessor sounds :initform nil :documentation "a sound or list of sounds to build the IAE container on")
   (channels :accessor channels :initform 1 :documentation "number of channels for audio output")
   (samplerate :accessor samplerate :initform 44100 :documentation "sample rate for audio output")
   (pipo-module :accessor pipo-module :initform "descr" :documentation "name of a pipo module for sound analysis")
   (descriptors :accessor descriptors :initform nil)
   (desc-tracks :accessor desc-tracks :initform nil)
   (segmentation :accessor segmentation :initform nil :documentation "segmentation params"))
  (:documentation "IAE is a multi-track container for sounds and sound descriptions are stored data.

 - Tracks can be computed and segmented using 'pipo' modules: \"desc\" \"ircamdescriptor\" \"slice:fft\" \"mfcc\" \"<desc,mfcc>\" ...

 - Segmentation is computed from the <segmentation> parameter which can be a chop-size or a list (module (param1 val1 (param2 val2) ...) where 'module' is one of \"chop\", \"onseg\", or \"gate\".
If <segmentation> is an integer value (chop-size), this value is considered the size (in milliseconds) for the \"chop\" segmentation mode."
   ))


(defmethod om::om-cleanup ((self iae::IAE))
  (when (iae::iaeengine-ptr self)
    (om::om-print-dbg "deleting engine of ~A [~A]" (list self (iae::iaeengine-ptr self)) "GC")
    (iae-lib::iae_delete (iae::iaeengine-ptr self))
    (setf (iae::iaeengine-ptr self) nil)
    ))


;;; called each time an instance is created
;;; => mostly memory allocations
(defmethod initialize-instance :after ((self iae::IAE) &rest initargs)
  (om::om-print-dbg "Initializing IAE for ~A" (list self) "IAE")
  (setf (iae::iaeengine-ptr self)
        (iae-lib::iae_new (iae::samplerate self) 512 (iae::channels self) 1
                          10000d0 100d0 ;; maxgrainduration, maxdelayduration (millisecond)
                          4800d0 ;; maxtransposition (midicents)
                          0.2d0 10000.0d0 ;; minperiod, maxperiod
                          ))
  )


;;======================================================
;; INITIALIZATION OF THE PIPO MODULE
;; module-name can be: "desc" "ircamdescriptor" "slice:fft" "mfcc" "<desc,mfcc>" "...:chop"
;;======================================================

(defmethod iae-init-pipo ((self iae))

  (let* ((*iae (iaeengine-ptr self))
         (main-pipo (if (listp (iae::pipo-module self)) "ircamdescriptor" (iae::pipo-module self)))
         (seg-list (if (numberp (segmentation self))
                       `("chop" ("size" ,(segmentation self)))
                     (segmentation self)))
         (pipo-string (if seg-list (concatenate 'string main-pipo ":" (car seg-list)) main-pipo)))

    (if (= 1 (iae-lib::iae_pipo_create *iae pipo-string))  ;;;  ; "basic" "ircamdescriptor" "mfcc" "slice:fft"  "...:chop"

        (let ()

          (when (string-equal main-pipo "ircamdescriptor")

            ;;; Set the PiPo ircamdescriptors we want to use (if we use this option with IAE)
            (let ((nparams (iae-lib::iae_pipo_param_num *iae))
                  (desc-list (if (listp (iae::pipo-module self)) (iae::pipo-module self) *default-ircamdescriptors*)))

              (loop for param-i from 0 to (1- nparams) do
                    (let ((name (iae-lib::iae_pipo_param_get_name *iae param-i)))
                      ;(om::om-print-dbg  "-- ~A (~A) = ~A"
                      ;                   (list (iae-lib::iae_pipo_param_get_description *iae param-i)
                      ;                         (iae-lib::iae_pipo_param_get_type *iae param-i)
                      ;                         (iae-lib::iae_pipo_param_get_ *iae param-i))
                      ;                   "OM-IAE")
                      (when (string-equal name "ircamdescriptor.descriptors")

            ;(let ((numdesc (iae-lib::iae_pipo_param_enum_get_num *iae name)))
            ;   (loop for d from 0 to (1- numdesc) do
            ;      (print  (iae-lib::iae_pipo_param_enum_get_element *iae name d))))
                        (loop for desc in desc-list
                              for d = 0 then (+ d 1) do
                              (iae-lib::iae_pipo_param_set_string *iae name d desc)
                              )
                        )
                      ))
              ))    ;;;  END SPECIFIC IrcamDescriptor SECTION

          ;(iae-lib::iae_pipo_param_set_int *iae "mvavrg.size" 0 10)

          (iae-lib::iae_pipo_param_set_int *iae "descr.hopsize" 0 512)
         ; (iae-lib::iae_pipo_param_set_int *iae "ircamdescriptor.hopsize" 0 512)
          (iae-lib::iae_pipo_param_set_int *iae "mfcc.hopsize" 0 512)

          (unless (find "mean" (cdr seg-list) :test #'string-equal :key #'car)
            (setf seg-list (append seg-list
                                   '(("mean" 1)))))

          (loop for param in (cdr seg-list) do
                ;;; set, e.g. "chop.size" etc.
                (let ((param-name (concatenate 'string (car seg-list) "." (car param))))
                  (iae-lib::iae_pipo_param_set_int *iae param-name 0 (cadr param))))

          (setf (desc-tracks self) ;;; compute descriptors and collect track indices
                (loop for i from 0 to (1- (length (sounds self)))
                      collect (iae-lib::iae_pipo_run *iae i)))

          (let ((num (iae-lib::iae_get_numdescriptors *iae)))
            (om::om-print-dbg  "[~D] descriptors" (list num) "IAE")
            (setf (descriptors self)
                  (loop for i from 0 to (1- num)
                        collect (let ((desc-name
                                       (or (ignore-errors
                                             (iae-lib::iae_get_descriptorname *iae i))
                                           (format nil "desc_~D" (1+ i)))))
                                  ;(om::om-print desc-name)
                                  desc-name
                                  )))
            ))

      (om::om-print "Error initializing PiPo" "IAE"))
    ))


;;; called additionally (and later) when an instance is created
;;; or updated in the visual program execution/manipulations
(defmethod om::om-init-instance ((self iae::IAE) &optional args)

  (setf (iae::sounds self) (om::list! (iae::sounds self)))

  (when (iae::sounds self)

    ;;; can be called several times for the different sources
    (loop for s in (iae::sounds self)
          for i from 0 do
          (if (om::get-sound-file s)
              (iae-lib::iae_read (iae::iaeengine-ptr self) (namestring (om::get-sound-file s)) (cffi-sys::null-pointer))
            (iae-lib::iae_read_buffer (iae::iaeengine-ptr self) (format nil "BufferSource_~D" i)
                                      (om::n-samples s) (om::n-channels s)
                                      ;;; if more than 1 channel, this should be an interleaved audio buffer !!
                                      (fli:dereference (om::om-sound-buffer-ptr (om::buffer s)) :index 0 :type :pointer)
                                      (coerce (om::sample-rate s) 'double-float))
            ))

    ;(iae-lib::iae_set_MarkerTrackSdif (iaeengine-ptr self) -1 "XCUD" "XCUD")
    ;(iae-lib::iae_set_DescriptorTrackSdif (iaeengine-ptr self) -1 "XCUD" "XCUD")

    (iae-init-pipo self)

    (om::om-print (iae::iae-info self) "IAE")

    (iae-lib::iae_update_kdtree (iae::iaeengine-ptr self) T)
    (om::om-print-dbg "KDTree updated." nil "IAE")
    (iae-lib::iae_set_SynthMode (iae::iaeengine-ptr self) 1)
    (om::om-print-dbg "IAE engine ready!" nil "IAE"))

  self)


; (gc-all)


;;;==============================================================================
;;; Connections with OM visual environment
;;;==============================================================================

(defmethod om::additional-class-attributes ((self iae::IAE))
  '(iae::channels iae::pipo-module iae::segmentation iae::samplerate))


;;;======================================================
;;; READ FROM IAE
;;;======================================================

(defun iae-info (iae)
  (iae-lib::iae_info_get_string (iaeengine-ptr iae) (oa::om-make-null-pointer)))


;;;=========================
;;; DESCRIPTORS
;;;=========================

(om::defmethod! iae-descriptors ((self iae))
  :doc "Returns the names of all descriptor tracks computed in an IAE instance.

 Note: some desciptor names used at initialization (e.g. MFCC, SpectralCrest, ...) produce more than one descriptor tracks (e.g. MFCC by default produces 12 tracks corresponding to 12 MFCC coefficients)."

  (descriptors self))


(om::defmethod! get-sound-descriptors ((self iae) src-index &optional (t1 0) (t2 nil) normalized)

  :indoc '("An IAE instance" "source index" "min time" "max time")
  :initvals '(nil 0 0 nil)
  :outdoc '("a list of time+descriptor value lists")
  :doc "Returns the descriptor values for segment <seg-index> in <self>."

  (let* ((*iae (iaeengine-ptr self))
         (numdesc (length (descriptors self)))
         (framedescbuffer (fli::allocate-foreign-object :type :float :nelems numdesc))
         (size (iae-lib::iae_get_track_size *iae src-index (nth src-index (desc-tracks self)))))
    (unwind-protect
        (let ((curr-time -1))
          (loop for i from 0 to (1- size)
                while (or (null t2) (<= curr-time t2))
                do (setq curr-time (iae-lib::iae_get_descriptor_data *iae src-index i framedescbuffer))
                when (and (>= curr-time t1) (or (null t2) (<= curr-time t2)))
                collect
                (cons (- curr-time t1)
                      (loop for i from 0 to (1- numdesc) collect
                            (let ((val (fli:dereference framedescbuffer :index i :type :float)))
                              (if normalized
                                  (iae-lib::iae_conv_descriptor_to_minmax *iae i val)
                                val))))
                ))
      (fli:free-foreign-object framedescbuffer))
    ))


(defmethod! get-segment-descriptors ((self iae::IAE) (src-index integer) (seg-index integer) normalized)

  :indoc '("An IAE instance" "source index" "segment index in source")
  :initvals '(nil 0 0)
  :outdoc '("a list (time of the segment / list of descriptor values)")
  :doc "Returns the time and descriptor values for segment <seg-index> in <src-index>."

  (when (and (iaeengine-ptr self)
             (descriptors self))

    (let* ((*iae (iaeengine-ptr self))
           (numdesc (length (descriptors self)))
           (framedescbuffer (cffi::foreign-alloc :float :count numdesc)))

      (unwind-protect
          (let* ((time (iae-lib::iae_get_descriptor_data *iae src-index seg-index framedescbuffer))
                 (data (loop for i from 0 to (1- numdesc) collect
                             (let ((val (cffi::mem-aref framedescbuffer :float i)))
                               (if normalized
                                   (iae-lib::iae_conv_descriptor_to_minmax *iae i val)
                                 val))
                             )))
            (list time data))

        (cffi::foreign-free framedescbuffer))

      )))


;;;==================================
;;; PARAMETERS
;;;==================================

(defclass! IAE-BUNDLE (om::osc-bundle)
  ((om::messages :accessor om::messages :initarg :messages :initform nil
                 :documentation "list of (param value(s))"))
  (:documentation "A utility-class to store and edit a set of IAE parameters."))

(defmethod! iae-param-set (&key (advance 5.0 advance-supplied-p)
                                (attack 5.0 attack-supplied-p)
                                (cyclic nil cyclic-supplied-p)
                                (duplicatechannels t duplicatechannels-supplied-p)
                                (duration 0.0 duration-supplied-p)
                                (durationvar '(0.0 0.0) durationvar-supplied-p)
                                (filterfreq 5000.0 filterfreq-supplied-p)
                                (filterfreqvar 0.0 filterfreqvar-supplied-p)
                                (filtergain 0.0 filtergain-supplied-p)
                                (filtermode 0 filtermode-supplied-p)
                                (filterq 0.0 filterq-supplied-p)
                                (filterqvar 0.0 filterqvar-supplied-p)
                                (level 0.0 level-supplied-p)
                                (levelvar 0.0 levelvar-supplied-p)
                                (microtiming t microtiming-supplied-p)
                                (offset 5.0 offset-supplied-p)
                                (outputdelays nil outputdelays-supplied-p)
                                (outputgains nil outputgains-supplied-p)
                                (period '(0.0 1.0) period-supplied-p)
                                (periodvar '(0.0 0.0) periodvar-supplied-p)
                                (positionvar 0.0 positionvar-supplied-p)
                                (release 5.0 release-supplied-p)
                                (resampling 0.0 resampling-supplied-p)
                                (resamplingvar 0.0 resamplingvar-supplied-p)
                                (reverse nil reverse-supplied-p))
  :initvals '(5.0 ; advance
              (5.0 0.0) ; attack
              nil ; cyclic
              t ; duplicatechannels
              (0.0 1.0) ; duration
              (0.0 0.0) ; durationvar
              5000.0 ; filterfreq
              0.0 ; filterfreqvar
              0.0 ; filtergain
              0 ; filtermode
              0.0 ; filterq
              0.0 ; filterqvar
              0.0 ; level
              0.0 ; levelvar
              t ; microtiming
              5.0 ; offset
              nil ; outputdelays
              nil ; outputgains
              (0.0 1.0) ; period
              (0.0 0.0) ; periodvar
              0.0 ; positionvar
              (5.0 0.0) ; release
              0.0 ; resampling
              0.0 ; resamplingvar
              nil ; reverse
              )
  :doc "A utility-function to set IAE parameters in IAE-PARAMS"
  (remove
   nil
   (list
    (when advance-supplied-p (list "advance" advance))
    (when attack-supplied-p (list "attack" attack))
    (when cyclic-supplied-p (list "cyclic" cyclic))
    (when duplicatechannels-supplied-p (list "duplicatechannels" duplicatechannels))
    (when duration-supplied-p (list "duration" duration))
    (when durationvar-supplied-p (list "durationvar" durationvar))
    (when filterfreq-supplied-p (list "filterfreq" filterfreq))
    (when filterfreqvar-supplied-p (list "filterfreqvar" filterfreqvar))
    (when filtergain-supplied-p (list "filtergain" filtergain))
    (when filtermode-supplied-p (list "filtermode" filtermode))
    (when filterq-supplied-p (list "filterq" filterq))
    (when filterqvar-supplied-p (list "filterqvar" filterqvar))
    (when level-supplied-p (list "level" level))
    (when levelvar-supplied-p (list "levelvar" levelvar))
    (when microtiming-supplied-p (list "microtiming" microtiming))
    (when offset-supplied-p (list "offset" offset))
    (when outputdelays-supplied-p (list "outputdelays" outputdelays))
    (when outputgains-supplied-p (list "outputgains" outputgains))
    (when period-supplied-p (list "period" period))
    (when periodvar-supplied-p (list "periodvar" periodvar))
    (when positionvar-supplied-p (list "positionvar" positionvar))
    (when release-supplied-p (list "release" release))
    (when resampling-supplied-p (list "resampling" resampling))
    (when resamplingvar-supplied-p (list "resamplingvar" resamplingvar))
    (when reverse-supplied-p (list "reverse" reverse))
    ))
  )


(defun make-iae-param-calls (*iae param-list)
  (loop for param in param-list
        do
        (let ((func (intern (string-upcase (concatenate 'string "iae_set_" (car param))) :iae-lib)))

          (cond
           ((fboundp func)
            (cond
             ((consp (cadr param))
              (apply func (cons *iae (loop for p in (cadr param) collect (coerce p 'double-float)))))
             ((floatp (cadr param))
              (apply func (list *iae (coerce (cadr param) 'double-float))))
             (t ;;; int , boolean
                (apply func (list *iae (cadr param))))))

           ((string-equal (car param) "outputgains")
            (let ((gains (cffi::foreign-alloc :float :initial-contents (loop for elt in (cadr param) collect (coerce elt 'double-float)))))
              (iae-lib::iae_set_outputchannelgain *iae (length (cadr param)) gains)
              (cffi-sys:foreign-free gains)))

           ((string-equal (car param) "outputdelays")
            (let ((delays (cffi::foreign-alloc :float :initial-contents (loop for elt in (cadr param) collect (coerce elt 'double-float)))))
              (iae-lib::iae_set_outputchannelgain *iae (length (cadr param)) delays)
              (cffi-sys:foreign-free delays)))

           (t
            (om::om-beep-msg "IAE-PARAMS: function ~A does not exist" func))
           ))
        ))


;;;=========================
;;; KNN
;;;=========================

(defmethod! iae-knn ((self iae::IAE) descriptor value weight k &optional radius)

  :indoc '("An IAE instance" "descriptor number(s)" "requested value(s)" "weight(s)" "number of soultions" "max radius of the search domain")
  :initvals '(nil 0 0.0 1.0 3 nil)
  :outdoc '("a list of candidate (source-index segment-index)")
  :doc "Searchs for k-best candidates (source-index and segment-position) in a IAE buffer, matching some value(s) for some given weighted descriptor(s)."

  (when (and (iaeengine-ptr self)
             (descriptors self))

    (let* ((*iae (iaeengine-ptr self))
           (n (length (descriptors self)))
           (vals (make-list n :initial-element 0.0))
           (weights (make-list n :initial-element 0.0)))

      (loop for desc in (om::list! descriptor)
            for i from 0
            do
            (if (>= desc n) (om-lisp:om-print-format "Error: no descriptor number ~D in IAE" (list desc) "OM-IAE")
              (let ((value (float (or (if (consp value) (nth i value) value) 0.0)))
                    (weight (float (or (if (consp weight) (nth i weight) weight) 1.0))))
                (setf (nth desc vals) value)
                (setf (nth desc weights) weight))
              ))

      (let ((value-array (cffi::foreign-alloc :float :initial-contents vals))
            (weight-array (cffi::foreign-alloc :float :initial-contents weights)))

        (unwind-protect

            (progn

              (iae-lib::iae_set_target *iae n value-array)
              (iae-lib::iae_set_weight *iae n weight-array)
              (iae-lib::iae_set_k *iae k)

              (when radius ;; typically r = [0 - 4]
                ;;; !! may lead to a list smaller than k
                (iae-lib::iae_set_radius *iae radius))

              (iae-lib::iae_select_new *iae nil)

              (loop for i from 0 to (1- k) collect
                    (let* ((selsrc (iae-lib::iae_get_SelectedSourceIndex *iae i))
                           (selind (iae-lib::iae_get_SelectedSegmentIndex *iae i)))
                      (list selsrc selind)))
              )

          (cffi::foreign-free value-array)
          (cffi::foreign-free weight-array)

          ))
      )))


;;;=========================
;;; SYNTH
;;;=========================

;;; Returns a sound buffer with a grain from given pos in IAE
(defmethod! iae-synth ((self iae::IAE) source position dur &key (gain 1.0) (attack 10) (release 10) outputgains outputdelays other-iae-params)
  :indoc '("An IAE instance" "source number"
           "position in source [marker-id or time in ms]"
           ""
           "duration [ms]"
           "gain" "attack time [ms]" "release time [ms]")
  :doc "Synthesizes a grain (SOUND buffer) from IAE.

- <source> is the source number in IAE (must be inferior to the total number of sources)
- <position> (int) is interpreted as the marker/segment index in <source> (must be inferior to the total number of segments)
- <position> (float) is interpreted as a time-position in milliseconds in source <source>
- <dur> is the duration of the output grain.

- <other-params> is a lits of list of the form ((\"param\" value) ...) corresponding to the parameters of IAE/MuBu.
"
  :initvals '(nil 0 0 200 1.0 10 10 nil)
  :outdoc '("sound")

  (when (iaeengine-ptr self)
    (let* ((*iae (iaeengine-ptr self))

           (graindur (if (and (or (null dur) (zerop dur))
                              (integerp position))  ;; mode "marker"
                         (iae-lib::iae_get_SegmentDuration *iae source position)
                       dur))

           (nsamples (ceiling (* graindur (iae::samplerate self) 0.001)))
           (omsnd (make-instance 'om::internal-sound :n-channels (channels self) :smpl-type :float
                                 :n-samples nsamples :sample-rate (iae::samplerate self)))
           (**samples (om::make-audio-buffer (channels self) nsamples)))

;   Granular = 0,    asynchronous granular synthesis
;   Segmented = 1,   concatenative synthesis (needs at least 1 marker)
;   Synchronous = 2  synchronous granular synthesis (needs at least 2 markers)
      (iae-lib::iae_set_SynthMode *iae (if (integerp position) 1 0))

      (when (< source (length (sounds self)))
        (iae-lib::iae_set_sourceindex *iae source))

      ;;; general params
      (iae-lib::iae_set_cyclic *iae nil)
      (iae-lib::iae_set_centeredGrains *iae nil)
      (iae-lib::iae_set_attack *iae (coerce attack 'double-float) 0.0d0)
      (iae-lib::iae_set_release *iae (coerce release 'double-float) 0.0d0)
      (iae-lib::iae_set_period *iae -0.0d0 0.0d0)

      (if (or (null dur) (zerop dur))
          (iae-lib::iae_set_duration *iae 0.0d0 1.0d0) ;;; duration of the segment
        (iae-lib::iae_set_duration *iae (coerce dur 'double-float) 0.0d0))

      (iae-lib::iae_set_gain *iae (coerce gain 'double-float))
      (iae-lib::iae_set_positionvar *iae 0.0d0)

      ;;; mode-specific
      (if (integerp position)
          (iae-lib::iae_set_markerindex *iae position)
        (iae-lib::iae_set_position *iae (coerce position 'double-float) 0.0d0))

      (when other-iae-params
        (make-iae-param-calls *iae other-iae-params))

      (when outputgains
        (let ((gains (cffi::foreign-alloc :float :initial-contents (loop for elt in outputgains collect (coerce elt 'double-float)))))
          (iae-lib::iae_set_outputchannelgain *iae (length outputgains) gains)
          (cffi-sys:foreign-free gains)))

      (when outputdelays
        (let ((delays (cffi::foreign-alloc :float :initial-contents (loop for elt in outputdelays collect (coerce elt 'double-float)))))
          (iae-lib::iae_set_outputchannelgain *iae (length outputdelays) delays)
          (cffi-sys:foreign-free delays)))

      ;;; generates the grain
      (iae-lib::iae_trigger *iae)
      (iae-lib::iae_synth *iae nsamples **samples (channels self))
      (setf (om::buffer omsnd) (om::make-om-sound-buffer-gc :ptr **samples :nch (channels self) :size nsamples))
      omsnd)))

;;; Returns a sound buffer with a grain from given set of descriptor values in IAE

;;; other option:
;;<request> can contain one or more triplets (desc,value,weight), where 'desc' is a descriptor number (as built-in teh IAE), and 'value' the targetted value for this descriptor. 'weight' is optional and will be set to 1.0 (maximum) by default.


;;; A mix of IAE-KNN and IAE-SYNTH
(defmethod! iae-synth-desc ((self iae::IAE) descriptor value weight dur &key (gain 1.0) (attack 10) (release 10) outputgains outputdelays other-iae-params)

  :indoc '("An IAE instance" "descriptor number(s)" "requested value(s)" "weight(s)" "duration [ms]" "gain" "attack time [ms]" "release time [ms]")
  :initvals '(nil 0 0.0 1.0 200 1.0 10 10)
  :outdoc '("sound")
  :doc "Synthesizes a grain (SOUND buffer) from IAE, resquesting some value(s) for some given descriptor(s)."

  (when (iaeengine-ptr self)
    (let* ((*iae (iaeengine-ptr self))
           (nsamples (ceiling (* dur (iae::samplerate self) 0.001)))
           (omsnd (make-instance 'om::internal-sound :n-channels (channels self) :smpl-type :float
                                 :n-samples nsamples :sample-rate (iae::samplerate self)))
           (**samples (om::make-audio-buffer (channels self) nsamples))
           ;; (framedescbuffer (fli::allocate-foreign-object :type :float :nelems (length (descriptors self))))
           )

;   Granular = 0,    asynchronous granular synthesis
;   Segmented = 1,   concatenative synthesis (needs at least 1 marker)
;   Synchronous = 2  synchronous granular synthesis (needs at least 2 markers)
      (iae-lib::iae_set_SynthMode *iae 1)

      ;;; general params
      (iae-lib::iae_set_Cyclic *iae nil)
      (iae-lib::iae_set_CenteredGrains *iae nil)
      (iae-lib::iae_set_Attack *iae (coerce attack 'double-float) 0.0d0)
      (iae-lib::iae_set_Release *iae (coerce release 'double-float) 0.0d0)
      (iae-lib::iae_set_period *iae -0.0d0 0.0d0)
      (iae-lib::iae_set_duration *iae (coerce dur 'double-float) 0.0d0)
      (iae-lib::iae_set_gain *iae (coerce gain 'double-float))

      (when other-iae-params
        (make-iae-param-calls *iae other-iae-params))

      (when outputgains
        (let ((gains (cffi::foreign-alloc :float :initial-contents (loop for elt in outputgains collect (coerce elt 'double-float)))))
          (iae-lib::iae_set_outputchannelgain *iae (length outputgains) gains)
          (cffi-sys:foreign-free gains)))

      (when outputdelays
        (let ((delays (cffi::foreign-alloc :float :initial-contents (loop for elt in outputdelays collect (coerce elt 'double-float)))))
          (iae-lib::iae_set_outputchannelgain *iae (length outputdelays) delays)
          (cffi-sys:foreign-free delays)))

      ;;; mode-specific
      (when (descriptors self)

        (let* ((n (length (descriptors self)))
               (vals (make-list n :initial-element 0.0))
               (weights (make-list n :initial-element 0.0)))

          (loop for desc in (om::list! descriptor)
                for i from 0
                do
                (if (>= desc n) (om-lisp:om-print-format "Error: no descriptor number ~D in IAE" (list desc) "OM-IAE")
                  (let ((value (float (or (if (consp value) (nth i value) value) 0.0)))
                        (weight (float (or (if (consp weight) (nth i weight) weight) 1.0))))
                    (setf (nth desc vals) value)
                    (setf (nth desc weights) weight))
                  ))

          (let ((value-array (cffi::foreign-alloc :float :initial-contents vals))
                (weight-array (cffi::foreign-alloc :float :initial-contents weights)))

            (unwind-protect
                (progn
                  (iae-lib::iae_set_target *iae n value-array)
                  (iae-lib::iae_set_weight *iae n weight-array)
                  (iae-lib::iae_set_k *iae 1)
                  ;;; generates the grain
                  (iae-lib::iae_select_new *iae T)   ;; T = force-trigger
                  (iae-lib::iae_synth *iae nsamples **samples (channels self)))

              (cffi::foreign-free value-array)
              (cffi::foreign-free weight-array))
            )))

      (setf (om::buffer omsnd) (om::make-om-sound-buffer-gc :ptr **samples :nch (channels self) :size nsamples))

      omsnd)))


;;;=========================
;;; DISPLAY
;;;=========================

(defmethod om::display-modes-for-object ((self iae::iae))
  '(:mini-view :text :hidden))


;;; do that better with IDs etc.
(defmethod om::get-cache-display-for-draw ((self iae::IAE) box)
  (declare (ignore box))
  (append (call-next-method)
          (list (iae::iae-info self))))


(defmethod om::draw-mini-view ((self iae::IAE) (box t) x y w h &optional time)
  (let ((display-cache (om::get-display-draw box)))
    (oa::om-with-font
     (oa::om-def-font :small)
     (loop for str in (om::string-lines-to-list (car display-cache))
           for y = 16 then (+ y 10) do
           (om::om-draw-string 10 y str))
     )))
