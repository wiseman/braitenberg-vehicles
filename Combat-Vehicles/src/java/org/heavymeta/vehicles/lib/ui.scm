(defun make-button (name action)
  (let* ((b (new 'java.awt.Button name))
	 (listener (new 'com.ibm.jikes.skij.misc.GenericCallback action)))
    (invoke b 'addActionListener listener)
    b))




(defun make-v-frame (world width height)
  (define (panel) (new 'java.awt.Panel))
  (define (constraint-value name)
    (peek-static 'java.awt.GridBagConstraints name))
  (define (.add container thing) (invoke container 'add thing))
  (let ((screen (panel))
        (buttons (panel))
        (renderer #f)
        (graphics #f)
        (step-size 15)
	(play-thread #f))
    (let ((frame (make-refreshed-window "JVehicles" width height
		   (lambda (g)
		     (when (and renderer graphics)
		       (invoke renderer 'render graphics world)))))
	  (mode 'stop))
      (labels ((step-n (n)
                 (if (= n 0)
                   #t
                   (invoke world 'step (long n) renderer graphics)))
               (play (frame)
		  (if (eq? mode 'play)
		   (progn
		     (step-n step-size)
		     (if (= (mod frame 100) 0)
		       (serialize-to-array world #f)
		       (invoke-static 'java.lang.Thread 'yield))
		     (play (+ frame step-size)))
		   ;; Let the thread disappear.
		   (setf play-thread #f))))
        (let ((play-button (make-button "Play"
                                        (lambda (event)
                                          (setf mode 'play)
					  (let ((thread (run-in-thread (lambda () (play 0)))))
					    (invoke thread 'setPriority
						    (- (invoke (current-thread) 'getPriority) 1))
					    (setf play-thread thread)))))
              (pause-button (make-button "Pause"
                                         (lambda (event)
                                           (setf mode 'pause))))
	      (trails-button (make-button "Trails"
					  (lambda (event)
					    (poke renderer 'trails
						  (not (peek renderer 'trails))))))
	      (in-button (make-button "In"
				      (lambda (event)
					(invoke renderer 'setScale
						(* (peek renderer 'scale) 1.2)))))
	      (out-button (make-button "Out"
				       (lambda (event)
					 (invoke renderer 'setScale
						 (* (peek renderer 'scale) (/ 1 1.2)))))))
          (let ((fp (panel))
                (gbl (new 'java.awt.GridBagLayout)))
            (invoke fp 'setLayout gbl)
            ;; Add screen.
            (let ((constraints (new 'java.awt.GridBagConstraints)))
              (mpoke constraints
                     `(anchor ,(constraint-value 'NORTH)
                       weightx 1.0
                       weighty 1.0
                       fill ,(constraint-value 'BOTH)))
              (invoke gbl 'setConstraints screen constraints)
              (.add fp screen))

            ;; Add buttons.
	    (invoke buttons 'setBackground (new 'java.awt.Color 128 128 128))
            (.add buttons play-button)
            (.add buttons pause-button)
	    (.add buttons in-button)
	    (.add buttons out-button)
	    (.add buttons trails-button)
            (let ((constraints (new 'java.awt.GridBagConstraints)))
              (mpoke constraints
                     `(anchor ,(constraint-value 'NORTH)
                       fill ,(constraint-value 'HORIZONTAL)
                       gridy 1))
              (invoke gbl 'setConstraints buttons constraints)
              (.add fp buttons))

	    (add-window-close-handler frame
				      (lambda ()
					(setf mode 'quit)
					(when play-thread
					  ;; Unsafe?
					  (invoke play-thread 'stop))))
            (invoke frame 'setSize width height)
            (.add frame fp)
            (invoke frame 'show)
            (setf graphics (invoke screen 'getGraphics))
            (setf renderer (new 'org.heavymeta.vehicles.VRenderer screen))))))))


                     
          
(defun serialize-to-array (obj compressed?)
  (let ((byte-stream (new 'java.io.ByteArrayOutputStream))
	(obj-stream #f))
    (if (not compressed?)
      (setf obj-stream (new 'java.io.ObjectOutputStream byte-stream))
      (let ((gzip-stream (new 'java.util.zip.GZIPOutputStream byte-stream)))
	(setf obj-stream (new 'java.io.ObjectOutputStream gzip-stream))))
    (invoke obj-stream 'writeObject obj)
    (invoke obj-stream 'flush)
    (invoke obj-stream 'close)
    (invoke byte-stream 'toByteArray)))

(defun unserialize-from-array (array compressed?)
  (let ((byte-stream (new 'java.io.ByteArrayInputStream array))
	(obj-stream #f))
    (if compressed?
      (let ((gzip-stream (new 'java.util.zip.GZIPInputStream byte-stream)))
	(setf obj-stream (new 'java.io.ObjectInputStream gzip-stream)))
      (setf obj-stream (new 'java.io.ObjectInputStream byte-stream)))
    (let ((obj (invoke obj-stream 'readObject)))
      (invoke obj-stream 'close)
      obj)))

(defun deep-clone (obj)
  (unserialize-from-array (serialize-to-array obj)))


(defun mod (x y)
  (let ((div (floor (/ x y))))
    (- x (* div y))))

