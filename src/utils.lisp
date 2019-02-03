
(in-package :mush)


(defmacro defthing (class-name superclass &rest fields)
  `(defclass ,class-name ,superclass
	    (,@(loop for field in fields collect
		    `(,field :initarg ,(intern (symbol-name field) "KEYWORD")
			     :accessor ,field
			     :initform '() )))))

(defun read-things-file (kind file)
  (with-open-file (stream (c+ root-dir file)
			:direction :input)
    (let ((things (read stream)))
      (loop for thing in things collect
	   (let ((out (make-instance kind)))
	     (loop for field in thing do
		  (eval (list 'setf (list (first field) out) `(quote ,(second field)))))
	     out)))))

(defun uuid-integer () (random (* 256 256)))

;; (defun read-all (stream)
;;   (loop for char = (read-char-no-hang stream nil :eof)
;;      until (or (null char) (eq char :eof)) collect char into msg
;;      finally (return (coerce msg 'string))))

(defun read-all (stream)
  "Returns a string interpreted from the octets in the stream.
   Or an empty string if something goes wrong."
  (if (listen stream)
      (let ((msg (loop for char = (read-byte stream nil :eof)
		    until (or (not (listen stream)) (eq char :eof))
		    collecting char)))
	(cond ((= (first msg) #xff) ; the client is sending telnet commands
	       (progn (send stream "telnet messages are not supported at this time") ""))
				    ; ideally there would be a telnet implementation here but
	                            ; ignoring it like this makes the server not die in a fire
	      (t
	       (or (ignore-errors   ; honestly this sucks but if there's no way to enforce unicode
		     (babel:octets-to-string  ; or ASCII it's all you can do to not kill the server.
		      (make-array (length msg)
				  :element-type '(unsigned-byte 8)
				  :initial-contents msg)))
		   (ignore-errors
		     (babel:octets-to-string
		      (make-array (length msg)
				  :element-type '(unsigned-byte 8)
				  :initial-contents msg)
		      :encoding :cp1252)) ; pure guess
		   (progn (send stream "please use plain ASCII or unicode") "")))))))

(defun c+ (&rest strs)
  (apply 'concatenate
	 (append
	  '(string)
	  (mapcar (lambda (x)
		    (if (numberp x)
			(write-to-string x)
			(if (symbolp x)
			    (symbol-name x)
			    x)))
		  strs))))

(defun lines (lines)
  (setq out "")
  (loop for line in lines
     do (setq out (c+ out line "~%")))
  out)

(defun join (str-list delim)
  (format nil (c+ "~{~A~^" delim "~}") str-list))

(defun join-and (str-list)
  (if (second str-list)
      (c+ (join (butlast str-list) ", ") ", and " (first (last str-list)))
      (first str-list)))


(defun send-lines (stream lines)
  (send stream (funcall #'lines lines)))

(defun line-wrap (string count)
  (let* ((words (cl-ppcre:split " " string))	 
	 (built-list '() )
	 (counter 0))
    (loop for word in words
       for i from 0 upto (length words) do
	 (setq built-list
	       (append built-list
		       (if (or (equal word "~%") (> (+ (length word) counter) count))
			   (progn (setq counter 0)
				  (list word (if (equal word "~%") "" "~%")))
			   (list word))))
	 (setq counter (+ counter 1 (length word)))
	 )
    (join built-list " ")))



(defmacro case-string (str &rest forms)
  (let* ((strval (gensym "STRVAL"))
         (cond-body (loop for (s . f) in forms 
                          collect `((member ,strval (quote ,(if (listp s) s (list s))) :test #'string=) ,@f))))
    `(let ((,strval ,str)) (cond ,@cond-body))))


(defun color-wrap (code text)
  (format nil (c+ code text "~c[0m") #\ESC #\ESC))

(defun purple (text)
  (color-wrap "~c[35m" text))

(defun blue (text)
  (color-wrap "~c[34m" text))

(defun red (text)
  (color-wrap "~c[31m" text))

(defun yellow (text)
  (color-wrap "~c[33m" text))

(defun green (text)
  (color-wrap "~c[32m" text))

(defun cyan (text)
  (color-wrap "~c[36" text))

(defun grey (text)
  (color-wrap "~c[37m" text))
