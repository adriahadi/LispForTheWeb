(mapcar #'ql:quickload '("spinneret"
			 "hunchentoot"
			 "parenscript"
			 "mito"
			 "easy-routes"))

(defpackage :retro-games
  (:use :cl :spinneret :hunchentoot :parenscript :mito))

(in-package :retro-games)

(defun retro-connect ()
  "Connect to the Retro Sqlite DB"
  (connect-toplevel :sqlite3
		    :database-name "retro.db"))

(defclass game ()
  ((name :col-type :varchar
	 :initarg :name
	 :reader name)
   (votes :col-type :integer
	  :accessor votes
	  :initform 0))
  (:metaclass dao-table-class)
  (:unique-keys name))

(defmethod print-object ((object game) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name votes) object
      (format stream "name: ~s with ~d votes" name votes))))

(defmethod vote-for ((obj game))
  (incf (votes obj)))

;; Backend
;; =======


(defun create-retro-table ()
  (ensure-table-exists 'game))

(retro-connect)
(create-retro-table)
(defparameter  *games* (select-dao 'game))

(defun game-from-name (name)
  (find name *games* :test #'string-equal :key #'name)) 

(defun game-stored? (game-name)
  (game-from-name game-name))

(defun add-game (name)
  (unless (game-stored? name)
    (let ((new-game (make-instance 'game :name name)))
      (insert-dao new-game)
      (push new-game *games*))))

(defun games ()
  (sort (copy-list *games*) #'> :key #'votes))

;; Web Server -- Hunchentoot
(defvar *server* nil)
(defparameter *port* 7889)

(defun start-server (&key (port *port*))
  (format t "~&Starting web server at port ~a" port)
  (force-output)
  (setf *server* (make-instance 'easy-routes:easy-routes-acceptor
				:port (or port *port*)))
  (start *server*))

(defun stop-server ()
  (stop *server*))

(defun publish-static-content ()
  (push (create-static-file-dispatcher-and-handler
	 "/logo.jpg" "static/Commodore64.jpg") *dispatch-table*)
  (push (create-static-file-dispatcher-and-handler
	 "/retro.css" "static/retro.css") *dispatch-table*))

;; Spinneret

(defmacro standard-page ((&key title script) &body body)
  "All pages on the Retro Games site will use the following macro"
  `(with-html-string
     (:doctype)
     (:html
      (:head
       (:title ,title)
       (:link :type "text/css"
	      :rel "stylesheet"
	      :href "/retro.css")
       ,(when script
	  `(:script :type "text/javascript"
		    (format nil "~a" ,script))))
      (:body
       (:header :id "header" ; Retro games header
		(:img :src "/logo.jpg"
		      :alt "Comodore 64"
		      :class "logo")
		(:span :class "strapline"
		       "Vote on your favourite Retro Game"))
       ,@body))))


(easy-routes:defroute retro-games ("/retro-games") ()
  (standard-page (:title "Top Retro Games")
    (:h1 "Vote on your all time favourite retro games!")
    (:p "Missing a game? Make it available for votes "
	(:a :href "new-game" "here"))
    (:h2 "Current Stand")
    (:div :id "chart" ; Used for CSS styling of the links.
	  (:ol
	   (dolist (game (games))
	     (:li (:a :href
		      (format nil "vote?name=~a" (url-encode
						  (name game)))
		      "Vote!")
		  (format nil "~a with ~d votes"
			  (name game) (votes game))))))))

(easy-routes:defroute vote ("/vote") (name)
  (when (game-stored? name)
    (let ((game (game-from-name name)))
      (vote-for game)
      (save-dao game)))
  (redirect "/retro-games"))

(easy-routes:defroute new-game ("/new-game") ()
  (standard-page (:title "Add a new game"
		  :script
		  (ps
		    (defvar add-form nil)
		    (defun validate-game-name (evt)
		      (when (= (@ add-form name value) "")
			(chain evt (ps-dhtml-symbols:prevent-default))
			(alert "Please enter a name.")))
		    (defun init ()
		      (setf add-form (chain document
					    (ps-dhtml-symbols:get-element-by-id "addform")))
		      (chain add-form
			     (ps-dhtml-symbols:add-event-listener
			      "submit" validate-game-name false)))
		    (setf (chain window onload) init)))
    (:h1 "Add a new game to the chart")
    (:form :action "/game-added" :method "post" :id "addform"
	   (:p "What is the name of the game?" (:br)
	       (:input :type "text" :name "name" :class "txt"))
	   (:p (:input :type "submit" :value "Add" :class "btn")))))


(easy-routes:defroute game-added ("/game-added" :method :post)
    (&post name)
  (unless (or (null name)
	      (zerop (length name)))
    (add-game name)
    (save-dao (game-from-name name)))
  (redirect "/retro-games"))

(publish-static-content)
(start-server)
