(mapcar #'ql:quickload '("spinneret"
			 "hunchentoot"
			 "parenscript"
			 "easy-routes"
			 "lass"
			 "bknr.datastore"))

(defpackage :retro-games
  (:use :cl :spinneret :hunchentoot :parenscript))

(in-package :retro-games)

(defun retro-db-connect ()
  "Connect to the BKNR datastore"
  (let ((object-subsystem
	  (make-instance
	   'bknr.datastore:store-object-subsystem)))
    (make-instance 'bknr.datastore:mp-store
		   :directory "/tmp/bknrstore/"
		   :subsystems (list object-subsystem))))

(defclass game (bknr.datastore:store-object)
  ((name :initarg :name
	 :reader name)
   (votes :accessor votes
	  :initform 0))
  (:metaclass bknr.datastore:persistent-class))

(defmethod print-object ((object game) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name votes) object
      (format stream "name: ~s with ~d votes" name votes))))


(defmethod vote-for ((obj game))
  (bknr.datastore:with-transaction ()
    (incf (votes obj))))

;; Backend
;; =======

(retro-db-connect)

(defun game-from-name (name)
  (let ((games (bknr.datastore:store-objects-with-class 'game)))
    (find name games :test #'string-equal :key #'name)))

(defun game-stored? (game-name)
  (game-from-name game-name))

(defun add-game (name)
  (unless (game-stored? name)
      (make-instance 'game :name name)))

(defun games ()
  (let ((games (copy-list
		(bknr.datastore:store-objects-with-class 'game))))
    (sort games #'> :key #'votes)))

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
	 "/logo.jpg" "static/Commodore64.jpg") *dispatch-table*))

;; CSS (Lass)
(defun generate-css ()
  (let ((color-body-background "#FFFFFF")
	(color-body "#000000")
	(color-header "#778899")
	(font-body "Arial, Helvetica, sans-serif")
	(color-link "#717171"))
    (lass:compile-and-write
     `(body
       :margin 0
       :margin-left 1em
       :padding 0
       :background-color ,color-body-background
       :color ,color-body
       :font-family ,font-body
       :border-top 2px solid "#2A4F6F")
     `("#header"
       :border-top 1px solid ,color-header
       :border-botom 1px dotted "#B2BCC6"
       :height 8em
       (.strapline
	:font "120% Georgia, \"Times New Roman\", Times, Serif"
	:color ,color-header
	:font-size 3em
	:background-color transparent
	:float left
	:margin-left 2em
	:margin-top 0.7em)
       (.logo
	:float left
	:margin-left 1.5em
	:margin-top 0.5em))
     '("#error"
       :color red)
     `("#chart"
       :font-family ,font-body
       :font-size .9em
       (li
	:padding 5px)
       ((:or (:and a :link) (:and a :visited))
	:margin-right 7px
	:padding 1px 7px 1px 7px
	:color black
	:background-color lightblue
	:text-decorator none
	:border-top 1px solid ,color-body
	:border-left 1px solid ,color-body
	:border-bottom 1px solid ,color-link
	:border-right 1px solid ,color-link)
       ((:and a :hover)
	:border-top 1px solid ,color-link
	:border-left 1px solid ,color-link
	:border-bottom 1px solid ,color-body
	:border-right 1px solid ,color-body))
     '(form
       :border 1px dotted "#aaaaaa"
       :padding 3px 6px 3px 6px)
     '((:and input .txt)
       :background-color "#ADD8E6"
       :border 1px inset "#00008B"
       :width 200px)
     '((:and input .btn)
       :border 1px outset "#00008B"
       :padding 2px 4px 2px 4px))))
       
;; HTML (Spinneret)

(defmacro standard-page ((&key title script) &body body)
  "All pages on the Retro Games site will use the following macro"
  `(with-html-string
     (:doctype)
     (:html
      (:head
       (:title ,title)
       (:style (generate-css))
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
      (vote-for game)))
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
    (add-game name))
  (redirect "/retro-games"))

(publish-static-content)
(start-server)
