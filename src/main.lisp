;; using
;; hunchentoot to host server.
;; spinneret dsl for html.
;; lass for css
;; parenscript for js.


(ql:quickload :hunchentoot)
(ql:quickload :spinneret)
(ql:quickload :lass)
(ql:quickload :parenscript)
(asdf:load-system :spinneret/ps)

(defparameter *public-root* "/Users/jalenmoore/Documents/cl-personal/public") ; remove hard-code

(defparameter *server* nil)
(defun safe-start-server (new-instance)
  "Verify that the current *SERVER* is not running before overriding with NEW-INSTANCE."
  (if *server*
      (hunchentoot:stop *server*))
  (setf *server* new-instance)
  (hunchentoot:start *server*))

(safe-start-server (make-instance 'hunchentoot:easy-acceptor
				  :port 33333
				  :document-root *public-root*))

(defparameter *css*
  '((html
     :min-height "100%")
    (:let ((light-text "#eee")
	   (dark-background "#1e1e1e")
	   (primary "#cacdcb")
	   (secondary "#eaeaea")
	   (shadow "rgb(0 0 0 / 0.15)"))
      (body :--scroll 0			; handled by js
	    :display "flex"
	    :flex-direction "column"
	    :margin "0rem"
	    :font-family "Victor Mono"
	    :height "auto"
	    :min-height "100vh")
      (button :padding "0.25rem 0.75rem"
	      :background-color #(primary))
      (a :padding "0.25rem"
	 :color "blue"
	 :text-decoration-style "dotted"
	 :text-decoration "underline")
      ((:and a :hover) :color #(dark-background)
		       :background-color #(secondary)
		       :border-radius "0.15rem")
      ((:and a :focus) :color #(dark-background)
		       :background-color #(primary)
		       :border-radius "0.15rem")
      (nav :z-index 999
	   :position "sticky"
	   :display "flex"
	   :align-items "center"
	   :top "0%"
	   :left "0%"
	   :background-color #(dark-background)
	   :padding "0.5rem 0.5rem"
	   (img :height "max(3rem, calc(6rem - var(--scroll) * 3rem))"
		:width "max(3rem, calc(6rem - var(--scroll) * 3rem))"
		:border-radius "10rem"
		:border-width "0.5rem"
		:border-style "solid"
		:border-color #(dark-background)
		:margin "0.25rem 0.75rem"
		:top "0.25rem")
	   (a :color #(light-text)
	      :padding "0.25rem 0.75rem")
	   (*#banner :margin "0rem 20%"
		     :flex "1 0")))))

(defmacro with-layout ((&key title) &body body)
  `(spinneret:with-html-string
     (:doctype)
     (:html
      (:head
       (:title ,title)
       (:style (apply #'lass:compile-and-write *css*))
       (:link :rel "stylesheet" :href "/fontawesome/css/all.css")
       (:script :src "https://unpkg.com/htmx.org@2.0.3"
		:integrity "sha384-0895/pl2MU10Hqc6jd4RvrthNlDiE9U1tWmX7WRESftEDRosgxNsQG/Ze9YMRzHq"
		:crossorigin "anonymous")
       (:link :rel "preconnect" :href "https://fonts.googleapis.com")
       (:link :rel "preconnect" :href "https://fonts.gstatic.com")
       (:link :rel "stylesheet" :href "https://fonts.googleapis.com/css2?family=Roboto:ital,wght@0,100;0,300;0,400;0,500;0,700;0,900;1,100;1,300;1,400;1,500;1,700;1,900&family=Victor+Mono:ital,wght@0,100..700;1,100..700&display=swap"))
      (:script (:raw
		(parenscript:ps
		  (defun get-y-scroll ()
		    (with-slots (scroll-y page-y-offset) window
		      (if scroll-y
			  scroll-y
			  page-y-offset)))
		  (defun handle-scroll ()
		    (let ((scroll-amount (get-y-scroll)))
		      (chain document body style
			     (set-property "--scroll" (/ scroll-amount (@ window inner-height)))))))))
      (:body :onscroll (:raw (parenscript:ps (handle-scroll)))
	     (:nav
	      (:a :href "/" "Home"))
	     (:img :src "/banner.jpg" :alt "banner" :id "banner")
	     (:article ,@body)))))

(hunchentoot:define-easy-handler (main :uri "/")
    ()
  (with-layout (:title "Hello")
    (:h1 "Hello!")
    (:button :data-hx-get "/test" :hx-swap "innerHTML" "Test")
    (:p "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Est igitur officium eius generis, quod nec in bonis ponatur nec in contrariis. Quid ergo aliud intellegetur nisi uti ne quae pars naturae neglegatur? Indicant pueri, in quibus ut in speculis natura cernitur. Duo Reges: constructio interrete. Qui autem voluptate vitam effici beatam putabit, qui sibi is conveniet, si negabit voluptatem crescere longinquitate? Vide, ne magis, inquam, tuum fuerit, cum re idem tibi, quod mihi, videretur, non nova te rebus nomina inponere. Nam, ut sint illa vendibiliora, haec uberiora certe sunt. Hoc dictum in una re latissime patet, ut in omnibus factis re, non teste moveamur. Aliter enim nosmet ipsos nosse non possumus. Quod idem cum vestri faciant, non satis magnam tribuunt inventoribus gratiam. Hoc dictum in una re latissime patet, ut in omnibus factis re, non teste moveamur. Cur igitur easdem res, inquam, Peripateticis dicentibus verbum nullum est, quod non intellegatur? Quid affers, cur Thorius, cur Caius Postumius, cur omnium horum magister, Orata, non iucundissime vixerit?")
    (:p "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Est igitur officium eius generis, quod nec in bonis ponatur nec in contrariis. Quid ergo aliud intellegetur nisi uti ne quae pars naturae neglegatur? Indicant pueri, in quibus ut in speculis natura cernitur. Duo Reges: constructio interrete. Qui autem voluptate vitam effici beatam putabit, qui sibi is conveniet, si negabit voluptatem crescere longinquitate? Vide, ne magis, inquam, tuum fuerit, cum re idem tibi, quod mihi, videretur, non nova te rebus nomina inponere. Nam, ut sint illa vendibiliora, haec uberiora certe sunt. Hoc dictum in una re latissime patet, ut in omnibus factis re, non teste moveamur. Aliter enim nosmet ipsos nosse non possumus. Quod idem cum vestri faciant, non satis magnam tribuunt inventoribus gratiam. Hoc dictum in una re latissime patet, ut in omnibus factis re, non teste moveamur. Cur igitur easdem res, inquam, Peripateticis dicentibus verbum nullum est, quod non intellegatur? Quid affers, cur Thorius, cur Caius Postumius, cur omnium horum magister, Orata, non iucundissime vixerit?")
    (:p "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Est igitur officium eius generis, quod nec in bonis ponatur nec in contrariis. Quid ergo aliud intellegetur nisi uti ne quae pars naturae neglegatur? Indicant pueri, in quibus ut in speculis natura cernitur. Duo Reges: constructio interrete. Qui autem voluptate vitam effici beatam putabit, qui sibi is conveniet, si negabit voluptatem crescere longinquitate? Vide, ne magis, inquam, tuum fuerit, cum re idem tibi, quod mihi, videretur, non nova te rebus nomina inponere. Nam, ut sint illa vendibiliora, haec uberiora certe sunt. Hoc dictum in una re latissime patet, ut in omnibus factis re, non teste moveamur. Aliter enim nosmet ipsos nosse non possumus. Quod idem cum vestri faciant, non satis magnam tribuunt inventoribus gratiam. Hoc dictum in una re latissime patet, ut in omnibus factis re, non teste moveamur. Cur igitur easdem res, inquam, Peripateticis dicentibus verbum nullum est, quod non intellegatur? Quid affers, cur Thorius, cur Caius Postumius, cur omnium horum magister, Orata, non iucundissime vixerit?")
    (:p "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Est igitur officium eius generis, quod nec in bonis ponatur nec in contrariis. Quid ergo aliud intellegetur nisi uti ne quae pars naturae neglegatur? Indicant pueri, in quibus ut in speculis natura cernitur. Duo Reges: constructio interrete. Qui autem voluptate vitam effici beatam putabit, qui sibi is conveniet, si negabit voluptatem crescere longinquitate? Vide, ne magis, inquam, tuum fuerit, cum re idem tibi, quod mihi, videretur, non nova te rebus nomina inponere. Nam, ut sint illa vendibiliora, haec uberiora certe sunt. Hoc dictum in una re latissime patet, ut in omnibus factis re, non teste moveamur. Aliter enim nosmet ipsos nosse non possumus. Quod idem cum vestri faciant, non satis magnam tribuunt inventoribus gratiam. Hoc dictum in una re latissime patet, ut in omnibus factis re, non teste moveamur. Cur igitur easdem res, inquam, Peripateticis dicentibus verbum nullum est, quod non intellegatur? Quid affers, cur Thorius, cur Caius Postumius, cur omnium horum magister, Orata, non iucundissime vixerit?")
    (:p "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Est igitur officium eius generis, quod nec in bonis ponatur nec in contrariis. Quid ergo aliud intellegetur nisi uti ne quae pars naturae neglegatur? Indicant pueri, in quibus ut in speculis natura cernitur. Duo Reges: constructio interrete. Qui autem voluptate vitam effici beatam putabit, qui sibi is conveniet, si negabit voluptatem crescere longinquitate? Vide, ne magis, inquam, tuum fuerit, cum re idem tibi, quod mihi, videretur, non nova te rebus nomina inponere. Nam, ut sint illa vendibiliora, haec uberiora certe sunt. Hoc dictum in una re latissime patet, ut in omnibus factis re, non teste moveamur. Aliter enim nosmet ipsos nosse non possumus. Quod idem cum vestri faciant, non satis magnam tribuunt inventoribus gratiam. Hoc dictum in una re latissime patet, ut in omnibus factis re, non teste moveamur. Cur igitur easdem res, inquam, Peripateticis dicentibus verbum nullum est, quod non intellegatur? Quid affers, cur Thorius, cur Caius Postumius, cur omnium horum magister, Orata, non iucundissime vixerit?")))

(let ((count 0))
  (hunchentoot:define-easy-handler (test :uri "/test")
      ()
    (spinneret:with-html-string
      (:p (progn
	    (setf count (1+ count))
	    count)))))

