;; using
;; hunchentoot to host server.
;; spinneret dsl for html.
;; lass for css
;; parenscript for js.
(defpackage :site/main
  (:use :cl) 
  (:export
   :safe-start-server))

(in-package :site/main)

(defparameter *server* nil)
(defun safe-start-server (&optional (port 33333))
  "Verify that the current *SERVER* is not running before overriding the server with a new PUBLIC-PATH."
  (if *server*
      (hunchentoot:stop *server*))
  (setf *server* (make-instance 'hunchentoot:easy-acceptor
                                :port port
 				:document-root (asdf:system-relative-pathname :site "public/")))
  (hunchentoot:start *server*))

(defparameter *css-simple*
  '((html :font-size "1rem"
	  :font-family "sans serif")
    (body :margin "1.5rem 2.5rem")
    (.width :max-width "40rem"
	    :margin "0 auto")))

(defmacro with-layout ((&key title) &body body)
  "Generate HTML content with a standard layout."
  `(spinneret:with-html-string
     (:doctype)
     (:html
      (:head
       (:title ,title)
       (:style (apply #'lass:compile-and-write *css-simple*))
       (:script :src "https://unpkg.com/htmx.org@2.0.3"
		:integrity "sha384-0895/pl2MU10Hqc6jd4RvrthNlDiE9U1tWmX7WRESftEDRosgxNsQG/Ze9YMRzHq"
		:crossorigin "anonymous")
       (:script (:raw
		 (parenscript:ps
		   (defun get-y-scroll ()
		     (with-slots (scroll-y page-y-offset) window
		       (if scroll-y
			   scroll-y
			   page-y-offset)))
		   (defun handle-scroll ()
		     (let ((scroll-amount (get-y-scroll)))
		       (parenscript:chain document body style
					  (set-property "--scroll" (/ scroll-amount (parenscript:@ window inner-height)))))))))
       (:body :onscroll (:raw (parenscript:ps (handle-scroll)))
	      (:header :class "width" "Jalen Moore")
	      (:article :class "width" ,@body))))))

(hunchentoot:define-easy-handler (home :uri "/")
    ()
  (with-layout (:title "Jalen Moore")
    (:button :data-hx-get "/test?count=1" :hx-swap "outerHTML" "0")
    (:button :data-hx-get "/test?count=1" :hx-swap "outerHTML" "0")
    (:p "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Est igitur officium eius generis, quod nec in bonis ponatur nec in contrariis. Quid ergo aliud intellegetur nisi uti ne quae pars naturae neglegatur? Indicant pueri, in quibus ut in speculis natura cernitur. Duo Reges: constructio interrete. Qui autem voluptate vitam effici beatam putabit, qui sibi is conveniet, si negabit voluptatem crescere longinquitate? Vide, ne magis, inquam, tuum fuerit, cum re idem tibi, quod mihi, videretur, non nova te rebus nomina inponere. Nam, ut sint illa vendibiliora, haec uberiora certe sunt. Hoc dictum in una re latissime patet, ut in omnibus factis re, non teste moveamur. Aliter enim nosmet ipsos nosse non possumus. Quod idem cum vestri faciant, non satis magnam tribuunt inventoribus gratiam. Hoc dictum in una re latissime patet, ut in omnibus factis re, non teste moveamur. Cur igitur easdem res, inquam, Peripateticis dicentibus verbum nullum est, quod non intellegatur? Quid affers, cur Thorius, cur Caius Postumius, cur omnium horum magister, Orata, non iucundissime vixerit?")
    (:p "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Est igitur officium eius generis, quod nec in bonis ponatur nec in contrariis. Quid ergo aliud intellegetur nisi uti ne quae pars naturae neglegatur? Indicant pueri, in quibus ut in speculis natura cernitur. Duo Reges: constructio interrete. Qui autem voluptate vitam effici beatam putabit, qui sibi is conveniet, si negabit voluptatem crescere longinquitate? Vide, ne magis, inquam, tuum fuerit, cum re idem tibi, quod mihi, videretur, non nova te rebus nomina inponere. Nam, ut sint illa vendibiliora, haec uberiora certe sunt. Hoc dictum in una re latissime patet, ut in omnibus factis re, non teste moveamur. Aliter enim nosmet ipsos nosse non possumus. Quod idem cum vestri faciant, non satis magnam tribuunt inventoribus gratiam. Hoc dictum in una re latissime patet, ut in omnibus factis re, non teste moveamur. Cur igitur easdem res, inquam, Peripateticis dicentibus verbum nullum est, quod non intellegatur? Quid affers, cur Thorius, cur Caius Postumius, cur omnium horum magister, Orata, non iucundissime vixerit?")
    (:p "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Est igitur officium eius generis, quod nec in bonis ponatur nec in contrariis. Quid ergo aliud intellegetur nisi uti ne quae pars naturae neglegatur? Indicant pueri, in quibus ut in speculis natura cernitur. Duo Reges: constructio interrete. Qui autem voluptate vitam effici beatam putabit, qui sibi is conveniet, si negabit voluptatem crescere longinquitate? Vide, ne magis, inquam, tuum fuerit, cum re idem tibi, quod mihi, videretur, non nova te rebus nomina inponere. Nam, ut sint illa vendibiliora, haec uberiora certe sunt. Hoc dictum in una re latissime patet, ut in omnibus factis re, non teste moveamur. Aliter enim nosmet ipsos nosse non possumus. Quod idem cum vestri faciant, non satis magnam tribuunt inventoribus gratiam. Hoc dictum in una re latissime patet, ut in omnibus factis re, non teste moveamur. Cur igitur easdem res, inquam, Peripateticis dicentibus verbum nullum est, quod non intellegatur? Quid affers, cur Thorius, cur Caius Postumius, cur omnium horum magister, Orata, non iucundissime vixerit?")
    (:p "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Est igitur officium eius generis, quod nec in bonis ponatur nec in contrariis. Quid ergo aliud intellegetur nisi uti ne quae pars naturae neglegatur? Indicant pueri, in quibus ut in speculis natura cernitur. Duo Reges: constructio interrete. Qui autem voluptate vitam effici beatam putabit, qui sibi is conveniet, si negabit voluptatem crescere longinquitate? Vide, ne magis, inquam, tuum fuerit, cum re idem tibi, quod mihi, videretur, non nova te rebus nomina inponere. Nam, ut sint illa vendibiliora, haec uberiora certe sunt. Hoc dictum in una re latissime patet, ut in omnibus factis re, non teste moveamur. Aliter enim nosmet ipsos nosse non possumus. Quod idem cum vestri faciant, non satis magnam tribuunt inventoribus gratiam. Hoc dictum in una re latissime patet, ut in omnibus factis re, non teste moveamur. Cur igitur easdem res, inquam, Peripateticis dicentibus verbum nullum est, quod non intellegatur? Quid affers, cur Thorius, cur Caius Postumius, cur omnium horum magister, Orata, non iucundissime vixerit?")))

(hunchentoot:define-easy-handler (about :uri "/about")
    ()
  (with-layout (:title "About")
    (:h1 "About")))

(hunchentoot:define-easy-handler (test :uri "/test")
    (count)
  (let ((int-count (parse-integer count)))
    (let ((next-query (format nil "/test?count=~a" (1+ int-count))))
      (spinneret:with-html-string
	(:button :data-hx-get next-query :hx-swap "outerHTML" int-count)))))
