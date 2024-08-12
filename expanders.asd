
(defsystem "expanders"
  :author "Héctor Galbis Sanchis"
  :description "Tool for defining expanders."
  :license "MIT"
  :depends-on ("alexandria")
  :components ((:module "src"
                :components ((:file "package")
                             (:file "expanders")))))

;; (defsystem "expanders/docs"
;;   :author "Héctor Galbis Sanchis"
;;   :description "Documentation of expanders."
;;   :license "MIT"
;;   :depends-on ("expanders")
;;   :defsystem-depends-on ("adp-github")
;;   :build-operation "adp-github-op"
;;   :components ((:module "scribble"
;;                 :components ((:file "package")
;;                              (:scribble "README")))))
