
(defsystem "expanders"
  :author "Héctor Galbis Sanchis"
  :description "Tool for defining expanders."
  :license "MIT"
  :depends-on ("alexandria" "ecclesia" "vault")
  :components ((:module "src"
                :components ((:file "package")
                             (:file "expanders")))))

;; (defsystem "expanders/docs"
;;   :author "Héctor Galbis Sanchis"
;;   :description "Documentation of expanders."
;;   :license "MIT"
;;   :depends-on ("expanders")
;;   :defsystem-depends-on ("adp-github")
;;   :class :adp-github
;;   :components ((:module "scribble"
;;                 :components ((:file "package")
;;                              (:scribble "README")))))
