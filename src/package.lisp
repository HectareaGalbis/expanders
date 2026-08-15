
(defpackage #:expanders
  (:use #:cl #:alexandria #:vault)
  (:nicknames #:exp)
  (:export #:make-expander #:expanderp #:defexpansion #:expansionp #:expand))
