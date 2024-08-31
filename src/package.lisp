
(defpackage #:expanders
  (:use #:cl #:alexandria)
  (:nicknames #:exp)
  (:export #:defexpander #:expanderp #:defexpansion #:expansionp #:expand))
