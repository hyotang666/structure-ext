(defpackage :structure-ext
  (:use :cl :structure-ext.left-arrow-accessors :structure-ext.as-class)
  (:export .
           #.(#0=(lambda (package)
                   (loop :for symbol :being :each :external-symbol :of package
                         :collect symbol))
              :structure-ext.left-arrow-accessors))
  (:export . #.(#0# :structure-ext.as-class)))