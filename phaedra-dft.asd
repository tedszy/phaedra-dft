;;;; phaedra-dft.asd

(asdf:defsystem #:phaedra-dft
  :description "Intuitive DFT exploration."
  :author "Ted Szylowiec <tedszy@gmail.com>"
  :license  "MIT License"
  :version "0.0.1"
  :depends-on nil
  :serial t
  :components ((:file "package")
	       (:file "phaedra-dft")
	       (:file "phaedra-dft-test")))

