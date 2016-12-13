(defsystem maxima-read-wxmx :depends-on (#+nil "maxima" "xmls" "uiop" "zip")
  :defsystem-depends-on ("maxima-file")
  :author "Robert Dodier"
  :license "GPLv2"
  :description "Functions to enable Maxima to parse and load wxmx (wxMaxima) files"
  :pathname "."
  :components
    ((:file "wxmx-parse-xml")
     (:maxima-file "wxmx_parse_xml")
     (:file "maxima-read-wxmx")))
