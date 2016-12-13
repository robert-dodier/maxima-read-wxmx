;; wxmx_parse_xml.lisp -- parse xml extracted from .wxmx file
;; copyright 2016 Robert Dodier
;; I release this work under terms of the GNU General Public License

;; ("foo" <attrs> ("bar" <attrs> <stuff>)) --> (($FOO <attrs>) (($BAR <attrs>) <stuff>))

(defun xml-to-expr (e)
  (if (atom e) e (cons (cons ($concat '\$ (first e)) (second e)) (mapcar #'xml-to-expr (rest (rest e))))))

(defun $read_xml (f)
  (xml-to-expr (with-open-file (s f) (xmls:parse s))))

