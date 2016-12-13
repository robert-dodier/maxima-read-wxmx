;; maxima_read_wxmx.lisp -- load Maxima code from wxmx file
;; copyright 2016 Robert Dodier
;; I release this work under terms of the GNU General Public License

(ql:quickload :zip)

(defun $open_zipfile (file-name-zip)
  (zip:open-zipfile file-name-zip))

(defun $get_zipfile_entry (zipfile entry-name)
  (zip:get-zipfile-entry entry-name zipfile))

;; I WONDER IF THIS FUNCTION SHOULD JUST RETURN AN ARRAY.
;; WHAT IF THE ENTRY CONTAINS AN IMAGE OR OTHER NON-TEXT FILE ??
(defun $zipfile_entry_contents (zipfile-entry)
  (coerce (mapcar #'code-char (coerce (zip:zipfile-entry-contents zipfile-entry) 'list)) 'string))

(defun $get_wxmx_content_xml (file-name-wxmx)
  ($zipfile_entry_contents ($get_zipfile_entry ($open_zipfile file-name-wxmx) "content.xml")))

;; IN is an input stream, as created by make_string_input_stream or whatever

(defun $load_stream (in)
  (catch 'macsyma-quit (continue in t)))

;; returns a string containing the Maxima code from the content.xml entry in FILE-NAME-WXMX
(defun $read_content_xml (file-name-wxmx)
  (let ((s (xmls:parse (make-string-input-stream (mfuncall '$get_wxmx_content_xml file-name-wxmx)))))
    (mfuncall '$join_lines (mfuncall '$extract_input (xml-to-expr s)))))

(defun $load_wxmx (file-name-wxmx)
  ($load_stream (make-string-input-stream ($read_content_xml file-name-wxmx))))

(defun $parse_content_xml (file-name-wxmx)
  (declare (special *mread-prompt*))
  (let*
    ((content-string ($read_content_xml file-name-wxmx))
     (input-stream (make-string-input-stream content-string))
     (*mread-prompt*))
    (cons '(mlist) (loop for x = (mread input-stream) while x collect (third x)))))
