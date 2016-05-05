(load "dates.el") ; This loads the date-table hash table.

(defun chomp-end (str)
  "Chomp tailing whitespace from STR."
  (replace-regexp-in-string (rx (* (any " \t\n")) eos)
                            ""
                            str))

(defun parse-elr-lines (filePath)
  "Parse an ELR file."
  (with-temp-buffer
    (insert-file-contents filePath)
    (let (result)
      (dolist (line (split-string (buffer-string) "\n" t) result)
        (setq result
              (cons
               (list
                (chomp-end (substring line 20 29)) ; SSN (first character is a 0, so it is skipped)
                (chomp-end (substring line 33 62)) ; Name
                (chomp-end (substring line 62 63)) ; Gender
                (chomp-end (substring line 66 76)) ; DOB
                ) result)))
      (reverse result))))

(defun add-term-date (record)
  (let ((ssn (car record)))
    (print ssn)
    (let ((term-date (gethash ssn date-table "")))
      (print term-date)
      (reverse
       (cons term-date
             (reverse record))))))

(defun get-term-dates (filePath)
  "Get term dates for the given employeees"
  (let ((buffer-name
         (apply 'format "term-dates-%s%s.%s" (current-time))))
    (with-current-buffer (generate-new-buffer buffer-name)
      (dolist (record (parse-elr-lines filePath))
        (insert
         (apply 'format
                '"%s\t%s\t%s\t%s\t%s\n" 
                (add-term-date record))))
      (switch-to-buffer buffer-name))))


(get-term-dates "elr0001v2.txt")
(get-term-dates "elr0002v2.txt")
