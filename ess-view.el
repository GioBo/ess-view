;;; ess-view.el --- A small package to view dataframes within spreadsheet softwares

;; Copyright (C) 2016 Bocci Gionata

;; Author: boccigionata <boccigionata@gmail.com>
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A small package to view dataframes using spreadsheet softwares

;;; Code:

(require 'ess)
(require 'ess-inf)
(require 'ess-site)
(require 'f)
(require 's)


(defvar   spreadsheet_program  (or
	     (executable-find "libreoffice")
	     (executable-find "openoffice")
	     (executable-find "gnumeric"))
  "Spreadsheet software to be used to show data."
    )


(defun print_vector(obj)
  " In case the passed object is a vector, print its
content in another temporart buffer instead of saving
it to a spreadsheet file."
  (let
      ((header (concat obj " contains the following elements: \n")))
    (ess-execute (concat "cat(" obj ",sep='\n')") nil "*BUFF*" header)
    )
  )


(defun random_string()
  "This function create a random string of 20 characters"
  (interactive)
  (setq rand_str "")
  (let ((mycharset '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "y" "v" "w" "x" "y" "z")))
    (dotimes (i 20)
      (setq rand_str (
		      concat rand_str (elt mycharset (random (length mycharset)))
			     )
	    )
      )
    )
  rand_str
  )


(defun create_env()
  "Creates a temporary environment (in order not to pollute
user's environments) where a temporary copy of the passed object
is going to be created and prepared for conversion to .csv"
  (interactive)
  (let*
      ((nome_env (random_string)))
    ;; it is very unlikely that the user has an environment which
    ;; has the same name of our random generated 20-char string,
    ;; but just to be sure, we run this cycle recursively
    ;; until we find an environment name which does not exist yet
    (if (ess-boolean-command (concat "is.environment(" nome_env ")\n"))
	  (create_env)
	  )
        nome_env
      )
    )


(defun send_to_R (stringa)
  "A wrapper function to send commands to the R process.
Argument STRINGA  is the command - as a string - to be passed to the R process."
  (ess-send-string (get-process "R") stringa  nil)
  )

(defun clean_data_frame (obj)
  "This function cleans the dataframe of interest.
Factors are converted to characters (less problems when exporting), NA and
'NA' are removed so that reading the dataset within the spreadsheet software
is clearer.
Argument OBJ is the name of the dataframe to be cleaned."
  (send_to_R (format "%s[sapply(%s,is.factor)]<-lapply(%s[sapply(%s,is.factor)],as.character)" obj obj obj obj))
  (send_to_R (format "%s[is.na(%s)]<-''\n" obj obj))
  (send_to_R (format "%s[%s=='NA']<-''\n" obj obj ))
  )
  
(defun data_frame_view (object save)
"This function is used in case the passed OBJECT is a data frame.
Argument SAVE if t means that the user wants to store the spreadsheet-modified
version of the dataframe in the original object."
;;  (interactive)
  (save-excursion

    (setq oggetto object)
    ;; create a temp environment where we will work
    (setq envir (create_env))
    (ess-send-string (get-process "R") (concat envir"<-new.env()\n") nil)
    ;; create a copy of the passed object in the custom environment
    (ess-send-string (get-process "R") (concat envir "$obj<-" object "\n") nil)
    ;; create a variable containing the complete name of the object
    ;; (in the form environm$object
    (setq newobj (concat envir "$obj"))
    ;; remove NA and NAN so that objects is easier to read in spreadsheet file
    (clean_data_frame newobj)
    ;; (ess-send-string (get-process "R") (format "%s[is.na(%s)]<-''\n" newobj newobj ) nil)
    ;; (ess-send-string (get-process "R") (format "%s[%s=='NA']<-''\n" newobj newobj ) nil)
    ;; create a csv temp file
    (setq temp_file (make-temp-file nil nil ".csv"))
    ;; write the passed object to the csv tempfile
    (setq stringa  (concat "write.table(" newobj ",file='" temp_file "',sep='|',row.names=FALSE)\n"))
    (ess-send-string (get-process "R") stringa)
    ;; wait a little just to be sure that the file has been written (is this necessary? to be checked)
    (sit-for 1)
    (setq finestre (current-window-configuration))
    ;; start the spreadsheet software to open the temp csv file
    (setq proc (start-process  "spreadsheet" nil  spreadsheet_program  temp_file))
    (if save
	(set-process-sentinel  proc 'write--sentinel)
      )

    (set-window-configuration finestre)
    ;; remove the temporary environment
    (ess-send-string (get-process "R") (format "rm(%s)" envir))
    )
  )

(defun write--sentinel (process signal)
  "Chech the spreadsheet (PROCESS) to intercepts when it is closed (SIGNAL).
The saved version of the file - in the csv fomat -is than converted back
to the R dataframe."
  (cond
   ((equal signal "finished\n")
    (progn
      (check_separator temp_file)
      (send_to_R (format "%s <- read.table('%s',header=TRUE,sep=',',stringsAsFactors=FALSE)\n" oggetto temp_file))
      )
    ))
)


(defun check_separator(filePath)
  "This is a tentative strategy for obtaining a file separated by commas
reagardless of the default field separator used by the spreadsheet software."
  (setq testo (s-split "\n" (f-read filePath) t))
  (setq testo (mapcar (lambda(x)(s-replace-all '(("\t" . ",") ("|" . ",")  (";" . ",") ) x)) testo))
  (setq testo (s-join "\n" testo))
  (f-write-text testo 'utf-8 filePath)
)

(defun ess-view-df()
  (interactive)
  (setq oggetto (ess-read-object-name "object to inspect:"))
  ;;(setq oggetto (car oggetto))
  (setq oggetto (substring-no-properties (car oggetto)))
  ;;(setq test (ess-boolean-command (concat "is.vector('" oggetto "')\n")))

  (cond
   ((ess-boolean-command (concat "is.vector(" oggetto ")\n")) (print_vector oggetto))
   ((ess-boolean-command (concat "is.data.frame(" oggetto ")\n")) (data_frame_view oggetto nil))
   (t (message "the object is neither a vector or a data.frame; don't know how to show it..."))
   )
  )


(defun ess-modify-df()
  (interactive)
  (setq oggetto (ess-read-object-name "object to modify:"))
  ;;(setq oggetto (car oggetto))
  (setq oggetto (substring-no-properties (car oggetto)))
  ;;(setq test (ess-boolean-command (concat "is.vector('" oggetto "')\n")))

  (cond
   ((ess-boolean-command (concat "is.vector(" oggetto ")\n")) (print_vector oggetto))
   ((ess-boolean-command (concat "is.data.frame(" oggetto ")\n")) (data_frame_view oggetto t))
   (t (message "the object is neither a vector or a data.frame; don't know how to show it..."))
   )
  )


;;(global-set-key (kbd "C-x w") 'ess-view-df)


(define-minor-mode ess-view-mode
  "Have a look ad dataframes."
  :lighter " ess-v"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-x w") 'ess-view-df)
	    (define-key map (kbd "C-x q") 'ess-modify-df)
            map)
  )


(add-hook 'ess-post-run-hook 'ess-view-mode)
(provide 'ess-view)


;;; ess-view.el ends here
