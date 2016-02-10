;;; ess-view.el --- A small package to view dataframes within spreadsheet softwares

;; Copyright (C) 2016 boccigionata

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

;; A small package to view dataframes within spreadsheet softwares

;;; Code:



(require 'ess-site)


(defvar   spreadsheet_program  (or
	     (executable-find "libreoffice")
	     (executable-find "openoffice")
	     (executable-find "gnumeric"))			 
  "Spreadsheet software to be used to show data"
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
user's environments) where temporary copy of the passed object
is going to be created and prepared for converting to .csv"
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
  (ess-send-string (get-process "R") stringa  nil)
  )

(defun clean_data_frame (obj)
  (interactive)
  (send_to_R (format "%s[sapply(%s,is.factor)]<-lapply(%s[sapply(%s,is.factor)],as.character)" obj obj obj obj))
  (send_to_R (format "%s[is.na(%s)]<-''\n" obj obj))
  (send_to_R (format "%s[%s=='NA']<-''\n" obj obj ))
  )
  
(defun data_frame_view (object)
" This function is used in case the passed object is a data frame"
  (interactive)
  (save-excursion

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
    (setq stringa  (concat "write.table(" newobj ",file='" temp_file "',sep=',',row.names=FALSE)\n"))
    (ess-send-string (get-process "R") stringa)
    ;; wait a little just to be sure that the file has been written (is this necessary? to be checked)
    (sit-for 1)
    (setq finestre (current-window-configuration))
    ;; start the spreadsheet software to open the temp csv file
    (start-process  "prova" nil  spreadsheet_program  temp_file)
    (set-window-configuration finestre)
    ;; remove the temporary environment
    (ess-send-string (get-process "R") (format "rm(%s)" envir))
    )
  )
  

(defun ess-view()
  (interactive)
  (setq oggetto (ess-read-object-name "object to inspect:"))
  ;;(setq oggetto (car oggetto))
  (setq oggetto (substring-no-properties (car oggetto)))
  ;;(setq test (ess-boolean-command (concat "is.vector('" oggetto "')\n")))

  (cond
   ((ess-boolean-command (concat "is.vector(" oggetto ")\n")) (print_vector oggetto))
   ((ess-boolean-command (concat "is.data.frame(" oggetto ")\n")) (data_frame_view oggetto))
   (t (message "the object is neither a vector or a data.frame; don't know how to show it"))
   )
  )

(global-set-key (kbd "C-x w") 'ess-view)
;;(message system-type)


   
(provide 'ess-view)


;;; ess-view.el ends here
