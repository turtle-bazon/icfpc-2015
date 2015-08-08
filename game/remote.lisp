(in-package :hextris)

(defvar *remote-api-uri* "https://davar.icfpcontest.org/teams/16/solutions")
(defvar *remote-api-key* "M72G+E7hDY2E9yoKXtsalHDF7lQTFwJY7oX/NunZzBg=")

(defun remote-make-solution-json (task-id seed command &optional tag)
  (json:encode-json-plist-to-string `(:problem-id ,task-id :seed ,seed
                                      :solution ,command
                                      ,@(when tag `(:tag ,tag)))))

(defun remote-make-solution-adt-json (task-id seed command-adt &optional tag)
  (remote-make-solution-json task-id seed (power-phrase-encode-adt command-adt) tag))

(defun remote-submit-raw (task-id seed command &optional tag)
  (format t "Submitting task ~a (~a) with seed ~a: ~a~%" task-id tag seed command)
  (http-request *remote-api-uri*
                :method :post
                :basic-authorization `("" ,*remote-api-key*)
                :content-type "application/json"
                :content (format nil "[~a]" (remote-make-solution-json task-id seed command tag))))

(defun remote-submit (game-loop-result task-id &optional tag)
  (iter (for solution in game-loop-result)
        (format t "Server response ~a~%"
                (remote-submit* task-id (getf solution :seed)
                                (getf solution :script)
                                (when tag (symbol-name (gensym tag)))))))

(defun remote-submit* (task-id seed command-adt &optional tag)
  (remote-submit-raw task-id seed (power-phrase-encode-adt command-adt) tag))
