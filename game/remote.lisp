(in-package :hextris)

(defvar *remote-api-uri* "https://davar.icfpcontest.org/teams/16/solutions")
(defvar *remote-api-key* "M72G+E7hDY2E9yoKXtsalHDF7lQTFwJY7oX/NunZzBg=")



(defun remote-submit-raw (task-id seed command &optional tag)
  (bind ((message `(:problem-id ,task-id :seed ,seed
                                :solution ,command
                                ,@(when tag `(:tag ,tag))))
         (message-json (format nil "[~a]" (json:encode-json-plist-to-string message))))
    (http-request *remote-api-uri*
                  :method :post
                  :basic-authorization `("" ,*remote-api-key*)
                  :content-type "application/json"
                  :content message-json)))

(defun remote-submit (task-id seed command-adt &optional tag)
  (remote-submit-raw task-id seed (power-phrase-encode-adt command-adt) tag))
