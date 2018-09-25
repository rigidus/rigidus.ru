(ql:quickload "ironclad")
(ql:quickload "closer-mop")
(ql:quickload "anaphora")
(ql:quickload "alexandria")
(ql:quickload "flexi-streams")
(ql:quickload "cl-ppcre")
(ql:quickload "postmodern")
(ql:quickload "restas")
(ql:quickload "restas-directory-publisher")
(ql:quickload "closure-template")
(ql:quickload "cl-json")

(restas:define-module #:node
  (:use #:closer-mop #:cl #:iter #:alexandria #:anaphora #:postmodern)
  (:shadowing-import-from :closer-mop
                          :defclass
                          :defmethod
                          :standard-class
                          :ensure-generic-function
                          :defgeneric
                          :standard-generic-function
                          :class-name))

(defparameter *tpl-path* (merge-pathnames
                          (make-pathname :directory '(:relative "org/lrn/crypto"))
                          (merge-pathnames
                           (make-pathname :directory '(:relative "repo/rigidus.ru"))
                           (user-homedir-pathname))))

(closure-template:compile-template
 :common-lisp-backend (merge-pathnames
                       (make-pathname :name "head" :type "htm")
                       *tpl-path*))

(closure-template:compile-template
 :common-lisp-backend (merge-pathnames
                       (make-pathname :name "swgr" :type "htm")
                       *tpl-path*))

(restas:start '#:node :port 2345)

(in-package :node)

(defparameter *base-dir*
  (merge-pathnames
   (make-pathname :directory '(:relative "repo/rigidus.ru"))
   (user-homedir-pathname)))

(restas:mount-module -css- (#:restas.directory-publisher)
  (:url "/css/")
  (restas.directory-publisher:*directory*
   (merge-pathnames (make-pathname :directory '(:relative "css"))
                    *base-dir*)))

(restas:mount-module -js- (#:restas.directory-publisher)
  (:url "/js/")
  (restas.directory-publisher:*directory*
   (merge-pathnames (make-pathname :directory '(:relative "js"))
                    *base-dir*)))

(restas:debug-mode-on)

;; (restas:debug-mode-off)

(setf hunchentoot:*catch-errors-p* t)

(defmacro bprint (var)
  `(subseq (with-output-to-string (*standard-output*)
             (pprint ,var)) 1))

(defun sha-256 (str)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256 (flexi-streams:string-to-octets str :external-format :utf-8))))

(defparameter *request-address* "localhost:2345")
(defparameter *blocks* nil)
(defparameter *contracts* (make-hash-table :test #'equal))
(defparameter *storages* (make-hash-table :test #'equal))
(defparameter *vfm-path* "/home/rigidus/repo/rigidus.ru/org/lrn/forth/src")
(defparameter *vfm-path-lib* "src64/jonesforth64.f")

(defmacro with-run-vfm ((sender amount) &body body)
  `(let* ((path *vfm-path*)
          (proc (sb-ext:run-program (format nil "~A/~A" path "forth64") '("asd" "qwe")
                                    :environment (list
                                                  (format nil "SENDER=~A" ,sender)
                                                  (format nil "AMOUNT=~A" ,amount))
                                    :wait nil :input :stream :output :stream))
          (base (alexandria:read-file-into-string (format nil "~A/~A" path *vfm-path-lib*))))
     (with-open-stream (input (sb-ext:process-input proc))
       (with-open-stream (output (sb-ext:process-output proc))
         (format input "~A" base)
         (force-output input)
         (unless (equal "VFM VERSION 47 OK" (read-line output))
           (error "VFM Welcome Error"))
         ,@body))))

(defun vfm-write (input msg &optional (terminator " CR"))
  (format t "»[~A~A~%]»~%~%" msg terminator) ;; два последних перевода строки - для отделения вывода
  (format input "~A~A~%" msg terminator)
  (force-output input))

(defun vfm-read (output &key (label "") (cnt 1))
  (loop :for idx :from 1 :to cnt :collect
     (let ((in-string (read-line output)))
       (format t "[~A] ~A «[~A]«~%" idx label in-string)
       in-string)))

(defun vfm-eval (str-lst get-current-storage)
  (let ((in-string (format nil "~{~A ~}" str-lst)))
    (setf in-string (ppcre:regex-replace-all "᚜" in-string "("))
    (setf in-string (ppcre:regex-replace-all "᚛" in-string ")"))
    (setf in-string (ppcre:regex-replace-all "«" in-string "\""))
    (setf in-string (ppcre:regex-replace-all "»" in-string "\""))
    (let ((eval-list (read-from-string in-string)))
      ;; (format t "~%★ ~A~%" (bprint eval-list))
      (let ((eval-result (eval `(let ((storage (funcall get-current-storage)))
                                  ,eval-list))))
        ;; (format t "~%☭ ~A~%" eval-result)
        eval-result))))

(defmacro vfm-repl (input output)
  `(block repl-block
     (handler-case
         (tagbody
          repl
            (setf result (vfm-eval (vfm-read ,output)))
            (vfm-write ,input result "")
            (go repl))
       (END-OF-FILE () (progn
                         (format t "----------------- end~%")
                         (return-from repl-block nil))))))

(defun vfm-dbg (param)
  (format t "{{===---~A---===}}~%" param)
  (format nil ""))

;; ." ᚜vfm-dbg-die «" .S  ." »᚛" CR
(defun vfm-dbg-die (param)
  (format t "{{===---~A---===}}~%" param)
  (format nil "BYE"))

(defun get-storage (hash)
  (gethash hash *storages* (make-hash-table :test #'equal)))

(defun set-storage (hash new)
  (setf (gethash hash *storages*) new))

(defun run-vfm (vfm base code params env run hash)
  (let* ((storage (make-hash-table :test #'equal))
         (path *vfm-path*)
         (proc (sb-ext:run-program
                vfm params :environment env :wait nil :input :stream :output :stream)))
    (with-open-stream (input (sb-ext:process-input proc))
      (with-open-stream (output (sb-ext:process-output proc))
        (format input "~a" base)
        (force-output input)
        (unless (equal "VFM VERSION 47 OK" (read-line output))
          (error "VFM Welcome Error"))
        (format t "~%~%----------------- begin~%")
        (let* ((result))
          (vfm-write input code "")
          (vfm-write input run "")
          ;; macroexpand of (vfm-repl input output)
          (block repl-block
            (handler-case
                (tagbody repl
                   (setf result
                         ;; (vfm-eval (vfm-read output) get-curent-storage)
                         (let ((in-string (format nil "~{~A ~}" (vfm-read output))))
                           (setf in-string (ppcre:regex-replace-all "᚜" in-string "("))
                           (setf in-string (ppcre:regex-replace-all "᚛" in-string ")"))
                           (setf in-string (ppcre:regex-replace-all "«" in-string "\""))
                           (setf in-string (ppcre:regex-replace-all "»" in-string "\""))
                           (let ((eval-list (read-from-string in-string)))
                             ;; (format t "~%★ ~A~%" (bprint eval-list))
                             (let ((eval-result (eval `(let ((storage (get-storage ,hash)))
                                                         (prog1 ,eval-list
                                                           (set-storage ,hash storage))))))
                               ;; (format t "~%☭ ~A~%" eval-result)
                               eval-result))))
                   (vfm-write input result "")
                   (go repl))
              (end-of-file nil
                (progn (format t "----------------- end~%")
                       (return-from repl-block nil)))))
          (values))))))

(defun make-endpoint (name group method notes curl &optional (parameters ""))
  (list :group group :method method :endpoint name
        :notes notes :curl curl :parameters parameters))

(defun make-resource (name endpoints)
  (list :resource name :endpoints endpoints))

(defun make-curl-get (resource endpoint)
  (format nil "curl -X GET --header 'Accept: application/json' 'http://~A/~A/~A'"
          *request-address* resource endpoint))

(defun make-curl-post (resource endpoint field body)
  (format nil "curl -X POST --header 'Accept: application/json' -d '{\"~A\":\"~A\"}' 'http://~A/~A/~A'" field body *request-address* resource endpoint))

(restas:define-route swgr ("/")
  (swgr:all
   (list
    :head (swgr:head)
    :body (swgr:body
           (list
            :resources (list
                        (make-resource
                         "blocks"
                         (list
                          (make-endpoint "get_height" "blocks" "get" "Request of height"
                                         (make-curl-get "blocks" "get_height"))
                          (make-endpoint "new_block" "blocks" "post" "Send new block from network"
                                         (make-curl-post "blocks" "new_block" "newblock" "{\\\"hash\\\":\\\"012345DEADBEEF\\\"}")
                                         (swgr:parameters
                                          (list :params
                                                (list
                                                 (list :name "newblock" :field "newblock" :descr "new block"
                                                       :body (format nil "{~%  \"hash\":\"012345DEADBEEF\"~%}"))))))))
                        (make-resource
                         "contracts"
                         (list
                          (make-endpoint "new_contract" "contracts" "post" "Publish new contract"
                                         (make-curl-post "contracts" "new_contract" "new_contract" "")
                                         (swgr:parameters
                                          (list :params
                                                (list
                                                 (list :name "contract_code" :field "contract_code" :descr "hash of contract"
                                                       :body "")))))
                          (make-endpoint "get_contract_storage" "contracts" "get" "Get storage variables"
                                         (make-curl-get "contracts" "get_contract_storage")
                                         (swgr:parameters
                                          (list :params
                                                (list
                                                 (list :name "hash" :field "hash" :descr "hash of contract" :tag "input"
                                                       :body (format nil "012345DEADBEEF"))))))
                          (make-endpoint "get_contract_code" "contracts" "get" "Get contract code"
                                         (make-curl-get "contracts" "get_contract_code")
                                         (swgr:parameters
                                          (list :params
                                                (list
                                                 (list :name "hash" :field "hash" :descr "hash of contract" :tag "input"
                                                       :body (format nil "012345DEADBEEF"))))))
                          (make-endpoint "call_contract" "contracts" "post" "call contract function"
                                         (make-curl-post "contracts" "call_contract" "call_contract" "")
                                         (swgr:parameters
                                          (list :params
                                                (list
                                                 (list :name "hash" :field "hash" :descr "hash of contract" :tag "input"
                                                       :body "")
                                                 (list :name "call_function" :field "call_function" :descr "name of function" :tag "input"
                                                       :body "")
                                                 (list :name "sender_hash" :field "sender_hash" :descr "hash of sender" :tag "input"
                                                       :body "")
                                                 (list :name "amount" :field "amount" :descr "amount of money" :tag "input"
                                                       :body "")))))))
                        (make-resource
                         "transactions"
                         (list
                          (make-endpoint "get_transaction" "transactions" "get" "Get transaction"
                                         (make-curl-get "transactions" "get_transaction"))
                          ))))))))

(restas:define-route blocks/new_block/post ("/blocks/new_block" :method :post)
  ;; (format nil "post::>> ~A~%" (bprint (hunchentoot:raw-post-data :force-text t))))
  ;; (format nil "post:=>~%~A~%" (bprint (cl-json:decode-json-from-string (hunchentoot:raw-post-data :force-text t)))))
  (let* ((req (cl-json:decode-json-from-string (hunchentoot:raw-post-data :force-text t)))
         (blk (cdr (assoc :newblock req)))
         (new (cl-json:decode-json-from-string blk)))
    (format nil "~A~%" (bprint (push new *blocks*)))))

(restas:define-route blocks/get_height ("/blocks/get_height")
  ;; (let ((params (hunchentoot:get-parameters*)))
  ;;   (if (= 0 (length params))
  ;;       (format nil "get: empty~%")
  ;;       (format nil "get: ~A~%" params))))
  (bprint (length *blocks*)))

(restas:define-route contracts/new_contract/post ("contracts/new_contract" :method :post)
  (let* ((req (cl-json:decode-json-from-string (hunchentoot:raw-post-data :force-text t)))
         (contract-code (cdr (assoc :contract--code req))))
    (setf contract-code (string-trim '(#\Space #\Tab #\Newline) contract-code))
    (let ((hash (sha-256 contract-code)))
      (setf (gethash hash *contracts*) contract-code)
      (setf (gethash hash *storages*) nil)
      (format nil "~A~%" (bprint hash)))))

(restas:define-route contracts/get_contract_storage ("contracts/get_contract_storage")
  (let ((hash (hunchentoot:get-parameter "hash")))
    (if (null hash)
        (format nil "Error: bad param!~%")
        (bprint (get-storage hash)))))

(restas:define-route contracts/get_contract_code ("contracts/get_contract_code")
  (let ((hash (hunchentoot:get-parameter "hash")))
    (if (null hash)
        (format nil "Error: bad param!~%")
        (let ((code (gethash hash *contracts* nil)))
          code))))

(restas:define-route contracts/call_contract/post ("contracts/call_contract" :method :post)
  ;; (format nil "post:=>~%~A~%" (bprint (cl-json:decode-json-from-string (hunchentoot:raw-post-data :force-text t)))))
  (let* ((req (cl-json:decode-json-from-string (hunchentoot:raw-post-data :force-text t)))
         (hash (cdr (assoc :hash req)))
         (call (cdr (assoc :call--function req)))
         (code (gethash hash *contracts*))
         (sender (cdr (assoc :sender--hash req)))
         (amount (cdr (assoc :amount req))))
    (if (null code)
        (format nil "~A~%" "Contract not exists")
        ;; (format nil "~A~%" code))))
        (run-vfm
         "/home/rigidus/repo/rigidus.ru/org/lrn/forth/src/forth64"
         (read-file-into-string "/home/rigidus/repo/rigidus.ru/org/lrn/forth/src/src64/jonesforth64.f")
         code ;; ": ALFA .\" ᚜do-beta-gamma᚛\" CR ;"
         '("asd" "qwe") (list (format nil "SENDER=~A" (sha-256 "sender")) (format nil "AMOUNT=~A" 100))
         "ALFA" hash))))

;; for a7482557631fe6fe4008aa9fabc6b17ac610f28f2e21c28756f303a9caf732e8
(defun cl-user::do-beta-gamma ()
  "BYE")


;; (let* ((hash "6b0264f3ca4bfeca3102927aee1ba98a4941585fa3d9c6519fe6ac032d18b38e")
;;        (code (gethash hash *contracts*)))
;;   (run-vfm
;;    "/home/rigidus/repo/rigidus.ru/org/lrn/forth/src/forth64"
;;    (read-file-into-string "/home/rigidus/repo/rigidus.ru/org/lrn/forth/src/src64/jonesforth64.f")
;;    ": ALFA .\" ᚜do-beta᚛\" CR ;"
;;    '("asd" "qwe") (list (format nil "SENDER=~A" (sha-256 "sender")) (format nil "AMOUNT=~A" 100))
;;    "ALFA" hash))

;; (run-vfm
;;  "/home/rigidus/repo/rigidus.ru/org/lrn/forth/src/forth64"
;;  (read-file-into-string "/home/rigidus/repo/rigidus.ru/org/lrn/forth/src/src64/jonesforth64.f")
;;  (read-file-into-string "/home/rigidus/repo/rigidus.ru/org/lrn/crypto/smart-g-nodes.f")
;;  '("asd" "qwe") (list (format nil "SENDER=~A" (sha-256 "sender")) (format nil "AMOUNT=~A" 100))
;;  "ADD-AMOUNT" "843e0047a395e005da8a3af9cf109e36cf2b071df99677068a1510618d50b516")

;; (get-storage "843e0047a395e005da8a3af9cf109e36cf2b071df99677068a1510618d50b516")

;; (maphash #'(lambda (k v)
;;              (print (list k  v)))
;;          (get-storage "843e0047a395e005da8a3af9cf109e36cf2b071df99677068a1510618d50b516"))

;; (get-storage "843e0047a395e005da8a3af9cf109e36cf2b071df99677068a1510618d50b516")
