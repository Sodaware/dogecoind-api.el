;; Required testing libraries
(require 'cl)
(require 'el-mock)

;; Load the library 
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(require 'dogecoind-api)

;; Setup test paths
(setq test-directory (file-name-directory (directory-file-name (file-name-directory load-file-name))))

;; Helper functions

(defun read-fixture (file)
  "Reads FILE from the fixtures directory and returns it as parsed JSON."
  
  ;; Get the file path
  (let* ((file-path (concat test-directory "/fixtures/" file))
         (file-contents (with-temp-buffer
                          (insert-file-contents file-path)
                          (buffer-string))))
    (json-read-from-string file-contents)))

(defun read-fixture-file-as-response (file)
  "Reads FILE from the fixtures directory and returns it as a fake HTTP response."

    (let* ((file-path (concat test-directory "/fixtures/" file))
           (file-contents (with-temp-buffer
                            (insert-file-contents file-path)
                            (buffer-string)))
           (response-buffer (get-buffer-create "*http 127.0.0.1:8334*")))

      ;; Insert the file contents into a new buffer along with a HTTP header
      ;; NOTE: I don't really think we need to bother adding the header, as it's
      ;; snipped off using url-http-end-of-headers
      (with-current-buffer response-buffer
        (insert (concat "HTTP/1.0 200 OK\n\n" file-contents))
        (set (make-local-variable 'url-http-end-of-headers) 18)
        (current-buffer))))


;; Mock Helpers

(defmacro mock-request (action &optional args fixture)
  (let ((fixture (if (null fixture) (concat action ".json") fixture)))
    (if (null args)
        `(mock (dogecoind-api--get-request ,action) => (read-fixture ,fixture))
      `(mock (dogecoind-api--get-request ,action ,args) => (read-fixture ,fixture)))))
