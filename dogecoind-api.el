;;; dogecoind-api.el --- API library for working with dogecoind

;; Copyright (C) 2014 Phil Newton

;; Author: Phil Newton <phil@sodaware.net>
;; Version: 1.0.0
;; Package-Requires: ((json "1.2"))
;; Keywords: dogecoin

;; This program is free software; you can redistribute it and/or modify
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

;; dogecoind-api provides library functionality for working with the dogecoind
;; process.

;;; Code:

;; Dependencies

(require 'json)
(require 'url-http)

(defvar url-http-end-of-headers)

;; Configuration

(defgroup dogecoind-api nil
  "dogecoind api extension"
  :group 'processes
  :prefix "dogecoind-api-")

(defcustom dogecoind-api-username nil
  "Your dogecoind username."
  :group 'dogecoind-api
  :type '(string))

(defcustom dogecoind-api-password nil
  "Your dogecoind password."
  :group 'dogecoind-api
  :type '(string))

(defcustom dogecoind-api-port 8334
  "Your dogecoind server port."
  :group 'dogecoind-api
  :type '(integer))

(defcustom dogecoind-api-address "127.0.0.1"
  "Your dogecoind server IP address."
  :group 'dogecoind-api
  :type '(string))


;; Main API methods

(defun dogecoind-api-get-info ()
  "Get an object containing various state info."
  

  (message (format "%s" (dogecoind-api--get-request "getinfo")))
  
  ;; Connect
  
  )


;; Request helpers

(defun dogecoind-api--get-request (action &optional params)
  "Request the server to perform ACTION with optional PARAMS."
  (let* ((http-auth-token (dogecoind-api--create-auth dogecoind-api-username dogecoind-api-password))
         (url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Basic " http-auth-token))))
         (url-request-data "{ \"method\": \"getinfo\" }"))

    (with-current-buffer (url-retrieve-synchronously "http://127.0.0.1:8334/")
      (goto-char (point-min))
      (goto-char url-http-end-of-headers)
      (prog1 (json-read)
        (kill-buffer)))))

(defun dogecoind-api--build-request (action &optional params)
  "Use ACTION, and optionally PARAMS, to build the JSON payload for an RPC request."
  (json-encode (append `((:action . ,action)) params)))

(defun dogecoind-api--create-auth (username password)
  "Use USERNAME and PASSWORD to create a HTTP authorization token."
  (base64-encode-string (concat username ":" password) t))

(provide 'dogecoind-api)
;;; dogecoind-api.el ends here
