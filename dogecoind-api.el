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
  (dogecoind-api--get-request "getinfo"))

(defun dogecoind-api-get-block-count ()
  "Get the number of blocks in the longest block chain."
  (assoc-default 'result (dogecoind-api--get-request "getblockcount")))

(defun dogecoind-api-get-difficulty ()
  "Get the proof-of-work difficulty as a multiple of the minimum difficulty."
  (assoc-default 'result (dogecoind-api--get-request "getdifficulty")))

(defun dogecoind-api-get-connection-count ()
  "Get the number of connections to other nodes."
  (dogecoind-api--get-request-result "getconnectioncount"))

(defun dogecoind-api-get-balance (&optional account)
  "Get the balance for all accounts, or just ACCOUNT if specified."
  (let ((params (unless (null account) `(,account))))
    (assoc-default 'result (dogecoind-api--get-request "getbalance" params))))

(defun dogecoind-api-get-account (address)
  "Get the account associated with ADDRESS."
  (dogecoind-api--get-request-result "getaccount" `(,address)))

(defun dogecoind-api-get-account-address (account &optional no-create)
  "Get the Dogecoin address for ACCOUNT, or create if ACCOUNT does not exist.

If NO-CREATE is true, the address will only be retrieved if the account exists."
  (if no-create
      (if (dogecoind-api-account-exists-p account)
          (dogecoind-api--get-request-result "getaccountaddress" `(,account))
        nil)
    (dogecoind-api--get-request-result "getaccountaddress" `(,account))))

(defun dogecoind-api-list-accounts ()
  "Get a list of account names and their balances."
  (let ((json-key-type 'string))
    (dogecoind-api--get-request "listaccounts")))

(defun dogecoind-api-get-received-by-account (&optional account min-confirmations)
  "Fetch the amount of DOGE received by ACCOUNT.

Can optionally specify MIN-CONFIRMATIONS, which will only include transactions
with at least that many confirmations."
  (dogecoind-api--get-request-result "getreceivedbyaccount" `(,account ,min-confirmations)))


(defun dogecoind-api-list-transactions (&optional account count from)
  "List transactions for ACCOUNT, up to COUNT and skipping the first FROM.

If no account is specified, transactions from all accounts will be listed."

  ;; Set defaults - "*" will list transactions from all accounts
  (let ((account (when (null account) "*"))
        (count (when (null count) 10))
        (from (when (null from) 0)))
    (dogecoind-api--get-request "listtransactions" `(,account ,count ,from))))


;; Account helpers

(defun dogecoind-api-account-exists-p (account)
  "Check if named ACCOUNT exists."
  (let ((accounts (dogecoind-api-list-accounts)))
    (not (null (assoc-default account accounts)))))


;; Server Helpers

(defun dogecoind-api-server-running-p ()
  "Check if the dogecoind server is running."
  (url-http-file-exists-p (dogecoind-api--build-endpoint)))


;; Request helpers

(defun dogecoind-api--get-request (action &optional params)
  "Perform ACTION on the server with optional PARAMS."
  (let* ((http-auth-token (dogecoind-api--create-auth dogecoind-api-username dogecoind-api-password))
         (url-request-method "GET")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Basic " http-auth-token))))
         (url-request-data (dogecoind-api--build-request action params)))
    (with-current-buffer (url-retrieve-synchronously (dogecoind-api--build-endpoint))
      (goto-char (point-min))
      (goto-char url-http-end-of-headers)
      (prog1 (json-read)
        (kill-buffer)))))

(defun dogecoind-api--get-request-result (action &optional params)
  "Perform ACTION on the server with optional PARAMS and return the 'result key."
  (assoc-default 'result (dogecoind-api--get-request action params)))

(defun dogecoind-api--build-request (action &optional params)
  "Use ACTION, and optionally PARAMS, to build the JSON payload for an RPC request."
  (let ((request (append `((:method . ,action)) `((:params . ,params)))))
    (when (null params)
      (setq request (delq (assoc :params request) request)))
    (json-encode request)))

(defun dogecoind-api--build-endpoint ()
  "Create the address endpoint to connect to the server."
  (format "http://%s:%s/" dogecoind-api-address dogecoind-api-port))

(defun dogecoind-api--create-auth (username password)
  "Use USERNAME and PASSWORD to create a HTTP authorization token."
  (base64-encode-string (concat username ":" password) t))

(provide 'dogecoind-api)
;;; dogecoind-api.el ends here
