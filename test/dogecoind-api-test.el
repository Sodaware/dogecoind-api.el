
;; API method tests

(ert-deftest dogecoind-api-test/can-get-info ()
  (with-mock
   (mock (dogecoind-api--get-request "getinfo") => (read-fixture "getinfo.json"))
   (let ((response (dogecoind-api-get-info)))
     (should (eq 1080000 (assoc-default 'version response)))
     (should (eq 70003 (assoc-default 'protocolversion response)))
     (should (eq :json-false (assoc-default 'testnet response))))))

(ert-deftest dogecoind-api-test/can-get-block-count ()
  (with-mock
   (mock (dogecoind-api--get-request "getblockcount") => (read-fixture "getblockcount.json"))
   (should (= 123456 (dogecoind-api-get-block-count)))))

(ert-deftest dogecoind-api-test/can-get-difficulty ()
  (with-mock
   (mock (dogecoind-api--get-request "getdifficulty") => (read-fixture "getdifficulty.json"))
   (should (= 654321.123456 (dogecoind-api-get-difficulty)))))

(ert-deftest dogecoind-api-test/can-get-balance-without-account ()
  (with-mock
   (mock (dogecoind-api--get-request "getbalance") => (read-fixture "getbalance.json"))
   (should (= 123.123465 (dogecoind-api-get-balance)))))

(ert-deftest dogecoind-api-test/can-get-balance-with-account ()
  (with-mock
   (mock (dogecoind-api--get-request "getbalance" '("testaccount")) => (read-fixture "getbalance-testaccount.json"))
   (should (= 321.123465 (dogecoind-api-get-balance "testaccount")))))

(ert-deftest dogecoind-api-test/can-list-accounts ()
  (with-mock
   (mock (dogecoind-api--get-request "listaccounts") => (read-fixture "listaccounts.json"))
   (let ((accounts (dogecoind-api-list-accounts)))
     (should (= 3 (length accounts)))
     (should (= 100 (assoc-default "Account One" accounts))))))

(ert-deftest dogecoind-api/can-get-account-address ()
  (with-mock
   (mock (dogecoind-api--get-request "getaccountaddress" `(,"testaccount")) => (read-fixture "getaccountaddress-testaccount.json"))
   (should (equal "abc123" (dogecoind-api-get-account-address "testaccount")))))

(ert-deftest dogecoind-api/get-account-address-does-not-create-account-if-no-create-set ()
  (with-mock
   (mock (dogecoind-api-account-exists-p "not-found") => nil)
   (not-called dogecoind-api--get-request-result)
   (should (null (dogecoind-api-get-account-address "not-found" t)))))

(ert-deftest dogecoind-api/can-get-address-account ()
  (with-mock
   (mock (dogecoind-api--get-request "getaccount" `(,"abc123")) => (read-fixture "getaccount-abc123.json"))
   (should (equal "testaccount" (dogecoind-api-get-account "abc123")))))

(ert-deftest dogecoind-api/can-get-connection-count ()
  (with-mock
   (mock (dogecoind-api--get-request "getconnectioncount") => (read-fixture "getconnectioncount.json"))
   (should (= 4 (dogecoind-api-get-connection-count)))))

;; Account helper tests

(ert-deftest dogecoind-api-test/account-exists-returns-t-for-valid-accounts ()
  (with-mock
   (mock (dogecoind-api--get-request "listaccounts") => (read-fixture "listaccounts.json"))
   (should (dogecoind-api-account-exists-p "Account One"))
   (should (booleanp (dogecoind-api-account-exists-p "Account One")))))

(ert-deftest dogecoind-api-test/account-exists-returns-nil-for-invalid-accounts ()
  (with-mock
   (mock (dogecoind-api--get-request "listaccounts") => (read-fixture "listaccounts.json"))
   (should (null (dogecoind-api-account-exists-p "Invalid Account")))))


;; SERVER HELPERS

(ert-deftest dogecoind-api-test/can-test-if-server-running ()
  (with-mock
   (mock (url-http-file-exists-p "http://127.0.0.1:8334/") => t)
   (should (dogecoind-api-server-running-p)))
  (with-mock
   (mock (url-http-file-exists-p "http://127.0.0.1:8334/") => nil)
   (should (null (dogecoind-api-server-running-p)))))


;; INTERNAL TESTS

(ert-deftest dogecoind-api-test/can-encode-auth ()
  (should (string=
           "dXNlcm5hbWU6cGFzc3dvcmQ="
           (dogecoind-api--create-auth "username" "password"))))

(ert-deftest dogecoind-api-test/can-build-request-without-params ()
  (should (string=
           "{\"method\":\"getinfo\"}"
           (dogecoind-api--build-request "getinfo"))))

(ert-deftest dogecoind-api-test/can-build-request-with-params ()
  (let ((expected "{\"method\":\"getinfo\", \"params\":[\"value\"]}"))
    (should (string= expected (dogecoind-api--build-request "getinfo" '("value"))))
    (should (string= expected (dogecoind-api--build-request "getinfo" '(:value))))))

(ert-deftest dogecoind-api-test/can-build-endpoint-with-custom-settings ()
  (let ((dogecoind-api-address "192.168.1.5")
        (dogecoind-api-port 1234))
    (should (string= "http://192.168.1.5:1234/" (dogecoind-api--build-endpoint)))))

(ert-deftest dogecoind-api-test/can-build-endpoint-with-default-settings ()
  (should (string= "http://127.0.0.1:8334/" (dogecoind-api--build-endpoint))))

(ert-deftest dogecoind-api-test/get-request-returns-json ()
  (with-mock
   (mock (url-retrieve-synchronously "http://127.0.0.1:8334/") => (read-fixture-file-as-response "getinfo.json"))
   (let* ((response (dogecoind-api--get-request "getinfo")))
     (should (eq 1080000 (assoc-default 'version response))))))

(ert-deftest dogecoind-api-test/can-get-request-result-returns-single-value ()
  (with-mock
   (mock (dogecoind-api--get-request "getbalance") => (read-fixture "getbalance.json"))
   (should (= 123.123465 (dogecoind-api--get-request-result "getbalance")))))

