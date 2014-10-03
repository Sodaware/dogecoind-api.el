
;; API method tests

(ert-deftest dogecoind-api-test/can-get-info ()
  (with-mock
   (mock (dogecoind-api--get-request "getinfo") => (read-fixture "getinfo.json"))
   
   (let ((response (dogecoind-api-get-info)))
     (should (eq 1080000 (assoc-default 'version response)))
     (should (eq 70003 (assoc-default 'protocolversion response)))
     (should (eq :json-false (assoc-default 'testnet response))))))


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
  (let ((expected "{\"method\":\"getinfo\", \"arg\":\"value\"}"))
    (should (string= expected (dogecoind-api--build-request "getinfo" `((:arg . ,"value")))))
    (should (string= expected (dogecoind-api--build-request "getinfo" `((:arg . ,:value)))))))

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
