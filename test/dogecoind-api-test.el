
;; INTERNAL TESTS

(ert-deftest dogecoind-api-test/can-encode-auth ()
  (should (string=
           "dXNlcm5hbWU6cGFzc3dvcmQ="
           (dogecoind-api--create-auth "username" "password"))))

(ert-deftest dogecoind-api-test/can-build-request-without-params ()
  (should (string=
           "{\"action\":\"getinfo\"}"
           (dogecoind-api--build-request "getinfo"))))

(ert-deftest dogecoind-api-test/can-build-request-with-params ()
  (let ((expected "{\"action\":\"getinfo\", \"arg\":\"value\"}"))
    (should (string= expected (dogecoind-api--build-request "getinfo" `((:arg . ,"value")))))
    (should (string= expected (dogecoind-api--build-request "getinfo" `((:arg . ,:value)))))))

(ert-deftest dogecoind-api-test/can-build-endpoint-with-custom-settings ()
  (let ((dogecoind-api-address "192.168.1.5")
        (dogecoind-api-port 1234))
    (should (string= "http://192.168.1.5:1234/" (dogecoind-api--build-endpoint)))))

(ert-deftest dogecoind-api-test/can-build-endpoint-with-default-settings ()
  (should (string= "http://127.0.0.1:8334/" (dogecoind-api--build-endpoint))))



;;
