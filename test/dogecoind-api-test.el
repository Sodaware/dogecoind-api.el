
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

;;
