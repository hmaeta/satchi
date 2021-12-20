(defpackage :satchi.view-latest
  (:use :cl)
  (:export :run))
(in-package :satchi.view-latest)

(defun run (gateways &key update-loading-fn
                          update-viewing-fn)
  (funcall update-loading-fn)
  (let ((gw-state-set (satchi.gateway:make-state-set)))
    (dolist (gw gateways)
      (let ((client (satchi.gateway:gateway-client gw)))
        (let ((ntfs (satchi.client:fetch-notifications client)))
          (satchi.gateway:state-set-add-state gw-state-set gw ntfs))))
    (funcall update-viewing-fn gw-state-set)))
