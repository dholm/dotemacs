;;; mqtt.el --- Initializes MQTT mode -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package mqtt-mode
  :if (executable-find "mosquitto_sub"))


(provide 'modes/mqtt)
;;; mqtt.el ends here
