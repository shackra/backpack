;; -*- no-byte-compile: t; -*-

(require 'backpack-pouch)
(require 'backpack-email-utils)

(backpack-load-user-configuration)



;; shamelessly copied from Emacs Bedrock (leave this at the end of the file, always)
(setq gc-cons-threshold (or backpack--initial-gc-threshold 800000))
