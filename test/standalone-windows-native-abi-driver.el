;;; standalone-windows-native-abi-driver.el --- Win64 bridge acceptance -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(load "test/standalone-native-funcall-v2-driver.el" nil t)
(load "test/support/windows-native-sentinel.el" nil t)
(windows-native-sentinel-run)
