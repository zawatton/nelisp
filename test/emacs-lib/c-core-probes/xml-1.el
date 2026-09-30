(libxml-available-p (libxml-available-p)
                    (with-temp-buffer
                      (insert (propertize "<root/>" 'xml-probe t))
                      (list (bufferp (current-buffer))
                            (equal (buffer-string) "<root/>")
                            (libxml-available-p))))
