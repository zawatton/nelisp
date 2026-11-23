S1 insert before point shifts restored point 9
S2 delete spanning point 3
S3 delete before point 5
S4 buffer switch restored (t 2)
S5 throw restores 3
S6 error restores 3
S7 error restores buffer t
S8 saved buffer killed inside body (body t nil)
S9 insertion at saved point stays before (type nil) (4 "abcXYdef")
S10 insert at point from other position 4
S11 body value and empty body (3 nil)
S12 nested (6)
S13 no marker left behind (2 t 2)
S14 point-marker type/pos/buffer (t 4 t nil)
S15 point-marker advances not on insert at it 4
S16 point-marker follows earlier insert 6
S17 point-min/max-marker (1 7 nil nil t)
S18 copy-marker integer (3 nil t)
S19 copy-marker type t advances (5 t)
S20 copy-marker of marker (5 t nil)
S21 copy-marker nil / nowhere (nil nil nil)
S22 make-marker (t nil nil nil)
