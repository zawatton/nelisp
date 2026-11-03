(x-get-resource
 (x-get-resource "geometry" "Geometry")
 (x-get-resource "foreground" "Foreground")
 (x-get-resource "reverseVideo" "ReverseVideo")
 (x-get-resource "no-such-resource" "NoSuchResource")
 (x-get-resource "geometry" "Geometry" "app")
 (x-get-resource "geometry" "Geometry" "app" "App")
 (x-get-resource "geometry" "Geometry" nil nil)
 (x-get-resource nil "Class")
 (x-get-resource 7 "Class")
 (x-get-resource "name" nil)
 (condition-case err
     (apply #'x-get-resource nil)
   (wrong-number-of-arguments
    (list 'ERR (car err) '(x-get-resource 0))))
 (condition-case err
     (apply #'x-get-resource '("geometry"))
   (wrong-number-of-arguments
    (list 'ERR (car err) '(x-get-resource 1))))
 (condition-case err
     (apply #'x-get-resource '("geometry" "Geometry" nil nil nil))
   (wrong-number-of-arguments
    (list 'ERR (car err) '(x-get-resource 5))))
)
