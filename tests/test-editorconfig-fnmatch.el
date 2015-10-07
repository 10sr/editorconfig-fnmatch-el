(require 'editorconfig-fnmatch)


(ert-deftest test-editorconfig-fnmatch-p ()
  (let ((cases-t
         '(("a.js" "a.js")
           ("/dir/a.js" "/dir/a.js")

           ("a.js" "*.js")
           ("a.js" "**.js")
           ("/dir/a.js" "/dir/*.js")
           ("/dir/a.js" "/dir/*")

           ("/dir/sub/a.js" "**.js")
           ("/dir/sub/a.py" "/dir/**.py")

           ("a.js" "?.js")
           ("abc.js" "a?c.js")
           ("/dir/a.js" "/dir/?.js")

           ("a.js" "[abc].js")
           ("b.js" "[abc].js")
           ("ab.js" "[abc]b.js")
           ("/dir/a.js" "/dir/[abc].js")
           ("ab[e/]cd.i" "ab[e/]cd.i")
           ("a.js" "[a-c].js")
           ("1.js" "[1-3].js")

           ("d.js" "[^abc].js")
           ("db.js" "[^abc]b.js")
           ("/dir/d.js" "/dir/[^abc].js")

           ("a.js" "a.{py,js}")
           ("a.py" "a.{py,js}")
           ("/dir/a.py" "/dir/a.{py,js}")
           ("/dir/a.py" "/dir/a.{py,js}")
           ("a.js" "*.{py,js}")
           ("a.py" "*.{py,js}")
           ("/dir/a.js" "/dir/*.{py,js}")
           ("/dir/a.py" "/dir/*.{py,js}")
           ("/dir/sub/a.py" "**.{py,js}")
           ("/dir/sub/a.py" "/dir/**.{py,js}")
           ("{single}.b" "{single}.b")
           ("{.f" "{.f")
           ("}.f" "}.f")

           ("1.js" "{0..3}.js")
           ("1.js" "{0..+3}.js")
           ("-1.js" "{-3..3}.js")
           ("-1.js" "{-3..3}.js")
           ))
        (cases-nil
         '(("a.js" "b.js")

           ("a.js" "*.py")
           ("/dir/a.js" "/dir/*.py")
           ("/dir/sub/a.js" "/dir/*.js")

           ("/dir/a.js" "/sub/**.js")
           ("/dir/sub/a.js" "/sub/**.js")

           ("ab.js" "?.js")
           ("ab.js" "?a.js")
           ("/dir/ab.js" "/dir/?.js")
           ("/dir/ab.js" "/dir/?a.js")

           ("d.js" "[abc].js")
           ("db.js" "[abc]b.js")
           ("/dir/d.js" "/dir/[abc].js")

           ("a.js" "[^abc].js")
           ("ab.js" "[^abc]b.js")
           ("/dir/a.js" "/dir/[^abc].js")

           ("a.el" "a.{py,js}")
           ("a.el" "*.{py,js}")
           ("/dir/a.el" "/dir/a.{py,js}")
           ("/dir/a.el" "/dir/*.{py,js}")
           ("/dir/a.el" "**.{py,js}")

           ("1.js" "{3..6}.js")
           ("-1.js" "{0..3}.js")
           ("-1.js" "{3..-3}.js")
           )))
    (dolist (args cases-t)
      (message "-> t: %S"
               `(editorconfig-fnmatch-p ,@args))
      (should (apply 'editorconfig-fnmatch-p
                     args)))
    (dolist (args cases-nil)
      (message "-> nil: %S"
               `(editorconfig-fnmatch-p ,@args))
      (should-not (apply 'editorconfig-fnmatch-p
                         args))))
  )
