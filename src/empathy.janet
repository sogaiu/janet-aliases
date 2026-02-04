(defn path-join
  [& parts]
  (def sep
    (if-let [sep (dyn :path-fs-sep)]
      sep
      (if (let [osw (os/which)]
            (or (= :windows osw) (= :mingw osw)))
        `\`
        "/")))
  #
  (string/join parts sep))

(comment

  (let [sep (dyn :path-fs-sep)]
    (defer (setdyn :path-fs-sep sep)
      (setdyn :path-fs-sep "/")
      (path-join "/tmp" "test.txt")))
  # =>
  "/tmp/test.txt"

  (let [sep (dyn :path-fs-sep)]
    (defer (setdyn :path-fs-sep sep)
      (setdyn :path-fs-sep "/")
      (path-join "/tmp" "foo" "test.txt")))
  # =>
  "/tmp/foo/test.txt"

  (let [sep (dyn :path-fs-sep)]
    (defer (setdyn :path-fs-sep sep)
      (setdyn :path-fs-sep `\`)
      (path-join "C:" "windows" "system32")))
  # =>
  `C:\windows\system32`

  )

(defn make-itemizer
  [& paths]
  (def todo-paths (reverse paths)) # pop used to process from end
  (def seen? @{})
  #
  (coro
    (while (def p (array/pop todo-paths))
      (def [ok? value] (protect (os/realpath p)))
      (when (and ok? (not (get seen? value)))
        (put seen? value true)
        (yield p)
        (when (= :directory (os/stat p :mode))
          (each subp (reverse (os/dir p))
            (array/push todo-paths (path-join p subp))))))))

(comment

  (def it (make-itemizer (dyn :syspath) "/etc/fonts"))

  (each p it (pp p))

  )

(comment

  (do
    (var res nil)
    (each p (make-itemizer "bundle")
      (when (string/has-suffix? "info.jdn" p)
        (set res true)
        (break)))
    res)
  # =>
  true

  )

(defn itemize
  [& paths]
  (def it (make-itemizer ;paths))
  #
  (seq [p :in it] p))

(comment

  (itemize (path-join (os/getenv "HOME") ".config"))

  )

(comment

  (do
    (var res nil)
    (each p (itemize ".")
      (when (string/has-suffix? "empathy.janet" p)
        (set res true)
        (break)))
    res)
  # =>
  true

  )

