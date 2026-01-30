(defn maybe-dump
  [& args]
  (assertf (even? (length args))
           "expected even number or args: %n" args)
  #
  (when (os/getenv "VERBOSE")
    (each [name value] (partition 2 args)
      (cond
        (dictionary? value)
        (do
          (pp [name])
          (eachp [k v] value
            (printf "%n: %n" k v)))
        #
        (indexed? value)
        (do
          (pp [name])
          (each v value
            (pp v)))
        #
        (pp [name value])))
    (print)))

########################################################################

(defn get-os-bits
  []
  (def os (or (dyn :os-override) (os/which)))
  (def bs-land (or (= :windows os) (= :mingw os)))
  (def sep (if bs-land `\` "/"))
  #
  {:os os
   :bs-land bs-land
   :sep sep})

# XXX: more edge cases to identify?
#      * consecutive separator handling matches
#        posix more than python / spork/path
(defn file-dir-path
  [path]
  (when (empty? path)
    (break path))
  #
  (def {:sep sep :bs-land bs-land} (get-os-bits))
  (if bs-land
    (when (peg/match ~(sequence :a `:\` -1) path)
      (break path))
    (when (= path "/")
      (break path)))
  #
  (def rev-path (string/reverse path))
  (def last-sep-idx (string/find sep rev-path))
  (def candidate
    (if-not last-sep-idx
      path
      (string/slice path 0
                    (dec (- (length path) last-sep-idx)))))
  #
  (if (not (string/find sep candidate))
    (string candidate sep)
    candidate))

(comment

  # an actual case we want to handle
  (file-dir-path "/etc/motd")
  # =>
  "/etc"

  # another actual case we want to handle
  (let [old (dyn :os-override)]
    (setdyn :os-override :windows)
    (defer (setdyn :os-override old)
      (file-dir-path `C:\Windows\System32\taskmgr.exe`)))
  # =>
  `C:\Windows\System32`

  # everything below here are edge cases we don't care about...

  (file-dir-path "")
  # =>
  ""

  (file-dir-path "/")
  # =>
  "/"

  # differs from python and spork
  (file-dir-path "//")
  # =>
  "/"

  (file-dir-path "/etc")
  # =>
  "/"

  # sames as python's os.path.dirname, different from unix dirname
  (file-dir-path "/etc/")
  # =>
  "/etc"

  (let [old (dyn :os-override)]
    (setdyn :os-override :windows)
    (defer (setdyn :os-override old)
      (file-dir-path `C:\Windows`)))
  # =>
  `C:\`

  (let [old (dyn :os-override)]
    (setdyn :os-override :windows)
    (defer (setdyn :os-override old)
      (file-dir-path `A:\`)))
  # =>
  `A:\`

  (let [old (dyn :os-override)]
    (setdyn :os-override :windows)
    (defer (setdyn :os-override old)
      (file-dir-path `Z:\`)))
  # =>
  `Z:\`

  )

(defn split-path
  [path]
  (def fd-path (file-dir-path path))
  (when (empty? path)
    (break @["" ""]))
  #
  (def {:sep sep :bs-land bs-land} (get-os-bits))
  (def sep-idxs (string/find-all sep path))
  (when (= 0 (length sep-idxs))
    (break @["" path]))
  #
  (when (= 1 (length sep-idxs))
    (break @[fd-path
             (string/slice path (inc (last sep-idxs)))]))
  #
  (def idx (inc (string/find fd-path path)))
  #
  @[fd-path
    (string/slice path (+ idx (length fd-path)) -1)])

(comment

  # one case we care about
  (split-path "/etc/motd")
  # =>
  @["/etc" "motd"]

  # another case we care about
  (split-path "/hello.janet")
  # =>
  @["/" "hello.janet"]

  (split-path "./blocks/blank.html")
  # =>
  @["./blocks" "blank.html"]

  (split-path "cli.janet")
  # =>
  @["" "cli.janet"]

  )

(defn abspath?
  [path &opt bs-land]
  (def os (os/which))
  (default bs-land (or (= :windows os) (= :mingw os)))
  (if bs-land
    # https://stackoverflow.com/a/23968430
    # https://learn.microsoft.com/en-us/dotnet/standard/io/file-path-formats
    (truthy? (peg/match ~(sequence :a `:\`) path))
    (string/has-prefix? "/" path)))

(defn diff-path
  [left right]
  (var i 0)
  (var done false)
  (def [shorter longer]
    (if (<= (length left) (length right))
      [left right]
      [right left]))
  (for j 0 (length shorter)
    (when (not= (get left j) (get right j))
      (set done true)
      (set i j)
      (break)))
  (cond
    (not done)
    [shorter (string/slice longer (length shorter))]
    #
    (= 0 i)
    ["" nil]
    #
    [(string/slice shorter 0 i) ""]))

(comment

  (diff-path "/usr/include"
             "/usr/include/fstab.h")
  # =>
  ["/usr/include" "/fstab.h"]

  (diff-path "/tmp" "hello")
  # =>
  ["" nil]

  (diff-path "/etc/motd" "/etc/issue")
  # =>
  ["/etc/" ""]

  )

(defn touch
  [path]
  (with [f (file/open path :w+)]
    true))

(defn make-shebang
  []
  "#! /usr/bin/env janet")

(comment

  (make-shebang)
  # =>
  "#! /usr/bin/env janet"

  )

(defn dt-stamp
  [&opt time]
  (def dt (os/date time))
  (def year (get dt :year))
  (def month (inc (get dt :month)))
  (def day (inc (get dt :month-day)))
  (def hours (get dt :hours))
  (def mins (get dt :minutes))
  (def secs (get dt :seconds))
  (string/format "%d-%02d-%02d_%02d-%02d-%02d"
                 year month day hours mins secs))

(comment

  (dt-stamp 0)
  # =>
  "1970-01-01_00-00-00"

  (dt-stamp 1234567890)
  # =>
  "2009-02-13_23-31-30"

  )

