(defn parse-args
  [args]
  (def the-args (array ;args))
  #
  (def head (get the-args 0))
  #
  (def default-opts @{:rest the-args})
  #
  (def conf
    (let [conf-file ".jell.jdn"]
      (when (= :file (os/stat conf-file :mode))
        (eachp [k v] (parse (slurp conf-file))
          (setdyn k v))
        true)))
  #
  (when (or (and (not conf) (not head))
            (= head "-h") (= head "--help"))
    (break (merge {:show-help true} default-opts)))
  #
  (when (or (and (not conf) (not head))
            (= head "-v") (= head "--version"))
    (break (merge {:show-version true} default-opts)))
  #
  (def opts
    (if head
      (if-not (and (string/has-prefix? "{" head)
                   (string/has-suffix? "}" head))
        default-opts
        (let [parsed
              (try (parse (string "@" head))
                ([e] (eprint e)
                     (errorf "failed to parse options: %n" head)))]
          (assertf (and parsed (table? parsed))
                   "expected table but found: %s" (type parsed))
          (array/remove the-args 0)
          parsed))
      @{}))
  # XXX: should opts have priority over the-args?
  (when (nil? (get opts :start-path))
    (put opts :start-path (dyn :start-path "src/main.janet")))
  #
  (when (nil? (get opts :obj-path))
    (put opts :obj-path (dyn :obj-path "obj")))
  #
  (when (nil? (get opts :out-path))
    (put opts :out-path (dyn :out-path "j.out")))
  #
  (when (nil? (get opts :flycheck))
    (put opts :flycheck (dyn :flycheck true)))
  #
  (when (nil? (get opts :add-shebang))
    (put opts :add-shebang (dyn :add-shebang true)))
  #
  (when-let [start-path (get the-args 0)]
    (put opts :start-path start-path)
    (array/remove the-args 0))
  #
  (when-let [out-path (get the-args 0)]
    (put opts :out-path out-path)
    (array/remove the-args 0))
  #
  (when-let [obj-path (get the-args 0)]
    (put opts :obj-path obj-path)
    (array/remove the-args 0))
  #
  (merge opts default-opts))

