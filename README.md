# empathy

Pretty printed errors built over clojure.spec

## Usage

Install locally from source. We're not up on Clojars yet.

There is one API function:

```clojure
(empathy.api/pretty-error! spec val)
```

`spec` is a clojure.spec specification. `val` is a value that presumably
does not conform to `spec`. `pretty-error!` will print out a human-friendly,
colored error message.

You can also extend predicate errors for custom error messages:

```clojure
(defmethod empathy.api/classify-error 'double?
  [pred x]
  {:type :type-error
   :expected (.getName java.lang.Double)
   :actual (.getName (.getClass x))
   :summary "a double"})
```

`:summary` inlines an English explanation into a possibly chained
series of summaries describing the problem.

## License

Copyright © 2016 Distributed Masonry

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
