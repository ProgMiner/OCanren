(lang dune 2.9)

(cram enable)

(generate_opam_files true)

(license LGPL-2.1-only)

(source
 (github PLTools/OCanren))

(name OCanren)

(package
 (version 0.3.0)
 (name OCanren)
 (synopsis "Implementation of miniKanren relational (logic) EDSL")
 (description
  "The family of miniKanren derivatives has many implementations. This\none is typed embedding to OCaml. The original implementation for\nthis is in scheme and can be found at https://github.com/michaelballantyne/faster-miniKanren .\n\nFeatures:\n1) disequality constraints\n2) relational standard library: lists, nats\n")
 (depends
  (ocaml
   (>= 4.14.2))
  dune-configurator
  ocamlfind
  camlp5
  logger-p5
  (GT
   (>= 0.5.2))
  (benchmark :with-test)
  (ocamlformat
   (and
    :with-test
    (>= 0.26.1)
    (< 0.27)))
  ppx_inline_test_nobase
  (mtime
   (>= 2.0.0)))
 (authors
  "Dmitrii Boulytchev https://github.com/dboulytchev"
  "Dmitrii Kosarev https://github.com/Kakadu"
  "Petr Lozov https://github.com/Lozov-Petr"
  "Evgenii Moiseenko https://github.com/eupp"
  "JetBrains Research https://research.jetbrains.org")
 (bug_reports "https://github.com/PLTools/OCanren/issues")
 (maintainers "Dmitrii Kosarev Kakadu@pm.me"))

(package
 (version 0.3.0)
 (name OCanren-ppx)
 (synopsis
  "Implementation of miniKanren relational (logic) EDSL: PPX extensions")
 (description
  "PPX rewriters for writing relational programs more conveniently.\n\n1) `ppx_fresh` -- a clone of original miniKanren syntax\n\n  fresh (x...) goal\n\nexpands into\n\n  Fresh.numeral (fun x ... -> goal)\n\n2) `ppx_distrib` for easier generation of type-safe wrappers: distrib functor, etc...\n\n  [%%distrib\n    type t = fully_abstract_typee\n    type ground = ...\n  ]\n\n3) `ppx_repr` for test. Expands `REPR(expr)` to `(string_of_expr expr, expr)`\n")
 (depends
  (ocaml
   (>= 4.14.2))
  dune-configurator
  (ppxlib
   (and
    (>= 0.26.0)
    (< 0.34.0)))
  ppx_inline_test_nobase
  ppx_expect_nobase)
 (authors
  "Dmitrii Kosarev https://github.com/Kakadu"
  "JetBrains Research https://research.jetbrains.org")
 (bug_reports "https://github.com/PLTools/OCanren/issues")
 (maintainers "Dmitrii Kosarev Kakadu@pm.me"))
