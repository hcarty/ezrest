(lang dune 2.7)

(name ezrest)

(using mdx 0.1)

(license Apache-2.0)

(maintainers "Hezekiah M. Carty <hez@0ok.org>")

(authors "Hezekiah M. Carty <hez@0ok.org>")

(source
 (github hcarty/ezrest))

(generate_opam_files true)

(package
 (name ezrest)
 (synopsis "Simple wrapper on top of the cohttp's client")
 (depends
  (ocaml
   (>= 4.08.0))
  (cohttp-lwt-unix
   (>= 2.5.4))
  (dune
   (>= 2.7.0))
  (fmt
   (>= 0.8.9))
  (lwt
   (>= 4.0.0))
  (lwt_ppx
   (>= 2.0.0))
  (mdx
   (and
    (>= 1.7.0)
    :with-test))
  (rresult
   (>= 0.6.0))
  (uri
   (>= 3.0.0))))
