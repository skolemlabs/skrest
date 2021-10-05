(lang dune 2.7)

(name skrest)

(using mdx 0.1)

(license Apache-2.0)

(maintainers "Zach Baylin <zach@skolem.tech>")

(authors "Hezekiah M. Carty <hez@0ok.org>" "Zach Baylin <zach@skolem.tech>")

(source
 (github skolemlabs/skrest))

(generate_opam_files true)

(package
 (name skrest)
 (synopsis "Simple wrapper on top of the cohttp's client")
 (depends
  (ocaml
   (>= 4.08.0))
  (cohttp-lwt-unix
   (>= 2.5.4))
  (dune
   (>= 2.7.0))
  (elastic-apm :build)
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
