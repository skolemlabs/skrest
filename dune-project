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
 (synopsis "Simple library for performing HTTP requests")
 (depends
  (ocaml
   (>= 4.08.0))
  (dune
   (>= 2.7.0))
  (skapm :build)
  (fmt
   (>= 0.8.9))
  (lwt
   (>= 4.0.0))
  (lwt_ppx
   (>= 2.0.0))
  (cohttp
   (>= 2.5.4))
  (mdx
   (and
    (>= 1.7.0)
    :with-test))
  (rresult
   (>= 0.6.0))
  (uri
   (>= 3.0.0))))

(package
 (name skrest_unix)
 (synopsis "Simple wrapper on top of the Cohttp client")
 (depends
  (ocaml
   (>= 4.08.0))
  (cohttp-lwt-unix
   (>= 2.5.4))
  (skrest :build)))

(package
 (name skrest_js)
 (synopsis "Simple wrapper on top of the Brr_io.Fetch client")
 (depends
  (ocaml
   (>= 4.08.0))
  (cohttp-lwt-jsoo
   (>= 2.5.4))
  (skrest :build)))
