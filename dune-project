(lang dune 2.9)
(name ml2riscv)
(version 0.1)
(using menhir 2.1)

(generate_opam_files true)

(package
 (name ml2riscv)
 (synopsis "ML to RISCV Compiler")
 (depends
  (ocaml (>= 4.14.2))
  menhir
  ppx_deriving))
