#!/bin/bash
ocamlbuild -use-ocamlfind -pkg z3 -pkg unix -pkg compsci631 -pkg ppx_test verif.d.byte

ocamlbuild -use-ocamlfind -pkg z3 -pkg unix -pkg compsci631 -pkg ppx_test tests.d.byte

