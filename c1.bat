ocamlopt -c  types.mli
ocamlopt -c  types.ml
ocamllex lexer.mll
ocamlyacc parser.mly
ocamlopt -c  parser.mli
ocamlopt -c lexer.ml
ocamlopt -c  parser.ml
ocamlopt -c  render.ml
ocamlopt -o render.exe graphics.cmxa types.cmx lexer.cmx parser.cmx render.cmx
del lexer.ml
del lexer.cmi
del lexer.cmx
del lexer.o
del render.cmi
del render.cmx
del render.o
del types.cmi
del types.cmx
del types.o
del parser.cmi
del parser.cmx
del parser.o
del parser.ml
del parser.mli
