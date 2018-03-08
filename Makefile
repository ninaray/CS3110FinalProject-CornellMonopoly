test:
	ocamlbuild -use-ocamlfind state_test.byte && ./state_test.byte

check:
	bash checkenv.sh && bash checktypes.sh

play:
	ocamlbuild -use-ocamlfind main.byte && ./main.byte

zip:
	zip final_project_src.zip *.ml* *.json

zipcheck:
	bash checkzip.sh

clean:
	ocamlbuild -clean
	rm -f checktypes.ml
	rm -f final_project_src.zip
