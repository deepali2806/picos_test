
all:
	ocamlfind ocamlopt -o test.exe -thread -linkpkg -package unix,picos,saturn -g \
	fun_queue.mli fun_queue.ml MVar.mli MVar.ml fifo.mli fifo.ml cancel.ml

clean:
	rm -f *~ *.cm* *.o *.out *.exe