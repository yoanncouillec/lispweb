all: dyn_eval_noform.out

%.out: %.scm
	bigloo $^ -o $@

clean:
	rm -rf *.out *~ *.o
