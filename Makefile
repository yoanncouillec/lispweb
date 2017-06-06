all: lispweb.out

%.out: %.scm
	bigloo $^ -o $@

clean:
	rm -rf *.out *~ *.o
