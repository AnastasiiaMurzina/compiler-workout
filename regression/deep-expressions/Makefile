TESTS=$(basename $(wildcard generated*.expr))

RC = ../../src/rc.opt

.PHONY: check $(TESTS)

check: $(TESTS)

$(TESTS): %: %.expr
<<<<<<< HEAD
	cat $@.input | $(RC) -i $< > $@.log && diff --strip-trailing-cr $@.log orig/$@.log
	cat $@.input | $(RC) -s $< > $@.log && diff --strip-trailing-cr $@.log orig/$@.log
=======
	RC_RUNTIME=../../runtime $(RC) $< && cat $@.input | ./$@ > $@.log && diff $@.log orig/$@.log
	cat $@.input | $(RC) -i $< > $@.log && diff $@.log orig/$@.log
	cat $@.input | $(RC) -s $< > $@.log && diff $@.log orig/$@.log
>>>>>>> a02d2f10f0fb937b9c20ce1bc554e66439d1508e

clean:
	rm -f *.log *.s *~ $(TESTS)
