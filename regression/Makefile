TESTS=test001 test002 test003 test004 test005 test006 test007 test008

RC=../src/rc.opt

.PHONY: check $(TESTS) 

check: $(TESTS) 

$(TESTS): %: %.expr
<<<<<<< HEAD
	cat $@.input | $(RC) -i $< > $@.log && diff --strip-trailing-cr $@.log orig/$@.log
	cat $@.input | $(RC) -s $< > $@.log && diff --strip-trailing-cr $@.log orig/$@.log
=======
	$(RC) $< && cat $@.input | ./$@ > $@.log && diff $@.log orig/$@.log
	cat $@.input | $(RC) -i $< > $@.log && diff $@.log orig/$@.log
	cat $@.input | $(RC) -s $< > $@.log && diff $@.log orig/$@.log
>>>>>>> a02d2f10f0fb937b9c20ce1bc554e66439d1508e

clean:
	rm -f test*.log *.s *~ $(TESTS)
