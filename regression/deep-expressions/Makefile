TESTS=$(basename $(wildcard generated*.expr))

RC = ../../src/rc.opt

.PHONY: check $(TESTS)

check: $(TESTS)

$(TESTS): %: %.expr
<<<<<<< HEAD
# 	@RC_RUNTIME=../../runtime $(RC) $< && cat $@.input | ./$@ > $@.log && diff $@.log orig/$@.log
	@cat $@.input | $(RC) -i $< > $@.log && diff $@.log orig/$@.log
	@cat $@.input | $(RC) -s $< > $@.log && diff $@.log orig/$@.log
=======
	@RC_RUNTIME=../../runtime $(RC) $< && cat $@.input | ./$@ > $@.log && diff $@.log orig/$@.log
	cat $@.input | $(RC) -i $< > $@.log && diff $@.log orig/$@.log
	cat $@.input | $(RC) -s $< > $@.log && diff $@.log orig/$@.log
>>>>>>> upstream/hw7

clean:
	find . -maxdepth 1 -name "*.log" -delete
	find . -maxdepth 1 -type f -not -name '*.*' -not -name 'Makefile' -delete
<<<<<<< HEAD
	rm -f *.s *~
=======
	rm -f *.s *~
>>>>>>> upstream/hw7
