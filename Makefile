R := R
RCMD := $(R) --slave

document:
	@$(RCMD) -e "devtools::document()"

test:
	@$(RCMD) -e "devtools::test()"

check:
	@$(RCMD) -e "devtools::check()"

revcheck:
	@$(RCMD) -e "if (!requireNamespace('revdepcheck')) install.packages('revdepcheck')"
	@$(RCMD) -e "revdepcheck::revdep_check()"
	@$(RCMD) -f "revdep/check.R"

crancheck: document
	@$(R) CMD build .
	@$(R) CMD check *.tar.gz

install:
	$(R) CMD INSTALL ./

clean:
	@rm -rf *.tar.gz *.Rcheck revdep
