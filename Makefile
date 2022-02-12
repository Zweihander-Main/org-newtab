SOURCE := zweigtd-newtab.el
TEST   := ./test/zweigtd-newtab-test.el

.PHONY: test

test:
	cask exec buttercup -L .

testWatch:
	while true; do \
		make test; \
		inotifywait -qre close_write $(SOURCE) $(TEST); \
	done

lint:
	cask exec elsa $(SOURCE)
