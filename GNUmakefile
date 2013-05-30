.PHONY: test

all:
	./rebar compile
test:
	./rebar eunit skip_deps=true
clean:
	./rebar clean skip_deps=true
clean_deps:
	./rebar clean
deps:
	./rebar get-deps
update_deps:
	./rebar update-deps
# eof
