.PHONY: deps test

ERL=`which erl`
REBAR=`which rebar`
HOSTNAME=`hostname`

run: compile
	exec ${ERL} -pa ${PWD}/ebin ${PWD}/deps/*/ebin -name grass@${HOSTNAME} -config ${PWD}/etc/app.config -s grass

compile:
	${REBAR} compile skip_deps=true

build: deps
	${REBAR} compile

deps:
	test -d logs || mkdir logs
	test -d deps || ${REBAR} get-deps

clean:
	${REBAR} clean

distclean: clean
	${REBAR} delete-deps
	rm -rf ${PWD}/logs
	rm -rf ${PWD}/deps
	rm -rf ${PWD}/*.db

test:
	${REBAR} compile eunit

docs:
	${REBAR} doc

help:
	@echo "Targets: run, compile, build, deps, clean, distclean, test and docs"