REBAR ?= rebar3
ROOT_TEST=_build/test/lib

ifndef suite
	SUITE_EXEC=
else
	SUITE_EXEC=-suite $(suite)_SUITE
endif

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

bench:
	$(REBAR) as bench compile
	erl -pa _build/bench/lib/*/ebin -noshell -eval "bench_pool:bench(100000, 4000)." -eval "init:stop()."

ct:
	mkdir -p log
	$(REBAR) ct --compile_only
	ct_run  -no_auto_compile \
			-cover test/cover.spec \
			-dir $(ROOT_TEST)/erlpool/test $(SUITE_EXEC) \
			-pa $(ROOT_TEST)/*/ebin \
			-logdir log \
			-erl_args -config test/sys.config
