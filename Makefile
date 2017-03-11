REBAR=rebar

compile:
	${REBAR} compile

clean:
	${REBAR} clean

bench:
	erl -pa ebin -pa deps/*/ebin -noshell -eval "bench_pool:bench(100000, 4000)." -eval "init:stop()."

ct:
	mkdir -p log
	ct_run -suite integrity_test_SUITE -pa ebin -pa deps/*/ebin -include include -logdir log

