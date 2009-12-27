all:
	(cd src;$(MAKE))

clean:
	(cd src;$(MAKE) clean)

test:
	(erl -pa ebin/ -noshell -run restfulierl_test test -run init stop)
