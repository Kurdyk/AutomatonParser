all:
	+$(MAKE) -C src
	+$(MAKE) -C p2

clean:
	+$(MAKE) clean -C src
	+$(MAKE) clean -C p2
	rm parser
	rm parser2