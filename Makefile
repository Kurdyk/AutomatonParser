all:
	+$(MAKE) -C src

p2:
	+$(MAKE) -C p2

clean:
	+$(MAKE) clean -C src
	+$(MAKE) clean -C p2
	rm parser