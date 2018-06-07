
all: clean main
	erl

main: *.erl
	erlc $?

clean:
	rm -f *.beam *.dump