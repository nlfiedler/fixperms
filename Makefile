.PHONY: all clean compile

all: compile

compile:
	rebar compile escriptize

clean:
	rebar clean
