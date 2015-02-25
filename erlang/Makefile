CC = gcc
CFLAGS = -Wall -g -march=native -O3 -fno-common -std=gnu99 -fvisibility=hidden

ERLC = erlc

all: kmeans.so main.beam kmeans.beam

%.so: %.c
	$(CC) $(CFLAGS) -fpic -shared -o $@ $<

%.beam: %.erl
	$(ERLC) $<

.PHONY: clean

clean:
	rm *.o *.beam
