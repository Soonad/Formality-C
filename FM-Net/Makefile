CC = gcc
CFLAGS = -O2
SRC = fm-net.c
LDFLAGS = -lm

default:
	$(CC) $(CFLAGS) $(SRC) -o test $(LDFLAGS)

.PHONY: clean
clean:
	rm -rfv test
