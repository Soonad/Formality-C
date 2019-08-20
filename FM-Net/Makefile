CC = gcc
CFLAGS = -Wall -O3
DFLAGS = -Wall -O0
SRC = main.c fm-net.c
LDFLAGS = -lm

default:
	$(CC) $(CFLAGS) $(SRC) -o test $(LDFLAGS)

inline:
	$(CC) $(CFLAGS) $(SRC) -o test_inlined $(LDFLAGS) -D_INLINED_REWRITE_

debug:
	$(CC) $(DFLAGS) $(SRC) -o debug_test $(LDFLAGS) -DDEBUG

.PHONY: clean
clean:
	rm -rfv test debug_test  $(SRC:.c=.o)
