CC = gcc
CFLAGS = -Wall -O2
SRC = main.c fm-net.c
LDFLAGS = -lm

default:
	$(CC) $(CFLAGS) $(SRC) -o test $(LDFLAGS)

.PHONY: clean
clean:
	rm -rfv test $(SRC:.c=.o)
