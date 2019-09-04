CC = gcc
CFLAGS = -Wall -O3
DFLAGS = -Wall -O0
SRC = main.c fm-net.c
LDFLAGS = -lm

default:
	$(CC) $(CFLAGS) $(SRC) -o fm_net_default $(LDFLAGS)

legacy:
	$(CC) $(CFLAGS) $(SRC) -o fm_net_legacy $(LDFLAGS) -D_LEGACY_REWRITE_

debug:
	$(CC) $(DFLAGS) $(SRC) -o fm_net_debug $(LDFLAGS) -D_LEGACY_REWRITE_

.PHONY: clean
clean:
	rm -rfv fm_net_default fm_net_legacy fm_net_debug $(SRC:.c=.o)
