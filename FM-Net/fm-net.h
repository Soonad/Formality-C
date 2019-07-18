#ifndef __FM_NET_H__
#define __FM_NET_H__

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <math.h>

#if !(defined(u32) || defined(u64))
#define u32 uint32_t
#define u64 uint64_t
#endif

typedef struct Node {
  u32 port[3];
  u32 info;
} Node;

typedef struct Net {
  u32 *nodes;
  u32 nodes_len;
  u32 *redex;
  u32 redex_len;
  u32 *freed;
  u32 freed_len;
} Net;

typedef struct Stats {
  u32 rewrites;
  u32 loops;
} Stats;

u64 Pointer(u32 addr, u32 port);
u32 addr_of(u64 ptrn);
u32 slot_of(u64 ptrn);
u64 Numeric(u32 numb);
u32 numb_of(u64 ptrn);
u32 type_of(u64 ptrn);

u32 alloc_node(Net *net, u32 type, u32 kind);
void free_node(Net *net, u32 addr);

u32 is_free(Net *net, u32 addr);
u32 is_numeric(Net *net, u32 addr, u32 slot);

u32 is_redex(Net *net, u32 addr);
void find_redexes(Net *net);

void set_port(Net *net, u32 addr, u32 slot, u64 ptrn);
u64 get_port(Net* net, u32 addr, u32 slot);

void set_type(Net* net, u32 addr, u32 type);
u32 get_type(Net* net, u32 addr);

u32 get_kind(Net* net, u32 addr);

// Given a pointer to a port, returns a pointer to the opposing port
u64 enter_port(Net* net, u64 ptrn);

// Connects two ports
void link_ports(Net *net, u64 a_ptrn, u64 b_ptrn);
// Disconnects a port, causing both sides to point to themselves
void unlink_port(Net* net, u64 a_ptrn);

// Rewrites an active pair
void rewrite(Net* net, u32 a_addr);

// Rewrites active pairs until none is left, reducing the graph to normal form
// This could be performed in parallel. Unreachable data is freed automatically.
Stats reduce(Net *net);

void print_pointer(u64 ptrn);
void print_net(Net* net);

#endif
