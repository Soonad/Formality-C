#include "fm-net.h"

const u32 PTR = 0;
const u32 NUM = 1;

const u32 NOD = 0;
const u32 OP1 = 1;
const u32 OP2 = 2;
const u32 ITE = 3;

inline u64 Pointer(u32 addr, u32 port) {
  return (u64)((addr << 2) + (port & 3));
}

inline u32 addr_of(u64 ptrn) {
  return (u32)(ptrn >> 2);
}

inline u32 slot_of(u64 ptrn) {
  return (u32)(ptrn & 3);
}

inline u64 Numeric(u32 numb) {
  return ((u64)numb | (u64)0x100000000);
}

inline u32 numb_of(u64 ptrn) {
  return (u32)ptrn;
}

inline u32 type_of(u64 ptrn) {
  return (ptrn >= (u64)0x100000000 ? NUM : PTR);
}

inline u32 alloc_node(Net *net, u32 type, u32 kind) {
  u32 addr;
  if (net->freed_len > 0) {
    addr = net->freed[--net->freed_len];
  } else {
    addr = net->nodes_len / 4;
    net->nodes_len += 4;
  }
  net->nodes[addr * 4 + 0] = addr * 4 + 0;
  net->nodes[addr * 4 + 1] = addr * 4 + 1;
  net->nodes[addr * 4 + 2] = addr * 4 + 2;
  net->nodes[addr * 4 + 3] = (kind << 6) + ((type & 0x7) << 3);
  return addr;
}

inline void free_node(Net *net, u32 addr) {
  net->nodes[addr * 4 + 0] = addr * 4 + 0;
  net->nodes[addr * 4 + 1] = addr * 4 + 1;
  net->nodes[addr * 4 + 2] = addr * 4 + 2;
  net->nodes[addr * 4 + 3] = 0;
  net->freed[net->freed_len++] = addr;
}

inline u32 is_free(Net *net, u32 addr) {
  return net->nodes[addr * 4 + 0] == addr * 4 + 0
      && net->nodes[addr * 4 + 1] == addr * 4 + 1
      && net->nodes[addr * 4 + 2] == addr * 4 + 2
      && net->nodes[addr * 4 + 3] == 0;
}

inline u32 is_numeric(Net *net, u32 addr, u32 slot) {
  return (net->nodes[addr * 4 + 3] >> slot) & 1;
}

inline void set_port(Net *net, u32 addr, u32 slot, u64 ptrn) {
  if (type_of(ptrn) == NUM) {
      net->nodes[addr * 4 + slot] = numb_of(ptrn);
      net->nodes[addr * 4 + 3]    = net->nodes[addr * 4 + 3] | (1 << slot);
    } else {
      net->nodes[addr * 4 + slot] = ptrn;
      net->nodes[addr * 4 + 3]    = net->nodes[addr * 4 + 3] & ~(1 << slot);
  }
}

inline u64 get_port(Net* net, u32 addr, u32 slot) {
  return (u64)net->nodes[addr * 4 + slot] + (is_numeric(net, addr, slot) ? (u64)0x100000000 : (u64)0);
}

inline void set_type(Net* net, u32 addr, u32 type) {
  net->nodes[addr * 4 + 3] = (net->nodes[addr * 4 + 3] & ~0b111000) | (type << 3);
}

inline u32 get_type(Net* net, u32 addr) {
  return (net->nodes[addr * 4 + 3] >> 3) & 0x7;
}

inline u32 get_kind(Net* net, u32 addr) {
  return net->nodes[addr * 4 + 3] >> 6;
}

// Given a pointer to a port, returns a pointer to the opposing port
u64 enter_port(Net* net, u64 ptrn) {
  if (type_of(ptrn) == NUM) {
    printf("[ERROR]\nCan't enter a numeric pointer.");
    return 0;
  } else {
    return get_port(net, addr_of(ptrn), slot_of(ptrn));
  }
}

u32 is_redex(Net *net, u32 addr) {
  u64 a_ptrn = Pointer(addr, 0);
  u64 b_ptrn = enter_port(net, a_ptrn);
  return type_of(b_ptrn) == NUM || (slot_of(b_ptrn) == 0 && !is_free(net, addr));
}

// Connects two ports
void link_ports(Net *net, u64 a_ptrn, u64 b_ptrn) {
  u32 a_numb = type_of(a_ptrn) == NUM;
  u32 b_numb = type_of(b_ptrn) == NUM;

  // Point ports to each-other
  if (!a_numb) set_port(net, addr_of(a_ptrn), slot_of(a_ptrn), b_ptrn);
  if (!b_numb) set_port(net, addr_of(b_ptrn), slot_of(b_ptrn), a_ptrn);

  // If both are main ports, add this to the list of active pairs
  if (!(a_numb && b_numb) && (a_numb || slot_of(a_ptrn) == 0) && (b_numb || slot_of(b_ptrn) == 0)) {
    net->redex[net->redex_len++] = a_numb ? addr_of(b_ptrn) : addr_of(a_ptrn);
  }
}

// Disconnects a port, causing both sides to point to themselves
void unlink_port(Net* net, u64 a_ptrn) {
  if (type_of(a_ptrn) == PTR) {
    u64 b_ptrn = enter_port(net, a_ptrn);
    if (type_of(b_ptrn) == PTR && enter_port(net, b_ptrn) == a_ptrn) {
      set_port(net, addr_of(a_ptrn), slot_of(a_ptrn), a_ptrn);
      set_port(net, addr_of(b_ptrn), slot_of(b_ptrn), b_ptrn);
    }
  }
}

inline void swap(u32* value1, u32* value2) {
  if (value1 != value2) {
    u32 temp = *value1;
    *value1 = *value2;
    *value2 = temp;
  }
}

// Rewrites an active pair
void rewrite(Net* net, u32 a_addr) {
  u64 b_ptrn = get_port(net, a_addr, 0);


  if (type_of(b_ptrn) == PTR) {
    u32 b_addr = addr_of(b_ptrn);
    u32 a_type = get_type(net, a_addr);
    u32 b_type = get_type(net, b_addr);

    // Permutations
    if (b_type == NOD && (a_type == OP1 || a_type == OP2 || a_type == ITE)) {
      swap(&a_addr, &b_addr);
      swap(&a_type, &b_type);
      b_ptrn = get_port(net, a_addr, 0);
    }
  }

  if (type_of(b_ptrn) == NUM) {
    u32 a_type = get_type(net, a_addr);
    u32 a_kind = get_kind(net, a_addr);

    // UnaryOperation
    if (a_type == OP1) {
      u64 dst = enter_port(net, Pointer(a_addr, 2));
      u32 fst = numb_of(b_ptrn);
      u32 snd = numb_of(enter_port(net, Pointer(a_addr, 1)));
      u64 res;
      switch (a_kind) {
        case  0: res = Numeric(fst + snd); break;
        case  1: res = Numeric(fst - snd); break;
        case  2: res = Numeric(fst * snd); break;
        case  3: res = Numeric(fst / snd); break;
        case  4: res = Numeric(fst % snd); break;
        case  5: res = Numeric((u32)(pow((float)fst, (float)snd))); break;
        case  6: res = Numeric((u32)(pow((float)fst, ((float)snd / pow(2.0,32.0))))); break;
        case  7: res = Numeric(fst & snd); break;
        case  8: res = Numeric(fst | snd); break;
        case  9: res = Numeric(fst ^ snd); break;
        case 10: res = Numeric(~snd); break;
        case 11: res = Numeric(fst >> snd); break;
        case 12: res = Numeric(fst << snd); break;
        case 13: res = Numeric(fst > snd ? 1 : 0); break;
        case 14: res = Numeric(fst < snd ? 1 : 0); break;
        case 15: res = Numeric(fst == snd ? 1 : 0); break;
        default: res = 0; printf("[ERROR]\nInvalid interaction."); break;
      }
      link_ports(net, dst, res);
      unlink_port(net, Pointer(a_addr, 0));
      unlink_port(net, Pointer(a_addr, 2));
      free_node(net, a_addr);

    // BinaryOperation
    } else if (a_type == OP2) {
      set_type(net, a_addr, OP1);
      link_ports(net, Pointer(a_addr, 0), enter_port(net, Pointer(a_addr, 1)));
      unlink_port(net, Pointer(a_addr, 1));
      link_ports(net, Pointer(a_addr, 1), b_ptrn);

    // NumberDuplication
    } else if (a_type == NOD) {
      link_ports(net, b_ptrn, enter_port(net, Pointer(a_addr, 1)));
      link_ports(net, b_ptrn, enter_port(net, Pointer(a_addr, 2)));
      free_node(net, a_addr);

    // IfThenElse
    } else if (a_type == ITE) {
      u32 cond_val = numb_of(b_ptrn) == 0;
      u64 pair_ptr = enter_port(net, Pointer(a_addr, 1));
      set_type(net, a_addr, NOD);
      link_ports(net, Pointer(a_addr, 0), pair_ptr);
      unlink_port(net, Pointer(a_addr, 1));
      u64 dest_ptr = enter_port(net, Pointer(a_addr, 2));
      link_ports(net, Pointer(a_addr, cond_val ? 2 : 1), dest_ptr);
        if (!cond_val) unlink_port(net, Pointer(a_addr, 2));
      link_ports(net, Pointer(a_addr, cond_val ? 1 : 2), Pointer(a_addr, cond_val ? 1 : 2));

    } else {
      printf("[ERROR]\nInvalid interaction.");
    }

  } else {
    u32 b_addr = addr_of(b_ptrn);
    u32 a_type = get_type(net, a_addr);
    u32 b_type = get_type(net, b_addr);
    u32 a_kind = get_kind(net, a_addr);
    u32 b_kind = get_kind(net, b_addr);

    // NodeAnnihilation, UnaryAnnihilation, BinaryAnnihilation
    if ( (a_type == NOD && b_type == NOD && a_kind == b_kind)
      || (a_type == OP1 && b_type == OP1)
      || (a_type == OP2 && b_type == OP2)
      || (a_type == ITE && b_type == ITE)) {
      u64 a_aux1_dest = enter_port(net, Pointer(a_addr, 1));
      u64 b_aux1_dest = enter_port(net, Pointer(b_addr, 1));
      link_ports(net, a_aux1_dest, b_aux1_dest);
      u64 a_aux2_dest = enter_port(net, Pointer(a_addr, 2));
      u64 b_aux2_dest = enter_port(net, Pointer(b_addr, 2));
      link_ports(net, a_aux2_dest, b_aux2_dest);
      for (u32 i = 0; i < 3; i++) {
        unlink_port(net, Pointer(a_addr, i));
        unlink_port(net, Pointer(b_addr, i));
      }
      free_node(net, a_addr);
      if (a_addr != b_addr) {
        free_node(net, b_addr);
      }

    // NodeDuplication, BinaryDuplication
    } else if
      (  (a_type == NOD && b_type == NOD && a_kind != b_kind)
      || (a_type == NOD && b_type == OP2)
      || (a_type == NOD && b_type == ITE)) {

      u32 p_addr = alloc_node(net, b_type, b_kind);
      u32 q_addr = alloc_node(net, a_type, a_kind);

      link_ports(net, Pointer(b_addr, 0), enter_port(net, Pointer(a_addr, 1)));
      link_ports(net, Pointer(p_addr, 0), enter_port(net, Pointer(a_addr, 2)));
      link_ports(net, Pointer(a_addr, 0), enter_port(net, Pointer(b_addr, 1)));
      link_ports(net, Pointer(q_addr, 0), enter_port(net, Pointer(b_addr, 2)));
      link_ports(net, Pointer(a_addr, 1), Pointer(b_addr, 1));
      link_ports(net, Pointer(q_addr, 1), Pointer(b_addr, 2));
      link_ports(net, Pointer(a_addr, 2), Pointer(p_addr, 1));
      link_ports(net, Pointer(q_addr, 2), Pointer(p_addr, 2));

    // UnaryDuplication
    } else if
      (  (a_type == NOD && b_type == OP1)
      || (a_type == ITE && b_type == OP1)) {

      u32 c_addr = alloc_node(net, b_type, b_kind);

      link_ports(net, Pointer(b_addr, 0), enter_port(net, Pointer(a_addr, 1)));
      link_ports(net, Pointer(c_addr, 0), enter_port(net, Pointer(a_addr, 2)));
      link_ports(net, Pointer(a_addr, 0), enter_port(net, Pointer(b_addr, 2)));
      link_ports(net, Pointer(c_addr, 1), enter_port(net, Pointer(b_addr, 1)));
      link_ports(net, Pointer(a_addr, 1), Pointer(b_addr, 2));
      link_ports(net, Pointer(a_addr, 2), Pointer(c_addr, 2));

    // InvalidInteraction
    } else {
      printf("[ERROR]\nInvalid interaction.");
    }
  }
}

// Rewrites active pairs until none is left, reducing the graph to normal form
// This could be performed in parallel. Unreachable data is freed automatically.
Stats reduce(Net *net) {
  Stats stats;
  stats.rewrites = 0;
  stats.loops = 0;
  while (net->redex_len > 0) {
    for (u32 i = 0, l = net->redex_len; i < l; ++i) {
      rewrite(net, net->redex[--net->redex_len]);
      ++stats.rewrites;
    }
    ++stats.loops;
  }
  return stats;
}

void find_redexes(Net *net) {
  net->redex_len = 0;
  for (u32 i = 0; i < net->nodes_len / 4; ++i) {
    u64 b_ptrn = enter_port(net, Pointer(i, 0));
    if ((type_of(b_ptrn) == NUM || addr_of(b_ptrn) >= i) && is_redex(net, i)) {
      net->redex[net->redex_len++] = i;
    }
  }
}

void print_pointer(u64 ptrn) {
  if (type_of(ptrn) == NUM) {
    printf("#%u", numb_of(ptrn));
  } else {
    printf("%u", addr_of(ptrn));
    switch (slot_of(ptrn)) {
      case 0: printf("a"); break;
      case 1: printf("b"); break;
      case 2: printf("c"); break;
    }
  }
}

void print_net(Net* net) {
  for (u32 i = 0; i < net->nodes_len / 4; i++) {
    if (is_free(net, i)) {
      printf("%u: ~\n", i);
    } else {
      u32 type = get_type(net, i);
      u32 kind = get_kind(net, i);
      printf("%u: ", i);
      printf("[%u:%u| ", type, kind);
      print_pointer(get_port(net, i, 0));
      printf(" ");
      print_pointer(get_port(net, i, 1));
      printf(" ");
      print_pointer(get_port(net, i, 2));
      printf("]");
      printf("...");
      printf("%d ", is_numeric(net, i, 0));
      printf("%d ", is_numeric(net, i, 1));
      printf("%d ", is_numeric(net, i, 2));
      printf("\n");
    }
  }
}
