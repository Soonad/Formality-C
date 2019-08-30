#include <stdint.h>

typedef uint32_t u32;

#define STATIC static
#define QUAL cfg::template

// Added pow
static u32 ipow(u32 base, u32 exp)
{
    u32 result = 1;
    for (;;)
    {
        if (exp & 1)
            result *= base;
        exp >>= 1;
        if (!exp)
            break;
        base *= base;
    }
    return result;
}


enum class cmd {
  // port of Node addr
    prim_of_node
  , aux0_of_node
  , aux1_of_node
  // immediate port
  , immediate
  // targets
  , target_of_prim
  , target_of_aux0
  , target_of_aux1
  //
  , target_of_immediate
};

enum class node_typ_nominal {
  era, con, op1, op2, ite, num
};

// Scoped enums has fixed underlying type, thus
// GCC warns
enum /* class */ node_typ {
  NOD, OP1, OP2, ITE
};

constexpr unsigned floorlog2(unsigned x)
{
  return x == 1 ? 0 : 1 + floorlog2(x >> 1);
}

constexpr unsigned num_of_bits_required(unsigned x)
{
  return x == 1 ? 0 : floorlog2(x - 1) + 1;
}

constexpr unsigned num_of_bits_4_node_typ = num_of_bits_required(ITE + 1);

template<typename PortPtrTy, typename NumTy> union PortTy {
  PortPtrTy targetPtr;
  NumTy targetNum;
};

// clang doesn't like casts in constexprs, hence temporarily this:
#define CONSTEXPR

// This uses ***different*** represenation, ports are addressed by int offset (in storage units)
// from the current port pointer
// `nodes` array **shall* be aligned on (sizeof(StorageTy)*4)-bytes boundary, i.e we may compute port number from the
// pointer value
struct config_i32array {

  static constexpr int nsz = sizeof(int);
  typedef int StorageTy;
  typedef u32 NumTy;
  typedef StorageTy OffTy;

  static constexpr int INFO = 3;
  static constexpr int node_size = INFO + 1;

  struct NetTy {
    StorageTy * nodes;
    u32 nodes_len; // FIXME??? Overflow???
    StorageTy * * redex;
    u32 redex_len; // FIXME??? Overflow???
    StorageTy * * freed;
    u32 freed_len; // FIXME??? Overflow???
  };

  NetTy net;

  static constexpr unsigned num_of_bits_4_label = sizeof(StorageTy) * 8 - num_of_bits_4_node_typ - INFO;

// Ref impl has *DIFFERENT* bit lengths
// To test things we chose the same repr as reference impl does
#ifndef USE_MY_REPR
  // Ref impl compatible
  struct info {
    unsigned is_num : 3;
    node_typ type : 3;
    StorageTy kind : sizeof(StorageTy) * 8 - 6;
  };
#else
  // bitfields seems to be low-endianish
  struct info {
    unsigned is_num : INFO;
    node_typ type : num_of_bits_4_node_typ;
    StorageTy kind : num_of_bits_4_label;
  };
#endif

  static_assert(sizeof(info) == sizeof(StorageTy), "info size should be of regular storage size!");

#define as_info(p) reinterpret_cast<info*>(p)

  template <cmd c>
  static
  StorageTy* node_ptr(StorageTy* p) {
    static_assert(c < cmd::target_of_prim, "node_ptr not impl for targets");
    if constexpr (c < cmd::immediate)
      return p;
    else
      return (StorageTy*)(uintptr_t(p) & (uintptr_t(-1) << 4));
  }

  // GAGO_CONST
  template <cmd c>
  static
  const StorageTy* node_ptr(const StorageTy* p) {
    static_assert(c < cmd::target_of_prim, "node_ptr not impl for targets");
    if constexpr (c < cmd::immediate)
      return p;
    else
      return (const StorageTy*)(uintptr_t(p) & (uintptr_t(-1) << 4));
  }

  template <cmd c>
  static
  StorageTy* port_ptr(StorageTy* p) {
    static_assert(c < cmd::target_of_prim, "port_ptr not impl for targets");
    if constexpr (c < cmd::immediate)
      return p + int(c);
    else
      return p;
  }

  // GAGO_CONST
  template <cmd c>
  static
  const StorageTy* port_ptr(const StorageTy* p) {
    static_assert(c < cmd::target_of_prim, "port_ptr not impl for targets");
    if constexpr (c < cmd::immediate)
      return p + int(c);
    else
      return p;
  }

  template <cmd c>
  static
  info* info_ptr(StorageTy* p) {
    return reinterpret_cast<info*>(node_ptr<c>(p) + INFO);
  }

  // GAGO_CONST
  template <cmd c>
  static
  const info* info_ptr(const StorageTy* p) {
    return reinterpret_cast<const info*>(node_ptr<c>(p) + INFO);
  }

  template <cmd c>
  static
  int slot_of(const StorageTy* p) {
    static_assert(c < cmd::target_of_prim, "slot_of not impl for targets");
    if constexpr (c < cmd::immediate)
      return int(c);
    else
      return int((intptr_t(p) & 0xf) / sizeof(StorageTy));
  }

  template <cmd c>
  static
  StorageTy* get_port(StorageTy* p) {
    StorageTy* pp = port_ptr<c>(p);
    return pp + *pp;
  }

  // GAGO_CONST
  template <cmd c>
  static
  const StorageTy* get_port(const StorageTy* p) {
    const StorageTy* pp = port_ptr<c>(p);
    return pp + *pp;
  }

  template <cmd c>
  static
  void set_port(StorageTy* p, const StorageTy* port_addr) {
    StorageTy* pp = port_ptr<c>(p);
    *pp = StorageTy(port_addr - pp);
  }

  template <cmd c>
  static
  NumTy get_number(StorageTy* p) {
    return NumTy(*port_ptr<c>(p));
  }

  template <cmd c>
  static
  void set_number(StorageTy* p, NumTy num) {
    *reinterpret_cast<NumTy*>(port_ptr<c>(p)) = num;
  }

  template <cmd c>
  static
  bool is_numeric(const StorageTy* p) {
    return (info_ptr<c>(p)->is_num >> slot_of<c>(p)) & 1;
  }

  // Conversion from reference representation
  // src is *node* address!
  template <unsigned n>
  static
  void convert_from_ref_repr(StorageTy* dst, const u32 ndx, const u32* src_) {
    static_assert(n <= INFO, "wrong offset for convert_from_ref_repr");
    const StorageTy * src = reinterpret_cast<const StorageTy*>(src_);
    if constexpr(n == INFO) {
      dst[n] = src[n];
    }
    else {
      if (is_numeric<cmd(n)>(src))
        dst[n] = src[n];
      else
        dst[n] = src[n] - (ndx + n);
    }
  }

  // Conversion to reference representation
  // src is *node* address!
  template <unsigned n>
  static
  void convert_to_ref_repr(u32* dst, const u32 ndx, const StorageTy* src_) {
    static_assert(n <= INFO, "wrong offset for convert_to_ref_repr");
    const u32 * src = reinterpret_cast<const u32*>(src_);
    if constexpr(n == INFO) {
      dst[n] = src[n];
    }
    else {
      if (is_numeric<cmd(n)>(src_))
        dst[n] = src[n];
      else
        dst[n] = src_[n] + (ndx + n);
    }
  }

  static
  node_typ get_type(StorageTy* p) {
    return info_ptr<cmd::prim_of_node>(p)->type;
  }

  template <node_typ ntype>
  static
  void set_type(StorageTy* p) {
    info_ptr<cmd::prim_of_node>(p)->type = ntype;
  }

  static
  StorageTy get_kind(StorageTy* p) {
    return info_ptr<cmd::prim_of_node>(p)->kind;
  }

  template <cmd c>
  void clean_port(StorageTy* p) {
    *port_ptr<c>(p) = 0;
  }

  static
  void clean_ports_with_info(StorageTy * node_addr, info i) {
    node_addr[0] = 0;
    node_addr[1] = 0;
    node_addr[2] = 0;
    *as_info(node_addr + INFO) = i;
  }

  StorageTy * alloc_node(node_typ type, StorageTy label) {
    StorageTy * addr;
    if (net.freed_len > 0) {
      addr = net.freed[--net.freed_len];
    }
    else {
      addr = net.nodes + net.nodes_len;
      net.nodes_len += node_size;
    }
    clean_ports_with_info(addr, info{ 0, type, label });
    return addr;
  }

  void free_node(StorageTy* addr) {
    clean_ports_with_info(addr, info{ 0, NOD, 0 });
    net.freed[net.freed_len++] = addr;
  }

  static constexpr
  bool is_free(const StorageTy* addr) {
    return
         addr[0] == 0
      && addr[1] == 0
      && addr[2] == 0
      && addr[INFO] == 0;
  }

  static
  bool is_redex(const StorageTy* node_addr) {
    if (is_numeric<cmd::prim_of_node>(node_addr))
      return true;
    const StorageTy* b_ptrn = get_port<cmd::prim_of_node>(node_addr);
    return slot_of<cmd::immediate>(b_ptrn) == int(cmd::prim_of_node) && !is_free(node_addr);
  }

  template <cmd c>
  void put_number_to_port(StorageTy* p, NumTy num) {
    set_number<c>(p, num);
    const int slot = slot_of<c>(p);
    info_ptr<c>(p)->is_num |= (1 << slot);
    if constexpr (c == cmd::prim_of_node) {
      net.redex[net.redex_len++] = node_ptr<c>(p);
    }
    else 
      if (slot == 0) {
        net.redex[net.redex_len++] = node_ptr<c>(p);
      }
  }

  template <cmd c>
  STATIC // we don't modify `redex` here, hence `static`
  int put_port_to_port(StorageTy* p, const StorageTy* port_addr) {
    set_port<c>(p, port_addr);
    const int slot = slot_of<c>(p);
    info_ptr<c>(p)->is_num &= ~(1 << slot);
    // redex update is moved to link_ports
    // since we need *both* ports to be prime
    return slot;
  }

  template <cmd c>
  void link_port_to_number(StorageTy* p, NumTy num) {
    if constexpr (c >= cmd::target_of_prim) {
      constexpr cmd newc = cmd(int(c) - int(cmd::target_of_prim));
      if (is_numeric<newc>(p))
        return;
      put_number_to_port<cmd::immediate>(get_port<newc>(p), num);
    }
    else {
      put_number_to_port<c>(p, num);
    }
  }

  template <cmd c>
  STATIC
  void unlink_port(StorageTy * p) {
#if 1
    static_assert(c <= cmd::immediate, "unlink_port is defined for explicitly addressed ports only");
    if (is_numeric<c>(p)) {
      return;
    }
    StorageTy* sp = port_ptr<c>(p);
    StorageTy* tp = get_port<c>(p);
    if (!is_numeric<cmd::immediate>(tp) && get_port<cmd::immediate>(tp) == sp) {
      *sp = 0;
      *tp = 0;
    }
#endif  
  }

  template <cmd lcmd, cmd rcmd>
  void link_ports(StorageTy* aptr, StorageTy* bptr) {
    if constexpr (rcmd >= cmd::target_of_prim) {
      // link_ports<rcmd, lcmd>(bptr, aptr);
      constexpr cmd newrc = cmd(int(rcmd) - int(cmd::target_of_prim));
      if (is_numeric<newrc>(bptr)) {
        link_port_to_number<lcmd>(aptr, get_number<newrc>(bptr));
      }
      else
        link_ports<lcmd, cmd::immediate>(aptr, get_port<newrc>(bptr));
    }
    // We don't know if the order is relevant
    // hence we don't shortcut here, and rewrite everything
    else if constexpr (lcmd >= cmd::target_of_prim) {
      constexpr cmd newlc = cmd(int(lcmd) - int(cmd::target_of_prim));
      if (is_numeric<newlc>(aptr)) {
        link_port_to_number<rcmd>(bptr, get_number<newlc>(aptr));
      }
      else
        link_ports<cmd::immediate, rcmd>(get_port<newlc>(aptr), bptr);
    }
    else {
      const int a_slot = put_port_to_port<lcmd>(aptr, port_ptr<rcmd>(bptr));
      const int b_slot = put_port_to_port<rcmd>(bptr, port_ptr<lcmd>(aptr));
      if constexpr ((lcmd == cmd::prim_of_node) && (rcmd == cmd::prim_of_node)) {
        net.redex[net.redex_len++] = node_ptr<rcmd>(bptr);
      }
      else {
        if (a_slot == 0 && b_slot == 0) {
          net.redex[net.redex_len++] = node_ptr<lcmd>(aptr);
        }
      }
    }
  }

#undef as_info
};

// return status
template <typename cfg>
CONSTEXPR
int rewrite(cfg & net, typename cfg::StorageTy * a_addr) {

  const auto a_type = cfg::get_type(a_addr);
  const auto a_kind = cfg::get_kind(a_addr);

  if CONSTEXPR (cfg::template is_numeric<cmd::prim_of_node>(a_addr)) {
    const auto fst = cfg::template get_number<cmd::prim_of_node>(a_addr);

    // UnaryOperation
    if (a_type == OP1) {
      const auto snd = cfg::template get_number<cmd::aux0_of_node>(a_addr);
      typename cfg::NumTy res;
      switch (a_kind) {
        case  0: res = fst + snd; break;
        case  1: res = fst - snd; break;
        case  2: res = fst * snd; break;
        case  3: res = fst / snd; break;
        case  4: res = fst % snd; break;
        case  5: res = (typename cfg::NumTy)(ipow(fst, snd)); break;
        // Don't understand what this op(6) does,
        // I've mechanically translated it, but appreantly it makes no sense at all (exp is always zero)
        case  6: res = (typename cfg::NumTy)(ipow(fst, snd / 0x1'0000'0000)); break;
        case  7: res = fst & snd; break;
        case  8: res = fst | snd; break;
        case  9: res = fst ^ snd; break;
        case 10: res = ~snd; break;
        case 11: res = fst >> snd; break;
        case 12: res = fst << snd; break;
        case 13: res = fst > snd ? 1 : 0; break;
        case 14: res = fst < snd ? 1 : 0; break;
        case 15: res = fst == snd ? 1 : 0; break;
        default: return 1;
      }
      net.template link_port_to_number<cmd::target_of_aux1>(a_addr, res);
      // GAGO_OPT_UNLINK create unlink_port immediate variant and rewrite this (and further down)!
      // Review all unlink usages
      // QUAL unlink_port<cmd::prim_of_node>(a_addr); // SHOULDN'T HAVE THIS
      QUAL unlink_port<cmd::aux1_of_node>(a_addr);
      net.free_node(a_addr);

    // BinaryOperation
    } else if (a_type == OP2) {
      cfg::template set_type<OP1>(a_addr);
      net.template link_ports<cmd::prim_of_node, cmd::target_of_aux0>(a_addr, a_addr);
      QUAL unlink_port<cmd::aux0_of_node>(a_addr);
      net.template link_port_to_number<cmd::aux0_of_node>(a_addr, fst);

    // NumberDuplication
    } else if (a_type == NOD) {
      net.template link_port_to_number<cmd::target_of_aux0>(a_addr, fst);
      net.template link_port_to_number<cmd::target_of_aux1>(a_addr, fst);
      net.free_node(a_addr);

    // IfThenElse
    } else if (a_type == ITE) {
      cfg::template set_type<NOD>(a_addr);
      // link to pair_ptr
      net.template link_ports<cmd::prim_of_node, cmd::target_of_aux0>(a_addr, a_addr);
      QUAL unlink_port<cmd::aux0_of_node>(a_addr);
      if (fst == 0) { // fst == cond_val
        net.template link_ports<cmd::aux1_of_node, cmd::target_of_aux1>(a_addr, a_addr);
        // net.template link_ports<cmd::aux0_of_node, cmd::aux0_of_node>(a_addr, a_addr);
        net.template clean_port<cmd::aux0_of_node>(a_addr);
      }
      else {
        net.template link_ports<cmd::aux0_of_node, cmd::target_of_aux1>(a_addr, a_addr);
        QUAL unlink_port<cmd::aux1_of_node>(a_addr);
        // net.template link_ports<cmd::aux1_of_node, cmd::aux1_of_node>(a_addr, a_addr);
        net.template clean_port<cmd::aux1_of_node>(a_addr);
      }
    } else {
      return 2;
    }

  } else {
    auto b_ptrn = cfg::template get_port<cmd::prim_of_node>(a_addr);
    auto b_addr = cfg::template node_ptr<cmd::immediate>(b_ptrn);
    auto b_type = cfg::get_type(b_addr);
    auto b_kind = cfg::get_kind(b_addr);

    // NodeAnnihilation, UnaryAnnihilation, BinaryAnnihilation
    if ( (a_type == NOD && b_type == NOD && a_kind == b_kind)
      || (a_type == OP1 && b_type == OP1)
      || (a_type == OP2 && b_type == OP2)
      || (a_type == ITE && b_type == ITE)) {

      net.template link_ports<cmd::target_of_aux0, cmd::target_of_aux0>(a_addr, b_addr);
      net.template link_ports<cmd::target_of_aux1, cmd::target_of_aux1>(a_addr, b_addr);
      
      QUAL unlink_port<cmd::prim_of_node>(a_addr);
      QUAL unlink_port<cmd::prim_of_node>(b_addr);
      QUAL unlink_port<cmd::aux0_of_node>(a_addr);
      QUAL unlink_port<cmd::aux0_of_node>(b_addr);
      QUAL unlink_port<cmd::aux1_of_node>(a_addr);
      QUAL unlink_port<cmd::aux1_of_node>(b_addr);
      net.free_node(a_addr);
      if (a_addr != b_addr) {
        net.free_node(b_addr);
      }

    // NodeDuplication, BinaryDuplication
    } else if
      (  (a_type == NOD && b_type == NOD && a_kind != b_kind)
      || (a_type == NOD && b_type == OP2)
      || (a_type == NOD && b_type == ITE)) {
      auto p_addr = net.alloc_node(b_type, b_kind);
      auto r_addr = net.alloc_node(a_type, a_kind);

      // Reuse b for q, a for s
      #define q_addr b_addr
      #define s_addr a_addr

      // below the lists of all available ports are laid down for each step (prepended with ++),
      // original steps are simply numbered and copypasted

      // FIXME!!! OPTIMIZE!, SHOULD look if q ports are numeric
      //++ a0, b0, p0, p1, p2, r0, r1, r2
      net.template link_ports<cmd::prim_of_node, cmd::target_of_aux0>(p_addr, a_addr);  // 4
      //++ a0, b0, a1, p1, p2, r0, r1, r2
      net.template link_ports<cmd::prim_of_node, cmd::target_of_aux1>(q_addr, a_addr);  // 5 b_addr is reused instead of q_addr (b0 is available)
      //++ a0, a2, a1, p1, p2, r0, r1, r2
      net.template link_ports<cmd::prim_of_node, cmd::target_of_aux0>(r_addr, b_addr);  // 6
      //++ a0, a2, a1, p1, p2, b1, r1, r2
      net.template link_ports<cmd::prim_of_node, cmd::target_of_aux1>(s_addr, b_addr);  // 7 a_addr is reused instead of s_addr (a0 is available)
      //++ b2, a2, a1, p1, p2, b1, r1, r2
      net.template link_ports<cmd::aux0_of_node, cmd::aux0_of_node>(r_addr, p_addr);    // 0
      //++ b2, a2, a1, p2, b1, r2
      net.template link_ports<cmd::aux0_of_node, cmd::aux1_of_node>(s_addr, p_addr);    // 1 a_addr is reused instead of s_addr (a1 is available)
      //++ b2, a2, b1, r2
      net.template link_ports<cmd::aux1_of_node, cmd::aux0_of_node>(r_addr, q_addr);    // 2 b_addr is reused instead of q_addr (b1 is available)
      //++ b2, a2
      net.template link_ports<cmd::aux1_of_node, cmd::aux1_of_node>(s_addr, q_addr);    // 3 a_addr is reused instead of s_addr b_addr is reused instead of q_addr

    // UnaryDuplication
    } else if
      (  (a_type == NOD && b_type == OP1)
      || (a_type == ITE && b_type == OP1)) {
      auto p_addr = net.alloc_node(b_type, b_kind);

      // Reuse b for q, a for s (defined above ^)
      // below the lists of all available ports are laid down for each step (prepended with ++),
      // original steps are simply copypasted
      // FIXME!!! OPTIMIZE!, SHOULD look if b ports are numeric
      // a0, b0, p0, p1, p2
      net.template link_ports<cmd::prim_of_node, cmd::target_of_aux0>(p_addr, a_addr);  // 4
      // a0, b0, a1, p1, p2
      net.template link_ports<cmd::prim_of_node, cmd::target_of_aux1>(q_addr, a_addr);  // 5 b_addr is reused instead of q_addr (b0 is available)
      // a0, a2, a1, p1, p2
      net.template link_ports<cmd::prim_of_node, cmd::target_of_aux1>(s_addr, b_addr);  // 6 a_addr is reused instead of s_addr (a0 is available)
      // b2, a2, a1, p1, p2
      net.template link_ports<cmd::aux0_of_node, cmd::target_of_aux0>(p_addr, b_addr);  // 0
      // b2, a2, a1, b1, p2
      // net.template link_ports<cmd::aux0_of_node, cmd::target_of_aux0>(q_addr, b_addr);  // 1 collapsed since b is q
      // b2, a2, a1, p2
      net.template link_ports<cmd::aux0_of_node, cmd::aux1_of_node>(s_addr, p_addr);    // 2 a_addr is reused instead of s_addr (a1 is available)
      // b2, a2
      net.template link_ports<cmd::aux1_of_node, cmd::aux1_of_node>(s_addr, q_addr);    // 3 a_addr is reused instead of s_addr b_addr is reused instead of q_addr

      #undef q_addr
      #undef s_addr
    // Permutations
    } else if (a_type == OP1 && b_type == NOD) {
      rewrite<cfg>(net, b_addr);
    } else if (a_type == OP2 && b_type == NOD) {
      rewrite<cfg>(net, b_addr);
    } else if (a_type == ITE && b_type == NOD) {
      rewrite<cfg>(net, b_addr);
    // InvalidInteraction
    } else {
      return 3;
    }
  }
  return 0;
}

static inline
void find_redexes(config_i32array& cfg) {
  cfg.net.redex_len = 0;
  for (u32 i = 0; i < cfg.net.nodes_len / 4; ++i) {
    int* node_ptr = cfg.net.nodes + i * 4;
    if (config_i32array::template is_numeric<cmd::aux0_of_node>(node_ptr) || (config_i32array::template node_ptr<cmd::immediate>(config_i32array::template get_port<cmd::prim_of_node>(node_ptr)) >= node_ptr)) {
      if (config_i32array::is_redex(node_ptr))
        cfg.net.redex[cfg.net.redex_len++] = node_ptr;
    }
  }
}

// We expect the work_arena to have size:
// 16                             // we use this to align things on 16-byte boundary (if necessary, I don't know if there exist any guaranties on host-allocated memory alignment)
//  + sizeof(int) * max_nodes     // net.nodes
//   + sizeof(*int) * max_redexes // net.redex
//    + sizeof(*int) * max_freed  // net.freed
//
// It might be allocated on javascript side.
//
// Better we should have a representation, which requires *no* conversion, so that we may use `nodes` array directy,
//   not copying nodes forth and back.

// The function returns 0 when succeeded, and non-null when failed.

extern "C"
int process(
    u32 * nodes
  , u32 nodes_len
  , void * work_arena
  , unsigned max_nodes
  , unsigned max_redexes
  ) {
  unsigned xalign = intptr_t(work_arena) & 0xf;
  int * work_nodes = reinterpret_cast<int*>(reinterpret_cast<char*>(work_arena) + xalign ? (0x10 - xalign) : 0);
  int ** redex = reinterpret_cast<int**>(work_nodes + max_nodes);
  int ** freed = redex + max_redexes;

  config_i32array cfg {work_nodes, 0, redex, 0, freed, 0};
  for (u32 i = 0; i < nodes_len; i += 4) {
    // Translate from their representation to ours
    int* curr_node = cfg.net.nodes + i;
    auto src = nodes + i;
    config_i32array::convert_from_ref_repr<0>(curr_node, i, src);
    config_i32array::convert_from_ref_repr<1>(curr_node, i, src);
    config_i32array::convert_from_ref_repr<2>(curr_node, i, src);
    config_i32array::convert_from_ref_repr<3>(curr_node, i, src);
    cfg.net.nodes_len += 4;
  }
  find_redexes(cfg);

  // Manually inline reduce
  while (cfg.net.redex_len > 0) {
    for (u32 i = 0, l = cfg.net.redex_len; i < l; ++i) {
      if (int r = rewrite<config_i32array>(cfg, cfg.net.redex[--cfg.net.redex_len]))
        return r;
    }
  }

  for (u32 i = 0; i < nodes_len; i += 4) {
    // Translate from our representation back to theirs (and put back to the original `nodes` array
    u32 * curr_node = nodes + i;
    auto src = cfg.net.nodes + i;
    config_i32array::convert_to_ref_repr<0>(curr_node, i, src);
    config_i32array::convert_to_ref_repr<1>(curr_node, i, src);
    config_i32array::convert_to_ref_repr<2>(curr_node, i, src);
    config_i32array::convert_to_ref_repr<3>(curr_node, i, src);
  }

  return 0;
}
