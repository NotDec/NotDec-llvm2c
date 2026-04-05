struct st {
  int value;
  st *next;
};

template <typename TLoad, typename TStore> union Ptr {
  TLoad load;
  TStore store;
};

using IntPtrDual = Ptr<int *, long *>;
using NodeDual = Ptr<st *, int *>;

IntPtrDual g_int_dual;
NodeDual g_node_dual;

struct Holder {
  Ptr<int *, int *> direct;
  NodeDual named;
};
