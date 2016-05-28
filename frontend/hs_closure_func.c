#include <Rts.h>

StgPtr _deRefStablePtr(HsStablePtr stablePtr) {
  return deRefStablePtr(stablePtr);
}
const char* _GET_CON_DESC(StgConInfoTable* iftbl) {
  return GET_CON_DESC(iftbl);
}
StgClosure* _UNTAG_CLOSURE(StgClosure* stgClosure) {
  return UNTAG_CLOSURE(stgClosure);
}

StgInfoTable* _get_itbl(StgClosure* stgClosure) {
  return get_itbl(stgClosure);
}
StgConInfoTable* _get_con_itbl(StgClosure* stgClosure) {
  return get_con_itbl(stgClosure);
}
StgHalfWord _GET_TAG(StgClosure* con) {
  return GET_TAG(con);
}
StgWord _GET_CLOSURE_TAG(StgClosure* stgClosure) {
  return GET_CLOSURE_TAG(stgClosure);
}

const char* test_func(HsStablePtr stablePtr) {
  const char* t = GET_CON_DESC(CON_INFO_PTR_TO_STRUCT(GET_INFO(UNTAG_CLOSURE((StgClosure *)deRefStablePtr(stablePtr)))));
  printf("%s", t);
  return t;
}
