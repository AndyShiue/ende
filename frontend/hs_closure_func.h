#include <Rts.h>
StgPtr _deRefStablePtr(HsStablePtr stablePtr);
const char* _GET_CON_DESC(StgConInfoTable* iftbl);
StgClosure* _UNTAG_CLOSURE(StgClosure* stgClosure);
StgInfoTable* _get_itbl(StgClosure* stgClosure);
StgConInfoTable* _get_con_itbl(StgClosure* stgClosure);
StgHalfWord _GET_TAG(StgClosure* con);
StgWord _GET_CLOSURE_TAG(StgClosure* stgClosure);
rtsBool _LOOKS_LIKE_CLOSURE_PTR(const void* p);
