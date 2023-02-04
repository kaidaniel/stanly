#ifndef KSAR_KSAR_H
#define KSAR_KSAR_H

#include "ConstantAbstractDomain.h"       // str, int, iterator,
#include "DirectProductAbstractDomain.h"  // data_frame := columns x allocation-site
#include "DisjointUnionAbstractDomain.h"  // value := DataFrame | builtin
#include "HashedAbstractEnvironment.h"    // variable -> value
#include "HashedSetAbstractDomain.h"      // columns, allocation-site

struct CFG;

#endif  // KSAR_KSAR_H