#pragma once
#include "handle.h"

namespace stanly {
#ifdef NDEBUG
using repr = handle;
#else
using repr = std::string_view;
#endif
}  // namespace stanly