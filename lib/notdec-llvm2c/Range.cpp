#include "notdec-llvm2c/Interface/Range.h"

namespace notdec {

std::string toString(const OffsetRange &a) { return a.str(); }

std::vector<int64_t> PrimeFactors(int64_t n) {
  std::vector<int64_t> r;
  for (int64_t i = 2; i * i <= n; i += 1 + (i > 2)) {
    while ((n % i) == 0) {
      r.push_back(i);
      n /= i;
    }
  }
  if (n != 1)
    r.push_back(n);
  return r;
}

OffsetRange OffsetRange::operator+(const OffsetRange &rhs) const {
  OffsetRange ret;
  ret.offset = this->offset + rhs.offset;
  // merge accesses
  // ignore Count as for now
  std::set<int64_t> Muls;
  for (auto &a : access) {
    Muls.insert(a.Size);
  }
  for (auto &a : rhs.access) {
    Muls.insert(a.Size);
  }
  for (auto &I : Muls) {
    ret.access.push_back(ArrayOffset(I));
  }
  return ret;
}

OffsetRange OffsetRange::operator*(const OffsetRange Rhs) const {
  OffsetRange Ret;
  Ret.offset = offset * Rhs.offset;
  // from small to big
  std::set<int64_t> Muls;
  for (uint64_t i = 0; i < access.size() + 1; i++) {
    for (uint64_t j = 0; j < Rhs.access.size() + 1; j++) {
      if (i == 0 && j == 0) {
        continue;
      }
      auto Val1 = i == 0 ? offset : access.at(i - 1).Size;
      auto Val2 = j == 0 ? Rhs.offset : access.at(j - 1).Size;
      Muls.insert(Val1 * Val2);
    }
  }
  for (auto &I : Muls) {
    Ret.access.push_back(ArrayOffset(I));
  }
  return Ret;
}

} // namespace notdec
