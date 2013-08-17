//===-- NewcpuMCAsmInfo.h - Newcpu asm properties --------------*- C++ -*--===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of the NewcpuMCAsmInfo class.
//
//===----------------------------------------------------------------------===//

#ifndef MBLAZETARGETASMINFO_H
#define MBLAZETARGETASMINFO_H

#include "llvm/MC/MCAsmInfo.h"

namespace llvm {
  class Target;

  class NewcpuMCAsmInfo : public MCAsmInfo {
    virtual void anchor();
  public:
    explicit NewcpuMCAsmInfo();
  };

} // namespace llvm

#endif
