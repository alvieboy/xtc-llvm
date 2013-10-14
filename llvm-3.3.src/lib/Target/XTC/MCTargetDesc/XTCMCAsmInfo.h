//===-- XTCMCAsmInfo.h - XTC asm properties --------------*- C++ -*--===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the declaration of the XTCMCAsmInfo class.
//
//===----------------------------------------------------------------------===//

#ifndef XTCTARGETASMINFO_H
#define XTCTARGETASMINFO_H

#include "llvm/MC/MCAsmInfo.h"

namespace llvm {
  class Target;

  class XTCMCAsmInfo : public MCAsmInfo {
    virtual void anchor();
  public:
    explicit XTCMCAsmInfo();
  };

} // namespace llvm

#endif
