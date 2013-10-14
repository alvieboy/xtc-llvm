//===-- XTC.h - Top-level interface for XTC ---------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the entry points for global functions defined in
// the LLVM XTC back-end.
//
//===----------------------------------------------------------------------===//

#ifndef TARGET_XTC_H
#define TARGET_XTC_H

#include "MCTargetDesc/XTCBaseInfo.h"
#include "MCTargetDesc/XTCMCTargetDesc.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {
  class XTCTargetMachine;
  class FunctionPass;
  class MachineCodeEmitter;

  FunctionPass *createXTCISelDag(XTCTargetMachine &TM);
  FunctionPass *createXTCDelaySlotFillerPass(XTCTargetMachine &TM);

} // end namespace llvm;

#endif
