//===-- Newcpu.h - Top-level interface for Newcpu ---------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the entry points for global functions defined in
// the LLVM Newcpu back-end.
//
//===----------------------------------------------------------------------===//

#ifndef TARGET_NEWCPU_H
#define TARGET_NEWCPU_H

#include "MCTargetDesc/NewcpuBaseInfo.h"
#include "MCTargetDesc/NewcpuMCTargetDesc.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {
  class NewcpuTargetMachine;
  class FunctionPass;
  class MachineCodeEmitter;

  FunctionPass *createNewcpuISelDag(NewcpuTargetMachine &TM);
  FunctionPass *createNewcpuDelaySlotFillerPass(NewcpuTargetMachine &TM);

} // end namespace llvm;

#endif
