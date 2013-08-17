//===-- NewcpuTargetInfo.cpp - Newcpu Target Implementation ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "Newcpu.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/TargetRegistry.h"
using namespace llvm;

Target llvm::TheNewcpuTarget;

extern "C" void LLVMInitializeNewcpuTargetInfo() {
  RegisterTarget<Triple::newcpu> X(TheNewcpuTarget, "newcpu", "Newcpu");
}
