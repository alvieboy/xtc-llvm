//===-- XTCTargetInfo.cpp - XTC Target Implementation ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "XTC.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/TargetRegistry.h"
using namespace llvm;

Target llvm::TheXTCTarget;

extern "C" void LLVMInitializeXTCTargetInfo() {
  RegisterTarget<Triple::xtc> X(TheXTCTarget, "xtc", "XTC");
}
