//===-- NewcpuTargetMachine.cpp - Define TargetMachine for Newcpu ---------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Implements the info about Newcpu target spec.
//
//===----------------------------------------------------------------------===//

#include "NewcpuTargetMachine.h"
#include "Newcpu.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/PassManager.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Target/TargetOptions.h"
using namespace llvm;

extern "C" void LLVMInitializeNewcpuTarget() {
  // Register the target.
  RegisterTargetMachine<NewcpuTargetMachine> X(TheNewcpuTarget);
}

// DataLayout --> Big-endian, 32-bit pointer/ABI/alignment
// The stack is always 8 byte aligned
// On function prologue, the stack is created by decrementing
// its pointer. Once decremented, all references are done with positive
// offset from the stack/frame pointer, using StackGrowsUp enables
// an easier handling.
NewcpuTargetMachine::
NewcpuTargetMachine(const Target &T, StringRef TT,
                    StringRef CPU, StringRef FS, const TargetOptions &Options,
                    Reloc::Model RM, CodeModel::Model CM,
                    CodeGenOpt::Level OL)
  : LLVMTargetMachine(T, TT, CPU, FS, Options, RM, CM, OL),
    Subtarget(TT, CPU, FS),
    DL("E-p:32:32:32-i8:8:8-i16:16:16"),
    InstrInfo(*this),
    FrameLowering(Subtarget),
    TLInfo(*this), TSInfo(*this),
    InstrItins(Subtarget.getInstrItineraryData()) {
}

namespace {
/// Newcpu Code Generator Pass Configuration Options.
class NewcpuPassConfig : public TargetPassConfig {
public:
  NewcpuPassConfig(NewcpuTargetMachine *TM, PassManagerBase &PM)
    : TargetPassConfig(TM, PM) {}

  NewcpuTargetMachine &getNewcpuTargetMachine() const {
    return getTM<NewcpuTargetMachine>();
  }

  virtual bool addInstSelector();
  virtual bool addPreEmitPass();
};
} // namespace

TargetPassConfig *NewcpuTargetMachine::createPassConfig(PassManagerBase &PM) {
  return new NewcpuPassConfig(this, PM);
}

// Install an instruction selector pass using
// the ISelDag to gen Newcpu code.
bool NewcpuPassConfig::addInstSelector() {
  addPass(createNewcpuISelDag(getNewcpuTargetMachine()));
  return false;
}

// Implemented by targets that want to run passes immediately before
// machine code is emitted. return true if -print-machineinstrs should
// print out the code after the passes.
bool NewcpuPassConfig::addPreEmitPass() {
  addPass(createNewcpuDelaySlotFillerPass(getNewcpuTargetMachine()));
  return true;
}
