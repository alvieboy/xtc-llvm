//===-- NewcpuMCTargetDesc.cpp - Newcpu Target Descriptions ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file provides Newcpu specific target descriptions.
//
//===----------------------------------------------------------------------===//

#include "NewcpuMCTargetDesc.h"
#include "InstPrinter/NewcpuInstPrinter.h"
#include "NewcpuMCAsmInfo.h"
#include "llvm/MC/MCCodeGenInfo.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TargetRegistry.h"

#define GET_INSTRINFO_MC_DESC
#include "NewcpuGenInstrInfo.inc"

#define GET_SUBTARGETINFO_MC_DESC
#include "NewcpuGenSubtargetInfo.inc"

#define GET_REGINFO_MC_DESC
#include "NewcpuGenRegisterInfo.inc"

using namespace llvm;


static MCInstrInfo *createNewcpuMCInstrInfo() {
  MCInstrInfo *X = new MCInstrInfo();
  InitNewcpuMCInstrInfo(X);
  return X;
}

static MCRegisterInfo *createNewcpuMCRegisterInfo(StringRef TT) {
  MCRegisterInfo *X = new MCRegisterInfo();
  InitNewcpuMCRegisterInfo(X, Newcpu::r7);
  return X;
}

static MCSubtargetInfo *createNewcpuMCSubtargetInfo(StringRef TT, StringRef CPU,
                                                    StringRef FS) {
  MCSubtargetInfo *X = new MCSubtargetInfo();
  InitNewcpuMCSubtargetInfo(X, TT, CPU, FS);
  return X;
}

static MCAsmInfo *createMCAsmInfo(const Target &T, StringRef TT) {
  Triple TheTriple(TT);
  switch (TheTriple.getOS()) {
  default:
    return new NewcpuMCAsmInfo();
  }
}

static MCCodeGenInfo *createNewcpuMCCodeGenInfo(StringRef TT, Reloc::Model RM,
                                                CodeModel::Model CM,
                                                CodeGenOpt::Level OL) {
  MCCodeGenInfo *X = new MCCodeGenInfo();
  if (RM == Reloc::Default)
    RM = Reloc::Static;
  if (CM == CodeModel::Default)
    CM = CodeModel::Small;
  X->InitMCCodeGenInfo(RM, CM, OL);
  return X;
}

static MCStreamer *createMCStreamer(const Target &T, StringRef TT,
                                    MCContext &Ctx, MCAsmBackend &MAB,
                                    raw_ostream &_OS,
                                    MCCodeEmitter *_Emitter,
                                    bool RelaxAll,
                                    bool NoExecStack) {
  Triple TheTriple(TT);

  if (TheTriple.isOSDarwin()) {
    llvm_unreachable("Newcpu does not support Darwin MACH-O format");
  }

  if (TheTriple.isOSWindows()) {
    llvm_unreachable("Newcpu does not support Windows COFF format");
  }

  return createELFStreamer(Ctx, MAB, _OS, _Emitter, RelaxAll, NoExecStack);
}

static MCInstPrinter *createNewcpuMCInstPrinter(const Target &T,
                                                unsigned SyntaxVariant,
                                                const MCAsmInfo &MAI,
                                                const MCInstrInfo &MII,
                                                const MCRegisterInfo &MRI,
                                                const MCSubtargetInfo &STI) {
  if (SyntaxVariant == 0)
    return new NewcpuInstPrinter(MAI, MII, MRI);
  return 0;
}

// Force static initialization.
extern "C" void LLVMInitializeNewcpuTargetMC() {
  // Register the MC asm info.
  RegisterMCAsmInfoFn X(TheNewcpuTarget, createMCAsmInfo);

  // Register the MC codegen info.
  TargetRegistry::RegisterMCCodeGenInfo(TheNewcpuTarget,
                                        createNewcpuMCCodeGenInfo);

  // Register the MC instruction info.
  TargetRegistry::RegisterMCInstrInfo(TheNewcpuTarget, createNewcpuMCInstrInfo);

  // Register the MC register info.
  TargetRegistry::RegisterMCRegInfo(TheNewcpuTarget,
                                    createNewcpuMCRegisterInfo);

  // Register the MC subtarget info.
  TargetRegistry::RegisterMCSubtargetInfo(TheNewcpuTarget,
                                          createNewcpuMCSubtargetInfo);

  // Register the MC code emitter
  TargetRegistry::RegisterMCCodeEmitter(TheNewcpuTarget,
                                        llvm::createNewcpuMCCodeEmitter);

  // Register the asm backend
  TargetRegistry::RegisterMCAsmBackend(TheNewcpuTarget,
                                       createNewcpuAsmBackend);

  // Register the object streamer
  TargetRegistry::RegisterMCObjectStreamer(TheNewcpuTarget,
                                           createMCStreamer);

  // Register the MCInstPrinter.
  TargetRegistry::RegisterMCInstPrinter(TheNewcpuTarget,
                                        createNewcpuMCInstPrinter);
}
