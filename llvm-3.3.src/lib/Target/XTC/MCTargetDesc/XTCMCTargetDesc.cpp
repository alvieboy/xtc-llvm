//===-- XTCMCTargetDesc.cpp - XTC Target Descriptions ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file provides XTC specific target descriptions.
//
//===----------------------------------------------------------------------===//

#include "XTCMCTargetDesc.h"
#include "InstPrinter/XTCInstPrinter.h"
#include "XTCMCAsmInfo.h"
#include "llvm/MC/MCCodeGenInfo.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TargetRegistry.h"

#define GET_INSTRINFO_MC_DESC
#include "XTCGenInstrInfo.inc"

#define GET_SUBTARGETINFO_MC_DESC
#include "XTCGenSubtargetInfo.inc"

#define GET_REGINFO_MC_DESC
#include "XTCGenRegisterInfo.inc"

using namespace llvm;


static MCInstrInfo *createXTCMCInstrInfo() {
  MCInstrInfo *X = new MCInstrInfo();
  InitXTCMCInstrInfo(X);
  return X;
}

static MCRegisterInfo *createXTCMCRegisterInfo(StringRef TT) {
  MCRegisterInfo *X = new MCRegisterInfo();
  InitXTCMCRegisterInfo(X, XTC::r13);
  return X;
}

static MCSubtargetInfo *createXTCMCSubtargetInfo(StringRef TT, StringRef CPU,
                                                    StringRef FS) {
  MCSubtargetInfo *X = new MCSubtargetInfo();
  InitXTCMCSubtargetInfo(X, TT, CPU, FS);
  return X;
}

static MCAsmInfo *createMCAsmInfo(const Target &T, StringRef TT) {
  Triple TheTriple(TT);
  switch (TheTriple.getOS()) {
  default:
    return new XTCMCAsmInfo();
  }
}

static MCCodeGenInfo *createXTCMCCodeGenInfo(StringRef TT, Reloc::Model RM,
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
    llvm_unreachable("XTC does not support Darwin MACH-O format");
  }

  if (TheTriple.isOSWindows()) {
    llvm_unreachable("XTC does not support Windows COFF format");
  }

  return createELFStreamer(Ctx, MAB, _OS, _Emitter, RelaxAll, NoExecStack);
}

static MCInstPrinter *createXTCMCInstPrinter(const Target &T,
                                                unsigned SyntaxVariant,
                                                const MCAsmInfo &MAI,
                                                const MCInstrInfo &MII,
                                                const MCRegisterInfo &MRI,
                                                const MCSubtargetInfo &STI) {
  if (SyntaxVariant == 0)
    return new XTCInstPrinter(MAI, MII, MRI);
  return 0;
}

// Force static initialization.
extern "C" void LLVMInitializeXTCTargetMC() {
  // Register the MC asm info.
  RegisterMCAsmInfoFn X(TheXTCTarget, createMCAsmInfo);

  // Register the MC codegen info.
  TargetRegistry::RegisterMCCodeGenInfo(TheXTCTarget,
                                        createXTCMCCodeGenInfo);

  // Register the MC instruction info.
  TargetRegistry::RegisterMCInstrInfo(TheXTCTarget, createXTCMCInstrInfo);

  // Register the MC register info.
  TargetRegistry::RegisterMCRegInfo(TheXTCTarget,
                                    createXTCMCRegisterInfo);

  // Register the MC subtarget info.
  TargetRegistry::RegisterMCSubtargetInfo(TheXTCTarget,
                                          createXTCMCSubtargetInfo);

  // Register the MC code emitter
  TargetRegistry::RegisterMCCodeEmitter(TheXTCTarget,
                                        llvm::createXTCMCCodeEmitter);

  // Register the asm backend
  TargetRegistry::RegisterMCAsmBackend(TheXTCTarget,
                                       createXTCAsmBackend);

  // Register the object streamer
  TargetRegistry::RegisterMCObjectStreamer(TheXTCTarget,
                                           createMCStreamer);

  // Register the MCInstPrinter.
  TargetRegistry::RegisterMCInstPrinter(TheXTCTarget,
                                        createXTCMCInstPrinter);
}
