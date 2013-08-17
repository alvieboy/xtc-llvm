//===-- NewcpuMCTargetDesc.h - Newcpu Target Descriptions -------*- C++ -*-===//
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

#ifndef NEWCPUMCTARGETDESC_H
#define NEWCPUMCTARGETDESC_H

#include "llvm/Support/DataTypes.h"

namespace llvm {
class MCAsmBackend;
class MCContext;
class MCCodeEmitter;
class MCInstrInfo;
class MCObjectWriter;
class MCRegisterInfo;
class MCSubtargetInfo;
class Target;
class StringRef;
class raw_ostream;

extern Target TheNewcpuTarget;

MCCodeEmitter *createNewcpuMCCodeEmitter(const MCInstrInfo &MCII,
                                         const MCRegisterInfo &MRI,
                                         const MCSubtargetInfo &STI,
                                         MCContext &Ctx);

MCAsmBackend *createNewcpuAsmBackend(const Target &T, StringRef TT,
                                     StringRef CPU);

MCObjectWriter *createNewcpuELFObjectWriter(raw_ostream &OS, uint8_t OSABI);
} // End llvm namespace

// Defines symbolic names for Newcpu registers.  This defines a mapping from
// register name to register number.
#define GET_REGINFO_ENUM
#include "NewcpuGenRegisterInfo.inc"

// Defines symbolic names for the Newcpu instructions.
#define GET_INSTRINFO_ENUM
#include "NewcpuGenInstrInfo.inc"

#define GET_SUBTARGETINFO_ENUM
#include "NewcpuGenSubtargetInfo.inc"

#endif
