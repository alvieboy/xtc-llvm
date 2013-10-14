//===-- XTCMCTargetDesc.h - XTC Target Descriptions -------*- C++ -*-===//
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

#ifndef XTCMCTARGETDESC_H
#define XTCMCTARGETDESC_H

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

extern Target TheXTCTarget;

MCCodeEmitter *createXTCMCCodeEmitter(const MCInstrInfo &MCII,
                                         const MCRegisterInfo &MRI,
                                         const MCSubtargetInfo &STI,
                                         MCContext &Ctx);

MCAsmBackend *createXTCAsmBackend(const Target &T, StringRef TT,
                                     StringRef CPU);

MCObjectWriter *createXTCELFObjectWriter(raw_ostream &OS, uint8_t OSABI);
} // End llvm namespace

// Defines symbolic names for XTC registers.  This defines a mapping from
// register name to register number.
#define GET_REGINFO_ENUM
#include "XTCGenRegisterInfo.inc"

// Defines symbolic names for the XTC instructions.
#define GET_INSTRINFO_ENUM
#include "XTCGenInstrInfo.inc"

#define GET_SUBTARGETINFO_ENUM
#include "XTCGenSubtargetInfo.inc"

#endif
