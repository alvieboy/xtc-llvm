//===-- XTCBaseInfo.h - Top level definitions for XTC -- --*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains small standalone helper functions and enum definitions for
// the XTC target useful for the compiler back-end and the MC libraries.
// As such, it deliberately does not include references to LLVM core
// code gen types, passes, etc..
//
//===----------------------------------------------------------------------===//

#ifndef XTCBASEINFO_H
#define XTCBASEINFO_H

#include "XTCMCTargetDesc.h"
#include "llvm/Support/ErrorHandling.h"

namespace llvm {

/// XTCII - This namespace holds all of the target specific flags that
/// instruction info tracks.
///
namespace XTCII {
  enum {
    // PseudoFrm - This represents an instruction that is a pseudo instruction
    // or one that has not been implemented yet.  It is illegal to code generate
    // it, but tolerated for intermediate implementation stages.
    FPseudo = 0,
    FALUg,
    FALUaz,
    FIMM,
    FormMask = 3
    //===------------------------------------------------------------------===//
    // XTC Specific MachineOperand flags.
    // MO_NO_FLAG,

    /// MO_GOT - Represents the offset into the global offset table at which
    /// the address the relocation entry symbol resides during execution.
    // MO_GOT,

    /// MO_GOT_CALL - Represents the offset into the global offset table at
    /// which the address of a call site relocation entry symbol resides
    /// during execution. This is different from the above since this flag
    /// can only be present in call instructions.
    // MO_GOT_CALL,

    /// MO_GPREL - Represents the offset from the current gp value to be used
    /// for the relocatable object file being produced.
    // MO_GPREL,

    /// MO_ABS_HILO - Represents the hi or low part of an absolute symbol
    /// address.
    // MO_ABS_HILO

  };
}

static inline bool isGPRXTCRegister(unsigned Reg) {
  return Reg <= 7;
}

static inline bool isDARXTCRegister(unsigned Reg) {
  switch (Reg) {
  case 0:
  case 1:
      return true;

    default:
      return false;
  }
}

/// getXTCRegisterNumbering - Given the enum value for some register, e.g.
/// XTC::R0, return the number that it corresponds to (e.g. 0).
static inline unsigned getXTCRegisterNumbering(unsigned RegEnum) {
  switch (RegEnum) {
    case XTC::r0     : return 0;
    case XTC::r1     : return 1;
    case XTC::r2     : return 2;
    case XTC::r3     : return 3;
    case XTC::r4     : return 4;
    case XTC::r5     : return 5;
    case XTC::r6     : return 6;
    case XTC::r7     : return 7;
    default: llvm_unreachable("Unknown register number!");
  }
}

/// getRegisterFromNumbering - Given the enum value for some register, e.g.
/// XTC::R0, return the number that it corresponds to (e.g. 0).
static inline unsigned getGPRXTCRegisterFromNumbering(unsigned Reg) {
  switch (Reg) {
    case 0  : return XTC::r0;
    case 1  : return XTC::r1;
    case 2  : return XTC::r2;
    case 3  : return XTC::r3;
    case 4  : return XTC::r4;
    case 5  : return XTC::r5;
    case 6  : return XTC::r6;
    case 7  : return XTC::r7;
    default: llvm_unreachable("Unknown register number!");
  }
}

} // end namespace llvm;

#endif
