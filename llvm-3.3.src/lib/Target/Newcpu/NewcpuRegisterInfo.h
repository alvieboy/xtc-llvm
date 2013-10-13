//===-- NewcpuRegisterInfo.h - Newcpu Register Information Impl -*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the Newcpu implementation of the TargetRegisterInfo
// class.
//
//===----------------------------------------------------------------------===//

#ifndef NEWCPUREGISTERINFO_H
#define NEWCPUREGISTERINFO_H

#include "Newcpu.h"
#include "llvm/Target/TargetRegisterInfo.h"

#define GET_REGINFO_HEADER
#include "NewcpuGenRegisterInfo.inc"

namespace llvm {
class NewcpuSubtarget;
class TargetInstrInfo;
class Type;

struct NewcpuRegisterInfo : public NewcpuGenRegisterInfo {
  const NewcpuSubtarget &Subtarget;
  const TargetInstrInfo &TII;

  NewcpuRegisterInfo(const NewcpuSubtarget &Subtarget,
                     const TargetInstrInfo &tii);

  /// Get PIC indirect call register
  static unsigned getPICCallReg();

  /// Code Generation virtual methods...
  const uint16_t *getCalleeSavedRegs(const MachineFunction* MF = 0) const;

  BitVector getReservedRegs(const MachineFunction &MF) const;

  /// Stack Frame Processing Methods
  void eliminateFrameIndex(MachineBasicBlock::iterator II,
                           int SPAdj, unsigned FIOperandNum,
                           RegScavenger *RS = NULL) const;

  void processFunctionBeforeFrameFinalized(MachineFunction &MF,
                                           RegScavenger *RS = NULL) const;


  const TargetRegisterClass *getPointerRegClass(const MachineFunction &MF, unsigned Kind) const;

  /// Debug information queries.


  unsigned getFrameRegister(const MachineFunction &MF) const;

  /// Exception handling queries.
  unsigned getEHExceptionRegister() const;
  unsigned getEHHandlerRegister() const;
};

} // end namespace llvm

#endif
