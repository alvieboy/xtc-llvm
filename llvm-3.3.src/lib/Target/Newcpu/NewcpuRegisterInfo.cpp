//===-- NewcpuRegisterInfo.cpp - Newcpu Register Information --------------===//
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

#define DEBUG_TYPE "newcpu-frame-info"

#include "NewcpuRegisterInfo.h"
#include "Newcpu.h"
#include "NewcpuMachineFunction.h"
#include "NewcpuSubtarget.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/ValueTypes.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetFrameLowering.h"
#include "llvm/Target/TargetInstrInfo.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

#define GET_REGINFO_TARGET_DESC
#include "NewcpuGenRegisterInfo.inc"

using namespace llvm;

NewcpuRegisterInfo::
NewcpuRegisterInfo(const NewcpuSubtarget &ST, const TargetInstrInfo &tii)
  : NewcpuGenRegisterInfo(Newcpu::r7), Subtarget(ST), TII(tii) {}

unsigned NewcpuRegisterInfo::getPICCallReg() {
  return Newcpu::r2;
}

const TargetRegisterClass *
NewcpuRegisterInfo::getPointerRegClass(const MachineFunction &MF, unsigned Kind)
                                                                       const {
  return &Newcpu::GPRegsRegClass;
}


//===----------------------------------------------------------------------===//
// Callee Saved Registers methods
//===----------------------------------------------------------------------===//

/// Newcpu Callee Saved Registers
const uint16_t* NewcpuRegisterInfo::
getCalleeSavedRegs(const MachineFunction *MF) const {
  // Newcpu callee-save register range is R20 - R31
  static const uint16_t CalleeSavedRegs[] = {
      Newcpu::r4, Newcpu::r5, Newcpu::r6,
      0
  };

  return CalleeSavedRegs;
}

BitVector NewcpuRegisterInfo::
getReservedRegs(const MachineFunction &MF) const {
  BitVector Reserved(getNumRegs());
  Reserved.set(Newcpu::r0);
  Reserved.set(Newcpu::r6);
  Reserved.set(Newcpu::r7);
  return Reserved;
}

// FrameIndex represent objects inside a abstract stack.
// We must replace FrameIndex with an stack/frame pointer
// direct reference.
void NewcpuRegisterInfo::
eliminateFrameIndex(MachineBasicBlock::iterator II, int SPAdj,
                    unsigned FIOperandNum, RegScavenger *RS) const {
  MachineInstr &MI = *II;
  MachineFunction &MF = *MI.getParent()->getParent();
  MachineFrameInfo *MFI = MF.getFrameInfo();
  unsigned OFIOperandNum = FIOperandNum == 2 ? 1 : 2;

  DEBUG(dbgs() << "\nFunction : " << MF.getName() << "\n";
        dbgs() << "<--------->\n" << MI);

  int FrameIndex = MI.getOperand(FIOperandNum).getIndex();
  int stackSize  = MFI->getStackSize();
  int spOffset   = MFI->getObjectOffset(FrameIndex);

  DEBUG(NewcpuFunctionInfo *NewcpuFI = MF.getInfo<NewcpuFunctionInfo>();
        dbgs() << "FrameIndex : " << FrameIndex << "\n"
               << "spOffset   : " << spOffset << "\n"
               << "stackSize  : " << stackSize << "\n"
               << "isFixed    : " << MFI->isFixedObjectIndex(FrameIndex) << "\n"
               << "isLiveIn   : " << NewcpuFI->isLiveIn(FrameIndex) << "\n"
               << "isSpill    : " << MFI->isSpillSlotObjectIndex(FrameIndex)
               << "\n" );

  // as explained on LowerFormalArguments, detect negative offsets
  // and adjust SPOffsets considering the final stack size.
  int Offset = (spOffset < 0) ? (stackSize - spOffset) : spOffset;
  Offset += MI.getOperand(OFIOperandNum).getImm();

  DEBUG(dbgs() << "Offset     : " << Offset << "\n" << "<--------->\n");

  MI.getOperand(OFIOperandNum).ChangeToImmediate(Offset);
  MI.getOperand(FIOperandNum).ChangeToRegister(getFrameRegister(MF), false);
}

void NewcpuRegisterInfo::
processFunctionBeforeFrameFinalized(MachineFunction &MF, RegScavenger *) const {
  // Set the stack offset where GP must be saved/loaded from.
  MachineFrameInfo *MFI = MF.getFrameInfo();
  NewcpuFunctionInfo *NewcpuFI = MF.getInfo<NewcpuFunctionInfo>();
  if (NewcpuFI->needGPSaveRestore())
    MFI->setObjectOffset(NewcpuFI->getGPFI(), NewcpuFI->getGPStackOffset());
}

unsigned NewcpuRegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  const TargetFrameLowering *TFI = MF.getTarget().getFrameLowering();

  return Newcpu::r14;
}

unsigned NewcpuRegisterInfo::getEHExceptionRegister() const {
  llvm_unreachable("What is the exception register");
}

unsigned NewcpuRegisterInfo::getEHHandlerRegister() const {
  llvm_unreachable("What is the exception handler register");
}
