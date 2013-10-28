//===-- XTCRegisterInfo.cpp - XTC Register Information --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the XTC implementation of the TargetRegisterInfo
// class.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "xtc-frame-info"

#include "XTCRegisterInfo.h"
#include "XTC.h"
#include "XTCMachineFunction.h"
#include "XTCSubtarget.h"
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
#include "XTCGenRegisterInfo.inc"

using namespace llvm;

XTCRegisterInfo::
XTCRegisterInfo(const XTCSubtarget &ST, const TargetInstrInfo &tii)
  : XTCGenRegisterInfo(XTC::r7), Subtarget(ST), TII(tii) {}

unsigned XTCRegisterInfo::getPICCallReg() {
  return XTC::r2;
}

const TargetRegisterClass *
XTCRegisterInfo::getPointerRegClass(const MachineFunction &MF, unsigned Kind)
                                                                       const {
  return &XTC::GPRegsRegClass;
}


//===----------------------------------------------------------------------===//
// Callee Saved Registers methods
//===----------------------------------------------------------------------===//

/// XTC Callee Saved Registers
const uint16_t* XTCRegisterInfo::
getCalleeSavedRegs(const MachineFunction *MF) const {
  // XTC callee-save register range is R20 - R31
  static const uint16_t CalleeSavedRegs[] = {
      XTC::r4, XTC::r5, XTC::r6,
      0
  };

  return CalleeSavedRegs;
}

BitVector XTCRegisterInfo::
getReservedRegs(const MachineFunction &MF) const {
  BitVector Reserved(getNumRegs());
  Reserved.set(XTC::r0);
  Reserved.set(XTC::r1);

  Reserved.set(XTC::r14);
  Reserved.set(XTC::r15);
  return Reserved;

}

// FrameIndex represent objects inside a abstract stack.
// We must replace FrameIndex with an stack/frame pointer
// direct reference.
void XTCRegisterInfo::
eliminateFrameIndex(MachineBasicBlock::iterator II, int SPAdj,
                    unsigned FIOperandNum, RegScavenger *RS) const {
  MachineInstr &MI = *II;
  MachineFunction &MF = *MI.getParent()->getParent();
  MachineFrameInfo *MFI = MF.getFrameInfo();
  unsigned OFIOperandNum = FIOperandNum + 1;
  //== 2 ? 1 : 2;

  DEBUG(dbgs() << "\nFunction : " << MF.getName() << "\n";
        dbgs() << "<--------->\n" << MI);

  int FrameIndex = MI.getOperand(FIOperandNum).getIndex();
  int stackSize  = MFI->getStackSize();
  int spOffset   = MFI->getObjectOffset(FrameIndex);

  DEBUG(XTCFunctionInfo *XTCFI = MF.getInfo<XTCFunctionInfo>();
        dbgs() << "FrameIndex : " << FrameIndex << "\n"
               << "spOffset   : " << spOffset << "\n"
               << "stackSize  : " << stackSize << "\n"
               << "isFixed    : " << MFI->isFixedObjectIndex(FrameIndex) << "\n"
               << "isLiveIn   : " << XTCFI->isLiveIn(FrameIndex) << "\n"
               << "isSpill    : " << MFI->isSpillSlotObjectIndex(FrameIndex)
               << "\n" );

  // as explained on LowerFormalArguments, detect negative offsets
  // and adjust SPOffsets considering the final stack size.
  int Offset = (spOffset < 0) ? (stackSize - spOffset) : spOffset;

  MI.getOperand(OFIOperandNum).print(dbgs());

  Offset += MI.getOperand(OFIOperandNum).getImm();

  DEBUG(dbgs() << "Offset     : " << Offset << "\n" << "<--------->\n");

  MI.getOperand(OFIOperandNum).ChangeToImmediate(Offset);
  MI.getOperand(FIOperandNum).ChangeToRegister(getFrameRegister(MF), false);
}

void XTCRegisterInfo::
processFunctionBeforeFrameFinalized(MachineFunction &MF, RegScavenger *) const {
  // Set the stack offset where GP must be saved/loaded from.
  MachineFrameInfo *MFI = MF.getFrameInfo();
  XTCFunctionInfo *XTCFI = MF.getInfo<XTCFunctionInfo>();
  if (XTCFI->needGPSaveRestore())
    MFI->setObjectOffset(XTCFI->getGPFI(), XTCFI->getGPStackOffset());
}

unsigned XTCRegisterInfo::getFrameRegister(const MachineFunction &MF) const {
  const TargetFrameLowering *TFI = MF.getTarget().getFrameLowering();

  return XTC::r14;
}

unsigned XTCRegisterInfo::getEHExceptionRegister() const {
  llvm_unreachable("What is the exception register");
}

unsigned XTCRegisterInfo::getEHHandlerRegister() const {
  llvm_unreachable("What is the exception handler register");
}
