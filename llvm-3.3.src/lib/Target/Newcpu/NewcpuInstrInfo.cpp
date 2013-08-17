//===-- NewcpuInstrInfo.cpp - Newcpu Instruction Information --------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file contains the Newcpu implementation of the TargetInstrInfo class.
//
//===----------------------------------------------------------------------===//

#include "NewcpuInstrInfo.h"
#include "NewcpuMachineFunction.h"
#include "NewcpuTargetMachine.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/ScoreboardHazardRecognizer.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/Debug.h"
#define GET_INSTRINFO_CTOR
#include "NewcpuGenInstrInfo.inc"

using namespace llvm;

NewcpuInstrInfo::NewcpuInstrInfo(NewcpuTargetMachine &tm)
  : NewcpuGenInstrInfo(),//Newcpu::ADJCALLSTACKDOWN, Newcpu::ADJCALLSTACKUP),
    TM(tm), RI(*TM.getSubtargetImpl(), *this) {}

static bool isZeroImm(const MachineOperand &op) {
  return op.isImm() && op.getImm() == 0;
}

/// isLoadFromStackSlot - If the specified machine instruction is a direct
/// load from a stack slot, return the virtual or physical register number of
/// the destination along with the FrameIndex of the loaded stack slot.  If
/// not, return 0.  This predicate must return 0 if the instruction has
/// any side effects other than loading from the stack slot.
unsigned NewcpuInstrInfo::
isLoadFromStackSlot(const MachineInstr *MI, int &FrameIndex) const {

    if (MI->getOpcode() == Newcpu::LDI) {
    if ((MI->getOperand(1).isFI()) && // is a stack slot
        (MI->getOperand(2).isImm()) &&  // the imm is zero
        (isZeroImm(MI->getOperand(2)))) {
      FrameIndex = MI->getOperand(1).getIndex();
      return MI->getOperand(0).getReg();
    }
    }

  return 0;
}

/// isStoreToStackSlot - If the specified machine instruction is a direct
/// store to a stack slot, return the virtual or physical register number of
/// the source reg along with the FrameIndex of the loaded stack slot.  If
/// not, return 0.  This predicate must return 0 if the instruction has
/// any side effects other than storing to the stack slot.
unsigned NewcpuInstrInfo::
isStoreToStackSlot(const MachineInstr *MI, int &FrameIndex) const {
    if (MI->getOpcode() == Newcpu::STI) {
    if ((MI->getOperand(1).isFI()) && // is a stack slot
        (MI->getOperand(2).isImm()) &&  // the imm is zero
        (isZeroImm(MI->getOperand(2)))) {
      FrameIndex = MI->getOperand(1).getIndex();
      return MI->getOperand(0).getReg();
    }
    }
  return 0;
}

/// insertNoop - If data hazard condition is found insert the target nop
/// instruction.
void NewcpuInstrInfo::
insertNoop(MachineBasicBlock &MBB, MachineBasicBlock::iterator MI) const {
    DebugLoc DL;
    BuildMI(MBB, MI, DL, get(Newcpu::NOP));
}

void NewcpuInstrInfo::
copyPhysReg(MachineBasicBlock &MBB,
            MachineBasicBlock::iterator I, DebugLoc DL,
            unsigned DestReg, unsigned SrcReg,
            bool KillSrc) const {

    DEBUG(dbgs()<<"Copy from reg "<<SrcReg<<" to reg "<<DestReg<<"\n");
    llvm::BuildMI(MBB, I, DL, get(Newcpu::MOVRA), DestReg)
        .addReg(SrcReg, getKillRegState(KillSrc));
}

void NewcpuInstrInfo::
storeRegToStackSlot(MachineBasicBlock &MBB, MachineBasicBlock::iterator I,
                    unsigned SrcReg, bool isKill, int FI,
                    const TargetRegisterClass *RC,
                    const TargetRegisterInfo *TRI) const {

    DebugLoc DL;
    if (RC == &Newcpu::GPRegsRegClass) {
        BuildMI(MBB, I, DL, get(Newcpu::STMRI)).addReg(SrcReg,getKillRegState(isKill))
            .addFrameIndex(FI).addImm(0); //.addFrameIndex(FI);
    } else if (RC==&Newcpu::ARegsRegClass) {
        BuildMI(MBB, I, DL, get(Newcpu::STMAI)).addReg(SrcReg,getKillRegState(isKill))
            .addFrameIndex(FI).addImm(0); //.addFrameIndex(FI);

    } else {
        llvm_unreachable("Cannot store this register to stack slot");
    }
}

void NewcpuInstrInfo::
loadRegFromStackSlot(MachineBasicBlock &MBB, MachineBasicBlock::iterator I,
                     unsigned DestReg, int FI,
                     const TargetRegisterClass *RC,
                     const TargetRegisterInfo *TRI) const {

    DebugLoc DL;

    if (RC == &Newcpu::GPRegsRegClass) {
        BuildMI(MBB, I, DL, get(Newcpu::LDI), DestReg)
            .addFrameIndex(FI).addImm(0); //.addFrameIndex(FI);
    } else if (RC==&Newcpu::ARegsRegClass) {
        BuildMI(MBB, I, DL, get(Newcpu::LDAI), DestReg)
            .addFrameIndex(FI).addImm(0); //.addFrameIndex(FI);
    } else {
        llvm_unreachable("Cannot load this register from stack slot");
    }
}

//===----------------------------------------------------------------------===//
// Branch Analysis
//===----------------------------------------------------------------------===//
bool NewcpuInstrInfo::AnalyzeBranch(MachineBasicBlock &MBB,
                                    MachineBasicBlock *&TBB,
                                    MachineBasicBlock *&FBB,
                                    SmallVectorImpl<MachineOperand> &Cond,
                                    bool AllowModify) const {
  // If the block has no terminators, it just falls into the block after it.
  MachineBasicBlock::iterator I = MBB.end();
  if (I == MBB.begin())
    return false;
  --I;
  while (I->isDebugValue()) {
    if (I == MBB.begin())
      return false;
    --I;
  }
  if (!isUnpredicatedTerminator(I))
    return false;

  // Get the last instruction in the block.
  MachineInstr *LastInst = I;

  // If there is only one terminator instruction, process it.
  unsigned LastOpc = LastInst->getOpcode();
  if (I == MBB.begin() || !isUnpredicatedTerminator(--I)) {
    if (Newcpu::isUncondBranchOpcode(LastOpc)) {
      TBB = LastInst->getOperand(0).getMBB();
      return false;
    }
    if (Newcpu::isCondBranchOpcode(LastOpc)) {
      // Block ends with fall-through condbranch.
      TBB = LastInst->getOperand(1).getMBB();
      Cond.push_back(MachineOperand::CreateImm(LastInst->getOpcode()));
      Cond.push_back(LastInst->getOperand(0));
      return false;
    }
    // Otherwise, don't know what this is.
    return true;
  }

  // Get the instruction before it if it's a terminator.
  MachineInstr *SecondLastInst = I;

  // If there are three terminators, we don't know what sort of block this is.
  if (SecondLastInst && I != MBB.begin() && isUnpredicatedTerminator(--I))
    return true;

  // If the block ends with something like BEQID then BRID, handle it.
  if (Newcpu::isCondBranchOpcode(SecondLastInst->getOpcode()) &&
      Newcpu::isUncondBranchOpcode(LastInst->getOpcode())) {
    TBB = SecondLastInst->getOperand(1).getMBB();
    Cond.push_back(MachineOperand::CreateImm(SecondLastInst->getOpcode()));
    Cond.push_back(SecondLastInst->getOperand(0));
    FBB = LastInst->getOperand(0).getMBB();
    return false;
  }

  // If the block ends with two unconditional branches, handle it.
  // The second one is not executed, so remove it.
  if (Newcpu::isUncondBranchOpcode(SecondLastInst->getOpcode()) &&
      Newcpu::isUncondBranchOpcode(LastInst->getOpcode())) {
    TBB = SecondLastInst->getOperand(0).getMBB();
    I = LastInst;
    if (AllowModify)
      I->eraseFromParent();
    return false;
  }

  // Otherwise, can't handle this.
  return true;
}

unsigned NewcpuInstrInfo::
InsertBranch(MachineBasicBlock &MBB, MachineBasicBlock *TBB,
             MachineBasicBlock *FBB,
             const SmallVectorImpl<MachineOperand> &Cond,
             DebugLoc DL) const {
#if 0
    // Shouldn't be a fall through.
  assert(TBB && "InsertBranch must not be told to insert a fallthrough");
  assert((Cond.size() == 2 || Cond.size() == 0) &&
         "Newcpu branch conditions have two components!");

  unsigned Opc = Newcpu::BRID;
  if (!Cond.empty())
    Opc = (unsigned)Cond[0].getImm();

  if (FBB == 0) {
    if (Cond.empty()) // Unconditional branch
      BuildMI(&MBB, DL, get(Opc)).addMBB(TBB);
    else              // Conditional branch
      BuildMI(&MBB, DL, get(Opc)).addReg(Cond[1].getReg()).addMBB(TBB);
    return 1;
  }

  BuildMI(&MBB, DL, get(Opc)).addReg(Cond[1].getReg()).addMBB(TBB);
  BuildMI(&MBB, DL, get(Newcpu::BRID)).addMBB(FBB);
#endif
  return 2;
}

unsigned NewcpuInstrInfo::RemoveBranch(MachineBasicBlock &MBB) const {
  MachineBasicBlock::iterator I = MBB.end();
  if (I == MBB.begin()) return 0;
  --I;
  while (I->isDebugValue()) {
    if (I == MBB.begin())
      return 0;
    --I;
  }

  if (!Newcpu::isUncondBranchOpcode(I->getOpcode()) &&
      !Newcpu::isCondBranchOpcode(I->getOpcode()))
    return 0;

  // Remove the branch.
  I->eraseFromParent();

  I = MBB.end();

  if (I == MBB.begin()) return 1;
  --I;
  if (!Newcpu::isCondBranchOpcode(I->getOpcode()))
    return 1;

  // Remove the branch.
  I->eraseFromParent();
  return 2;
}

bool NewcpuInstrInfo::ReverseBranchCondition(SmallVectorImpl<MachineOperand>
                                               &Cond) const {
  assert(Cond.size() == 2 && "Invalid Newcpu branch opcode!");
  switch (Cond[0].getImm()) {
  default:            return true;
#if 0
  case Newcpu::BEQ:   Cond[0].setImm(Newcpu::BNE); return false;
  case Newcpu::BNE:   Cond[0].setImm(Newcpu::BEQ); return false;
  case Newcpu::BGT:   Cond[0].setImm(Newcpu::BLE); return false;
  case Newcpu::BGE:   Cond[0].setImm(Newcpu::BLT); return false;
  case Newcpu::BLT:   Cond[0].setImm(Newcpu::BGE); return false;
  case Newcpu::BLE:   Cond[0].setImm(Newcpu::BGT); return false;
  case Newcpu::BEQI:  Cond[0].setImm(Newcpu::BNEI); return false;
  case Newcpu::BNEI:  Cond[0].setImm(Newcpu::BEQI); return false;
  case Newcpu::BGTI:  Cond[0].setImm(Newcpu::BLEI); return false;
  case Newcpu::BGEI:  Cond[0].setImm(Newcpu::BLTI); return false;
  case Newcpu::BLTI:  Cond[0].setImm(Newcpu::BGEI); return false;
  case Newcpu::BLEI:  Cond[0].setImm(Newcpu::BGTI); return false;
  case Newcpu::BEQD:  Cond[0].setImm(Newcpu::BNED); return false;
  case Newcpu::BNED:  Cond[0].setImm(Newcpu::BEQD); return false;
  case Newcpu::BGTD:  Cond[0].setImm(Newcpu::BLED); return false;
  case Newcpu::BGED:  Cond[0].setImm(Newcpu::BLTD); return false;
  case Newcpu::BLTD:  Cond[0].setImm(Newcpu::BGED); return false;
  case Newcpu::BLED:  Cond[0].setImm(Newcpu::BGTD); return false;
  case Newcpu::BEQID: Cond[0].setImm(Newcpu::BNEID); return false;
  case Newcpu::BNEID: Cond[0].setImm(Newcpu::BEQID); return false;
  case Newcpu::BGTID: Cond[0].setImm(Newcpu::BLEID); return false;
  case Newcpu::BGEID: Cond[0].setImm(Newcpu::BLTID); return false;
  case Newcpu::BLTID: Cond[0].setImm(Newcpu::BGEID); return false;
  case Newcpu::BLEID: Cond[0].setImm(Newcpu::BGTID); return false;
#endif
  }
}

/// getGlobalBaseReg - Return a virtual register initialized with the
/// the global base register value. Output instructions required to
/// initialize the register in the function entry block, if necessary.
///
unsigned NewcpuInstrInfo::getGlobalBaseReg(MachineFunction *MF) const {

    NewcpuFunctionInfo *NewcpuFI = MF->getInfo<NewcpuFunctionInfo>();
  unsigned GlobalBaseReg = NewcpuFI->getGlobalBaseReg();
  if (GlobalBaseReg != 0)
    return GlobalBaseReg;

  llvm_unreachable("Cannot get Global Base Reg yet");

  // Insert the set of GlobalBaseReg into the first MBB of the function
  MachineBasicBlock &FirstMBB = MF->front();
  MachineBasicBlock::iterator MBBI = FirstMBB.begin();
  MachineRegisterInfo &RegInfo = MF->getRegInfo();
  const TargetInstrInfo *TII = MF->getTarget().getInstrInfo();

  GlobalBaseReg = RegInfo.createVirtualRegister(&Newcpu::GPRegsRegClass);

  BuildMI(FirstMBB, MBBI, DebugLoc(), TII->get(TargetOpcode::COPY),
          GlobalBaseReg).addReg(Newcpu::R6);
  RegInfo.addLiveIn(Newcpu::R6);

  NewcpuFI->setGlobalBaseReg(GlobalBaseReg);
  return GlobalBaseReg;
}
