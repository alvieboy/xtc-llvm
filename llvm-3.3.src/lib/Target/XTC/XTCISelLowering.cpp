//===-- XTCISelLowering.cpp - XTC DAG Lowering Implementation -------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the interfaces that XTC uses to lower LLVM code into a
// selection DAG.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "xtc-lower"
#include "XTCISelLowering.h"
#include "XTCMachineFunction.h"
#include "XTCSubtarget.h"
#include "XTCTargetMachine.h"
#include "XTCTargetObjectFile.h"
#include "llvm/CodeGen/CallingConvLower.h"
#include "llvm/CodeGen/MachineFrameInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/SelectionDAGISel.h"
#include "llvm/CodeGen/ValueTypes.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include <stdio.h>
using namespace llvm;

static bool CC_XTC_AssignReg(unsigned &ValNo, MVT &ValVT, MVT &LocVT,
                                CCValAssign::LocInfo &LocInfo,
                                ISD::ArgFlagsTy &ArgFlags,
                                CCState &State);

const char *llvm::XTCCC::XTCCCToString(CC cc) {
    switch (cc) {
    default:
        DEBUG(dbgs()<<"Unknown "<<(int)cc);
        llvm_unreachable("Unknown condition code");
    case EQ: return "eq";
    case NE: return "ne";
    case GT: return "gt";
    case LT: return "lt";
    case GE: return "ge";
    case LE: return "le";
    case ULE: return "ule";
    case UGE: return "uge";
    case ULT: return "ult";
    case UGT: return "ugt";
    }
}

const char *XTCTargetLowering::getTargetNodeName(unsigned Opcode) const {

    switch (Opcode) {
    case XTCISD::JmpLink    : return "XTCISD::JmpLink";
    case XTCISD::GPRel      : return "XTCISD::GPRel";
    case XTCISD::Wrap       : return "XTCISD::Wrap";
    case XTCISD::Ret        : return "XTCISD::Ret";
    case XTCISD::SELECT_CC  : return "XTCISD::SELECT_CC";
    case XTCISD::ADD        : return "XTCISD::ADD";
    case XTCISD::SUB        : return "XTCISD::SUB";
    case XTCISD::ADDC       : return "XTCISD::ADDC";
    case XTCISD::SBB        : return "XTCISD::SBB";
    case XTCISD::XOR        : return "XTCISD::XOR";
    case XTCISD::AND        : return "XTCISD::AND";
    case XTCISD::BRCOND     : return "XTCISD::BRCOND";
    case XTCISD::SETCC      : return "XTCISD::SETCC";
    case XTCISD::CMP        : return "XTCISD::CMP";
    case XTCISD::BR_CC      : return "XTCISD::BR_CC";
    case XTCISD::CALL       : return "XTCISD::CALL";
    default                    :
      llvm_unreachable("Unknown node name");
      return NULL;
  }
}

XTCTargetLowering::XTCTargetLowering(XTCTargetMachine &TM)
: TargetLowering(TM, new XTCTargetObjectFile()) {

    DEBUG(dbgs()<<__PRETTY_FUNCTION__<<" called"<<"\n");
  Subtarget = &TM.getSubtarget<XTCSubtarget>();

  // XTC does not have i1 type, so use i32 for
  // setcc operations results (slt, sgt, ...).
  setBooleanContents(ZeroOrOneBooleanContent);
  setBooleanVectorContents(ZeroOrOneBooleanContent); // FIXME: Is this correct?

  // Set up the register classes
  addRegisterClass(MVT::i32, &XTC::GPRegsRegClass);
  //addRegisterClass(MVT::i32, &XTC::SPRegsRegClass);
#if 0
  if (Subtarget->hasFPU()) {
    addRegisterClass(MVT::f32, &XTC::GPRegsRegClass);
    setOperationAction(ISD::ConstantFP, MVT::f32, Legal);
  }

#endif
     /*
  setIndexedLoadAction(ISD::PRE_INC, MVT::i8, Legal);
  setIndexedLoadAction(ISD::PRE_INC, MVT::i16, Legal);
  setIndexedLoadAction(ISD::PRE_INC, MVT::i32, Legal);
  setIndexedLoadAction(ISD::POST_INC, MVT::i8, Legal);
  setIndexedLoadAction(ISD::POST_INC, MVT::i16, Legal);
  setIndexedLoadAction(ISD::POST_INC, MVT::i32, Legal);
  setIndexedLoadAction(ISD::PRE_DEC, MVT::i8, Legal);
  setIndexedLoadAction(ISD::PRE_DEC, MVT::i16, Legal);
  setIndexedLoadAction(ISD::PRE_DEC, MVT::i32, Legal);
  setIndexedLoadAction(ISD::POST_DEC, MVT::i8, Legal);
  setIndexedLoadAction(ISD::POST_DEC, MVT::i16, Legal);
  setIndexedLoadAction(ISD::POST_DEC, MVT::i32, Legal);

  setIndexedStoreAction(ISD::PRE_INC, MVT::i8, Legal);
  setIndexedStoreAction(ISD::PRE_INC, MVT::i16, Legal);
  setIndexedStoreAction(ISD::PRE_INC, MVT::i32, Legal);
  setIndexedStoreAction(ISD::POST_INC, MVT::i8, Legal);
  setIndexedStoreAction(ISD::POST_INC, MVT::i16, Legal);
  setIndexedStoreAction(ISD::POST_INC, MVT::i32, Legal);
  setIndexedStoreAction(ISD::PRE_DEC, MVT::i8, Legal);
  setIndexedStoreAction(ISD::PRE_DEC, MVT::i16, Legal);
  setIndexedStoreAction(ISD::PRE_DEC, MVT::i32, Legal);
  setIndexedStoreAction(ISD::POST_DEC, MVT::i8, Legal);
  setIndexedStoreAction(ISD::POST_DEC, MVT::i16, Legal);
  setIndexedStoreAction(ISD::POST_DEC, MVT::i32, Legal);
       */
  // Floating point operations which are not supported
  setOperationAction(ISD::FREM,       MVT::f32, Expand);
  setOperationAction(ISD::FMA,        MVT::f32, Expand);
  setOperationAction(ISD::UINT_TO_FP, MVT::i8,  Expand);
  setOperationAction(ISD::UINT_TO_FP, MVT::i16, Expand);
  setOperationAction(ISD::UINT_TO_FP, MVT::i32, Expand);
  setOperationAction(ISD::FP_TO_UINT, MVT::i32, Expand);
  setOperationAction(ISD::FP_ROUND,   MVT::f32, Expand);
  setOperationAction(ISD::FP_ROUND,   MVT::f64, Expand);
  setOperationAction(ISD::FCOPYSIGN,  MVT::f32, Expand);
  setOperationAction(ISD::FCOPYSIGN,  MVT::f64, Expand);
  setOperationAction(ISD::FSIN,       MVT::f32, Expand);
  setOperationAction(ISD::FCOS,       MVT::f32, Expand);
  setOperationAction(ISD::FSINCOS,    MVT::f32, Expand);
  setOperationAction(ISD::FPOWI,      MVT::f32, Expand);
  setOperationAction(ISD::FPOW,       MVT::f32, Expand);
  setOperationAction(ISD::FLOG,       MVT::f32, Expand);
  setOperationAction(ISD::FLOG2,      MVT::f32, Expand);
  setOperationAction(ISD::FLOG10,     MVT::f32, Expand);
  setOperationAction(ISD::FEXP,       MVT::f32, Expand);

  // Load extented operations for i1 types must be promoted
  setLoadExtAction(ISD::EXTLOAD,  MVT::i1,  Promote);
  setLoadExtAction(ISD::ZEXTLOAD, MVT::i1,  Promote);
  setLoadExtAction(ISD::SEXTLOAD, MVT::i1,  Promote);

  // Sign extended loads must be expanded
  setLoadExtAction(ISD::SEXTLOAD, MVT::i8, Expand);
  setLoadExtAction(ISD::SEXTLOAD, MVT::i16, Expand);

  // XTC has no REM or DIVREM operations.
  setOperationAction(ISD::UREM,    MVT::i32, Expand);
  setOperationAction(ISD::SREM,    MVT::i32, Expand);
  setOperationAction(ISD::SDIVREM, MVT::i32, Expand);
  setOperationAction(ISD::UDIVREM, MVT::i32, Expand);

  // If the processor doesn't support multiply then expand it
  if (!Subtarget->hasMul()) {
    setOperationAction(ISD::MUL, MVT::i32, Expand);
  }

  // If the processor doesn't support 64-bit multiply then expand
  //if (!Subtarget->hasMul() || !Subtarget->hasMul64()) {
    setOperationAction(ISD::MULHS, MVT::i32, Expand);
    setOperationAction(ISD::MULHS, MVT::i64, Expand);
    setOperationAction(ISD::MULHU, MVT::i32, Expand);
    setOperationAction(ISD::MULHU, MVT::i64, Expand);
//  }

  // If the processor doesn't support division then expand
//  if (!Subtarget->hasDiv()) {
    setOperationAction(ISD::UDIV, MVT::i32, Expand);
    setOperationAction(ISD::SDIV, MVT::i32, Expand);
 // }

  // Expand unsupported conversions
  setOperationAction(ISD::BITCAST, MVT::f32, Expand);
  setOperationAction(ISD::BITCAST, MVT::i32, Expand);

  // XTC doesn't have MUL_LOHI
  setOperationAction(ISD::SMUL_LOHI, MVT::i32, Expand);
  setOperationAction(ISD::UMUL_LOHI, MVT::i32, Expand);
  setOperationAction(ISD::SMUL_LOHI, MVT::i64, Expand);
  setOperationAction(ISD::UMUL_LOHI, MVT::i64, Expand);

  setOperationAction(ISD::BRCOND           , MVT::Other, Expand);
  setOperationAction(ISD::BRCOND           , MVT::i32, Expand);
  setOperationAction(ISD::SELECT           , MVT::Other, Expand);
  setOperationAction(ISD::SELECT           , MVT::i32, Expand);

  setOperationAction(ISD::SELECT_CC, MVT::Other, Custom);
  setOperationAction(ISD::SELECT_CC, MVT::i32, Custom);


  setOperationAction(ISD::SETCC            , MVT::i1,  Promote);
  setOperationAction(ISD::SETCC            , MVT::i8,  Promote);
  setOperationAction(ISD::SETCC            , MVT::i16, Promote);

  setOperationAction(ISD::SETCC            , MVT::i32, Expand);

  // Used by legalize types to correctly generate the setcc result.
  // Without this, every float setcc comes with a AND/OR with the result,
  // we don't want this, since the fpcmp result goes to a flag register,
  // which is used implicitly by brcond and select operations.

  setOperationAction(ISD::SELECT          , MVT::i1   , Promote);
  setOperationAction(ISD::SELECT_CC       , MVT::i1   , Promote);
  setOperationAction(ISD::BRCOND          , MVT::i1   , Promote);
  setOperationAction(ISD::SETCC           , MVT::i1   , Promote);

  AddPromotedToType(ISD::SETCC, MVT::i1, MVT::i32);
  AddPromotedToType(ISD::BRCOND, MVT::i1, MVT::i32);
  AddPromotedToType(ISD::SELECT, MVT::i1, MVT::i32);
  AddPromotedToType(ISD::SELECT_CC, MVT::i1, MVT::i32);

  AddPromotedToType(ISD::SETCC, MVT::i8, MVT::i32);
  AddPromotedToType(ISD::BRCOND, MVT::i8, MVT::i32);
  AddPromotedToType(ISD::SELECT, MVT::i8, MVT::i32);
  AddPromotedToType(ISD::SELECT_CC, MVT::i8, MVT::i32);

  // XTC Custom Operations
  setOperationAction(ISD::GlobalAddress,      MVT::i32,   Custom);
  setOperationAction(ISD::GlobalTLSAddress,   MVT::i32,   Custom);

  setOperationAction(ISD::JumpTable,          MVT::i32,   Expand);

  setOperationAction(ISD::ConstantPool,       MVT::i32,   Custom);

  // Variable Argument support
  setOperationAction(ISD::VASTART,            MVT::Other, Custom);
  setOperationAction(ISD::VAEND,              MVT::Other, Expand);
  setOperationAction(ISD::VAARG,              MVT::Other, Expand);
  setOperationAction(ISD::VACOPY,             MVT::Other, Expand);


  //setOperationAction(ISD::ADD,             MVT::i32, Custom);


  // Operations not directly supported by XTC.
  setOperationAction(ISD::DYNAMIC_STACKALLOC, MVT::i32,   Expand);
  setOperationAction(ISD::BR_JT,              MVT::Other, Expand);
  setOperationAction(ISD::BR_CC,              MVT::f32,   Expand);
  setOperationAction(ISD::BR_CC,              MVT::i32,   Custom);
  setOperationAction(ISD::SIGN_EXTEND_INREG,  MVT::i1,    Expand);
  setOperationAction(ISD::ROTL,               MVT::i32,   Expand);
  setOperationAction(ISD::ROTR,               MVT::i32,   Expand);
  setOperationAction(ISD::SHL_PARTS,          MVT::i32,   Expand);
  setOperationAction(ISD::SRA_PARTS,          MVT::i32,   Expand);
  setOperationAction(ISD::SRL_PARTS,          MVT::i32,   Expand);
  setOperationAction(ISD::CTLZ,               MVT::i32,   Expand);
  setOperationAction(ISD::CTLZ_ZERO_UNDEF,    MVT::i32,   Expand);
  setOperationAction(ISD::CTTZ,               MVT::i32,   Expand);
  setOperationAction(ISD::CTTZ_ZERO_UNDEF,    MVT::i32,   Expand);
  setOperationAction(ISD::CTPOP,              MVT::i32,   Expand);
  setOperationAction(ISD::BSWAP,              MVT::i32,   Expand);
  // We don't have line number support yet.
  setOperationAction(ISD::EH_LABEL,          MVT::Other, Expand);

  // Use the default for now
  setOperationAction(ISD::STACKSAVE,         MVT::Other, Expand);
  setOperationAction(ISD::STACKRESTORE,      MVT::Other, Expand);

  // XTC doesn't have extending float->double load/store
  setLoadExtAction(ISD::EXTLOAD, MVT::f32, Expand);
  setTruncStoreAction(MVT::f64, MVT::f32, Expand);

  setMinFunctionAlignment(2);
  setSupportJumpTables(false);

  setStackPointerRegisterToSaveRestore(XTC::r15);
  computeRegisterProperties();
}


static unsigned TranslateToXTCCC(ISD::CondCode SetCCOpcode,
                                    SDValue &LHS, SDValue &RHS,
                                    SelectionDAG &DAG) {
    switch (SetCCOpcode) {
    default: llvm_unreachable("Invalid integer condition!");
    case ISD::SETEQ:  return XTCCC::EQ;
    case ISD::SETGT:  return XTCCC::GT;
    case ISD::SETGE:  return XTCCC::GE;
    case ISD::SETLT:  return XTCCC::LT;
    case ISD::SETLE:  return XTCCC::LE;
    case ISD::SETNE:  return XTCCC::NE;

    case ISD::SETULT: return XTCCC::ULT;
    case ISD::SETUGT: return XTCCC::UGT;
    case ISD::SETULE: return XTCCC::ULE;
    case ISD::SETUGE: return XTCCC::UGE;

    }
}


EVT XTCTargetLowering::getSetCCResultType(EVT VT) const {
    if (!VT.isVector()) return MVT::i32;
    return VT.changeVectorElementTypeToInteger();
}

SDValue XTCTargetLowering::LowerArith(SDValue Op,
                                         SelectionDAG &DAG) const {

    EVT VT = Op.getNode()->getValueType(0);
    SDVTList VTs = DAG.getVTList(VT, MVT::i32);

    unsigned Opc;
    switch (Op.getOpcode()) {
    default: llvm_unreachable("Invalid code");
    case ISD::ADD: Opc = XTCISD::ADD; break;
    case ISD::SUB: Opc = XTCISD::SUB; break;
    }

    return DAG.getNode(Opc, Op->getDebugLoc(), VTs, Op.getOperand(0),
                       Op.getOperand(1));

}

SDValue XTCTargetLowering::EmitCmp(SDValue Op0, SDValue Op1, unsigned XTCCC,
                                      SelectionDAG &DAG) const {
/*    if (ConstantSDNode *C = dyn_cast<ConstantSDNode>(Op1))
        if (C->getAPIntValue() == 0)
            return EmitTest(Op0, X86CC, DAG);
            */

    if (Op0.getValueType() != MVT::i32 ||
        Op1.getValueType() != MVT::i32) {
        llvm_unreachable("Cannot compare different types");
    }
    DebugLoc dl = Op0.getDebugLoc();
/*    if ((Op0.getValueType() == MVT::i8 || Op0.getValueType() == MVT::i16 ||
         Op0.getValueType() == MVT::i32 || Op0.getValueType() == MVT::i64)) {
        // Use SUB instead of CMP to enable CSE between SUB and CMP.
        SDVTList VTs = DAG.getVTList(Op0.getValueType(), MVT::i32);
        SDValue Sub = DAG.getNode(XTCISD::CMP, dl, VTs,
                                  Op0, Op1);
        return SDValue(Sub.getNode(), 1);
        }
        */
    return DAG.getNode(XTCISD::CMP, dl, MVT::Glue, Op0, Op1);
}


SDValue XTCTargetLowering::LowerSETCC(SDValue Op,
                                         SelectionDAG &DAG) const {

    MVT VT = Op.getValueType().getSimpleVT();

    DEBUG(dbgs()<<"Lowering SETCC\n");

    if (VT.isVector()) {
        llvm_unreachable("No lower for vector setcc");
    }

    DEBUG(dbgs()<<"Op: \n");
    Op.dump();
    Op.getOperand(0).dump();
    Op.getOperand(1).dump();
    Op.getOperand(2).dump();

    assert(VT == MVT::i32 && "SetCC type must be 32-bit integer");

    SDValue Op0 = Op.getOperand(0);
    SDValue Op1 = Op.getOperand(1);
    DebugLoc dl = Op.getDebugLoc();
    ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(2))->get();

    DEBUG(dbgs()<<"Condcode "<<CC<<"\n");

    unsigned XTCCC = TranslateToXTCCC(CC, Op0, Op1, DAG);

    SDValue Flag = EmitCmp(Op0, Op1, XTCCC, DAG);


    SDValue Zero = DAG.getConstant(0, VT);
    SDVTList VTs = DAG.getVTList(Op.getValueType(), MVT::Glue);
    SDValue One  = DAG.getConstant(1, VT);

    SmallVector<SDValue, 4> Ops;

    Ops.push_back(One);
    Ops.push_back(Zero);
    Ops.push_back( DAG.getConstant(XTCCC, MVT::i32) );
    Ops.push_back(Flag);

    return DAG.getNode(XTCISD::SELECT_CC, dl, VTs, &Ops[0], Ops.size());

    llvm_unreachable("Not yet");
}

SDValue XTCTargetLowering::LowerBRCOND(SDValue Op,
                                         SelectionDAG &DAG) const {

    SDValue Chain = Op.getOperand(0);
    SDValue Cond  = Op.getOperand(1);
    SDValue Dest  = Op.getOperand(2);
    DebugLoc dl = Op.getDebugLoc();
    SDValue CC;

    if (Cond.getOpcode() == ISD::SETCC) {
        SDValue NewCond = LowerSETCC(Cond,DAG);
        if (NewCond.getNode())
            Cond = NewCond;
    }

    //unsigned xc =TranslateToXTCCC(DAG.getConstant(Cond, MVT::i32)));

    CC = DAG.getConstant(XTCCC::NE, MVT::i32);

    return DAG.getNode(XTCISD::BRCOND, dl, Op.getValueType(),
                       Chain, Dest, CC, Cond);
}

SDValue XTCTargetLowering::LowerBR_CC(SDValue Op,
                                         SelectionDAG &DAG) const {

    DEBUG( dbgs() << __PRETTY_FUNCTION__ << "\n");
    SDValue Chain = Op.getOperand(0);
    ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(1))->get();
    SDValue Op0 = Op.getOperand(2);
    SDValue Op1 = Op.getOperand(3);
    SDValue Dest = Op.getOperand(4);

    DebugLoc dl = Op.getDebugLoc();

    unsigned XTCCC = TranslateToXTCCC(CC, Op0, Op1, DAG);

    SDValue Flag = DAG.getNode(XTCISD::CMP, dl, MVT::Glue, Op0, Op1);

    return DAG.getNode(XTCISD::BR_CC, dl, MVT::Other,
                       Chain, Dest, DAG.getConstant(XTCCC,MVT::i32), Flag);

    llvm_unreachable("No lower BRCOND");
}

SDValue XTCTargetLowering::LowerOperation(SDValue Op,
                                             SelectionDAG &DAG) const {

    DEBUG(dbgs()<<__PRETTY_FUNCTION__<<" called"<<"\n");
    switch (Op.getOpcode())
    {
    case ISD::ConstantPool:       return LowerConstantPool(Op, DAG);
    case ISD::GlobalAddress:      return LowerGlobalAddress(Op, DAG);
    //case ISD::GlobalTLSAddress:   return LowerGlobalTLSAddress(Op, DAG);
    //case ISD::JumpTable:          return LowerJumpTable(Op, DAG);
    case ISD::SELECT_CC:          return LowerSELECT_CC(Op, DAG);
    //case ISD::SELECT:             return LowerSELECT(Op, DAG);
    case ISD::VASTART:            return LowerVASTART(Op, DAG);
    /*
     case ISD::ADD:
     case ISD::SUB:                return LowerArith(Op, DAG);
     */
    case ISD::BRCOND:             return LowerBRCOND(Op, DAG);
    case ISD::BR_CC:             return LowerBR_CC(Op, DAG);
    case ISD::SETCC:              return LowerSETCC(Op, DAG);
    default:
        llvm_unreachable("unimplemented operand");
    }
    return SDValue();
}

//===----------------------------------------------------------------------===//
//  Lower helper functions
//===----------------------------------------------------------------------===//
MachineBasicBlock*
XTCTargetLowering::EmitInstrWithCustomInserter(MachineInstr *MI,
                                                  MachineBasicBlock *MBB)
                                                  const {

DEBUG(dbgs()<<__PRETTY_FUNCTION__<<" called");
switch (MI->getOpcode()) {
  default: llvm_unreachable("Unexpected instr type to insert");
  case XTC::SELECT_CC:
    return EmitCustomSelect(MI, MBB);
#if 0
  case XTC::ShiftRL:
  case XTC::ShiftRA:
  case XTC::ShiftL:
    return EmitCustomShift(MI, MBB);

  case XTC::Select_FCC:

  case XTC::CAS32:
  case XTC::SWP32:
  case XTC::LAA32:
  case XTC::LAS32:
  case XTC::LAD32:
  case XTC::LAO32:
  case XTC::LAX32:
  case XTC::LAN32:
    return EmitCustomAtomic(MI, MBB);

  case XTC::MEMBARRIER:
    // The Microblaze does not need memory barriers. Just delete the pseudo
    // instruction and finish.
    MI->eraseFromParent();
    return MBB;
#endif
  }
}

MachineBasicBlock*
XTCTargetLowering::EmitCustomShift(MachineInstr *MI,
                                      MachineBasicBlock *MBB) const {
  const TargetInstrInfo *TII = getTargetMachine().getInstrInfo();
  DebugLoc dl = MI->getDebugLoc();
DEBUG(dbgs()<<__PRETTY_FUNCTION__<<" called");

  // To "insert" a shift left instruction, we actually have to insert a
  // simple loop.  The incoming instruction knows the destination vreg to
  // set, the source vreg to operate over and the shift amount.
  const BasicBlock *LLVM_BB = MBB->getBasicBlock();
  MachineFunction::iterator It = MBB;
  ++It;

  // start:
  //   andi     samt, samt, 31
  //   beqid    samt, finish
  //   add      dst, src, r0
  // loop:
  //   addik    samt, samt, -1
  //   sra      dst, dst
  //   bneid    samt, loop
  //   nop
  // finish:
  MachineFunction *F = MBB->getParent();
  MachineRegisterInfo &R = F->getRegInfo();
  MachineBasicBlock *loop = F->CreateMachineBasicBlock(LLVM_BB);
  MachineBasicBlock *finish = F->CreateMachineBasicBlock(LLVM_BB);
  F->insert(It, loop);
  F->insert(It, finish);

  // Update machine-CFG edges by transferring adding all successors and
  // remaining instructions from the current block to the new block which
  // will contain the Phi node for the select.
  finish->splice(finish->begin(), MBB,
                 llvm::next(MachineBasicBlock::iterator(MI)),
                 MBB->end());
  finish->transferSuccessorsAndUpdatePHIs(MBB);

  // Add the true and fallthrough blocks as its successors.
  MBB->addSuccessor(loop);
  MBB->addSuccessor(finish);

  // Next, add the finish block as a successor of the loop block
  loop->addSuccessor(finish);
  loop->addSuccessor(loop);

#if 0

  unsigned IAMT = R.createVirtualRegister(&XTC::GPRegsRegClass);
  BuildMI(MBB, dl, TII->get(XTC::ANDI), IAMT)
    .addReg(MI->getOperand(2).getReg())
    .addImm(31);

  unsigned IVAL = R.createVirtualRegister(&XTC::GPRegsRegClass);
  BuildMI(MBB, dl, TII->get(XTC::ADDIK), IVAL)
    .addReg(MI->getOperand(1).getReg())
    .addImm(0);

  BuildMI(MBB, dl, TII->get(XTC::BEQID))
    .addReg(IAMT)
    .addMBB(finish);

  unsigned DST = R.createVirtualRegister(&XTC::GPRegsRegClass);
  unsigned NDST = R.createVirtualRegister(&XTC::GPRegsRegClass);
  BuildMI(loop, dl, TII->get(XTC::PHI), DST)
    .addReg(IVAL).addMBB(MBB)
    .addReg(NDST).addMBB(loop);

  unsigned SAMT = R.createVirtualRegister(&XTC::GPRegsRegClass);
  unsigned NAMT = R.createVirtualRegister(&XTC::GPRegsRegClass);
  BuildMI(loop, dl, TII->get(XTC::PHI), SAMT)
    .addReg(IAMT).addMBB(MBB)
    .addReg(NAMT).addMBB(loop);

  if (MI->getOpcode() == XTC::ShiftL)
    BuildMI(loop, dl, TII->get(XTC::ADD), NDST).addReg(DST).addReg(DST);
  else if (MI->getOpcode() == XTC::ShiftRA)
    BuildMI(loop, dl, TII->get(XTC::SRA), NDST).addReg(DST);
  else if (MI->getOpcode() == XTC::ShiftRL)
    BuildMI(loop, dl, TII->get(XTC::SRL), NDST).addReg(DST);
  else
    llvm_unreachable("Cannot lower unknown shift instruction");

  BuildMI(loop, dl, TII->get(XTC::ADDIK), NAMT)
    .addReg(SAMT)
    .addImm(-1);

  BuildMI(loop, dl, TII->get(XTC::BNEID))
    .addReg(NAMT)
    .addMBB(loop);

  BuildMI(*finish, finish->begin(), dl,
          TII->get(XTC::PHI), MI->getOperand(0).getReg())
    .addReg(IVAL).addMBB(MBB)
    .addReg(NDST).addMBB(loop);

  // The pseudo instruction is no longer needed so remove it
  MI->eraseFromParent();

#endif
  return finish;
}

MachineBasicBlock*
XTCTargetLowering::EmitCustomSelect(MachineInstr *MI,
                                       MachineBasicBlock *MBB) const {
  const TargetInstrInfo *TII = getTargetMachine().getInstrInfo();
  DebugLoc dl = MI->getDebugLoc();
  DEBUG(dbgs()<<__PRETTY_FUNCTION__<<" called\n");

  // To "insert" a SELECT_CC instruction, we actually have to insert the
  // diamond control-flow pattern.  The incoming instruction knows the
  // destination vreg to set, the condition code register to branch on, the
  // true/false values to select between, and a branch opcode to use.
  const BasicBlock *LLVM_BB = MBB->getBasicBlock();
  MachineFunction::iterator It = MBB;
  ++It;

  //  thisMBB:
  //  ...
  //   TrueVal = ...
  //   setcc r1, r2, r3
  //   bNE   r1, r0, copy1MBB
  //   fallthrough --> copy0MBB
  MachineFunction *F = MBB->getParent();
  MachineBasicBlock *flsBB = F->CreateMachineBasicBlock(LLVM_BB);
  MachineBasicBlock *dneBB = F->CreateMachineBasicBlock(LLVM_BB);

  {
      int i;
      for (i=0;i<MI->getNumOperands();i++) {
          DEBUG(dbgs()<<"Operand "<<i<<" "<<MI->getOperand(i)<<"\n");
      }
  }


  unsigned Opc;
  DEBUG( dbgs() << "Branch condition: "<<MI->getOperand(1).getImm() <<"\n");
  /*
  switch (MI->getOperand(1).getImm()) {
  default: llvm_unreachable("Unknown branch condition");

  case XTCCC::EQ: Opc = XTC::BEQID; break;
  case XTCCC::NE: Opc = XTC::BNEID; break;
  case XTCCC::GT: Opc = XTC::BGTID; break;
  case XTCCC::LT: Opc = XTC::BLTID; break;
  case XTCCC::GE: Opc = XTC::BGEID; break;
  case XTCCC::LE: Opc = XTC::BLEID; break;

  } */
  Opc = XTC::BCOND;

  F->insert(It, flsBB);
  F->insert(It, dneBB);

  // Transfer the remainder of MBB and its successor edges to dneBB.
  dneBB->splice(dneBB->begin(), MBB,
                llvm::next(MachineBasicBlock::iterator(MI)),
                MBB->end());
  dneBB->transferSuccessorsAndUpdatePHIs(MBB);

  MBB->addSuccessor(flsBB);
  MBB->addSuccessor(dneBB);
  flsBB->addSuccessor(dneBB);

  BuildMI(MBB, dl, TII->get(Opc))
    .addMBB(dneBB).addImm(MI->getOperand(1).getImm());

  //  sinkMBB:
  //   %Result = phi [ %FalseValue, copy0MBB ], [ %TrueValue, thisMBB ]
  //  ...
  //BuildMI(dneBB, dl, TII->get(XTC::PHI), MI->getOperand(0).getReg())
  //  .addReg(MI->getOperand(1).getReg()).addMBB(flsBB)
  //  .addReg(MI->getOperand(2).getReg()).addMBB(BB);

  BuildMI(*dneBB, dneBB->begin(), dl,
          TII->get(XTC::PHI), MI->getOperand(0).getReg())
    .addReg(MI->getOperand(3).getReg()).addMBB(flsBB)
    .addReg(MI->getOperand(2).getReg()).addMBB(MBB);

  MI->eraseFromParent();   // The pseudo instruction is gone now.
  return dneBB;
}

MachineBasicBlock*
XTCTargetLowering::EmitCustomAtomic(MachineInstr *MI,
                                       MachineBasicBlock *MBB) const {
  const TargetInstrInfo *TII = getTargetMachine().getInstrInfo();
  DebugLoc dl = MI->getDebugLoc();
DEBUG(dbgs()<<__PRETTY_FUNCTION__<<" called");

  // All atomic instructions on the Microblaze are implemented using the
  // load-linked / store-conditional style atomic instruction sequences.
  // Thus, all operations will look something like the following:
  //
  //  start:
  //    lwx     RV, RP, 0
  //    <do stuff>
  //    swx     RV, RP, 0
  //    addic   RC, R0, 0
  //    bneid   RC, start
  //
  //  exit:
  //
  // To "insert" a shift left instruction, we actually have to insert a
  // simple loop.  The incoming instruction knows the destination vreg to
  // set, the source vreg to operate over and the shift amount.
  const BasicBlock *LLVM_BB = MBB->getBasicBlock();
  MachineFunction::iterator It = MBB;
  ++It;

  // start:
  //   andi     samt, samt, 31
  //   beqid    samt, finish
  //   add      dst, src, r0
  // loop:
  //   addik    samt, samt, -1
  //   sra      dst, dst
  //   bneid    samt, loop
  //   nop
  // finish:
  MachineFunction *F = MBB->getParent();
  MachineRegisterInfo &R = F->getRegInfo();

  // Create the start and exit basic blocks for the atomic operation
  MachineBasicBlock *start = F->CreateMachineBasicBlock(LLVM_BB);
  MachineBasicBlock *exit = F->CreateMachineBasicBlock(LLVM_BB);
  F->insert(It, start);
  F->insert(It, exit);

  // Update machine-CFG edges by transferring adding all successors and
  // remaining instructions from the current block to the new block which
  // will contain the Phi node for the select.
  exit->splice(exit->begin(), MBB, llvm::next(MachineBasicBlock::iterator(MI)),
               MBB->end());
  exit->transferSuccessorsAndUpdatePHIs(MBB);

  // Add the fallthrough block as its successors.
  MBB->addSuccessor(start);
#if 0

  BuildMI(start, dl, TII->get(XTC::LWX), MI->getOperand(0).getReg())
    .addReg(MI->getOperand(1).getReg())
    .addReg(XTC::R0);

  MachineBasicBlock *final = start;
  unsigned finalReg = 0;

  switch (MI->getOpcode()) {
  default: llvm_unreachable("Cannot lower unknown atomic instruction!");

  case XTC::SWP32:
    finalReg = MI->getOperand(2).getReg();
    start->addSuccessor(exit);
    start->addSuccessor(start);
    break;

  case XTC::LAN32:
  case XTC::LAX32:
  case XTC::LAO32:
  case XTC::LAD32:
  case XTC::LAS32:
  case XTC::LAA32: {
    unsigned opcode = 0;
    switch (MI->getOpcode()) {
    default: llvm_unreachable("Cannot lower unknown atomic load!");
    case XTC::LAA32: opcode = XTC::ADDIK; break;
    case XTC::LAS32: opcode = XTC::RSUBIK; break;
    case XTC::LAD32: opcode = XTC::AND; break;
    case XTC::LAO32: opcode = XTC::OR; break;
    case XTC::LAX32: opcode = XTC::XOR; break;
    case XTC::LAN32: opcode = XTC::AND; break;
    }

    finalReg = R.createVirtualRegister(&XTC::GPRegsRegClass);
    start->addSuccessor(exit);
    start->addSuccessor(start);

    BuildMI(start, dl, TII->get(opcode), finalReg)
      .addReg(MI->getOperand(0).getReg())
      .addReg(MI->getOperand(2).getReg());

    if (MI->getOpcode() == XTC::LAN32) {
      unsigned tmp = finalReg;
      finalReg = R.createVirtualRegister(&XTC::GPRegsRegClass);
      BuildMI(start, dl, TII->get(XTC::XORI), finalReg)
        .addReg(tmp)
        .addImm(-1);
    }
    break;
  }

  case XTC::CAS32: {
    finalReg = MI->getOperand(3).getReg();
    final = F->CreateMachineBasicBlock(LLVM_BB);

    F->insert(It, final);
    start->addSuccessor(exit);
    start->addSuccessor(final);
    final->addSuccessor(exit);
    final->addSuccessor(start);

    unsigned CMP = R.createVirtualRegister(&XTC::GPRegsRegClass);
    BuildMI(start, dl, TII->get(XTC::CMP), CMP)
      .addReg(MI->getOperand(0).getReg())
      .addReg(MI->getOperand(2).getReg());

    BuildMI(start, dl, TII->get(XTC::BNEID))
      .addReg(CMP)
      .addMBB(exit);

    final->moveAfter(start);
    exit->moveAfter(final);
    break;
  }
  }

  unsigned CHK = R.createVirtualRegister(&XTC::GPRegsRegClass);
  BuildMI(final, dl, TII->get(XTC::SWX))
    .addReg(finalReg)
    .addReg(MI->getOperand(1).getReg())
    .addReg(XTC::R0);

  BuildMI(final, dl, TII->get(XTC::ADDIC), CHK)
    .addReg(XTC::R0)
    .addImm(0);

  BuildMI(final, dl, TII->get(XTC::BNEID))
    .addReg(CHK)
    .addMBB(start);

  // The pseudo instruction is no longer needed so remove it
  MI->eraseFromParent();
#endif
  return exit;
}

//===----------------------------------------------------------------------===//
//  Misc Lower Operation implementation
//===----------------------------------------------------------------------===//
//

SDValue XTCTargetLowering::LowerSELECT_CC(SDValue Op,
                                          SelectionDAG &DAG) const {
  SDValue LHS = Op.getOperand(0);
  SDValue RHS = Op.getOperand(1);
  SDValue TrueVal = Op.getOperand(2);
  SDValue FalseVal = Op.getOperand(3);

  ISD::CondCode CC = cast<CondCodeSDNode>(Op.getOperand(4))->get();

  unsigned XTCCC = TranslateToXTCCC(CC, LHS, RHS, DAG);

  DebugLoc dl = Op.getDebugLoc();

  unsigned Opc;

  DEBUG(dbgs()<<__PRETTY_FUNCTION__<<" called\n");

  SDValue CompareFlag;
  if (LHS.getValueType() == MVT::i32) {

      Opc = XTCISD::SELECT_CC;
      CompareFlag = DAG.getNode(XTCISD::CMP, dl, MVT::Glue, LHS, RHS);
  } else {
      llvm_unreachable("Cannot lower select_cc with unknown type");
  }

  return DAG.getNode(Opc, dl, TrueVal.getValueType(), TrueVal, FalseVal,
                     DAG.getConstant(XTCCC, MVT::i32),
                     CompareFlag);
}

SDValue XTCTargetLowering::
LowerGlobalAddress(SDValue Op, SelectionDAG &DAG) const {
  // FIXME there isn't actually debug info here
  DebugLoc dl = Op.getDebugLoc();
  const GlobalValue *GV = cast<GlobalAddressSDNode>(Op)->getGlobal();
  SDValue GA = DAG.getTargetGlobalAddress(GV, dl, MVT::i32);

  return DAG.getNode(XTCISD::Wrap, dl, MVT::i32, GA);
}

SDValue XTCTargetLowering::
LowerGlobalTLSAddress(SDValue Op, SelectionDAG &DAG) const {
  llvm_unreachable("TLS not implemented for MicroBlaze.");
}

SDValue XTCTargetLowering::
LowerJumpTable(SDValue Op, SelectionDAG &DAG) const {
  SDValue ResNode;
  SDValue HiPart;
  // FIXME there isn't actually debug info here
  DebugLoc dl = Op.getDebugLoc();

  EVT PtrVT = Op.getValueType();
  JumpTableSDNode *JT  = cast<JumpTableSDNode>(Op);

  SDValue JTI = DAG.getTargetJumpTable(JT->getIndex(), PtrVT, 0);
  return DAG.getNode(XTCISD::Wrap, dl, MVT::i32, JTI);
}

SDValue XTCTargetLowering::
LowerConstantPool(SDValue Op, SelectionDAG &DAG) const {
  SDValue ResNode;
  ConstantPoolSDNode *N = cast<ConstantPoolSDNode>(Op);
  const Constant *C = N->getConstVal();
  DebugLoc dl = Op.getDebugLoc();

  SDValue CP = DAG.getTargetConstantPool(C, MVT::i32, N->getAlignment(),
                                         N->getOffset(), 0);
  return DAG.getNode(XTCISD::Wrap, dl, MVT::i32, CP);
}

SDValue XTCTargetLowering::LowerVASTART(SDValue Op,
                                           SelectionDAG &DAG) const {
  MachineFunction &MF = DAG.getMachineFunction();
  XTCFunctionInfo *FuncInfo = MF.getInfo<XTCFunctionInfo>();

  DebugLoc dl = Op.getDebugLoc();
  SDValue FI = DAG.getFrameIndex(FuncInfo->getVarArgsFrameIndex(),
                                 getPointerTy());

  // vastart just stores the address of the VarArgsFrameIndex slot into the
  // memory location argument.
  const Value *SV = cast<SrcValueSDNode>(Op.getOperand(2))->getValue();
  return DAG.getStore(Op.getOperand(0), dl, FI, Op.getOperand(1),
                      MachinePointerInfo(SV),
                      false, false, 0);
}

//===----------------------------------------------------------------------===//
//                      Calling Convention Implementation
//===----------------------------------------------------------------------===//

#include "XTCGenCallingConv.inc"

static bool CC_XTC_AssignReg(unsigned &ValNo, MVT &ValVT, MVT &LocVT,
                                CCValAssign::LocInfo &LocInfo,
                                ISD::ArgFlagsTy &ArgFlags,
                                CCState &State) {
    static const uint16_t ArgRegs[] = {
        XTC::r1, XTC::r2, XTC::r3, 0
    };

    const unsigned NumArgRegs = array_lengthof(ArgRegs);
    unsigned Reg = State.AllocateReg(ArgRegs, NumArgRegs);
    if (!Reg) {
        llvm_unreachable("Cannot assign reg!");
        return false;
    }
    unsigned SizeInBytes = ValVT.getSizeInBits() >> 3;
    State.AllocateStack(SizeInBytes, SizeInBytes);
    State.addLoc(CCValAssign::getReg(ValNo, ValVT, Reg, LocVT, LocInfo));

    return true;
}

//===----------------------------------------------------------------------===//
//                  Call Calling Convention Implementation
//===----------------------------------------------------------------------===//

/// LowerCall - functions arguments are copied from virtual regs to
/// (physical regs)/(stack frame), CALLSEQ_START and CALLSEQ_END are emitted.
/// TODO: isVarArg, isTailCall.
SDValue XTCTargetLowering::
LowerCall(TargetLowering::CallLoweringInfo &CLI,
          SmallVectorImpl<SDValue> &InVals) const {
  SelectionDAG &DAG                     = CLI.DAG;
  DebugLoc &dl                          = CLI.DL;
  SmallVector<ISD::OutputArg, 32> &Outs = CLI.Outs;
  SmallVector<SDValue, 32> &OutVals     = CLI.OutVals;
  SmallVector<ISD::InputArg, 32> &Ins   = CLI.Ins;
  SDValue Chain                         = CLI.Chain;
  SDValue Callee                        = CLI.Callee;
  bool &isTailCall                      = CLI.IsTailCall;
  CallingConv::ID CallConv              = CLI.CallConv;
  bool isVarArg                         = CLI.IsVarArg;

  DEBUG(dbgs() << "Lowering CALL\n" );

  // XTC does not yet support tail call optimization
  isTailCall = false;

  // The XTC requires stack slots for arguments passed to var arg
  // functions even if they are passed in registers.
  bool needsRegArgSlots = isVarArg;

  MachineFunction &MF = DAG.getMachineFunction();
  MachineFrameInfo *MFI = MF.getFrameInfo();
  const TargetFrameLowering &TFI = *MF.getTarget().getFrameLowering();

  // Analyze operands of the call, assigning locations to each operand.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, isVarArg, DAG.getMachineFunction(),
                 getTargetMachine(), ArgLocs, *DAG.getContext());
  CCInfo.AnalyzeCallOperands(Outs, CC_XTC);

  // Get a count of how many bytes are to be pushed on the stack.
  unsigned NumBytes = CCInfo.getNextStackOffset();

  // Variable argument function calls require a minimum of 24-bytes of stack
  if (isVarArg && NumBytes < 24) NumBytes = 24;

  Chain = DAG.getCALLSEQ_START(Chain, DAG.getIntPtrConstant(NumBytes, true));
  DEBUG(dbgs() << "CALLSEQ_START\n");
  Chain.dump();

  SmallVector<std::pair<unsigned, SDValue>, 8> RegsToPass;
  SmallVector<SDValue, 8> MemOpChains;

  // Walk the register/memloc assignments, inserting copies/loads.
  for (unsigned i = 0, e = ArgLocs.size(); i != e; ++i) {
    CCValAssign &VA = ArgLocs[i];
    MVT RegVT = VA.getLocVT();
    SDValue Arg = OutVals[i];

    // Promote the value if needed.
    switch (VA.getLocInfo()) {
    default: llvm_unreachable("Unknown loc info!");
    case CCValAssign::Full: break;
    case CCValAssign::SExt:
      Arg = DAG.getNode(ISD::SIGN_EXTEND, dl, RegVT, Arg);
      break;
    case CCValAssign::ZExt:
      Arg = DAG.getNode(ISD::ZERO_EXTEND, dl, RegVT, Arg);
      break;
    case CCValAssign::AExt:
      Arg = DAG.getNode(ISD::ANY_EXTEND, dl, RegVT, Arg);
      break;
    }

    // Arguments that can be passed on register must be kept at
    // RegsToPass vector
    if (VA.isRegLoc()) {
      RegsToPass.push_back(std::make_pair(VA.getLocReg(), Arg));
    } else {
      // Register can't get to this point...
      assert(VA.isMemLoc());

      // Since we are alread passing values on the stack we don't
      // need to worry about creating additional slots for the
      // values passed via registers.
      needsRegArgSlots = false;

      // Create the frame index object for this incoming parameter
      unsigned ArgSize = VA.getValVT().getSizeInBits()/8;
      unsigned StackLoc = VA.getLocMemOffset() + 4;
      int FI = MFI->CreateFixedObject(ArgSize, StackLoc, true);

      SDValue PtrOff = DAG.getFrameIndex(FI,getPointerTy());

      // emit ISD::STORE whichs stores the
      // parameter value to a stack Location
      MemOpChains.push_back(DAG.getStore(Chain, dl, Arg, PtrOff,
                                         MachinePointerInfo(),
                                         false, false, 0));
    }
  }

  // If we need to reserve stack space for the arguments passed via registers
  // then create a fixed stack object at the beginning of the stack.
  if (needsRegArgSlots && TFI.hasReservedCallFrame(MF))
    MFI->CreateFixedObject(28,0,true);

  // Transform all store nodes into one single node because all store
  // nodes are independent of each other.
  if (!MemOpChains.empty())
    Chain = DAG.getNode(ISD::TokenFactor, dl, MVT::Other,
                        &MemOpChains[0], MemOpChains.size());

  // Build a sequence of copy-to-reg nodes chained together with token
  // chain and flag operands which copy the outgoing args into registers.
  // The InFlag in necessary since all emitted instructions must be
  // stuck together.
  SDValue InFlag;
  for (unsigned i = 0, e = RegsToPass.size(); i != e; ++i) {
    Chain = DAG.getCopyToReg(Chain, dl, RegsToPass[i].first,
                             RegsToPass[i].second, InFlag);
    InFlag = Chain.getValue(1);
  }

  // If the callee is a GlobalAddress/ExternalSymbol node (quite common, every
  // direct call is) turn it into a TargetGlobalAddress/TargetExternalSymbol
  // node so that legalize doesn't hack it.
  if (GlobalAddressSDNode *G = dyn_cast<GlobalAddressSDNode>(Callee))
    Callee = DAG.getTargetGlobalAddress(G->getGlobal(), dl,
                                getPointerTy(), 0, 0);
  else if (ExternalSymbolSDNode *S = dyn_cast<ExternalSymbolSDNode>(Callee))
    Callee = DAG.getTargetExternalSymbol(S->getSymbol(),
                                getPointerTy(), 0);

  // XTCJmpLink = #chain, #target_address, #opt_in_flags...
  //             = Chain, Callee, Reg#1, Reg#2, ...
  //
  // Returns a chain & a flag for retval copy to use.
  SDVTList NodeTys = DAG.getVTList(MVT::Other, MVT::Glue);
  SmallVector<SDValue, 8> Ops;
  Ops.push_back(Chain);
  Ops.push_back(Callee);

  // Add argument registers to the end of the list so that they are
  // known live into the call.
  for (unsigned i = 0, e = RegsToPass.size(); i != e; ++i) {
    Ops.push_back(DAG.getRegister(RegsToPass[i].first,
                                  RegsToPass[i].second.getValueType()));
  }

  if (InFlag.getNode())
    Ops.push_back(InFlag);

  Chain  = DAG.getNode(XTCISD::CALL, dl, NodeTys, &Ops[0], Ops.size());
  InFlag = Chain.getValue(1);

  // Create the CALLSEQ_END node.

  Chain = DAG.getCALLSEQ_END(Chain, DAG.getIntPtrConstant(NumBytes, true),
                             DAG.getIntPtrConstant(0, true), InFlag);

  if (!Ins.empty())
    InFlag = Chain.getValue(1);

  // Handle result values, copying them out of physregs into vregs that we
  // return.
  return LowerCallResult(Chain, InFlag, CallConv, isVarArg,
                         Ins, dl, DAG, InVals);
}

/// LowerCallResult - Lower the result values of a call into the
/// appropriate copies out of appropriate physical registers.
SDValue XTCTargetLowering::
LowerCallResult(SDValue Chain, SDValue InFlag, CallingConv::ID CallConv,
                bool isVarArg, const SmallVectorImpl<ISD::InputArg> &Ins,
                DebugLoc dl, SelectionDAG &DAG,
                SmallVectorImpl<SDValue> &InVals) const {
  // Assign locations to each value returned by this call.
  SmallVector<CCValAssign, 16> RVLocs;
  CCState CCInfo(CallConv, isVarArg, DAG.getMachineFunction(),
                 getTargetMachine(), RVLocs, *DAG.getContext());

  CCInfo.AnalyzeCallResult(Ins, RetCC_XTC);

  DEBUG(dbgs()<<"LowerCallResult: RVlocs "<<RVLocs.size()<<"\n");
  // Copy all of the result registers out of their specified physreg.
  for (unsigned i = 0; i != RVLocs.size(); ++i) {
    Chain = DAG.getCopyFromReg(Chain, dl, RVLocs[i].getLocReg(),
                               RVLocs[i].getValVT(), InFlag).getValue(1);
    InFlag = Chain.getValue(2);
    InVals.push_back(Chain.getValue(0));
  }

  return Chain;
}

//===----------------------------------------------------------------------===//
//             Formal Arguments Calling Convention Implementation
//===----------------------------------------------------------------------===//

/// LowerFormalArguments - transform physical registers into
/// virtual registers and generate load operations for
/// arguments places on the stack.
SDValue XTCTargetLowering::
LowerFormalArguments(SDValue Chain, CallingConv::ID CallConv, bool isVarArg,
                     const SmallVectorImpl<ISD::InputArg> &Ins,
                     DebugLoc dl, SelectionDAG &DAG,
                     SmallVectorImpl<SDValue> &InVals) const {
  MachineFunction &MF = DAG.getMachineFunction();
  MachineFrameInfo *MFI = MF.getFrameInfo();
  XTCFunctionInfo *XTCFI = MF.getInfo<XTCFunctionInfo>();

  unsigned StackReg = MF.getTarget().getRegisterInfo()->getFrameRegister(MF);
  XTCFI->setVarArgsFrameIndex(0);

  // Used with vargs to acumulate store chains.
  std::vector<SDValue> OutChains;

  // Keep track of the last register used for arguments
  unsigned ArgRegEnd = 0;

  // Assign locations to all of the incoming arguments.
  SmallVector<CCValAssign, 16> ArgLocs;
  CCState CCInfo(CallConv, isVarArg, DAG.getMachineFunction(),
                 getTargetMachine(), ArgLocs, *DAG.getContext());

  CCInfo.AnalyzeFormalArguments(Ins, CC_XTC);
  SDValue StackPtr;

  for (unsigned i = 0, e = ArgLocs.size(); i != e; ++i) {
    CCValAssign &VA = ArgLocs[i];

    // Arguments stored on registers
    if (VA.isRegLoc()) {
      MVT RegVT = VA.getLocVT();
      ArgRegEnd = VA.getLocReg();
      const TargetRegisterClass *RC;

      if (RegVT == MVT::i32)
        RC = &XTC::GPRegsRegClass;
      else if (RegVT == MVT::f32)
        RC = &XTC::GPRegsRegClass;
      else
        llvm_unreachable("RegVT not supported by LowerFormalArguments");

      // Transform the arguments stored on
      // physical registers into virtual ones
      unsigned Reg = MF.addLiveIn(ArgRegEnd, RC);
      SDValue ArgValue = DAG.getCopyFromReg(Chain, dl, Reg, RegVT);

      // If this is an 8 or 16-bit value, it has been passed promoted
      // to 32 bits.  Insert an assert[sz]ext to capture this, then
      // truncate to the right size. If if is a floating point value
      // then convert to the correct type.
      if (VA.getLocInfo() != CCValAssign::Full) {
        unsigned Opcode = 0;
        if (VA.getLocInfo() == CCValAssign::SExt)
          Opcode = ISD::AssertSext;
        else if (VA.getLocInfo() == CCValAssign::ZExt)
          Opcode = ISD::AssertZext;
        if (Opcode)
          ArgValue = DAG.getNode(Opcode, dl, RegVT, ArgValue,
                                 DAG.getValueType(VA.getValVT()));
        ArgValue = DAG.getNode(ISD::TRUNCATE, dl, VA.getValVT(), ArgValue);
      }

      InVals.push_back(ArgValue);
    } else { // VA.isRegLoc()
      // sanity check
      assert(VA.isMemLoc());

      // The last argument is not a register
      ArgRegEnd = 0;

      // The stack pointer offset is relative to the caller stack frame.
      // Since the real stack size is unknown here, a negative SPOffset
      // is used so there's a way to adjust these offsets when the stack
      // size get known (on EliminateFrameIndex). A dummy SPOffset is
      // used instead of a direct negative address (which is recorded to
      // be used on emitPrologue) to avoid mis-calc of the first stack
      // offset on PEI::calculateFrameObjectOffsets.
      // Arguments are always 32-bit.
      unsigned ArgSize = VA.getLocVT().getSizeInBits()/8;
      unsigned StackLoc = VA.getLocMemOffset() + 4;
      int FI = MFI->CreateFixedObject(ArgSize, 0, true);
      XTCFI->recordLoadArgsFI(FI, -StackLoc);
      XTCFI->recordLiveIn(FI);

      // Create load nodes to retrieve arguments from the stack
      SDValue FIN = DAG.getFrameIndex(FI, getPointerTy());
      InVals.push_back(DAG.getLoad(VA.getValVT(), dl, Chain, FIN,
                                   MachinePointerInfo::getFixedStack(FI),
                                   false, false, false, 0));
    }
  }
#if 0
  // To meet ABI, when VARARGS are passed on registers, the registers
  // must have their values written to the caller stack frame. If the last
  // argument was placed in the stack, there's no need to save any register.
  if ((isVarArg) && ArgRegEnd) {
    if (StackPtr.getNode() == 0)
      StackPtr = DAG.getRegister(StackReg, getPointerTy());

    // The last register argument that must be saved is XTC::R10
    const TargetRegisterClass *RC = &XTC::GPRegsRegClass;

    unsigned Begin = getXTCRegisterNumbering(XTC::R5);
    unsigned Start = getXTCRegisterNumbering(ArgRegEnd+1);
    unsigned End   = getXTCRegisterNumbering(XTC::R10);
    unsigned StackLoc = Start - Begin + 1;

    for (; Start <= End; ++Start, ++StackLoc) {
      unsigned Reg = getXTCRegisterFromNumbering(Start);
      unsigned LiveReg = MF.addLiveIn(Reg, RC);
      SDValue ArgValue = DAG.getCopyFromReg(Chain, dl, LiveReg, MVT::i32);

      int FI = MFI->CreateFixedObject(4, 0, true);
      XTCFI->recordStoreVarArgsFI(FI, -(StackLoc*4));
      SDValue PtrOff = DAG.getFrameIndex(FI, getPointerTy());
      OutChains.push_back(DAG.getStore(Chain, dl, ArgValue, PtrOff,
                                       MachinePointerInfo(),
                                       false, false, 0));

      // Record the frame index of the first variable argument
      // which is a value necessary to VASTART.
      if (!XTCFI->getVarArgsFrameIndex())
        XTCFI->setVarArgsFrameIndex(FI);
    }
  }

  // All stores are grouped in one node to allow the matching between
  // the size of Ins and InVals. This only happens when on varg functions
  if (!OutChains.empty()) {
    OutChains.push_back(Chain);
    Chain = DAG.getNode(ISD::TokenFactor, dl, MVT::Other,
                        &OutChains[0], OutChains.size());
  }
#endif
  return Chain;
}

//===----------------------------------------------------------------------===//
//               Return Value Calling Convention Implementation
//===----------------------------------------------------------------------===//

SDValue XTCTargetLowering::
LowerReturn(SDValue Chain, CallingConv::ID CallConv, bool isVarArg,
            const SmallVectorImpl<ISD::OutputArg> &Outs,
            const SmallVectorImpl<SDValue> &OutVals,
            DebugLoc dl, SelectionDAG &DAG) const {
  // CCValAssign - represent the assignment of
    // the return value to a location

    printf("LOWERING RETURN\n");
  SmallVector<CCValAssign, 16> RVLocs;

  // CCState - Info about the registers and stack slot.
  CCState CCInfo(CallConv, isVarArg, DAG.getMachineFunction(),
                 getTargetMachine(), RVLocs, *DAG.getContext());

  // Analize return values.
  CCInfo.AnalyzeReturn(Outs, RetCC_XTC);

  SDValue Flag;
  SmallVector<SDValue, 4> RetOps(1, Chain);

  // If this function is using the interrupt_handler calling convention
  // then use "rtid r14, 0" otherwise use "rtsd r15, 8"
#if 0
  unsigned Ret = (CallConv == CallingConv::MBLAZE_INTR) ? XTCISD::IRet
                                                        : XTCISD::Ret;
  unsigned Reg = (CallConv == CallingConv::MBLAZE_INTR) ? XTC::R14
      : XTC::R15;
#endif
  unsigned Reg = XTC::r1;
  unsigned Ret = XTCISD::Ret;

  RetOps.push_back(DAG.getRegister(Reg, MVT::i32));


  // Copy the result values into the output registers.

  for (unsigned i = 0; i != RVLocs.size(); ++i) {
    CCValAssign &VA = RVLocs[i];
    assert(VA.isRegLoc() && "Can only return in registers!");
    printf("Copying to reg\n");
    Chain = DAG.getCopyToReg(Chain, dl, VA.getLocReg(),
                             OutVals[i], Flag);

    // guarantee that all emitted copies are
    // stuck together, avoiding something bad
    Flag = Chain.getValue(1);
    RetOps.push_back(DAG.getRegister(VA.getLocReg(), VA.getLocVT()));
  }

  RetOps[0] = Chain;  // Update chain.

  // Add the flag if we have it.
  if (Flag.getNode())
    RetOps.push_back(Flag);

  return DAG.getNode(Ret, dl, MVT::Other, &RetOps[0], RetOps.size());
}

//===----------------------------------------------------------------------===//
//                           XTC Inline Assembly Support
//===----------------------------------------------------------------------===//

/// getConstraintType - Given a constraint letter, return the type of
/// constraint it is for this target.
XTCTargetLowering::ConstraintType XTCTargetLowering::
getConstraintType(const std::string &Constraint) const
{
  // XTC specific constrainy
  //
  // 'd' : An address register. Equivalent to r.
  // 'y' : Equivalent to r; retained for
  //       backwards compatibility.
  // 'f' : Floating Point registers.
  if (Constraint.size() == 1) {
    switch (Constraint[0]) {
      default : break;
      case 'd':
      case 'y':
      case 'f':
        return C_RegisterClass;
    }
  }
  return TargetLowering::getConstraintType(Constraint);
}

/// Examine constraint type and operand type and determine a weight value.
/// This object must already have been set up with the operand type
/// and the current alternative constraint selected.
TargetLowering::ConstraintWeight
XTCTargetLowering::getSingleConstraintMatchWeight(
    AsmOperandInfo &info, const char *constraint) const {
  ConstraintWeight weight = CW_Invalid;
  Value *CallOperandVal = info.CallOperandVal;
    // If we don't have a value, we can't do a match,
    // but allow it at the lowest weight.
  if (CallOperandVal == NULL)
    return CW_Default;
  Type *type = CallOperandVal->getType();
  // Look at the constraint type.
  switch (*constraint) {
  default:
    weight = TargetLowering::getSingleConstraintMatchWeight(info, constraint);
    break;
  case 'd':
  case 'y':
    if (type->isIntegerTy())
      weight = CW_Register;
    break;
  case 'f':
    if (type->isFloatTy())
      weight = CW_Register;
    break;
  }
  return weight;
}

/// Given a register class constraint, like 'r', if this corresponds directly
/// to an LLVM register class, return a register of 0 and the register class
/// pointer.
std::pair<unsigned, const TargetRegisterClass*> XTCTargetLowering::
getRegForInlineAsmConstraint(const std::string &Constraint, EVT VT) const {
  if (Constraint.size() == 1) {
    switch (Constraint[0]) {
    case 'r':
      return std::make_pair(0U, &XTC::GPRegsRegClass);
      // TODO: These can't possibly be right, but match what was in
      // getRegClassForInlineAsmConstraint.
    case 'd':
    case 'y':
    case 'f':
      if (VT == MVT::f32)
        return std::make_pair(0U, &XTC::GPRegsRegClass);
    }
  }
  return TargetLowering::getRegForInlineAsmConstraint(Constraint, VT);
}

bool XTCTargetLowering::
isOffsetFoldingLegal(const GlobalAddressSDNode *GA) const {
  // The XTC target isn't yet aware of offsets.
  return false;
}

bool XTCTargetLowering::isFPImmLegal(const APFloat &Imm, EVT VT) const {
  return VT != MVT::f32;
}

bool XTCTargetLowering::getPreIndexedAddressParts(SDNode *N, SDValue &Base,
                                                     SDValue &Offset,
                                                     ISD::MemIndexedMode &AM,
                                                     SelectionDAG &DAG) const
{
    bool isLoad = true;
    SDValue Ptr;
    EVT VT;
    return false;

    unsigned Alignment;
    if (LoadSDNode *LD = dyn_cast<LoadSDNode>(N)) {
        return false;
        Ptr = LD->getBasePtr();
        VT = LD->getMemoryVT();
        Alignment = LD->getAlignment();
    } else if (StoreSDNode *ST = dyn_cast<StoreSDNode>(N)) {
        Ptr = ST->getBasePtr();
        VT  = ST->getMemoryVT();
        Alignment = ST->getAlignment();
        isLoad = false;
    } else
        return false;
        
    if (!SelectAddrRegImm(Ptr, Base, Offset, DAG)) {
        DEBUG(dbgs()<<"Cannot select RegImm!");
        return false;
    }
    DEBUG(dbgs()<<"PRE_INC successful!");
    N->dump();
    DEBUG(dbgs()<<"Base: \n");
    Base.dump();
    DEBUG(dbgs()<<"Offset: \n");
    Offset.dump();
    DEBUG(dbgs()<<"Ptr: \n");
    Ptr.dump();
    DEBUG(dbgs()<<"\n");


    AM = ISD::PRE_INC;
    return true;


    //llvm_unreachable("getPreIndexedAddressParts not implemented");
}

bool XTCTargetLowering::getPostIndexedAddressParts(SDNode *N, SDValue &Base,
                                                     SDValue &Offset,
                                                     ISD::MemIndexedMode &AM,
                                                     SelectionDAG &DAG) const
{
    llvm_unreachable("getPostIndexedAddressParts not implemented");
}

/// isIntS32Immediate - This method tests to see if the node is either a 32-bit
/// or 64-bit immediate, and if the value can be accurately represented as a
/// sign extension from a 32-bit value.  If so, this returns true and the
/// immediate.
static bool isIntS32Immediate(SDNode *N, int32_t &Imm) {
  unsigned Opc = N->getOpcode();
  if (Opc != ISD::Constant)
    return false;

  Imm = (int32_t)cast<ConstantSDNode>(N)->getZExtValue();
  if (N->getValueType(0) == MVT::i32)
    return Imm == (int32_t)cast<ConstantSDNode>(N)->getZExtValue();
  else
    return Imm == (int64_t)cast<ConstantSDNode>(N)->getZExtValue();
}

static bool isIntS32Immediate(SDValue Op, int32_t &Imm) {
  return isIntS32Immediate(Op.getNode(), Imm);
}


/// SelectAddressRegReg - Given the specified addressed, check to see if it
/// can be represented as an indexed [r+r] operation.  Returns false if it
/// can be more efficiently represented with [r+imm].
bool XTCTargetLowering::SelectAddrRegReg(SDValue N, SDValue &Base, SDValue &Index, SelectionDAG &DAG) const {

    DEBUG(dbgs()<<"ALVIE "<<__PRETTY_FUNCTION__<<" called\n");
    return false;

    if (N.getOpcode() == ISD::FrameIndex) {
        DEBUG(dbgs() << "Frame index in SelectAddrRegReg, promoting to RegImm");
        return false;
    }
    if (N.getOpcode() == ISD::TargetExternalSymbol ||
        N.getOpcode() == ISD::TargetGlobalAddress)
        return false;  // direct calls.

    int32_t imm = 0;
    DEBUG( dbgs() << "Check for ADD/OR\n");
    if (N.getOpcode() == ISD::ADD || N.getOpcode() == ISD::OR) {
        DEBUG( dbgs() << "YES:Check for ADD/OR");
        if (isIntS32Immediate(N.getOperand(1), imm))
            return false;    // r+i

        if (N.getOperand(0).getOpcode() == ISD::TargetJumpTable ||
            N.getOperand(1).getOpcode() == ISD::TargetJumpTable)
            return false; // jump tables.
        DEBUG( dbgs() << "Ok, we can do this with R+R...\n" );
        Base = N.getOperand(0);
        Index = N.getOperand(1);
        return true;
    }

    return false;
}

/// Returns true if the address N can be represented by a base register plus
/// a signed 32-bit displacement [r+imm], and if it is not better
/// represented as reg+reg.
bool XTCTargetLowering::SelectAddrRegImm(SDValue N, SDValue &Base, SDValue &Disp,SelectionDAG &DAG) const {
  // If this can be more profitably realized as r+r, fail.
    DEBUG(dbgs()<<__PRETTY_FUNCTION__<<"SelectAddrRegImm called\n");

    if (SelectAddrRegReg(N, Base, Disp,DAG))
        return false;

  if (N.getOpcode() == ISD::ADD || N.getOpcode() == ISD::OR) {
    int32_t imm = 0;
    if (isIntS32Immediate(N.getOperand(1), imm)) {
      Disp = DAG.getTargetConstant(imm, MVT::i32);
      if (FrameIndexSDNode *FI = dyn_cast<FrameIndexSDNode>(N.getOperand(0))) {
        Base = DAG.getTargetFrameIndex(FI->getIndex(), N.getValueType());
      } else {
        Base = N.getOperand(0);
      }
      return true; // [r+i]
    }
  } else if (ConstantSDNode *CN = dyn_cast<ConstantSDNode>(N)) {
    // Loading from a constant address.
    uint32_t Imm = CN->getZExtValue();
    Disp = DAG.getTargetConstant(Imm, CN->getValueType(0));
    Base = DAG.getRegister(XTC::r0, CN->getValueType(0));
    return true;
  }

  Disp = DAG.getTargetConstant(0, getPointerTy());
  if (FrameIndexSDNode *FI = dyn_cast<FrameIndexSDNode>(N))
    Base = DAG.getTargetFrameIndex(FI->getIndex(), N.getValueType());
  else {
      //Disp = N;    // NOTA: isto deve ser o verdadeiro offset...
      Base = N;
  }
  return true;      // [r+0]
}

SDValue XTCTargetLowering::PerformDAGCombine(SDNode *N,
                                                DAGCombinerInfo &DCI) const {
    SelectionDAG &DAG = DCI.DAG;
    switch (N->getOpcode()) {
    case XTCISD::SETCC:       //return PerformSETCCCombine(N, DAG, DCI, Subtarget);
        {
            DebugLoc DL = N->getDebugLoc();
            SDValue CC = N->getOperand(1);
            SDValue FLAGS = N->getOperand(0);
            DEBUG(dbgs()<<"\nCC: ");
            CC.dump();
            DEBUG(dbgs()<<"\nFLAGS: ");
            FLAGS.dump();

            DEBUG(dbgs()<<"Opcode "<<getTargetNodeName(CC.getOpcode()));

            if (CC.getOpcode() != XTCISD::CMP)
                return SDValue();

            // Remove setcc..

            llvm_unreachable("Combine SETCC");
        }
        break;
    }
    return SDValue();
}


