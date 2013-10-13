//===-- NewcpuInstrInfo.td - Newcpu Instruction defs -------*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Instruction format superclass
//===----------------------------------------------------------------------===//
include "NewcpuInstrFormats.td"

//===----------------------------------------------------------------------===//
// Newcpu type profiles
//===----------------------------------------------------------------------===//

def SDT_NewcpuSelectCC 	: SDTypeProfile<1, 3, [SDTCisSameAs<0, 1>]>;
def SDT_NewcpuSetCC     : SDTypeProfile<1, 2,
                                  [SDTCisVT<0, i32>,
                                   SDTCisVT<1, i32>, SDTCisVT<2, i32>]>;

def SDT_NewcpuRet     	: SDTypeProfile<0, 1, [SDTCisInt<0>]>;

//def SDT_NewcpuIRet    : SDTypeProfile<0, 1, [SDTCisInt<0>]>;
//def SDT_NewcpuJmpLink : SDTypeProfile<0, -1, [SDTCisVT<0, i32>]>;
//def SDT_MBCallSeqStart : SDCallSeqStart<[SDTCisVT<0, i32>]>;
//def SDT_MBCallSeqEnd   : SDCallSeqEnd<[SDTCisVT<0, i32>, SDTCisVT<1, i32>]>;
def SDT_NewcpuBRCOND   : SDTypeProfile<0, 3,
                                  [SDTCisVT<0, OtherVT>,
                                   SDTCisVT<1, i32>,SDTCisVT<1, i32>]>;

def SDT_NewcpuBRCC         : SDTypeProfile<0, 2, [SDTCisVT<0, OtherVT>,
                                                  SDTCisVT<1, i32>]>;

def SDT_NewcpuCmp          : SDTypeProfile<0, 2, [SDTCisSameAs<0, 1>]>;


//===----------------------------------------------------------------------===//
// Newcpu specific nodes
//===----------------------------------------------------------------------===//

def NewcpuRet     : SDNode<"NewcpuISD::Ret", SDT_NewcpuRet,
                           [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;

def NewcpuSelectCC: SDNode<"NewcpuISD::SELECT_CC", SDT_NewcpuSelectCC,
                           [SDNPInGlue]>;

def NewcpuWrapper   : SDNode<"NewcpuISD::Wrap", SDTIntUnaryOp>;

def Newcpusetcc   : SDNode<"NewcpuISD::SETCC",    SDT_NewcpuSetCC>;

def Newcpubrcc    : SDNode<"NewcpuISD::BR_CC", SDT_NewcpuBRCC,
                            [SDNPHasChain, SDNPInGlue]>;

def Newcpucmp : SDNode<"NewcpuISD::CMP", SDT_NewcpuCmp, [SDNPOutGlue]>;

//===----------------------------------------------------------------------===//
// Newcpu Instruction Predicate Definitions.
//===----------------------------------------------------------------------===//
def HasMul       : Predicate<"Subtarget.hasMul()">;

//===----------------------------------------------------------------------===//
// Newcpu Operand, Complex Patterns and Transformations Definitions.
//===----------------------------------------------------------------------===//

class RegConstraint<string C> {
  string Constraints = C;
}
class NoEncode<string E> {
  string DisableEncoding = E;
}

// Condition codes

def NEWCPU_COND_C   : PatLeaf<(i32 0)>;
def NEWCPU_COND_NC  : PatLeaf<(i32 1)>;
def NEWCPU_COND_Z   : PatLeaf<(i32 2)>;
def NEWCPU_COND_NZ  : PatLeaf<(i32 3)>;
def NEWCPU_COND_CZ  : PatLeaf<(i32 4)>;
def NEWCPU_COND_NCZ : PatLeaf<(i32 5)>;
def NEWCPU_COND_CNZ : PatLeaf<(i32 6)>;
def NEWCPU_COND_NCNZ : PatLeaf<(i32 7)>;


def NewcpuMemAsmOperand : AsmOperandClass {
  let Name = "Mem";
  let SuperClasses = [];
}

// Instruction operand types
def brtarget    : Operand<OtherVT>;
def calltarget  : Operand<i32>;
def simm32      : Operand<i32>;

def memrr : Operand<iPTR> {
  let PrintMethod = "printMemOperand";
  let MIOperandInfo = (ops iPTR:$ptrreg, GPRegs:$regoffset);
  let ParserMatchClass = NewcpuMemAsmOperand;
}

def memri : Operand<iPTR> {
  let PrintMethod = "printMemOperand";
  let MIOperandInfo = (ops iPTR:$reg, imm);
  let ParserMatchClass = NewcpuMemAsmOperand;
}

// Newcpu Address Mode. SDNode frameindex could possibily be a match
// since load and store instructions from stack used it.

def iaddr : ComplexPattern<i32, 2, "SelectAddrRegImm", [frameindex], []>;
def xaddr : ComplexPattern<i32, 2, "SelectAddrRegReg", [], []>;

//===----------------------------------------------------------------------===//
// Pseudo instructions
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Instructions specific format
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Arithmetic Instructions
//===----------------------------------------------------------------------===//

// ALU operation that targets GPR registers and first operand is a DAR
/* this one costs the two ALU */

let Constraints="$dst = $src1" in  {
    class ArithG<bits<3> aluop, string instr_asm, SDNode OpNode, InstrItinClass itin> :
            NewcpuGENERIC<(outs GPRegs:$dst), (ins GPRegs:$src1, GPRegs:$src2),
             !strconcat(instr_asm, "   $dst, $src2"),
               [(set GPRegs:$dst, (OpNode GPRegs:$src1, GPRegs:$src2))], itin>;
}

let isAsCheapAsAMove = 1 in {
  multiclass Arith<bits<3> aluop, string instr_asm, SDNode OpNode,
  InstrItinClass itin> {
      def G : ArithG<aluop,instr_asm,  OpNode, itin>;
    }
}

// Simple ops

let neverHasSideEffects = 1 in  {
    class NOP<bits<12> op, string instr_asm, InstrItinClass itin> :
            NewcpuGENERIC<(outs), (ins), instr_asm, [], itin>;
}

let isAsCheapAsAMove = 1, isReMaterializable=1 in {

    def IM: NewcpuGENERIC<(outs GPRegs:$a), (ins i32imm:$src),
       "limr $src, $a", [(set GPRegs:$a , imm:$src)], IIC_ALU1>;
}

let mayLoad=1,canFoldAsLoad=1,isReMaterializable=1 in {

    def LDW: NewcpuGENERIC<(outs GPRegs:$dst), (ins memrr:$addr),
                  "ldw $addr, $dst",
                  [(set i32:$dst, (load xaddr:$addr))], IIC_ALU1>;

    def LDWI: NewcpuGENERIC<(outs GPRegs:$dst), (ins memri:$addr),
                  "ldwi $addr, $dst",
                  [(set i32:$dst, (load iaddr:$addr))], IIC_ALU1>;

}

def NOP: NOP<0, "nop", IIC_ALU1>;

def Newcpubrcond    : SDNode<"NewcpuISD::BRCOND",
    SDT_NewcpuBRCOND, [SDNPHasChain]>;




let Constraints="$dst = $src1" in {
    def ADDI: NewcpuGENERIC<(outs GPRegs:$dst), (ins GPRegs:$src1, i32imm:$v),
    "addi $v, $dst", [(set GPRegs:$dst, (add GPRegs:$src1, imm:$v))], IIC_ALU1>;
}


let Defs = [FLAGS] in {
    def CMPI: NewcpuGENERIC<(outs), (ins GPRegs:$src1, i32imm:$v), "cmpi $v, $src1", [(Newcpucmp i32:$src1, imm:$v),(implicit FLAGS)], IIC_ALU1>;

    def CMP: NewcpuGENERIC<(outs), (ins GPRegs:$a, GPRegs:$b), "cmp $a, $b", [(Newcpucmp i32:$a, i32:$b),(implicit FLAGS)], IIC_ALU1>;
}
let isCommutable=1 in {
    defm ADD:  Arith<0x0, "add ",  add,  IIC_ALU1>;
    defm AND:  Arith<0x2, "and ",  and,  IIC_ALU1>;
    defm OR:   Arith<0x3, "or ",   or,   IIC_ALU1>;
    defm SUB:  Arith<0x3, "sub ",  sub,  IIC_ALU1>;
}


let isReturn=1, isTerminator=1 in {
    def RET:    NewcpuGENERIC<(outs), (ins), "RET", [(NewcpuRet r1)], IIC_ALU1>;
}

/*
let Constraints = "$ptr = $ptr_wb" in {

    def STPOSTI: NewcpuGENERIC<(outs GPRegs:$ptr_wb), (ins GPRegs:$val, GPRegs:$ptr, i32imm:$offset),
            "STI+ $val, [$ptr] $offset ", [(set GPRegs:$ptr_wb,
                  (post_store GPRegs:$val, GPRegs:$ptr, imm:$offset))], IIC_ALU1>;


    def STPREI: NewcpuGENERIC<(outs GPRegs:$ptr_wb), (ins GPRegs:$val, GPRegs:$ptr, i32imm:$offset),
            "ST+I $val, [$ptr] $offset ", [(set GPRegs:$ptr_wb,
                  (pre_store GPRegs:$val, GPRegs:$ptr, imm:$offset))], IIC_ALU1>;
}
*/
//def POSTSTORE: ST_PostInc <"memw", "STriw", IntRegs, i32imm>, AddrModeRel;

//def: Pat<(pre_store (i32 GPRegs:$src1), GPRegs:$src2, (i32 imm:$offset)), (POSTSTORE (i32 GPRegs:$src1), GPRegs:$src2, (i32 imm:$offset))>;

def STR:  NewcpuGENERIC<(outs), (ins GPRegs:$dst, GPRegs:$val),
    "ST $val, [$dst]", [(store (i32 GPRegs:$val) ,xaddr:$dst)], IIC_ALU1>;

def ST:   NewcpuGENERIC<(outs), (ins GPRegs:$src, memrr:$addr),
    "ST $src, [$addr]", [(store (i32 GPRegs:$src) ,xaddr:$addr)], IIC_ALU1>;

def STI:   NewcpuGENERIC<(outs), (ins GPRegs:$src, memri:$addr),
    "ST $src, [$addr]", [(store (i32 GPRegs:$src) ,iaddr:$addr)], IIC_ALU1>;


//let usesCustomInserter = 1,
/*
 let    Constraints = "$ptr = $ptr_wb,@earlyclobber $ptr_wb" in {

        def STIp: NewcpuGENERIC<(outs GPRegs:$ptr_wb), (ins GPRegs:$val, GPRegs:$ptr, i32imm:$offset),
            "STIp $val, [++$ptr] $offset ", [(set GPRegs:$ptr_wb,
                  (pre_store GPRegs:$val, GPRegs:$ptr, imm:$offset))], IIC_ALU1>;

                  }
                  */


                         /*
def STIp: NewcpuGENERIC<(outs GPRegs:$result), (ins GPRegs:$value, memri:$addr), "STIp $value, [++$addr]", [], IIC_ALU1>,
                            RegConstraint<"$addr.reg = $result">,NoEncode<"$result">;
                         */
/*let mayLoad=1 in
    def LDIp: NewcpuGENERIC<(outs GPRegs:$val, GPRegs:$result), (ins memri:$addr), "LDIp [++$addr]", [], IIC_ALU1>,
                            RegConstraint<"$addr.reg = $result">,NoEncode<"$result">;
  */

//def STIp: NewcpuGENERIC<(outs), (ins GPRegs:$s, memri:$addr),"STIp $s, $addr", [], IIC_ALU1>;
    /*
let mayLoad=1 in {

    def LDinc: NewcpuGENERIC<(outs GPRegs:$dst, RPTR:$result ), (ins memri:$addr),
                  "LD [$addr], $dst",
                  [], IIC_ALU1>, RegConstraint<"$addr.reg = $result">,NoEncode<"$result">;
}
      */


//let isAsCheapAsAMove=1 in {
//    def MOV: NewcpuGENERIC<(outs GPRegs:$dst), (ins GPRegs:$src), "MOV $src, $dst", [(set GPRegs:$dst ,GPRegs:$src)], IIC_ALU1>;
//}

//def : Pat<(store (i32 GPRegs:$dst), (i32 ARegs:$iaddr)),STGA>;

//def : Pat<(load xaddr:$addr), (i32 (LD xaddr:$addr))>;

// Branch...

                           /*
let isBranch = 1, isIndirectBranch = 1, isTerminator = 1, hasCtrlDep = 1 in
class BranchI<string instr_asm, PatFrag Cond> :
              NewcpuGENERIC<(outs), (ins brtarget:$target),
 	      !strconcat(instr_asm, "   $target"),
	      [(Newcpubrcc bb:$target, Cond)], IIC_ALU1> {
              }

  def BEQ    :  BranchI<"beq    ", NEWCPU_COND_Z>;
  def BNE    :  BranchI<"bne    ", NEWCPU_COND_NZ>;
  def BLT    :  BranchI<"blt    ", NEWCPU_COND_CZ>;
  def BLE    :  BranchI<"ble    ", NEWCPU_COND_C>;
  def BGT    :  BranchI<"bgt    ", NEWCPU_COND_C>;
  def BGE    :  BranchI<"bge    ", NEWCPU_COND_NC>;
  def BRI    :  BranchI<"bri    ", NEWCPU_COND_C>;

let isBranch = 1, isTerminator = 1, hasCtrlDep = 1,
    isBarrier = 1 in {
  def BRID   :  BranchI<"brid   ", NEWCPU_COND_C>;
  def BRAID  :  BranchI<"braid  ", NEWCPU_COND_C>;
  }                            */

let Uses = [FLAGS], isBranch = 1, isTerminator = 1, isBarrier = 1, hasCtrlDep = 1  in {

def BCOND: NewcpuGENERIC<(outs), (ins brtarget:$dst, simm32:$cc),
    "b$cc $dst",
    [(Newcpubrcc bb:$dst, imm:$cc)], IIC_ALU1>;
}

let isBranch = 1, isTerminator = 1, isBarrier = 1 in {

    def BRI: NewcpuGENERIC<(outs), (ins brtarget:$dst),
        "bri $dst",
        [(br bb:$dst)], IIC_ALU1>;
}

             /*
let Uses = [FLAGS] in {
    def : Pat<(brcond (setcc (i32 GPRegs:$L), (i32 GPRegs:$R), SETEQ), bb:$T),
    (BEQ (CMP GPRegs:$R, GPRegs:$L), bb:$T)>;
    def : Pat<(brcond (setcc (i32 GPRegs:$L), (i32 GPRegs:$R), SETNE), bb:$T),
    (BNE (CMP GPRegs:$R, GPRegs:$L), bb:$T)>;
    def : Pat<(brcond (setcc (i32 GPRegs:$L), (i32 GPRegs:$R), SETGT), bb:$T),
    (BGT (CMP GPRegs:$R, GPRegs:$L), bb:$T)>;
    def : Pat<(brcond (setcc (i32 GPRegs:$L), (i32 GPRegs:$R), SETLT), bb:$T),
    (BLT (CMP GPRegs:$R, GPRegs:$L), bb:$T)>;
    def : Pat<(brcond (setcc (i32 GPRegs:$L), (i32 GPRegs:$R), SETGE), bb:$T),
    (BGE (CMP GPRegs:$R, GPRegs:$L), bb:$T)>;
    def : Pat<(brcond (setcc (i32 GPRegs:$L), (i32 GPRegs:$R), SETLE), bb:$T),
    (BLE (CMP GPRegs:$R, GPRegs:$L), bb:$T)>;
 }
               */


// Jump tables, global addresses, and constant pools
/*
def : Pat<(NewcpuWrapper tglobaladdr:$in), (IM tglobaladdr:$in)>;
def : Pat<(NewcpuWrapper tjumptable:$in),  (IM tjumptable:$in)>;
def : Pat<(NewcpuWrapper tconstpool:$in),  (IM tconstpool:$in)>;
*/

/*
def : Pat<(brcond (setcc (i32 GPRegs:$L), (i32 0), SETEQ), bb:$T),
          (BEQ GPRegs:$L, bb:$T)>;
def : Pat<(brcond (setcc (i32 GPRegs:$L), (i32 0), SETNE), bb:$T),
          (BNE GPRegs:$L, bb:$T)>;
def : Pat<(brcond (setcc (i32 GPRegs:$L), (i32 0), SETGT), bb:$T),
          (BGT GPRegs:$L, bb:$T)>;
def : Pat<(brcond (setcc (i32 GPRegs:$L), (i32 0), SETLT), bb:$T),
          (BLT GPRegs:$L, bb:$T)>;
def : Pat<(brcond (setcc (i32 GPRegs:$L), (i32 0), SETGE), bb:$T),
          (BGE GPRegs:$L, bb:$T)>;
def : Pat<(brcond (setcc (i32 GPRegs:$L), (i32 0), SETLE), bb:$T),
          (BLE GPRegs:$L, bb:$T)>;
*/

def : Pat<(br bb:$T), (BRI bb:$T)>;
//def : Pat<(brind GPRegs:$T), (BRAD GPRegs:$T)>;

/*
def : Pat<(brcond (setcc (i32 GPRegs:$L), (i32 0), SETUGT), bb:$T),
          (BGT (CMPU (i32 R0), GPRegs:$L), bb:$T)>;
def : Pat<(brcond (setcc (i32 GPRegs:$L), (i32 0), SETULT), bb:$T),
          (BLT (CMPU (i32 R0), GPRegs:$L), bb:$T)>;
def : Pat<(brcond (setcc (i32 GPRegs:$L), (i32 0), SETUGE), bb:$T),
          (BGE (CMPU (i32 R0), GPRegs:$L), bb:$T)>;
def : Pat<(brcond (setcc (i32 GPRegs:$L), (i32 0), SETULE), bb:$T),
          (BLE (CMPU (i32 R0), GPRegs:$L), bb:$T)>;
*/


/*
def : Pat<(setcc (i32 GPRegs:$L), (i32 0), SETEQ),
          (Select_CC (ADDIK (i32 R0), 1), (ADDIK (i32 R0), 0), GPRegs:$L, 1)>;
                        */
/*
def : Pat<(setcc (i32 GPR:$L), (i32 0), SETNE),
          (Select_CC (ADDIK (i32 R0), 1), (ADDIK (i32 R0), 0), GPR:$L, 2)>;

def : Pat<(setcc (i32 GPR:$L), (i32 0), SETGT),
          (Select_CC (ADDIK (i32 R0), 1), (ADDIK (i32 R0), 0), GPR:$L, 3)>;

def : Pat<(setcc (i32 GPR:$L), (i32 0), SETLT),
          (Select_CC (ADDIK (i32 R0), 1), (ADDIK (i32 R0), 0), GPR:$L, 4)>;

def : Pat<(setcc (i32 GPR:$L), (i32 0), SETGE),
          (Select_CC (ADDIK (i32 R0), 1), (ADDIK (i32 R0), 0), GPR:$L, 5)>;

def : Pat<(setcc (i32 GPR:$L), (i32 0), SETLE),
          (Select_CC (ADDIK (i32 R0), 1), (ADDIK (i32 R0), 0), GPR:$L, 6)>;
*/
