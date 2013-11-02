//===-- XTCInstrInfo.td - XTC Instruction defs -------*- tablegen -*-===//
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
include "XTCInstrFormats.td"

//===----------------------------------------------------------------------===//
// XTC type profiles
//===----------------------------------------------------------------------===//

def SDT_XTCSelectCC 	: SDTypeProfile<1, 3, [SDTCisSameAs<0, 1>, SDTCisSameAs<1, 2>, SDTCisVT<3, i32>]>;


def SDT_XTCSetCC     : SDTypeProfile<1, 2,
                                  [SDTCisVT<0, i32>,
                                   SDTCisVT<1, i32>, SDTCisVT<2, i32>]>;

def SDT_XTCRet     	: SDTypeProfile<0, 1, [SDTCisInt<0>]>;

//def SDT_XTCIRet    : SDTypeProfile<0, 1, [SDTCisInt<0>]>;
//def SDT_XTCJmpLink : SDTypeProfile<0, -1, [SDTCisVT<0, i32>]>;

def SDT_XTCCallSeqStart : SDCallSeqStart<[SDTCisVT<0, i32>]>;
def SDT_XTCCallSeqEnd   : SDCallSeqEnd<[SDTCisVT<0, i32>, SDTCisVT<1, i32>]>;

def SDT_XTCBRCOND   : SDTypeProfile<0, 3,
                                  [SDTCisVT<0, OtherVT>,
                                   SDTCisVT<1, i32>,SDTCisVT<1, i32>]>;

def SDT_XTCBRCC         : SDTypeProfile<0, 2, [SDTCisVT<0, OtherVT>,
                                                  SDTCisVT<1, i32>]>;

def SDT_XTCCmp          : SDTypeProfile<0, 2, [SDTCisSameAs<0, 1>]>;


//===----------------------------------------------------------------------===//
// XTC specific nodes
//===----------------------------------------------------------------------===//

def XTCRet     : SDNode<"XTCISD::Ret", SDT_XTCRet,
                           [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;

def XTCSelectCC: SDNode<"XTCISD::SELECT_CC", SDT_XTCSelectCC, [SDNPInGlue]>;

def XTCWrapper   : SDNode<"XTCISD::Wrap", SDTIntUnaryOp>;

def XTCsetcc   : SDNode<"XTCISD::SETCC",    SDT_XTCSetCC>;

def XTCbrcc    : SDNode<"XTCISD::BR_CC", SDT_XTCBRCC,
                            [SDNPHasChain, SDNPInGlue]>;

def XTCcmp : SDNode<"XTCISD::CMP", SDT_XTCCmp, [SDNPOutGlue]>;

def XTCcallseq_start : SDNode<"ISD::CALLSEQ_START", SDT_XTCCallSeqStart, [SDNPHasChain, SDNPOutGlue]>;

def XTCcallseq_end   : SDNode<"ISD::CALLSEQ_END", SDT_XTCCallSeqEnd, [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue]>;

//===----------------------------------------------------------------------===//
// XTC Instruction Predicate Definitions.
//===----------------------------------------------------------------------===//
def HasMul       : Predicate<"Subtarget.hasMul()">;

//===----------------------------------------------------------------------===//
// XTC Operand, Complex Patterns and Transformations Definitions.
//===----------------------------------------------------------------------===//

class RegConstraint<string C> {
  string Constraints = C;
}
class NoEncode<string E> {
  string DisableEncoding = E;
}

// Condition codes

def XTC_COND_C   : PatLeaf<(i32 0)>;
def XTC_COND_NC  : PatLeaf<(i32 1)>;
def XTC_COND_Z   : PatLeaf<(i32 2)>;
def XTC_COND_NZ  : PatLeaf<(i32 3)>;
def XTC_COND_CZ  : PatLeaf<(i32 4)>;
def XTC_COND_NCZ : PatLeaf<(i32 5)>;
def XTC_COND_CNZ : PatLeaf<(i32 6)>;
def XTC_COND_NCNZ : PatLeaf<(i32 7)>;


def XTCMemAsmOperand : AsmOperandClass {
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
  let ParserMatchClass = XTCMemAsmOperand;
}

def memri : Operand<iPTR> {
  let PrintMethod = "printMemOperand";
  let MIOperandInfo = (ops iPTR:$reg, imm);
  let ParserMatchClass = XTCMemAsmOperand;
}

let PrintMethod = "printCCOperand" in
  def cc : Operand<i32>;

// XTC Address Mode. SDNode frameindex could possibily be a match
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
            XTCGENERIC<(outs GPRegs:$dst), (ins GPRegs:$src1, GPRegs:$src2),
             !strconcat(instr_asm, "   $dst, $src2"),
               [(set GPRegs:$dst, (OpNode GPRegs:$src1, GPRegs:$src2))], itin>;
}


let Defs = [r15], Uses = [r15] in {
def ADJCALLSTACKDOWN : XTCPseudo<(outs), (ins i32imm:$amt),
                               "!ADJCALLSTACKDOWN $amt",
                               [(XTCcallseq_start timm:$amt)]>;
def ADJCALLSTACKUP : XTCPseudo<(outs), (ins i32imm:$amt1, i32imm:$amt2),
                            "!ADJCALLSTACKUP $amt1",
                            [(XTCcallseq_end timm:$amt1, timm:$amt2)]>;
}

def SDT_XTCCall    : SDTypeProfile<0, -1, [SDTCisVT<0, i32>]>;
def call          : SDNode<"XTCISD::CALL", SDT_XTCCall,
                           [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue,
                            SDNPVariadic]>;

let isCall=1, hasDelaySlot=1 in {

    def CALL: XTCGENERIC<(outs), (ins memri:$src, variable_ops),
        "call $src", [ (call iaddr:$src)], IIC_ALU1>;

    def CALLR: XTCGENERIC<(outs), (ins memrr:$src, variable_ops),
        "call $src", [ (call xaddr:$src)], IIC_ALU1>;

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
            XTCGENERIC<(outs), (ins), instr_asm, [], itin>;
}

let isAsCheapAsAMove = 1, isReMaterializable=1 in {

    def IM: XTCGENERIC<(outs GPRegs:$a), (ins i32imm:$src),
       "limr $src, $a", [(set (i32 GPRegs:$a) , imm:$src)], IIC_ALU1>;
}

let mayLoad=1,canFoldAsLoad=1,isReMaterializable=1 in {


    def LDWI: XTCGENERIC<(outs GPRegs:$dst), (ins memri:$addr),
                  "ldwi ($addr), $dst",
                  [(set i32:$dst, (load iaddr:$addr))], IIC_ALU1>;

    def LDBI: XTCGENERIC<(outs GPRegs:$dst), (ins memri:$addr),
                  "ldbi ($addr), $dst",
                  [(set i32:$dst, (zextloadi8 iaddr:$addr))], IIC_ALU1>;

    def LDSI: XTCGENERIC<(outs GPRegs:$dst), (ins memri:$addr),
                  "ldsi ($addr), $dst",
                  [(set i32:$dst, (zextloadi16 iaddr:$addr))], IIC_ALU1>;

}

def : Pat<(i32 (extloadi8 iaddr:$src)), (LDBI iaddr:$src)>;
def : Pat<(i32 (extloadi16 iaddr:$src)), (LDSI iaddr:$src)>;


def NOP: NOP<0, "nop", IIC_ALU1>;

def XTCbrcond    : SDNode<"XTCISD::BRCOND",
    SDT_XTCBRCOND, [SDNPHasChain]>;




let Constraints="$dst = $src1", Defs = [FLAGS] in {
    def ADDI: XTCGENERIC<(outs GPRegs:$dst), (ins GPRegs:$src1, i32imm:$v),
    "addi $v, $dst", [(set (i32 GPRegs:$dst), (add (i32 GPRegs:$src1), imm:$v)),(implicit FLAGS)], IIC_ALU1>;
}


let Defs = [FLAGS] in {
    def CMPI: XTCGENERIC<(outs), (ins GPRegs:$src1, i32imm:$v), "cmpi $v, $src1", [(XTCcmp i32:$src1, imm:$v),(implicit FLAGS)], IIC_ALU1>;

    def CMP: XTCGENERIC<(outs), (ins GPRegs:$a, GPRegs:$b), "cmp $a, $b", [(XTCcmp i32:$a, i32:$b),(implicit FLAGS)], IIC_ALU1>;
}
let isCommutable=1 in {
    defm ADD:  Arith<0x0, "add ",  add,  IIC_ALU1>;
    defm AND:  Arith<0x2, "and ",  and,  IIC_ALU1>;
    defm OR:   Arith<0x3, "or ",   or,   IIC_ALU1>;
    defm SUB:  Arith<0x3, "sub ",  sub,  IIC_ALU1>;
    defm SHL:  Arith<0x3, "shl ",  shl,  IIC_ALU1>;
    defm SRL:  Arith<0x3, "srl ",  srl,  IIC_ALU1>;
    defm XOR:  Arith<0x3, "xor ",  xor,  IIC_ALU1>;
}


let isReturn=1, isTerminator=1, hasDelaySlot = 1 in {
    def RET:    XTCGENERIC<(outs), (ins), "RET", [(XTCRet r1)], IIC_ALU1>;
}


let Constraints = "$ptr = $ptr_wb" in {

    def STWPOSTI: XTCGENERIC<(outs GPRegs:$ptr_wb), (ins GPRegs:$val, GPRegs:$ptr, i32imm:$offset),
            "stw+ $val, ($ptr) $offset) ", [(set GPRegs:$ptr_wb,
                  (post_store (i32 GPRegs:$val), (i32 GPRegs:$ptr), imm:$offset))], IIC_ALU1>;


    def STWPREI: XTCGENERIC<(outs GPRegs:$ptr_wb), (ins GPRegs:$val, GPRegs:$ptr, i32imm:$offset),
            "st+w $val, ($ptr) $offset ", [(set GPRegs:$ptr_wb,
                  (pre_store (i32 GPRegs:$val), (i32 GPRegs:$ptr), imm:$offset))], IIC_ALU1>;
}

//def POSTSTORE: ST_PostInc <"memw", "STriw", IntRegs, i32imm>, AddrModeRel;

//def: Pat<(pre_store (i32 GPRegs:$src1), GPRegs:$src2, (i32 imm:$offset)), (POSTSTORE (i32 GPRegs:$src1), GPRegs:$src2, (i32 imm:$offset))>;

def STBI:  XTCGENERIC<(outs), (ins GPRegs:$val, memri:$dst),
    "stbi $val, ($dst)", [(truncstorei8 (i32 GPRegs:$val) ,iaddr:$dst)], IIC_ALU1>;

def STSI:  XTCGENERIC<(outs), (ins GPRegs:$val, memri:$dst),
    "stsi $val, ($dst)", [(truncstorei16 (i32 GPRegs:$val) ,iaddr:$dst)], IIC_ALU1>;

def STWI:   XTCGENERIC<(outs), (ins GPRegs:$val, memri:$dst),
    "stwi $val, ($dst)", [(store (i32 GPRegs:$val), iaddr:$dst)], IIC_ALU1>;



//let usesCustomInserter = 1,
/*
 let    Constraints = "$ptr = $ptr_wb,@earlyclobber $ptr_wb" in {

        def STIp: XTCGENERIC<(outs GPRegs:$ptr_wb), (ins GPRegs:$val, GPRegs:$ptr, i32imm:$offset),
            "STIp $val, [++$ptr] $offset ", [(set GPRegs:$ptr_wb,
                  (pre_store GPRegs:$val, GPRegs:$ptr, imm:$offset))], IIC_ALU1>;

                  }
                  */


                         /*
def STIp: XTCGENERIC<(outs GPRegs:$result), (ins GPRegs:$value, memri:$addr), "STIp $value, [++$addr]", [], IIC_ALU1>,
                            RegConstraint<"$addr.reg = $result">,NoEncode<"$result">;
                         */
/*let mayLoad=1 in
    def LDIp: XTCGENERIC<(outs GPRegs:$val, GPRegs:$result), (ins memri:$addr), "LDIp [++$addr]", [], IIC_ALU1>,
                            RegConstraint<"$addr.reg = $result">,NoEncode<"$result">;
  */

//def STIp: XTCGENERIC<(outs), (ins GPRegs:$s, memri:$addr),"STIp $s, $addr", [], IIC_ALU1>;
    /*
let mayLoad=1 in {

    def LDinc: XTCGENERIC<(outs GPRegs:$dst, RPTR:$result ), (ins memri:$addr),
                  "LD [$addr], $dst",
                  [], IIC_ALU1>, RegConstraint<"$addr.reg = $result">,NoEncode<"$result">;
}
      */



    def SEXTB: XTCGENERIC<(outs GPRegs:$dst), (ins GPRegs:$src),
    "sextb $src, $dst", [], IIC_ALU1>;

    def SEXTS: XTCGENERIC<(outs GPRegs:$dst), (ins GPRegs:$src),
    "sexts $src, $dst", [], IIC_ALU1>;

def : Pat <(sext_inreg (i32 GPRegs:$src1), i8),
           (SEXTB GPRegs:$src1)>;

def : Pat <(sext_inreg (i32 GPRegs:$src1), i16),
           (SEXTS GPRegs:$src1)>;


let isAsCheapAsAMove=1, neverHasSideEffects=1 in {
    def MOV: XTCGENERIC<(outs GPRegs:$dst), (ins GPRegs:$src), "copy $src, $dst", [(set (i32 GPRegs:$dst) ,(i32 GPRegs:$src) )], IIC_ALU1>;
}


//def : Pat<(store (i32 GPRegs:$dst), (i32 ARegs:$iaddr)),STGA>;

//def : Pat<(load xaddr:$addr), (i32 (LD xaddr:$addr))>;

// Branch...

                           /*
let isBranch = 1, isIndirectBranch = 1, isTerminator = 1, hasCtrlDep = 1 in
class BranchI<string instr_asm, PatFrag Cond> :
              XTCGENERIC<(outs), (ins brtarget:$target),
 	      !strconcat(instr_asm, "   $target"),
	      [(XTCbrcc bb:$target, Cond)], IIC_ALU1> {
              }

  def BEQ    :  BranchI<"beq    ", XTC_COND_Z>;
  def BNE    :  BranchI<"bne    ", XTC_COND_NZ>;
  def BLT    :  BranchI<"blt    ", XTC_COND_CZ>;
  def BLE    :  BranchI<"ble    ", XTC_COND_C>;
  def BGT    :  BranchI<"bgt    ", XTC_COND_C>;
  def BGE    :  BranchI<"bge    ", XTC_COND_NC>;
  def BRI    :  BranchI<"bri    ", XTC_COND_C>;

let isBranch = 1, isTerminator = 1, hasCtrlDep = 1,
    isBarrier = 1 in {
  def BRID   :  BranchI<"brid   ", XTC_COND_C>;
  def BRAID  :  BranchI<"braid  ", XTC_COND_C>;
  }                            */

let Uses = [FLAGS], isBranch = 1, isTerminator = 1, hasDelaySlot = 1, isBarrier = 1, hasCtrlDep = 1  in {

def BCOND: XTCGENERIC<(outs), (ins brtarget:$dst, cc:$cc),
    "bri$cc $dst",
    [(XTCbrcc bb:$dst, imm:$cc)], IIC_ALU1>;
}

let isBranch = 1, isTerminator = 1, isBarrier = 1, hasDelaySlot = 1 in {

    def BRI: XTCGENERIC<(outs), (ins brtarget:$dst),
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

def : Pat<(XTCWrapper tglobaladdr:$in), (IM tglobaladdr:$in)>;
def : Pat<(XTCWrapper tjumptable:$in),  (IM tjumptable:$in)>;
def : Pat<(XTCWrapper tconstpool:$in),  (IM tconstpool:$in)>;

def : Pat<(call tglobaladdr:$dst),      (IM tglobaladdr:$dst)>;

//def : Pat<(call texternalsym:$dst),     (CALL texternalsym:$dst)>;

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

let usesCustomInserter = 1 in {
  def SELECT_CC : XTCPseudo<(outs GPRegs:$dst),
  (ins simm32:$cond, GPRegs:$T, GPRegs:$F),
      "# SELECT_CC PSEUDO!",
      [(set GPRegs:$dst,
        (XTCSelectCC i32:$T, i32:$F, imm:$cond))]>;
}

let usesCustomInserter = 1 in {
  def SELECT_CCr : XTCPseudo<(outs GPRegs:$dst),
  (ins GPRegs:$cond, GPRegs:$T, GPRegs:$F),
      "# SELECT_CC PSEUDO!",
      [(set GPRegs:$dst,
        (XTCSelectCC GPRegs:$cond, GPRegs:$T, GPRegs:$F))]>;
}

def : Pat<(select (setne GPRegs:$lhs, 0), GPRegs:$T, GPRegs:$F),
        (SELECT_CC GPRegs:$lhs, GPRegs:$T, GPRegs:$F)>;

def : Pat<(select (seteq GPRegs:$lhs, 0), GPRegs:$T, GPRegs:$F),
        (SELECT_CC GPRegs:$lhs, GPRegs:$F, GPRegs:$T)>;
                            /*
def : Pat<(select (setle GPRegs:$lhs, GPRegs:$rhs), GPRegs:$T, GPRegs:$F),
          (SELECT_CC (LSS_3r GPRegs:$rhs, GPRegs:$lhs), GPRegs:$F, GPRegs:$T)>;
def : Pat<(select (setule GPRegs:$lhs, GPRegs:$rhs), GPRegs:$T, GPRegs:$F),
          (SELECT_CC (LSU_3r GPRegs:$rhs, GPRegs:$lhs), GPRegs:$F, GPRegs:$T)>;
def : Pat<(select (setge GPRegs:$lhs, GPRegs:$rhs), GPRegs:$T, GPRegs:$F),
          (SELECT_CC (LSS_3r GPRegs:$lhs, GPRegs:$rhs), GPRegs:$F, GPRegs:$T)>;
def : Pat<(select (setuge GPRegs:$lhs, GPRegs:$rhs), GPRegs:$T, GPRegs:$F),
          (SELECT_CC (LSU_3r GPRegs:$lhs, GPRegs:$rhs), GPRegs:$F, GPRegs:$T)>;
def : Pat<(select (setne GPRegs:$lhs, GPRegs:$rhs), GPRegs:$T, GPRegs:$F),
          (SELECT_CC (EQ_3r GPRegs:$lhs, GPRegs:$rhs), GPRegs:$F, GPRegs:$T)>;
def : Pat<(select (setne GPRegs:$lhs, immUs:$rhs), GPRegs:$T, GPRegs:$F),
          (SELECT_CC (EQ_2rus GPRegs:$lhs, immUs:$rhs), GPRegs:$F, GPRegs:$T)>;
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
