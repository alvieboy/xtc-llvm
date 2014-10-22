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

def SDT_XTCJmpLink : SDTypeProfile<0, -1, [SDTCisVT<0, i32>]>;

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

def XTCJmpLink : SDNode<"XTCISD::JmpLink",SDT_XTCJmpLink,
    [SDNPHasChain,SDNPOptInGlue,SDNPOutGlue,
     SDNPVariadic]>;

def XTCcmp : SDNode<"XTCISD::CMP", SDT_XTCCmp, [SDNPOutGlue]>;

def XTCcallseq_start : SDNode<"ISD::CALLSEQ_START", SDT_XTCCallSeqStart, [SDNPHasChain, SDNPOutGlue]>;

def XTCcallseq_end   : SDNode<"ISD::CALLSEQ_END", SDT_XTCCallSeqEnd, [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue]>;
def XTCbrcond    : SDNode<"XTCISD::BRCOND",
    SDT_XTCBRCOND, [SDNPHasChain]>;

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

def imm8 : PatLeaf<(imm), [{
  return N->getZExtValue() == ((N->getZExtValue()) & 0xff) ;
}]>;

def imm24 : PatLeaf<(imm), [{
  return N->getZExtValue() == ((N->getZExtValue()) & 0xffffff) ;
}]>;

def memri : Operand<iPTR> {
  let PrintMethod = "printMemOperand";
  let MIOperandInfo = (ops GPRegs:$reg, imm24);
  let ParserMatchClass = XTCMemAsmOperand;
}

let PrintMethod = "printCCOperand" in
  def cc : Operand<i32>;

  // XTC Addressing Modes.
  // Only I+R addressing mode supported.

def iaddr : ComplexPattern<i32, 2, "SelectAddrRegImm", [frameindex], []>;

//===----------------------------------------------------------------------===//
// Pseudo instructions
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Instructions specific format
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Arithmetic Instructions
//===----------------------------------------------------------------------===//

// ALU operations //

let Constraints="$dst = $src2" in  {
    class Arith<bits<3> aluop, string instr_asm, SDNode OpNode, InstrItinClass itin> :
    XTCGENERIC<(outs GPRegs:$dst), (ins GPRegs:$src1, GPRegs:$src2),
             !strconcat(instr_asm, "   $src1, $dst"),
               [(set GPRegs:$dst, (OpNode GPRegs:$src1, GPRegs:$src2))], itin>;

    class ArithF<bits<3> aluop, string instr_asm, SDNode OpNode, InstrItinClass itin> :
            XTCGENERIC<(outs GPRegs:$dst), (ins GPRegs:$src1, GPRegs:$src2),
             !strconcat(instr_asm, "   $src1, $dst"),
        [(set GPRegs:$dst, (OpNode GPRegs:$src1, GPRegs:$src2)), (implicit PSR)], itin>;

    class INVArith<bits<3> aluop, string instr_asm, SDNode OpNode, InstrItinClass itin> :
    XTCGENERIC<(outs GPRegs:$dst), (ins GPRegs:$src1, GPRegs:$src2),
             !strconcat(instr_asm, "   $src1, $dst"),
               [(set GPRegs:$dst, (OpNode GPRegs:$src2, GPRegs:$src1))], itin>;

    class INVArithF<bits<3> aluop, string instr_asm, SDNode OpNode, InstrItinClass itin> :
            XTCGENERIC<(outs GPRegs:$dst), (ins GPRegs:$src1, GPRegs:$src2),
             !strconcat(instr_asm, "   $src1, $dst"),
               [(set GPRegs:$dst, (OpNode GPRegs:$src2, GPRegs:$src1)), (implicit PSR)], itin>;
}


/* IMMed */
let AddedComplexity = 1 in {
    class INVArithFImm<bits<3> aluop, string instr_asm, SDNode OpNode, InstrItinClass itin> :
             XTCGENERIC<(outs GPRegs:$dst), (ins GPRegs:$src1, i32imm: $i),
             !strconcat(instr_asm, "   $src1, $dst, $i"),
        [(set GPRegs:$dst, (OpNode GPRegs:$src1, imm:$i) )], itin>;

    class ArithImm<bits<3> aluop, string instr_asm, SDNode OpNode, InstrItinClass itin> :
             XTCGENERIC<(outs GPRegs:$dst), (ins GPRegs:$src1, i32imm: $i),
             !strconcat(instr_asm, "   $src1, $dst, $i"),
        [(set GPRegs:$dst, (OpNode GPRegs:$src1, imm:$i))], itin>;

    class INVArithImm<bits<3> aluop, string instr_asm, SDNode OpNode, InstrItinClass itin> :
             XTCGENERIC<(outs GPRegs:$dst), (ins GPRegs:$src1, i32imm: $i),
             !strconcat(instr_asm, "   $src1, $dst, $i"),
        [(set GPRegs:$dst, (OpNode GPRegs:$src1, imm:$i) )], itin>;
}

/* DREG-modified instructions */
let AddedComplexity = 1 in {
class ArithD<bits<3> aluop, string instr_asm, SDNode OpNode, InstrItinClass itin> :
    XTCGENERIC<(outs GPRegs:$dst), (ins GPRegs:$src1, GPRegs:$src2),
             !strconcat(instr_asm, "   $src1, $src2, $dst /* $dst <= $src1 op $src2 */"),
               [(set GPRegs:$dst, (OpNode GPRegs:$src1, GPRegs:$src2))], itin>;

class ArithDF<bits<3> aluop, string instr_asm, SDNode OpNode, InstrItinClass itin> :
            XTCGENERIC<(outs GPRegs:$dst), (ins GPRegs:$src1, GPRegs:$src2),
             !strconcat(instr_asm, "   $src1, $src2, $dst"),
               [(set GPRegs:$dst, (OpNode GPRegs:$src1, GPRegs:$src2)), (implicit PSR)], itin>;

class INVArithD<bits<3> aluop, string instr_asm, SDNode OpNode, InstrItinClass itin> :
            XTCGENERIC<(outs GPRegs:$dst), (ins GPRegs:$src1, GPRegs:$src2),
             !strconcat(instr_asm, "   $src1, $src2, $dst /* $dst <= $src2 op $src1 */"),
               [(set GPRegs:$dst, (OpNode GPRegs:$src2, GPRegs:$src1))], itin>;

class INVArithDF<bits<3> aluop, string instr_asm, SDNode OpNode, InstrItinClass itin> :
            XTCGENERIC<(outs GPRegs:$dst), (ins GPRegs:$src1, GPRegs:$src2),
             !strconcat(instr_asm, "   $src1, $src2, $dst"),
               [(set GPRegs:$dst, (OpNode GPRegs:$src2, GPRegs:$src1)), (implicit PSR)], itin>;
}

let Defs = [r15], Uses = [r15] in {
def ADJCALLSTACKDOWN : XTCPseudo<(outs), (ins i32imm:$amt),
                               "!ADJCALLSTACKDOWN $amt",
                               [(XTCcallseq_start timm:$amt)]>;
def ADJCALLSTACKUP : XTCPseudo<(outs), (ins i32imm:$amt1, i32imm:$amt2),
                            "!ADJCALLSTACKUP $amt1",
                            [(XTCcallseq_end timm:$amt1, timm:$amt2)]>;
}
/*
def SDT_XTCCall    : SDTypeProfile<0, -1, [SDTCisVT<0, i32>]>;

def call          : SDNode<"XTCISD::CALL", SDT_XTCCall,
                           [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue,
                            SDNPVariadic]>;
*/
let isCall=1, hasDelaySlot=1, Defs = [r1,r2,r3,r7,r8,r9,r10,r11,r12,r13,r14] in {

    def BR: XTCGENERIC<(outs), (ins i32imm:$src, GPRegs:$save),
    "br $src, $save",[],IIC_ALU2>;


    def CALL: XTCGENERIC<(outs), (ins calltarget:$src, GPRegs:$savereg, variable_ops),
        "br $src, $savereg", [], IIC_ALU2>;
}




// Simple ops

let neverHasSideEffects = 1 in  {
    class NOP<bits<12> op, string instr_asm, InstrItinClass itin> :
            XTCGENERIC<(outs), (ins), instr_asm, [], itin>;
}

let isAsCheapAsAMove = 1, isReMaterializable=1 in {

    def IM: XTCGENERIC<(outs GPRegs:$a), (ins i32imm:$src),
       "limr $src, $a", [(set (i32 GPRegs:$a) , imm:$src)], IIC_ALU2>;
}

let mayLoad=1,canFoldAsLoad=1,isReMaterializable=1 in {

    def LDW: XTCGENERIC<(outs GPRegs:$dst), (ins memri:$addr),
                  "ldw.i ($addr), $dst",
                  [(set i32:$dst, (load iaddr:$addr))], IIC_ALU2MEM>;

    def LDSPR: XTCGENERIC<(outs SPRegs:$dst), (ins memri:$addr),
                  "ldspr.i ($addr), $dst",
                  [(set i32:$dst, (load iaddr:$addr))], IIC_ALU2MEM>;

    def LDB: XTCGENERIC<(outs GPRegs:$dst), (ins memri:$addr),
                  "ldb.i ($addr), $dst",
                  [(set i32:$dst, (zextloadi8 iaddr:$addr))], IIC_ALU2MEM>;

    def LDS: XTCGENERIC<(outs GPRegs:$dst), (ins memri:$addr),
                  "lds.i ($addr), $dst",
                  [(set i32:$dst, (zextloadi16 iaddr:$addr))], IIC_ALU2MEM>;
}

def : Pat<(i32 (extloadi8 iaddr:$src)),  (LDB iaddr:$src)>;
def : Pat<(i32 (extloadi16 iaddr:$src)), (LDS iaddr:$src)>;


def NOP: NOP<0, "nop", IIC_ALU1>;

let isSelect = 1 in {
    def ADDRCC: XTCPseudo<(outs GPRegs:$out),(ins GPRegs:$src, pred:$p),
       "addri", []>;
}

let Defs = [PSR] in {
    def CMPI: XTCGENERIC<(outs), (ins GPRegs:$src1, i32imm:$v),
        "cmpi $v, $src1", [(XTCcmp i32:$src1, imm:$v),(implicit PSR)], IIC_ALU2>;

    def CMP: XTCGENERIC<(outs), (ins GPRegs:$a, GPRegs:$b),
        "cmp $b, $a", [(XTCcmp i32:$a, i32:$b),(implicit PSR)], IIC_ALU1>;
}

let isCommutable =  1 in {
    def ADDD:   INVArithD<0x0,    "add.d ",  add,  IIC_ALU1>;
    def ADD:    INVArith<0x0,    "add ",  add,  IIC_ALU1>;

    def ANDD:   INVArithD<0x2,    "and.d ",  and,  IIC_ALU1>;
    def AND:    INVArith<0x2,    "and ",  and,  IIC_ALU1>;

    def ORD:    INVArithD<0x3,    "or.d ",   or,   IIC_ALU1>;
    def OR:     INVArith<0x3,    "or ",   or,   IIC_ALU1>;

    def XORD:   ArithD<0x3, "xor.d ",  xor,  IIC_ALU1>;
    def XOR:    Arith<0x3, "xor ",  xor,  IIC_ALU1>;
}
let AddedComplexity=10 in {
def ADDEI:  INVArithImm<0x0, "add.i ",  add,  IIC_ALU1>;
//def ANDEI:  INVArithFImm<0x2,    "and.i ",  and,  IIC_ALU1>;
//def OREI:   INVArithFImm<0x3,    "or.i ",   or,   IIC_ALU1>;
//def XOREI:  ArithImm<0x3, "xor.i ",  xor,  IIC_ALU1>;
}

    def SUBEI:  INVArithFImm<0x3, "sub.i ",  sub,  IIC_ALU1>;
    def SUBD:   INVArithD<0x3, "sub.d ",  sub,  IIC_ALU1>;
    def SUB:    INVArith<0x3, "sub ",  sub,  IIC_ALU1>;

    def SHLD:  INVArithD<0x3, "shl.d ",  shl,  IIC_ALU1>;
    def SRLD:  INVArithD<0x3, "srl.d ",  srl,  IIC_ALU1>;
    def SRAD:  INVArithD<0x3, "sra.d ",  sra,  IIC_ALU1>;

    def SHL:  INVArith<0x3, "shl ",  shl,  IIC_ALU1>;
    def SRL:  INVArith<0x3, "srl ",  srl,  IIC_ALU1>;
    def SRA:  INVArith<0x3, "sra ",  sra,  IIC_ALU1>;

    def SHLI:  INVArithImm<0x3, "shl.i ",  shl,  IIC_ALU1>;
    def SRLI:  INVArithImm<0x3, "srl.i ",  srl,  IIC_ALU1>;
    def SRAI:  INVArithImm<0x3, "sra.i ",  sra,  IIC_ALU1>;


let Defs = [PSR] in {
    let isCommutable = 1 in {
      def ADDDC:  INVArithDF<0x0, "cadd.d ",  addc,  IIC_ALU1>;
      def ADDC:  INVArithF<0x0, "cadd ",  addc,  IIC_ALU1>;
    }
    def SUBC:  INVArithF<0x0, "csub ",  subc,  IIC_ALU1>;
}

let Defs = [PSR], Uses = [PSR] in {
    let isCommutable = 1 in {
      def ADDDE:  INVArithDF<0x0, "addc.d ",  adde,  IIC_ALU1>;
      def ADDE:  INVArithF<0x0, "addc ",  adde,  IIC_ALU1>;
    }
    def SUBE:  INVArithF<0x0, "subb ",  sube,  IIC_ALU1>;
}

let Defs=[Y], isCommutable = 1 in {
    def MULD   : ArithD<0x0, "mul.d ", mul,   IIC_ALU1>;
    def MUL    : Arith<0x0, "mul ", mul,   IIC_ALU1>;
}
/*
 let isReturn=1, isTerminator=1, hasDelaySlot = 1 in {
    def RETD: XTCGENERIC<(outs), (ins), "jmp r13, r0", [(XTCRet r0)], IIC_ALU1>;
}*/

let isReturn=1, isTerminator=1, hasDelaySlot = 1, isBarrier = 1 in {
    def RET: XTCGENERIC<(outs), (ins GPRegs:$base, GPRegs:$save), "jmp $base, $save", [], IIC_ALU1>;
}

let isBranch=1, hasDelaySlot = 1, isBarrier = 1 in {
    def JMP: XTCGENERIC<(outs), (ins GPRegs:$base, GPRegs:$save), "jmp $base, $save", [], IIC_ALU1>;
}
let isCall=1, hasDelaySlot=1, isBarrier = 1, Defs = [r1,r2,r3,r7,r8,r9,r10,r11,r12,r13,r14] in {
    def CALLR: XTCGENERIC<(outs), (ins GPRegs:$base, GPRegs:$save), "jmp $base, $save", [], IIC_ALU1>;
}


def : Pat<(XTCRet GPRegs:$target), (RET $target, (i32 r0))>;


let Constraints = "$ptr = $ptr_wb" in {

    def STWPOSTI: XTCGENERIC<(outs GPRegs:$ptr_wb), (ins GPRegs:$val, GPRegs:$ptr, i32imm:$offset),
            "stw+ $val, ($ptr) $offset) ", [(set GPRegs:$ptr_wb,
                  (post_store (i32 GPRegs:$val), (i32 GPRegs:$ptr), imm:$offset))], IIC_ALU2MEM>;


    def STWPREI: XTCGENERIC<(outs GPRegs:$ptr_wb), (ins GPRegs:$val, GPRegs:$ptr, i32imm:$offset),
            "st+w $val, ($ptr) $offset ", [(set GPRegs:$ptr_wb,
                  (pre_store (i32 GPRegs:$val), (i32 GPRegs:$ptr), imm:$offset))], IIC_ALU2MEM>;
}


def STB:  XTCGENERIC<(outs), (ins GPRegs:$val, memri:$dst),
    "stb.i $val, ($dst)", [(truncstorei8 (i32 GPRegs:$val) ,iaddr:$dst)], IIC_ALU2MEM>;

def STS:  XTCGENERIC<(outs), (ins GPRegs:$val, memri:$dst),
    "sts.i $val, ($dst)", [(truncstorei16 (i32 GPRegs:$val) ,iaddr:$dst)], IIC_ALU2MEM>;

def STW:   XTCGENERIC<(outs), (ins GPRegs:$val, memri:$dst),
    "stw.i $val, ($dst)", [(store (i32 GPRegs:$val), iaddr:$dst)], IIC_ALU2MEM>;

def STSPR:   XTCGENERIC<(outs), (ins SPRegs:$val, memri:$dst),
    "stspr.i $val, ($dst)", [(store (i32 SPRegs:$val), iaddr:$dst)], IIC_ALU2MEM>;


    def SEXTB: XTCGENERIC<(outs GPRegs:$dst), (ins GPRegs:$src),
    "sextb $src, $dst", [], IIC_ALU2>;

    def SEXTS: XTCGENERIC<(outs GPRegs:$dst), (ins GPRegs:$src),
    "sexts $src, $dst", [], IIC_ALU2>;

def : Pat <(sext_inreg (i32 GPRegs:$src1), i8),
           (SEXTB GPRegs:$src1)>;

def : Pat <(sext_inreg (i32 GPRegs:$src1), i16),
           (SEXTS GPRegs:$src1)>;


let Uses = [PSR], isBranch = 1, isTerminator = 1, hasDelaySlot = 1, hasCtrlDep = 1 , isIndirectBranch = 1 in {

def BCOND: XTCGENERIC<(outs), (ins brtarget:$dst, cc:$cc),
    "br.c$cc $dst, r0",
    [(XTCbrcc bb:$dst, imm:$cc)], IIC_ALU1>;

}

let isBranch = 1, isTerminator = 1, isBarrier = 1, hasDelaySlot = 1, isIndirectBranch = 1 in {

    def BRI: XTCGENERIC<(outs), (ins brtarget:$dst),
        "br $dst, r0",
        [(br bb:$dst)], IIC_ALU1>;

}
let isBranch = 1, isTerminator = 1, isBarrier = 1, hasDelaySlot = 1 in {
    def BRR: XTCGENERIC<(outs), (ins GPRegs:$dst),
        "br $dst, r0",
        [], IIC_ALU1>;
}

// Jump tables, global addresses, and constant pools

// Call
def : Pat<(XTCJmpLink (i32 tglobaladdr:$dst)), (BR tglobaladdr:$dst, (i32 r13))>;
def : Pat<(XTCJmpLink (i32 texternalsym:$dst)),(BR texternalsym:$dst,(i32 r13))>;
def : Pat<(XTCJmpLink (i32 GPRegs:$dst)),(CALLR $dst,(i32 r13))>;

//def : Pat<(XTCJmpLink GPRegs:$dst), (JMP GPRegs:$dst, (i32 r0))>;

def : Pat<(XTCWrapper tglobaladdr:$in), (IM tglobaladdr:$in)>;
def : Pat<(XTCWrapper tjumptable:$in),  (IM tjumptable:$in)>;
def : Pat<(XTCWrapper tconstpool:$in),  (IM tconstpool:$in)>;

// Branch patterns

def : Pat<(br bb:$T), (BRI bb:$T)>;
def : Pat<(brind GPRegs:$T), (BRR GPRegs:$T)>;


let Constraints="$src1 = $dst", Defs = [PSR] in {
    def ADDI: XTCGENERIC<(outs GPRegs:$dst), (ins GPRegs:$src1, i32imm:$v),
    "addi $v, $dst /* $src1 */", [(set GPRegs:$dst, (add GPRegs:$src1, imm:$v)),(implicit PSR)], IIC_ALU2>;
}

def ADDRI: XTCGENERIC<(outs GPRegs:$dst), (ins GPRegs:$src, i32imm:$v),
"addr.i $src, $dst, $v", [(set GPRegs:$dst, (add GPRegs:$src, imm:$v)),(implicit PSR)], IIC_ALU2>;

def ADDR: XTCGENERIC<(outs GPRegs:$dst), (ins GPRegs:$src),
"addr $src, $dst", [(set GPRegs:$dst, GPRegs:$src),(implicit PSR)], IIC_ALU2>;

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
    /*
def : Pat<(select (setne GPRegs:$lhs, 0), GPRegs:$T, GPRegs:$F),
        (SELECT_CC GPRegs:$lhs, GPRegs:$T, GPRegs:$F)>;

def : Pat<(select (seteq GPRegs:$lhs, 0), GPRegs:$T, GPRegs:$F),
        (SELECT_CC GPRegs:$lhs, GPRegs:$F, GPRegs:$T)>;
        */

/* Peepholes  - Mostly used for R0 special usage */

def : Pat<(store (i32 0), iaddr:$dst), (STW (i32 r0), iaddr:$dst)>;
def : Pat<(truncstorei8 (i8 0), iaddr:$dst), (STB (i32 r0), iaddr:$dst)>;
def : Pat<(truncstorei16 (i16 0), iaddr:$dst), (STS (i32 r0), iaddr:$dst)>;
