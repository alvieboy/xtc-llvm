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

// def SDTNewcpuSelectCC : SDTypeProfile<1, 3, [SDTCisSameAs<0, 1>]>;
def SDT_NewcpuRet     : SDTypeProfile<0, 1, [SDTCisInt<0>]>;
//def SDT_NewcpuIRet    : SDTypeProfile<0, 1, [SDTCisInt<0>]>;
def SDT_NewcpuJmpLink : SDTypeProfile<0, -1, [SDTCisVT<0, i32>]>;
def SDT_MBCallSeqStart : SDCallSeqStart<[SDTCisVT<0, i32>]>;
def SDT_MBCallSeqEnd   : SDCallSeqEnd<[SDTCisVT<0, i32>, SDTCisVT<1, i32>]>;

//===----------------------------------------------------------------------===//
// Newcpu specific nodes
//===----------------------------------------------------------------------===//

def NewcpuRet     : SDNode<"NewcpuISD::Ret", SDT_NewcpuRet,
                           [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;
/*
def NewcpuIRet    : SDNode<"NewcpuISD::IRet", SDT_NewcpuIRet,
                           [SDNPHasChain, SDNPOptInGlue, SDNPVariadic]>;
        */
def NewcpuJmpLink : SDNode<"NewcpuISD::JmpLink",SDT_NewcpuJmpLink,
                           [SDNPHasChain,SDNPOptInGlue,SDNPOutGlue,
                            SDNPVariadic]>;

def NewcpuWrapper   : SDNode<"NewcpuISD::Wrap", SDTIntUnaryOp>;

def callseq_start : SDNode<"ISD::CALLSEQ_START", SDT_MBCallSeqStart,
                           [SDNPHasChain, SDNPOutGlue]>;

def callseq_end   : SDNode<"ISD::CALLSEQ_END", SDT_MBCallSeqEnd,
                           [SDNPHasChain, SDNPOptInGlue, SDNPOutGlue]>;

//===----------------------------------------------------------------------===//
// Newcpu Instruction Predicate Definitions.
//===----------------------------------------------------------------------===//
def HasMul       : Predicate<"Subtarget.hasMul()">;

//===----------------------------------------------------------------------===//
// Newcpu Operand, Complex Patterns and Transformations Definitions.
//===----------------------------------------------------------------------===//

def NewcpuMemAsmOperand : AsmOperandClass {
  let Name = "Mem";
  let SuperClasses = [];
}

def NewcpuFslAsmOperand : AsmOperandClass {
  let Name = "Fsl";
  let SuperClasses = [];
}

// Instruction operand types
def brtarget    : Operand<OtherVT>;
def calltarget  : Operand<i32>;
def simm11      : Operand<i32>;
def simm32      : Operand<i32>;
def uimm11      : Operand<i32>;

// FSL Operand
def fslimm      : Operand<i32> {
  let PrintMethod = "printFSLImm";
  let ParserMatchClass = NewcpuFslAsmOperand;
}

def memrr : Operand<i32> {
  let PrintMethod = "printMemOperand";
  let MIOperandInfo = (ops VRegs, VRegs);
  let ParserMatchClass = NewcpuMemAsmOperand;
}

def memri : Operand<i32> {
  let PrintMethod = "printMemOperand";
  let MIOperandInfo = (ops VRegs, simm32);
  let ParserMatchClass = NewcpuMemAsmOperand;
}

def AM2OffsetImmAsmOperand : AsmOperandClass { let Name = "AM2OffsetImm"; }
def iaddroff : Operand<i32>,
                ComplexPattern<i32, 2, "SelectAddrMode2OffsetImm",
                [], [SDNPWantRoot]> {
  //let EncoderMethod = "getAddrMode2OffsetOpValue";
  let PrintMethod = "printAddrMode2OffsetOperand";
  let ParserMatchClass = AM2OffsetImmAsmOperand;
  let MIOperandInfo = (ops GPRegs, i32imm);
}



// Node immediate fits as 16-bit sign extended on target immediate.
def immSExt11  : PatLeaf<(imm), [{
  return (N->getZExtValue() >> 11) == 0;
}]>;

// Node immediate fits as 16-bit zero extended on target immediate.
// The LO16 param means that only the lower 16 bits of the node
// immediate are caught.
// e.g. addiu, sltiu
def immZExt11  : PatLeaf<(imm), [{
  return (N->getZExtValue() >> 11) == 0;
}]>;

// Newcpu Address Mode. SDNode frameindex could possibily be a match
// since load and store instructions from stack used it.
def iaddr : ComplexPattern<i32, 2, "SelectAddrRegImm", [frameindex], []>;
def xaddr : ComplexPattern<iPTR, 2, "SelectAddrRegReg", [], []>;

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
class ArithGGG<bits<3> aluop, string instr_asm, SDNode OpNode,InstrItinClass itin> :
            NewcpuGENERIC<(outs GPRegs:$dst), (ins GPRegs:$b, GPRegs:$c),
    !strconcat(instr_asm, "   $b, $c, $dst"),
               [(set GPRegs:$dst, (OpNode GPRegs:$b, GPRegs:$c))], itin>;

    class ArithAGG<bits<3> aluop, string instr_asm, SDNode OpNode, InstrItinClass itin> :
            NewcpuGENERIC<(outs GPRegs:$dst), (ins ARegs:$b, GPRegs:$c),
               !strconcat(instr_asm, "   $b, $c, $dst"),
               [(set GPRegs:$dst, (OpNode ARegs:$b, GPRegs:$c))], itin>;

// ALU operation that targets GPR registers and first operand is also GPR
class ArithGAG<bits<3> aluop, string instr_asm, SDNode OpNode,InstrItinClass itin> :
            NewcpuGENERIC<(outs GPRegs:$dst), (ins GPRegs:$b, ARegs:$c),
               !strconcat(instr_asm, "   $b, $c, $dst"),
               [(set GPRegs:$dst, (OpNode GPRegs:$b, ARegs:$c))], itin>;

// ALU operation that targets DAR registers and first operand is GPR
class ArithGAA<bits<3> aluop, string instr_asm, SDNode OpNode, InstrItinClass itin> :
            NewcpuGENERIC<(outs ARegs:$dst), (ins GPRegs:$b, ARegs:$c),
               !strconcat(instr_asm, "   $b, $c, $dst"),
               [(set ARegs:$dst, (OpNode GPRegs:$b, ARegs:$c))], itin>;

// ALU operation that targets DAR registers and first operand is also DAR
class ArithAGA<bits<3> aluop, string instr_asm, SDNode OpNode,InstrItinClass itin> :
            NewcpuGENERIC<(outs ARegs:$dst), (ins ARegs:$b, GPRegs:$c),
    !strconcat(instr_asm, "   $b, $c, $dst"),
               [(set ARegs:$dst, (OpNode ARegs:$b, GPRegs:$c))], itin>;

            /* this one costs the two ALU */
class ArithGGA<bits<3> aluop, string instr_asm, SDNode OpNode,InstrItinClass itin> :
            NewcpuGENERIC<(outs ARegs:$dst), (ins GPRegs:$b, GPRegs:$c),
    !strconcat(instr_asm, "   $b, $c, $dst"),
               [(set ARegs:$dst, (OpNode GPRegs:$b, GPRegs:$c))], itin>;


            // ALU operation that targets DAR registers and first operand is also DAR
/*class Store<string instr_asm, InstrItinClass itin> :
               NewcpuGENERIC< (outs), (ins GPR:$dst, DAR:$addr),
               !strconcat(instr_asm, "   $dst, $addr"),
               [], itin>;
  */
// Simple ops

let neverHasSideEffects = 1 in  {
    class NOP<bits<12> op, string instr_asm, InstrItinClass itin> :
            NewcpuGENERIC<(outs), (ins), instr_asm, [], itin>;
}

def i11imm      : Operand<i32> {
  let DecoderMethod= "DecodeSimm16";
}

let isAsCheapAsAMove = 1, isReMaterializable=1 in {
def IM: NewcpuGENERIC<(outs ARegs:$a), (ins i32imm:$src), "IM $src", [(set ARegs:$a , imm:$src)], IIC_ALU1>;
//def IMR: NewcpuGENERIC<(outs GPRegs:$a), (ins i32imm:$src), "IMR $a, $src", [(set GPRegs:$a , imm:$src)], IIC_ALU1>;
}
/*
let isAsCheapAsAMove = 1 in {
  def IMMF   :   ImmFirst<0x8, imm, "immf  ", imm>;
}

  */
// Small immediates.
/*def : Pat<(i32 simm11:$val),
 (IMM (i32), imm:$val)>;*/

let isAsCheapAsAMove = 1 in {
  multiclass Arith<bits<3> aluop, string instr_asm, SDNode OpNode,
    InstrItinClass itin> {
        def GGG : ArithGGG<aluop,instr_asm, OpNode,itin>;
       /* def GAG : ArithGAG<aluop,instr_asm, OpNode,itin>;
        def AGG : ArithAGG<aluop,instr_asm, OpNode,itin>;
        def GAA : ArithGAA<aluop,instr_asm, OpNode,itin>;
        def AGA : ArithAGA<aluop,instr_asm, OpNode,itin>;
        def GGA : ArithGGA<aluop,instr_asm, OpNode,itin>;*/
  }
}
let mayLoad=1,canFoldAsLoad=1,isReMaterializable=1 in {

    def LD: NewcpuGENERIC<(outs GPRegs:$dst), (ins memrr:$addr),
                  "LD [$addr], $dst",
                  [(set i32:$dst, (load xaddr:$addr))], IIC_ALU1>;

    def LDI: NewcpuGENERIC<(outs GPRegs:$dst), (ins memri:$addr),
                  "LD [$addr], $dst",
                  [(set i32:$dst, (load iaddr:$addr))], IIC_ALU1>;

    def LDA: NewcpuGENERIC<(outs ARegs:$dst), (ins memrr:$addr),
                  "LD [$addr], $dst",
                  [(set i32:$dst, (load xaddr:$addr))], IIC_ALU1>;

    def LDAI: NewcpuGENERIC<(outs ARegs:$dst), (ins memri:$addr),
                  "LD [$addr], $dst",
                  [(set i32:$dst, (load iaddr:$addr))], IIC_ALU1>;
}

def NOP: NOP<0, "NOP", IIC_ALU1>;

defm ADD:  Arith<0x0, "ADD ",  add,  IIC_ALU1>;
defm ADDC: Arith<0x1, "ADDC ", addc, IIC_ALU1>;
defm AND:  Arith<0x2, "AND ",  and,  IIC_ALU1>;
defm OR:   Arith<0x3, "OR ",   or,   IIC_ALU1>;
defm SUB:  Arith<0x3, "SUB ",  sub,  IIC_ALU1>;

//def STA:    NewcpuGENERICStore<"STA ", IIC_ALU>;
let isReturn=1, isTerminator=1 in {
        def RET:    NewcpuGENERIC<(outs), (ins), "RET", [(NewcpuRet A)], IIC_ALU1>;
        def RETR:    NewcpuGENERIC<(outs), (ins GPRegs:$r), "RETR $r", [(NewcpuRet GPRegs:$r)], IIC_ALU1>;
        //def RETR:    NewcpuGENERIC<(outs), (ins GPRegs:$a), "RETR $a", [(NewcpuRet GPRegs:$a)], IIC_ALU1>;
        //def RET:    NewcpuGENERIC<(outs), (ins), "RET ", [(NewcpuRet)], IIC_ALU1>;
}
//def RET:    NewcpuGENERIC<(outs), (ins), "RET ", [(NewcpuRet)], IIC_ALU1>;

def STGA:  NewcpuGENERIC<(outs), (ins GPRegs:$dst, ARegs:$val), "ST $val, [$dst]", [(store (i32 ARegs:$val) ,xaddr:$dst)], IIC_ALU1>;
def STAG:  NewcpuGENERIC<(outs), (ins ARegs:$dst, GPRegs:$val), "ST $val, [$dst]", [(store (i32 GPRegs:$val) ,xaddr:$dst)], IIC_ALU1>;


def STMA:   NewcpuGENERIC<(outs), (ins ARegs:$src, memrr:$addr),
    "ST $src, [$addr]", [(store (i32 ARegs:$src) ,xaddr:$addr)], IIC_ALU1>;

def STMR:   NewcpuGENERIC<(outs), (ins GPRegs:$src, memrr:$addr),
    "ST $src, [$addr]", [(store (i32 GPRegs:$src) ,xaddr:$addr)], IIC_ALU1>;

def STMRI:   NewcpuGENERIC<(outs), (ins GPRegs:$src, memri:$addr),
    "ST $src, [$addr]", [(store (i32 GPRegs:$src) ,iaddr:$addr)], IIC_ALU1>;

def STMAI:   NewcpuGENERIC<(outs), (ins ARegs:$src, memri:$addr),
    "ST $src, [$addr]", [(store (i32 ARegs:$src) ,iaddr:$addr)], IIC_ALU1>;


//let usesCustomInserter = 1,
//let    Constraints = "$ptr = $ptr_wb,@earlyclobber $ptr_wb" in {

        def STI: NewcpuGENERIC<(outs GPRegs:$ptr_wb), (ins GPRegs:$val, GPRegs:$ptr, iaddroff:$offset),
            "STI $val, [$ptr] $offset ", [(set GPRegs:$ptr_wb,
                  (pre_store GPRegs:$val, GPRegs:$ptr, iaddroff:$offset))], IIC_ALU1>;

  //  }

let isAsCheapAsAMove=1 in {
def MOVRA: NewcpuGENERIC<(outs GPRegs:$dst), (ins ARegs:$src), "MOV $src, $dst", [(set GPRegs:$dst ,ARegs:$src)], IIC_ALU1>;
def MOVAR: NewcpuGENERIC<(outs ARegs:$dst), (ins GPRegs:$src), "MOV $src, $dst", [(set ARegs:$dst ,GPRegs:$src)], IIC_ALU1>;
def MOVRR: NewcpuGENERIC<(outs GPRegs:$dst), (ins GPRegs:$src), "MOV  $src, $dst", [(set GPRegs:$dst ,GPRegs:$src)], IIC_ALU1>;
}

//def : Pat<(store (i32 GPRegs:$dst), (i32 ARegs:$iaddr)),STGA>;

//def : Pat<(load xaddr:$addr), (i32 (LD xaddr:$addr))>;


// Jump tables, global addresses, and constant pools

def : Pat<(NewcpuWrapper tglobaladdr:$in), (IM tglobaladdr:$in)>;
def : Pat<(NewcpuWrapper tjumptable:$in),  (IM tjumptable:$in)>;
def : Pat<(NewcpuWrapper tconstpool:$in),  (IM tconstpool:$in)>;


