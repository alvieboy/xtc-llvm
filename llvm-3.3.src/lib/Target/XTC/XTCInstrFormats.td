//===-- XTCInstrFormats.td - MB Instruction defs --------*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

// Format specifies the encoding used by the instruction.  This is part of the
// ad-hoc solution used to emit machine instruction encodings by our machine
// code emitter.
class Format<bits<6> val> {
      bits<6> Value = val;
}

def FPseudo : Format<0>;
def FALUgag   : Format<1>;
def FALUagg   : Format<2>;
def FALUgaa   : Format<3>;
def FALUaga   : Format<4>;

def FIMM    : Format<5>;  // Immediate load

//===----------------------------------------------------------------------===//
//  Describe XTC instructions format
//
//  CPU INSTRUCTION FORMATS
//
//  opcode  - operation code.
//  rd      - dst reg.
//  ra      - first src. reg.
//  rb      - second src. reg.
//  imm16   - 16-bit immediate value.
//
//===----------------------------------------------------------------------===//

// Generic XTC Format
class XTCInst<bits<4> op, Format form, dag outs, dag ins, string asmstr,
                 list<dag> pattern, InstrItinClass itin> : Instruction {
  let Namespace = "XTC";
  field bits<16> Inst;

  bits<4> opcode = op;
  Format Form = form;
  bits<6> FormBits = Form.Value;

  // Top 4 bits are the 'opcode' field
  let Inst{0-3} = opcode;

  // If the instruction is marked as a pseudo, set isCodeGenOnly so that the
  // assembler and disassmbler ignore it.
  let isCodeGenOnly = !eq(!cast<string>(form), "FPseudo");

  dag OutOperandList = outs;
  dag InOperandList  = ins;

  let AsmString   = asmstr;
  let Pattern     = pattern;
  let Itinerary   = itin;

  // TSFlags layout should be kept in sync with XTCInstrInfo.h.
  let TSFlags{5-0}   = FormBits;
}

//===----------------------------------------------------------------------===//
// Pseudo instruction class
//===----------------------------------------------------------------------===//
class XTCPseudo<dag outs, dag ins, string asmstr, list<dag> pattern>:
      XTCInst<0x0, FPseudo, outs, ins, asmstr, pattern, IIC_Pseudo>;

class XTCGENERIC<dag outs, dag ins, string asmstr, list<dag> pattern,InstrItinClass itin>:
      XTCInst<0x0, FPseudo, outs, ins, asmstr, pattern, itin>;

//===----------------------------------------------------------------------===//
// Type A instruction class in XTC : <|opcode|rd|ra|rb|flags|>
//===----------------------------------------------------------------------===//

class TIMM<bits<4> op, bits<11> imm, dag outs, dag ins, string asmstr,
         list<dag> pattern, InstrItinClass itin> :
         XTCInst<op,FIMM,outs, ins, asmstr, pattern, itin>
{
    bits<11> imm11 = imm;
    let Inst{5-15} = imm11;
}
