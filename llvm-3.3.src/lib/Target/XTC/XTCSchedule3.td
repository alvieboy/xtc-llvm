//===-- XTCSchedule5.td - XTC Scheduling Definitions -*- tablegen -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// XTC instruction itineraries for the five stage pipeline.
//===----------------------------------------------------------------------===//
def XTCPipe3Itineraries : ProcessorItineraries<
  [Fetch,Decode,ExecuteA1,ExecuteA2,Memory,WriteBack], [], [

  InstrItinData< IIC_ALU1,[ 
               InstrStage<1,[Fetch]>    // one cycle in fetch stage
                , InstrStage<1,[Decode]>   // one cycle in decode stage
                , InstrStage<1,[ExecuteA1]>   // one cycle in execute stage
               ],
	       [ 3                    // result ready after three cycles
               , 2                    // first operand read after one cycle
               , 2 ]>,                 // second operand read after one cycle

  InstrItinData< IIC_ALU2,
               [ InstrStage<1,[Fetch]>    // one cycle in fetch stage
               , InstrStage<1,[Decode]>   // one cycle in decode stage
               , InstrStage<1,[ExecuteA2]> ],  // one cycle in execute stage
               [ 3                    // result ready after three cycles
               , 2                    // first operand read after one cycle
               , 2 ]>,                 // second operand read after one cycle

  InstrItinData< IIC_ALU2MEM,
               [ InstrStage<1,[Fetch]>    // one cycle in fetch stage
                , InstrStage<1,[Decode]>   // one cycle in decode stage
                , InstrStage<1,[ExecuteA2]>   // one cycle in execute stage
                , InstrStage<3,[Memory]>   // N cycle in memory stage
                , InstrStage<1,[WriteBack]> ],  // one cycle in writeback stage
               [ 6                    // result ready after three cycles
               , 2                    // first operand read after one cycle
               , 2 ]>                 // second operand read after one cycle
]>;

def XTC3Model : SchedMachineModel {
  let IssueWidth = 2; // 2 micro-ops are dispatched per cycle.
  let MinLatency = 1; // OperandCycles are interpreted as MinLatency.
  let LoadLatency = 3; // Optimistic load latency assuming bypass.
                       // This is overriden by OperandCycles if the
                       // Itineraries are queried instead.
  let MispredictPenalty = 5; // Based on estimate of pipeline depth.

  let Itineraries = XTCPipe3Itineraries;
}