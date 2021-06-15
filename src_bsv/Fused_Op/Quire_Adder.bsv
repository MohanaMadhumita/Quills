// Library imports
//fifo input, funnel first stage and add into accumulaor


import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;
import Vector::*;
import PAClib :: * ;

import Posit_Numeric_Types :: *;
import Posit_User_Types :: *;
import Quire	::*;
import Normalizer          :: *;

interface Quire_Adder_IFC ;
  interface Put #(Vector#(N_melodica, Quire_Acc)) add;    // add all the quires
  //method Bit#(QuireWidth) read_quire;                   // start quire read
  interface Get #(Prenorm_Posit) read_posit;  // quire read response
endinterface

interface Quire_IFC;
   interface Put #(Quire_Acc) accumulate;    // add a value into the quire
   interface Put #(Posit_Extract) init;      // initialize a valu in quire
   method Action read_req;                   // start quire read
   interface Get #(Prenorm_Posit) read_rsp;  // quire read response
`ifdef ACCEL
	 	method Quire_Acc read_quire;
	 	method Action reset_quire;
`endif

endinterface

function Pipe #(Vector#(N_melodica, Quire_Acc), Vector#(1, Bit#(QuireWidth))) mkQFnl();
	let qfunnel = mkFunnel ();
	return qfunnel;
endfunction

module [Module] mkQuire_funnel (Server#(Vector#(N_melodica, Quire_Acc), Vector#(1, Bit#(QuireWidth))));	//pipe converted into module with server ifc
	let s <- mkQFnl();
	return s;
endmodule


module mkQuire_Adder (Quire_Adder_IFC );
	Bit #(2) verbosity = 3;  
	Reg#(Bit#(3)) i <- mkReg(valueOf(N_melodica));
	FIFOF #(Vector#(N_melodica, Quire_Acc))  fifo_input_quire <- mkFIFOF1;
	Server#(Vector#(N_melodica, Quire_Acc), Vector#(1, Bit#(QuireWidth)))  quire_stage          <- mkQuire_funnel;
	Quire_IFC                           quire_accumulator          <- mkQuire (verbosity);

	rule put_funnel;		
		let input_q = fifo_input_quire.first;
		quire_stage.request.put(input_q);
		fifo_input_quire.deq;
	endrule

	rule add_quire ;
		let z = quire_stage.response.get;
		quire_accumulator.accumulate.put(z);
	endrule




/*interface Server inoutifc;
      interface Put request;
         method Action put (Quire q);
		let dIn = q;
		Bit#(QuireWidthMinus1) carry_int_frac = dIn[valueOf(QuireWidthMinus2):0];
		Bit#(CarryWidthPlusIntWidthPlusFracWidthQuire) twos_complement_carry_int_frac = msb(dIn) == 1'b0 ? carry_int_frac : twos_complement(carry_int_frac);
		// now we have do signed sum of the values since the numbers are basiclly integer.fractions
		fifo_input_quire.enq(twos_complement_carry_int_frac);
		//to see if 
  	 endmethod
      endinterface
      interface Get response = toGet (rg_quire_accumulator);
   endinterface*/

endmodule
