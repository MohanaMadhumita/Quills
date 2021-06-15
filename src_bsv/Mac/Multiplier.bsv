// Copyright (c) HPC Lab, Department of Electrical Engineering, IIT Bombay
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.package Extracter_Types;

package Multiplier;
// --------------------------------------------------------------
// This package defines:
//
// mkMultiplier: 3-stage posit multiplier
// --------------------------------------------------------------

import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;

import Utils :: *;
import Multiplier_Types :: *;
import MAC_Commons  :: *;
import Posit_Numeric_Types :: *;
import Posit_User_Types :: *;

(* synthesize *)
module mkMultiplier (Multiplier_IFC );
	// make a FIFO to store 
   	FIFOF #(Outputs_Mul )  fifo_output_reg <- mkFIFOF1;
	FIFOF #(Stage0_m )  fifo_stage0_reg <- mkFIFOF1;
	//This function is used to identify nan cases

	function Bit#(1) fv_check_for_nan(PositType z_i1, PositType z_i2,Bit#(1) nan1,Bit#(1) nan2 );
		if ((z_i1 == INF && z_i2 == ZERO)||(z_i2 == INF && z_i1 == ZERO)||(nan1 == 1'b1)||(nan2 == 1'b1))
			//nan flag = 1 when one input is infinity and other zero
			return 1'b1;
		else 
			return 1'b0;
	endfunction

	//This function finds the product of the fraction bits
	function Tuple3#(Bit#(1), Bit#(FracWidthMul4Plus2),Bit#(ScaleWidthPlus1)) fv_calculate_product_frac(Bit#(1) sgn1,Bit#(1)sgn2,Bit#(FracWidthPlus1)f1,Bit#(FracWidthPlus1)f2);
		Bit#(FracWidthMul4Plus2)frac_product;
		Bit#(ScaleWidthPlus1) frac_shift;
		Bit#(ScaleWidthPlus1) zero_one = '1;
		//frac_product gives the product of the two fractions
		// its size = sum of sizes of input fractions = FracWidth + 1 + FracWidth + 1 
		frac_product = extend(f1) * extend(f2);
		frac_product = (frac_product << (valueOf(FracWidthMul4MinusFracWidthMul2)));
		//frac_shift gives the number of bit shift in fraction product such that the MSB is 1
		frac_shift =  (min(extend(zero_one),extend(pack(countZerosMSB(frac_product)))));
		//Sign is given by the xor of the two signs
		//sign = 1 if negative number
		return tuple3(sgn1 ^ sgn2,(frac_product<<frac_shift),frac_shift); 
	endfunction
	
	
	//This function finds the sum of the scale bits since the scale value has 2^scale contribution in the product
	function Tuple2#(Int#(ScaleWidthPlus1), Int#(LogFracWidthPlus1)) fv_calculate_sum_scale(Int#(ScaleWidthPlus1 ) s1,Int#(ScaleWidthPlus1)s2,Bit#(ScaleWidthPlus1) frac_shift);
			Int#(ScaleWidthPlus2) scale;
			//Scale is calculated as the sum of the respective scale
			// bounded the value to prevent wrap around
			// we also add the fraction bit shift(the shift was done to get msb =1) to accomodate more frac bits
			scale =  signExtend(s1)+ signExtend(s2)- unpack(extend(frac_shift)-1);
			// now we bound the scale further to bound its value between min and max
			return fv_calculate_scale_shift(scale);
			//Scale_bound a;
			//a.scale = boundedMinus(boundedPlus(s1,s2) , unpack(extend(frac_shift)-1));
			//return a.scale ; 
	endfunction

	// --------
        // Pipeline stages
	//stage_1: scale calculation
	rule stage_1;
		//dIn reads the values from input pipeline register 
      		let dIn = fifo_stage0_reg.first;  fifo_stage0_reg.deq;
		// data to be stored in stored in fifo that will be used in stage 1
		//calling function to get sum of scale
		match{.scale0, .frac_change0} = fv_calculate_sum_scale(dIn.scale1,dIn.scale2,dIn.fracshift);
		//taking care of corner cases for zero infinity flag
		PositType zero_infinity_flag0 = (((dIn.frac == 0) && dIn.ziflag == REGULAR) ? ZERO :dIn.ziflag);
		Bit#(FracWidthMul4Plus2) mask1 = frac_change0 <= 0 ?~('1<<abs(frac_change0)+1):?;                
		let output_regf = Outputs_Mul {
			//taking care of corner cases for nan flag 
			nan_flag :dIn.nanflag,
			//also include the case when fraction bit msb = 0
			zero_infinity_flag : zero_infinity_flag0,
			sign : dIn.sign,
			scale : scale0,
			//truncate to remove the hidden bit
			// shift to get the sum to smaller value
			// shift by frac_change to accomodate the overflow of scale 
			frac : frac_change0 <= 0 ? truncate(dIn.frac >>(abs(frac_change0)+2)) : '1,// we want the output of size FWQ;if the frac_change <0 that means the scale was actually less than min so we shift the frac bits right frac_change to save the information, if frac_change>0 means we have scale more than max.. if we have to round this number we use scale = max and frac = max 
			truncated_frac_msb : zero_infinity_flag0 == ZERO ? 1'b0 : (frac_change0 <= 0 ?dIn.frac [abs(frac_change0)+1] : 1'b1),
			truncated_frac_zero : zero_infinity_flag0 == ZERO ? 1'b1 : (frac_change0 <= 0 ?((dIn.frac  & mask1) ==  0 ? 1'b1 : 1'b0) : 1'b0)};

   		fifo_output_reg.enq(output_regf);
		`ifdef RANDOM_PRINT
		$display("dout.frac %b frac %b shift %b scale %b frac_change %b  ",output_regf.frac,dIn.frac,dIn.fracshift,scale0,frac_change0);
		$display(" truncated_frac_msb %b truncated_frac_zero %b",output_regf.truncated_frac_msb,output_regf.truncated_frac_zero);
		`endif   	
	endrule

interface Server inoutifc;
      interface Put request;
         method Action put (Inputs_Mul p);
		// stage_0: INPUT STAGE and fraction calculation
		//dIn reads the values from input pipeline register 
      		let dIn = p;
		// data to be stored in stored in fifo that will be used in stage 0
		//to see what the hidden bit of each fraction bit will be thus sending that bit for product
		let zero_flag = dIn.zero_infinity_flag1 == ZERO ? 2'b01 : ( dIn.zero_infinity_flag2 == ZERO ? 2'b10 : 2'b11);
		// calling function to get product of fractions
		match{ .sign0, .frac0, .fracshift0} = fv_calculate_product_frac(dIn.sign1,dIn.sign2,{zero_flag[1],dIn.frac1},{zero_flag[0],dIn.frac2});

		//taking care of corner cases for zero infinity flag
		let ziflag = fv_check_for_z_i(dIn.zero_infinity_flag1,dIn.zero_infinity_flag2);
                let stage0_regf = Stage0_m {
			//taking care of corner cases for nan flag 
			nanflag : fv_check_for_nan(dIn.zero_infinity_flag1,dIn.zero_infinity_flag2,dIn.nanflag1,dIn.nanflag2),
			//also include the case when fraction bit msb = 0
			ziflag : ziflag,
			sign : sign0,
			scale1 : dIn.scale1,
			scale2 : dIn.scale2,
			frac : frac0,
			fracshift : fracshift0};
		`ifdef RANDOM_PRINT
		$display("frac1 %b frac2 %b scale1 %b scale2 %b",dIn.frac1,dIn.frac2,dIn.scale1,dIn.scale2);
		`endif
   		fifo_stage0_reg.enq(stage0_regf);
   endmethod
      endinterface
      interface Get response = toGet (fifo_output_reg);
   endinterface
endmodule
endpackage: Multiplier


