
**Contents of Files**
1. Quills.bsv : Complete coprocessor code
2. Testbench_accel.bsv : Sends instructions o Quills
3. Scheduler_types.bsv : Used to modify PositWidth, Number of Melodicas , TCM Word length, Nuber of lines in TCM
4. Makefile

**Modyfying parameters and instructions**
1. Change Testbench_accel.bsv file to send the required instruction into the Coprocessor
    >Instruction Template:
    >   tuple7(DOT_P _Opcode_,  <br />
    >          16'h0001 _pointer address TCM A_,  <br />
    >          32'h00000010 _No. of columns in matrix A_,<br />  
    >          32'h00000010 _No. of rows in matrix A_,  <br />
    >          16'h0001 _pointer address TCM B_,  <br />
    >          32'h00000010 _No. of columns in matrix B_,  <br />
    >          32'h00000010 _No. of rows in matrix B_
    >          )
2. To change PositWidth : Change the value in the code line `typedef xx PositWidth;` in Scheduler_Types.bsv file  <br />
    Go to src/lib directory and run `./Gen_Posit_Numeric_Types --posit-width 8 --exp-width 1 --float-width 32` for positwidth 8 and similarly change for other widths
    
3. To change Number of Melodicas: Change the value in the code line `typedef x N_melodica;` in Scheduler_Types.bsv file  <br />

**Building commands**
1. Inside src_bsv run `make compile`
2. go to src_bsv/build and run `bsc -sim -e mkTestbench -o ./mktb_bsim` (Make sure there is a tcm_mem.hex file in this directory)
3. in src_bsv/build, run `./mktb_bsim` to get the output
