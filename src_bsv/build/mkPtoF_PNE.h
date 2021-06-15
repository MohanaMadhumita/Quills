/*
 * Generated by Bluespec Compiler (build cd96b228)
 * 
 * On Tue Jun 15 18:31:46 IST 2021
 * 
 */

/* Generation options: */
#ifndef __mkPtoF_PNE_h__
#define __mkPtoF_PNE_h__

#include "bluesim_types.h"
#include "bs_module.h"
#include "bluesim_primitives.h"
#include "bs_vcd.h"


/* Class declaration for the mkPtoF_PNE module */
class MOD_mkPtoF_PNE : public Module {
 
 /* Clock handles */
 private:
  tClock __clk_handle_0;
 
 /* Clock gate handles */
 public:
  tUInt8 *clk_gate[0];
 
 /* Instantiation parameters */
 public:
 
 /* Module state */
 public:
  MOD_Fifo<tUInt64> INST_ffO;
 
 /* Constructor */
 public:
  MOD_mkPtoF_PNE(tSimStateHdl simHdl, char const *name, Module *parent, tUInt8 ARG_verbosity);
 
 /* Symbol init methods */
 private:
  void init_symbols_0();
 
 /* Reset signal definitions */
 private:
  tUInt8 PORT_RST_N;
 
 /* Port definitions */
 public:
  tUInt8 PORT_verbosity;
 
 /* Publicly accessible definitions */
 public:
 
 /* Local definitions */
 private:
  tUInt32 DEF_v__h912;
  tUInt32 DEF_v__h213;
 
 /* Rules */
 public:
 
 /* Methods */
 public:
  void METH_request_put(tUInt32 ARG_request_put);
  tUInt8 METH_RDY_request_put();
  tUInt64 METH_response_get();
  tUInt8 METH_RDY_response_get();
 
 /* Reset routines */
 public:
  void reset_RST_N(tUInt8 ARG_rst_in);
 
 /* Static handles to reset routines */
 public:
 
 /* Pointers to reset fns in parent module for asserting output resets */
 private:
 
 /* Functions for the parent module to register its reset fns */
 public:
 
 /* Functions to set the elaborated clock id */
 public:
  void set_clk_0(char const *s);
 
 /* State dumping routine */
 public:
  void dump_state(unsigned int indent);
 
 /* VCD dumping routines */
 public:
  unsigned int dump_VCD_defs(unsigned int levels);
  void dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkPtoF_PNE &backing);
  void vcd_defs(tVCDDumpType dt, MOD_mkPtoF_PNE &backing);
  void vcd_prims(tVCDDumpType dt, MOD_mkPtoF_PNE &backing);
};

#endif /* ifndef __mkPtoF_PNE_h__ */
