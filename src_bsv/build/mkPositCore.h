/*
 * Generated by Bluespec Compiler (build cd96b228)
 * 
 * On Tue Jun 15 18:31:46 IST 2021
 * 
 */

/* Generation options: */
#ifndef __mkPositCore_h__
#define __mkPositCore_h__

#include "bluesim_types.h"
#include "bs_module.h"
#include "bluesim_primitives.h"
#include "bs_vcd.h"
#include "mkMultiplier.h"
#include "mkExtracter.h"
#include "mkFtoP_PNE.h"
#include "mkNormalizer.h"
#include "mkPtoF_PNE.h"
#include "mkQuire.h"


/* Class declaration for the mkPositCore module */
class MOD_mkPositCore : public Module {
 
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
  MOD_Fifo<tUInt8> INST_cmd_stg2_f;
  MOD_Fifo<tUInt8> INST_cmd_stg3_f;
  MOD_mkMultiplier INST_divider;
  MOD_mkExtracter INST_extracter1;
  MOD_mkExtracter INST_extracter2;
  MOD_Fifo<tUWide> INST_ffI;
  MOD_Fifo<tUWide> INST_ffO;
  MOD_mkFtoP_PNE INST_ftop;
  MOD_mkMultiplier INST_multiplier;
  MOD_mkNormalizer INST_normalizer;
  MOD_CReg<tUInt8> INST_ops_in_flight;
  MOD_mkPtoF_PNE INST_ptof;
  MOD_mkQuire INST_quire;
 
 /* Constructor */
 public:
  MOD_mkPositCore(tSimStateHdl simHdl, char const *name, Module *parent);
 
 /* Symbol init methods */
 private:
  void init_symbols_0();
 
 /* Reset signal definitions */
 private:
  tUInt8 PORT_RST_N;
 
 /* Port definitions */
 public:
  tUWide PORT_server_core_request_put;
  tUWide PORT_server_core_response_get;
 
 /* Publicly accessible definitions */
 public:
  tUWide DEF_ffI_first____d8;
  tUInt8 DEF_cmd_stg2_f_first____d68;
  tUInt8 DEF_ffI_first_BITS_3_TO_0___d9;
 
 /* Local definitions */
 private:
  tUWide DEF_divider_response_get___d127;
  tUWide DEF_multiplier_response_get___d118;
  tUInt32 DEF_extracter1_response_get___d73;
  tUInt32 DEF_normalizer_response_get___d147;
  tUWide DEF_ffO_first____d161;
  tUWide DEF_quire_read_quire____d52;
  tUInt8 DEF_x__h3117;
  tUInt8 DEF_normalizer_response_get_47_BITS_3_TO_2___d151;
  tUInt8 DEF_normalizer_response_get_47_BIT_0___d154;
  tUInt8 DEF_x1__h3111;
  tUWide DEF__3_CONCAT_quire_read_quire__2_CONCAT_0___d53;
  tUWide DEF__2_CONCAT_DONTCARE_CONCAT_normalizer_response_g_ETC___d158;
  tUWide DEF_DONTCARE_CONCAT_normalizer_response_get_47_BIT_ETC___d149;
  tUWide DEF__1_CONCAT_DONTCARE_CONCAT_ptof_response_get_33__ETC___d142;
  tUWide DEF_DONTCARE_CONCAT_ptof_response_get_33_BITS_34_T_ETC___d135;
  tUInt64 DEF_extracter1_response_get_3_CONCAT_extracter2_re_ETC___d75;
 
 /* Rules */
 public:
  void RL_extract_stg1();
  void RL_rl_float_to_posit_stg1();
  void RL_rl_read_quire_stg1();
  void RL_rl_read_quire();
  void RL_rl_reset_quire();
  void RL_rl_fma_stg2();
  void RL_rl_fda_stg2();
  void RL_rl_posit_to_float_stg2();
  void RL_rl_float_to_posit_stg2();
  void RL_rl_init_quire_stg2();
  void RL_rl_read_quire_stg2();
  void RL_rl_fma_stg3();
  void RL_rl_fda_stg3();
  void RL_rl_posit_to_float_stg3();
  void RL_rl_float_to_posit_stg3();
  void RL_rl_read_quire_stg3();
 
 /* Methods */
 public:
  void METH_server_core_request_put(tUWide ARG_server_core_request_put);
  tUInt8 METH_RDY_server_core_request_put();
  tUWide METH_server_core_response_get();
  tUInt8 METH_RDY_server_core_response_get();
 
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
  void dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkPositCore &backing);
  void vcd_defs(tVCDDumpType dt, MOD_mkPositCore &backing);
  void vcd_prims(tVCDDumpType dt, MOD_mkPositCore &backing);
  void vcd_submodules(tVCDDumpType dt, unsigned int levels, MOD_mkPositCore &backing);
};

#endif /* ifndef __mkPositCore_h__ */
