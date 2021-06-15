/*
 * Generated by Bluespec Compiler (build cd96b228)
 * 
 * On Tue Jun 15 18:31:46 IST 2021
 * 
 */
#include "bluesim_primitives.h"
#include "mkTestbench.h"


/* Literal declarations */
static unsigned int const UWide_literal_163_h60001000000100000001000010000001000000010_arr[] = { 16u,
												 16u,
												 1048577u,
												 1048576u,
												 65536u,
												 6u };
static tUWide const UWide_literal_163_h60001000000100000001000010000001000000010(163u,
										 UWide_literal_163_h60001000000100000001000010000001000000010_arr);


/* String declarations */
static std::string const __str_literal_1("rule Insr", 9u);


/* Constructor */
MOD_mkTestbench::MOD_mkTestbench(tSimStateHdl simHdl, char const *name, Module *parent)
  : Module(simHdl, name, parent),
    __clk_handle_0(BAD_CLOCK_HANDLE),
    INST_count(simHdl, "count", this, 32u, 0u, (tUInt8)0u),
    INST_scheduler(simHdl, "scheduler", this),
    PORT_RST_N((tUInt8)1u),
    DEF_v__h246(12297829382473034410llu)
{
  symbol_count = 4u;
  symbols = new tSym[symbol_count];
  init_symbols_0();
}


/* Symbol init fns */

void MOD_mkTestbench::init_symbols_0()
{
  init_symbol(&symbols[0u], "b__h101", SYM_DEF, &DEF_b__h101, 32u);
  init_symbol(&symbols[1u], "count", SYM_MODULE, &INST_count);
  init_symbol(&symbols[2u], "RL_instr", SYM_RULE);
  init_symbol(&symbols[3u], "scheduler", SYM_MODULE, &INST_scheduler);
}


/* Rule actions */

void MOD_mkTestbench::RL_instr()
{
  tUInt32 DEF_count_PLUS_1___d5;
  DEF_b__h101 = INST_count.METH_read();
  DEF_count_PLUS_1___d5 = DEF_b__h101 + 1u;
  INST_scheduler.METH_server_sched_request_put(UWide_literal_163_h60001000000100000001000010000001000000010);
  INST_count.METH_write(DEF_count_PLUS_1___d5);
  if (!(PORT_RST_N == (tUInt8)0u))
    DEF_v__h246 = dollar_time(sim_hdl);
  if (!(PORT_RST_N == (tUInt8)0u))
    dollar_display(sim_hdl, this, "s,64", &__str_literal_1, DEF_v__h246);
}


/* Methods */


/* Reset routines */

void MOD_mkTestbench::reset_RST_N(tUInt8 ARG_rst_in)
{
  PORT_RST_N = ARG_rst_in;
  INST_scheduler.reset_RST_N(ARG_rst_in);
  INST_count.reset_RST(ARG_rst_in);
}


/* Static handles to reset routines */


/* Functions for the parent module to register its reset fns */


/* Functions to set the elaborated clock id */

void MOD_mkTestbench::set_clk_0(char const *s)
{
  __clk_handle_0 = bk_get_or_define_clock(sim_hdl, s);
}


/* State dumping routine */
void MOD_mkTestbench::dump_state(unsigned int indent)
{
  printf("%*s%s:\n", indent, "", inst_name);
  INST_count.dump_state(indent + 2u);
  INST_scheduler.dump_state(indent + 2u);
}


/* VCD dumping routines */

unsigned int MOD_mkTestbench::dump_VCD_defs(unsigned int levels)
{
  vcd_write_scope_start(sim_hdl, inst_name);
  vcd_num = vcd_reserve_ids(sim_hdl, 4u);
  unsigned int num = vcd_num;
  for (unsigned int clk = 0u; clk < bk_num_clocks(sim_hdl); ++clk)
    vcd_add_clock_def(sim_hdl, this, bk_clock_name(sim_hdl, clk), bk_clock_vcd_num(sim_hdl, clk));
  vcd_write_def(sim_hdl, bk_clock_vcd_num(sim_hdl, __clk_handle_0), "CLK", 1u);
  vcd_write_def(sim_hdl, num++, "RST_N", 1u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "b__h101", 32u);
  vcd_set_clock(sim_hdl, num, __clk_handle_0);
  vcd_write_def(sim_hdl, num++, "v__h246", 64u);
  num = INST_count.dump_VCD_defs(num);
  if (levels != 1u)
  {
    unsigned int l = levels == 0u ? 0u : levels - 1u;
    num = INST_scheduler.dump_VCD_defs(l);
  }
  vcd_write_scope_end(sim_hdl);
  return num;
}

void MOD_mkTestbench::dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkTestbench &backing)
{
  vcd_defs(dt, backing);
  vcd_prims(dt, backing);
  if (levels != 1u)
    vcd_submodules(dt, levels - 1u, backing);
}

void MOD_mkTestbench::vcd_defs(tVCDDumpType dt, MOD_mkTestbench &backing)
{
  unsigned int num = vcd_num;
  if (dt == VCD_DUMP_XS)
  {
    vcd_write_x(sim_hdl, num++, 1u);
    vcd_write_x(sim_hdl, num++, 32u);
    vcd_write_x(sim_hdl, num++, 64u);
  }
  else
    if (dt == VCD_DUMP_CHANGES)
    {
      if ((backing.PORT_RST_N) != PORT_RST_N)
      {
	vcd_write_val(sim_hdl, num, PORT_RST_N, 1u);
	backing.PORT_RST_N = PORT_RST_N;
      }
      ++num;
      if ((backing.DEF_b__h101) != DEF_b__h101)
      {
	vcd_write_val(sim_hdl, num, DEF_b__h101, 32u);
	backing.DEF_b__h101 = DEF_b__h101;
      }
      ++num;
      if ((backing.DEF_v__h246) != DEF_v__h246)
      {
	vcd_write_val(sim_hdl, num, DEF_v__h246, 64u);
	backing.DEF_v__h246 = DEF_v__h246;
      }
      ++num;
    }
    else
    {
      vcd_write_val(sim_hdl, num++, PORT_RST_N, 1u);
      backing.PORT_RST_N = PORT_RST_N;
      vcd_write_val(sim_hdl, num++, DEF_b__h101, 32u);
      backing.DEF_b__h101 = DEF_b__h101;
      vcd_write_val(sim_hdl, num++, DEF_v__h246, 64u);
      backing.DEF_v__h246 = DEF_v__h246;
    }
}

void MOD_mkTestbench::vcd_prims(tVCDDumpType dt, MOD_mkTestbench &backing)
{
  INST_count.dump_VCD(dt, backing.INST_count);
}

void MOD_mkTestbench::vcd_submodules(tVCDDumpType dt, unsigned int levels, MOD_mkTestbench &backing)
{
  INST_scheduler.dump_VCD(dt, levels, backing.INST_scheduler);
}