/*
 * Generated by Bluespec Compiler (build cd96b228)
 * 
 * On Tue Jun 15 18:31:46 IST 2021
 * 
 */
#include "bluesim_primitives.h"
#include "mkNormalizer.h"


/* Constructor */
MOD_mkNormalizer::MOD_mkNormalizer(tSimStateHdl simHdl,
				   char const *name,
				   Module *parent,
				   tUInt8 ARG_verbosity)
  : Module(simHdl, name, parent),
    __clk_handle_0(BAD_CLOCK_HANDLE),
    INST_fifo_output_reg(simHdl, "fifo_output_reg", this, 20u, 1u, (tUInt8)1u, 0u),
    PORT_RST_N((tUInt8)1u),
    PORT_verbosity(ARG_verbosity)
{
  symbol_count = 2u;
  symbols = new tSym[symbol_count];
  init_symbols_0();
}


/* Symbol init fns */

void MOD_mkNormalizer::init_symbols_0()
{
  init_symbol(&symbols[0u], "fifo_output_reg", SYM_MODULE, &INST_fifo_output_reg);
  init_symbol(&symbols[1u], "verbosity", SYM_PORT, &PORT_verbosity, 2u);
}


/* Rule actions */


/* Methods */

void MOD_mkNormalizer::METH_request_put(tUInt32 ARG_request_put)
{
  tUInt32 DEF_IF_request_put_BITS_22_TO_21_EQ_0_THEN_request_ETC___d172;
  tUInt32 DEF_b__h2950;
  tUInt8 DEF_x__h2427;
  tUInt32 DEF_y__h426;
  tUInt32 DEF_x__h425;
  tUInt8 DEF_x__h696;
  tUInt32 DEF_y__h427;
  tUInt8 DEF_y__h2628;
  tUInt32 DEF_x__h710;
  tUInt8 DEF__theResult____h153;
  tUInt8 DEF_NOT_14_MINUS_IF_request_put_BIT_19_THEN_0_MINU_ETC___d51;
  tUInt8 DEF_INV_IF_NOT_IF_request_put_BIT_19_THEN_1_SL_14__ETC___d90;
  tUInt8 DEF__12_MINUS_IF_14_MINUS_IF_request_put_BIT_19_THE_ETC___d98;
  tUInt8 DEF_NOT_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_M_ETC___d88;
  tUInt8 DEF_NOT_14_MINUS_IF_request_put_BIT_19_THEN_0_MINU_ETC___d157;
  tUInt8 DEF_mask_e__h1375;
  tUInt8 DEF_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_MINUS_ETC___d87;
  tUInt8 DEF__12_MINUS_IF_14_MINUS_IF_request_put_BIT_19_THE_ETC___d93;
  tUInt8 DEF_NOT_14_MINUS_IF_request_put_BIT_19_THEN_0_MINU_ETC___d108;
  tUInt8 DEF_IF_NOT_IF_request_put_BIT_19_THEN_1_SL_14_MINU_ETC___d106;
  tUInt8 DEF_y__h2626;
  tUInt8 DEF_NOT_14_MINUS_IF_request_put_BIT_19_THEN_0_MINU_ETC___d102;
  tUInt8 DEF_x__h1348;
  tUInt8 DEF_expo_new__h1443;
  tUInt8 DEF_expo_new__h1474;
  tUInt8 DEF__theResult___fst__h1377;
  tUInt8 DEF_IF_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_MI_ETC___d85;
  tUInt8 DEF_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_MINUS_ETC___d67;
  tUInt8 DEF_IF_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_MI_ETC___d133;
  tUInt8 DEF_truncated_frac_msb__h148;
  tUInt8 DEF_flag_prev_truncate__h152;
  tUInt8 DEF_NOT_NOT_14_MINUS_IF_request_put_BIT_19_THEN_0__ETC___d126;
  tUInt8 DEF_IF_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_MI_ETC___d96;
  tUInt8 DEF__12_MINUS_IF_14_MINUS_IF_request_put_BIT_19_THE_ETC___d66;
  tUInt8 DEF_request_put_BIT_0_6_AND_INV_request_put_BIT_1__ETC___d83;
  tUInt8 DEF_x__h2625;
  tUInt8 DEF_shift_new0__h143;
  tUInt8 DEF_shift_new__h1473;
  tUInt8 DEF_request_put_BIT_14_5_AND_1_MINUS_14_MINUS_IF_r_ETC___d28;
  tUInt8 DEF__theResult___snd_snd__h1526;
  tUInt8 DEF_IF_request_put_BIT_18_0_AND_NOT_request_put_BI_ETC___d16;
  tUInt8 DEF__14_MINUS_IF_request_put_BIT_19_THEN_0_MINUS_re_ETC___d23;
  tUInt8 DEF__14_MINUS_IF_request_put_BIT_19_THEN_0_MINUS_re_ETC___d24;
  tUInt8 DEF_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_MINUS_ETC___d37;
  tUInt32 DEF_k__h698;
  tUInt32 DEF_k__h653;
  tUInt32 DEF_IF_IF_NOT_IF_request_put_BIT_19_THEN_1_SL_14_M_ETC___d124;
  tUInt32 DEF_a__h197;
  tUInt32 DEF_output_regf_posit__h167;
  tUInt8 DEF_i__h2535;
  tUInt8 DEF_request_put_BITS_13_TO_2_2_BIT_IF_14_MINUS_IF__ETC___d70;
  tUInt8 DEF__12_MINUS_IF_14_MINUS_IF_request_put_BIT_19_THE_ETC___d61;
  tUInt8 DEF_NOT_14_MINUS_IF_request_put_BIT_19_THEN_0_MINU_ETC___d64;
  tUInt8 DEF_x__h2582;
  tUInt8 DEF_NOT_14_MINUS_IF_request_put_BIT_19_THEN_0_MINU_ETC___d73;
  tUInt8 DEF_i1__h2632;
  tUInt8 DEF_x__h2742;
  tUInt8 DEF_request_put_BIT_0___d76;
  tUInt8 DEF_request_put_BIT_1___d68;
  tUInt8 DEF_request_put_BIT_14___d25;
  tUInt8 DEF_request_put_BIT_19___d4;
  tUInt8 DEF_request_put_BIT_23___d3;
  tUInt8 DEF_x__h1477;
  tUInt8 DEF_x_BIT_0___h1507;
  tUInt8 DEF_request_put_BITS_18_TO_15_1_PLUS_1___d12;
  tUInt32 DEF_IF_request_put_BIT_19_THEN_1_SL_14_MINUS_0_MIN_ETC___d19;
  tUInt8 DEF_IF_request_put_BIT_19_THEN_1_SL_14_MINUS_0_MIN_ETC___d20;
  tUInt32 DEF_b__h1345;
  tUInt8 DEF__0_CONCAT_IF_14_MINUS_IF_request_put_BIT_19_THE_ETC___d39;
  tUInt32 DEF_IF_request_put_BIT_19_THEN_1_SL_14_MINUS_0_MIN_ETC___d44;
  tUInt32 DEF_k_expo__h146;
  tUInt32 DEF_b__h409;
  tUInt32 DEF_IF_NOT_IF_request_put_BIT_19_THEN_1_SL_14_MINU_ETC___d120;
  tUInt8 DEF__0_MINUS_request_put_BITS_19_TO_15_BITS_3_TO_0___d7;
  tUInt8 DEF_a__h592;
  tUInt32 DEF_bs__h2631;
  tUInt32 DEF_x__h2408;
  tUInt32 DEF_frac__h145;
  DEF_bs__h2631 = (tUInt32)(4095u & (ARG_request_put >> 2u));
  DEF_a__h592 = (tUInt8)((tUInt8)31u & (ARG_request_put >> 15u));
  DEF__0_MINUS_request_put_BITS_19_TO_15_BITS_3_TO_0___d7 = (tUInt8)((tUInt8)15u & ((tUInt8)31u & ((tUInt8)0u - DEF_a__h592)));
  DEF_request_put_BITS_18_TO_15_1_PLUS_1___d12 = (tUInt8)15u & (((tUInt8)((tUInt8)15u & (ARG_request_put >> 15u))) + (tUInt8)1u);
  DEF_request_put_BIT_23___d3 = (tUInt8)(ARG_request_put >> 23u);
  DEF_request_put_BIT_19___d4 = (tUInt8)((tUInt8)1u & (ARG_request_put >> 19u));
  DEF_request_put_BIT_14___d25 = (tUInt8)((tUInt8)1u & (ARG_request_put >> 14u));
  DEF_request_put_BIT_1___d68 = (tUInt8)((tUInt8)1u & (ARG_request_put >> 1u));
  DEF_request_put_BIT_0___d76 = (tUInt8)((tUInt8)1u & ARG_request_put);
  DEF_IF_request_put_BIT_18_0_AND_NOT_request_put_BI_ETC___d16 = (tUInt8)((tUInt8)1u & (ARG_request_put >> 18u)) && !((tUInt8)(DEF_request_put_BITS_18_TO_15_1_PLUS_1___d12 >> 3u)) ? (tUInt8)15u : DEF_request_put_BITS_18_TO_15_1_PLUS_1___d12;
  DEF__14_MINUS_IF_request_put_BIT_19_THEN_0_MINUS_re_ETC___d23 = (tUInt8)15u & ((tUInt8)14u - (DEF_request_put_BIT_19___d4 ? DEF__0_MINUS_request_put_BITS_19_TO_15_BITS_3_TO_0___d7 : DEF_IF_request_put_BIT_18_0_AND_NOT_request_put_BI_ETC___d16));
  DEF_x__h1477 = primShiftL8(2u,
			     2u,
			     (tUInt8)1u,
			     4u,
			     (tUInt8)(DEF__14_MINUS_IF_request_put_BIT_19_THEN_0_MINUS_re_ETC___d23));
  DEF_x_BIT_0___h1507 = (tUInt8)((tUInt8)1u & DEF_x__h1477);
  DEF__14_MINUS_IF_request_put_BIT_19_THEN_0_MINUS_re_ETC___d24 = DEF__14_MINUS_IF_request_put_BIT_19_THEN_0_MINUS_re_ETC___d23 == (tUInt8)0u;
  DEF_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_MINUS_ETC___d37 = DEF__14_MINUS_IF_request_put_BIT_19_THEN_0_MINUS_re_ETC___d24 ? DEF__14_MINUS_IF_request_put_BIT_19_THEN_0_MINUS_re_ETC___d23 : (tUInt8)15u & (DEF__14_MINUS_IF_request_put_BIT_19_THEN_0_MINUS_re_ETC___d23 - (tUInt8)1u);
  DEF__12_MINUS_IF_14_MINUS_IF_request_put_BIT_19_THE_ETC___d61 = (tUInt8)15u & ((tUInt8)12u - DEF_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_MINUS_ETC___d37);
  DEF_x__h2742 = (tUInt8)15u & (DEF__12_MINUS_IF_14_MINUS_IF_request_put_BIT_19_THE_ETC___d61 - (tUInt8)2u);
  DEF_x__h2582 = (tUInt8)15u & (DEF__12_MINUS_IF_14_MINUS_IF_request_put_BIT_19_THE_ETC___d61 - (tUInt8)1u);
  DEF__12_MINUS_IF_14_MINUS_IF_request_put_BIT_19_THE_ETC___d66 = DEF__12_MINUS_IF_14_MINUS_IF_request_put_BIT_19_THE_ETC___d61 == (tUInt8)0u;
  DEF_expo_new__h1474 = DEF_x_BIT_0___h1507 & DEF_request_put_BIT_14___d25;
  DEF_expo_new__h1443 = DEF__14_MINUS_IF_request_put_BIT_19_THEN_0_MINUS_re_ETC___d24 && DEF_request_put_BIT_14___d25;
  DEF__12_MINUS_IF_14_MINUS_IF_request_put_BIT_19_THE_ETC___d93 = DEF__12_MINUS_IF_14_MINUS_IF_request_put_BIT_19_THE_ETC___d61 == (tUInt8)1u;
  DEF_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_MINUS_ETC___d87 = DEF_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_MINUS_ETC___d37 == (tUInt8)0u;
  DEF_mask_e__h1375 = ((tUInt8)15u & ((tUInt8)1u - DEF__14_MINUS_IF_request_put_BIT_19_THEN_0_MINUS_re_ETC___d23)) == (tUInt8)0u;
  DEF_request_put_BIT_14_5_AND_1_MINUS_14_MINUS_IF_r_ETC___d28 = DEF_request_put_BIT_14___d25 & DEF_mask_e__h1375;
  DEF__theResult___fst__h1377 = DEF_request_put_BIT_14_5_AND_1_MINUS_14_MINUS_IF_r_ETC___d28 ? DEF_expo_new__h1474 : DEF_expo_new__h1443;
  DEF_x__h1348 = DEF__14_MINUS_IF_request_put_BIT_19_THEN_0_MINUS_re_ETC___d24 ? DEF__theResult___fst__h1377 : DEF_request_put_BIT_14___d25;
  DEF_b__h1345 = primShiftL32(15u,
			      15u,
			      (tUInt32)(32767u & ((tUInt32)(DEF_x__h1348))),
			      4u,
			      (tUInt8)(DEF_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_MINUS_ETC___d37));
  DEF__0_CONCAT_IF_14_MINUS_IF_request_put_BIT_19_THE_ETC___d39 = (tUInt8)(DEF_b__h1345 >> 14u);
  DEF_NOT_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_M_ETC___d88 = !DEF_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_MINUS_ETC___d87;
  DEF__12_MINUS_IF_14_MINUS_IF_request_put_BIT_19_THE_ETC___d98 = DEF__12_MINUS_IF_14_MINUS_IF_request_put_BIT_19_THE_ETC___d61 < (tUInt8)2u;
  DEF_NOT_14_MINUS_IF_request_put_BIT_19_THEN_0_MINU_ETC___d51 = !DEF__14_MINUS_IF_request_put_BIT_19_THEN_0_MINUS_re_ETC___d24;
  DEF_x__h710 = primShiftR32(15u,
			     15u,
			     32767u,
			     4u,
			     (tUInt8)(DEF_IF_request_put_BIT_18_0_AND_NOT_request_put_BI_ETC___d16));
  DEF_k__h698 = 32767u & ~DEF_x__h710;
  DEF_y__h2628 = (tUInt8)1u & ~DEF_request_put_BIT_1___d68;
  DEF_x__h2625 = DEF_request_put_BIT_0___d76 & DEF_y__h2628;
  DEF_x__h696 = (tUInt8)15u & ((tUInt8)14u - DEF__0_MINUS_request_put_BITS_19_TO_15_BITS_3_TO_0___d7);
  DEF_k__h653 = primShiftL32(15u, 15u, 1u, 4u, (tUInt8)(DEF_x__h696));
  DEF_IF_request_put_BIT_19_THEN_1_SL_14_MINUS_0_MIN_ETC___d19 = DEF_request_put_BIT_19___d4 ? DEF_k__h653 : DEF_k__h698;
  DEF_IF_request_put_BIT_19_THEN_1_SL_14_MINUS_0_MIN_ETC___d44 = 32767u & (DEF_IF_request_put_BIT_19_THEN_1_SL_14_MINUS_0_MIN_ETC___d19 + DEF_b__h1345);
  DEF_IF_request_put_BIT_19_THEN_1_SL_14_MINUS_0_MIN_ETC___d20 = (tUInt8)(DEF_IF_request_put_BIT_19_THEN_1_SL_14_MINUS_0_MIN_ETC___d19 >> 14u);
  DEF_k_expo__h146 = (((!DEF_IF_request_put_BIT_19_THEN_1_SL_14_MINUS_0_MIN_ETC___d20 && DEF__0_CONCAT_IF_14_MINUS_IF_request_put_BIT_19_THE_ETC___d39) || (DEF_IF_request_put_BIT_19_THEN_1_SL_14_MINUS_0_MIN_ETC___d20 && !DEF__0_CONCAT_IF_14_MINUS_IF_request_put_BIT_19_THE_ETC___d39)) && !((tUInt8)(DEF_IF_request_put_BIT_19_THEN_1_SL_14_MINUS_0_MIN_ETC___d44 >> 14u))) || (DEF_IF_request_put_BIT_19_THEN_1_SL_14_MINUS_0_MIN_ETC___d20 && DEF__0_CONCAT_IF_14_MINUS_IF_request_put_BIT_19_THE_ETC___d39) ? 32767u : DEF_IF_request_put_BIT_19_THEN_1_SL_14_MINUS_0_MIN_ETC___d44;
  DEF_IF_NOT_IF_request_put_BIT_19_THEN_1_SL_14_MINU_ETC___d106 = DEF_k_expo__h146 == 32767u;
  DEF_INV_IF_NOT_IF_request_put_BIT_19_THEN_1_SL_14__ETC___d90 = (tUInt8)1u & ~((tUInt8)((tUInt8)1u & DEF_k_expo__h146));
  DEF_x__h2427 = (tUInt8)1u & ((tUInt8)0u - DEF_request_put_BIT_14___d25);
  DEF_shift_new__h1473 = (tUInt8)3u & DEF_x__h2427;
  DEF__theResult___snd_snd__h1526 = DEF_request_put_BIT_14_5_AND_1_MINUS_14_MINUS_IF_r_ETC___d28 ? DEF_shift_new__h1473 : (tUInt8)0u;
  DEF_shift_new0__h143 = DEF__14_MINUS_IF_request_put_BIT_19_THEN_0_MINUS_re_ETC___d24 ? DEF__theResult___snd_snd__h1526 : (tUInt8)0u;
  DEF_x__h2408 = primShiftR32(13u,
			      13u,
			      (tUInt32)(8191u & ((((tUInt32)(DEF_NOT_14_MINUS_IF_request_put_BIT_19_THEN_0_MINU_ETC___d51)) << 12u) | DEF_bs__h2631)),
			      2u,
			      (tUInt8)(DEF_shift_new0__h143));
  DEF_frac__h145 = (tUInt32)(4095u & DEF_x__h2408);
  DEF_i1__h2632 = (tUInt8)3u & (DEF_shift_new0__h143 - (tUInt8)2u);
  DEF_NOT_14_MINUS_IF_request_put_BIT_19_THEN_0_MINU_ETC___d73 = primExtract8(1u,
									      12u,
									      (tUInt32)(DEF_frac__h145),
									      4u,
									      (tUInt8)(DEF_x__h2582),
									      4u,
									      (tUInt8)(DEF_x__h2582));
  DEF_NOT_14_MINUS_IF_request_put_BIT_19_THEN_0_MINU_ETC___d64 = primExtract8(1u,
									      12u,
									      (tUInt32)(DEF_frac__h145),
									      4u,
									      (tUInt8)(DEF__12_MINUS_IF_14_MINUS_IF_request_put_BIT_19_THE_ETC___d61),
									      4u,
									      (tUInt8)(DEF__12_MINUS_IF_14_MINUS_IF_request_put_BIT_19_THE_ETC___d61));
  DEF_i__h2535 = (tUInt8)3u & (DEF_shift_new0__h143 - (tUInt8)1u);
  DEF_request_put_BITS_13_TO_2_2_BIT_IF_14_MINUS_IF__ETC___d70 = primExtract8(1u,
									      12u,
									      (tUInt32)(DEF_bs__h2631),
									      2u,
									      (tUInt8)(DEF_i__h2535),
									      2u,
									      (tUInt8)(DEF_i__h2535));
  DEF_NOT_NOT_14_MINUS_IF_request_put_BIT_19_THEN_0__ETC___d126 = !DEF_NOT_14_MINUS_IF_request_put_BIT_19_THEN_0_MINU_ETC___d73;
  DEF_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_MINUS_ETC___d67 = DEF_shift_new0__h143 == (tUInt8)0u;
  DEF_IF_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_MI_ETC___d96 = DEF_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_MINUS_ETC___d67 ? !DEF_request_put_BIT_1___d68 : !DEF_request_put_BITS_13_TO_2_2_BIT_IF_14_MINUS_IF__ETC___d70;
  DEF_truncated_frac_msb__h148 = DEF_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_MINUS_ETC___d67 ? DEF_request_put_BIT_1___d68 : DEF_request_put_BITS_13_TO_2_2_BIT_IF_14_MINUS_IF__ETC___d70;
  DEF_flag_prev_truncate__h152 = DEF__12_MINUS_IF_14_MINUS_IF_request_put_BIT_19_THE_ETC___d66 ? DEF_truncated_frac_msb__h148 : DEF_NOT_14_MINUS_IF_request_put_BIT_19_THEN_0_MINU_ETC___d73;
  DEF_NOT_14_MINUS_IF_request_put_BIT_19_THEN_0_MINU_ETC___d102 = primExtract32(32u,
										12u,
										(tUInt32)(DEF_frac__h145),
										4u,
										(tUInt8)(DEF_x__h2742),
										4u,
										(tUInt8)0u) == 0u;
  DEF_y__h2626 = primExtract32(32u,
			       12u,
			       (tUInt32)(DEF_bs__h2631),
			       2u,
			       (tUInt8)(DEF_i1__h2632),
			       2u,
			       (tUInt8)0u) == 0u;
  DEF_request_put_BIT_0_6_AND_INV_request_put_BIT_1__ETC___d83 = DEF_x__h2625 & DEF_y__h2626;
  switch (DEF_shift_new0__h143) {
  case (tUInt8)0u:
    DEF_IF_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_MI_ETC___d133 = !DEF_request_put_BIT_0___d76;
    break;
  case (tUInt8)1u:
    DEF_IF_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_MI_ETC___d133 = !DEF_x__h2625;
    break;
  default:
    DEF_IF_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_MI_ETC___d133 = !DEF_request_put_BIT_0_6_AND_INV_request_put_BIT_1__ETC___d83;
  }
  switch (DEF_shift_new0__h143) {
  case (tUInt8)0u:
    DEF_IF_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_MI_ETC___d85 = DEF_request_put_BIT_0___d76;
    break;
  case (tUInt8)1u:
    DEF_IF_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_MI_ETC___d85 = DEF_x__h2625;
    break;
  default:
    DEF_IF_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_MI_ETC___d85 = DEF_request_put_BIT_0_6_AND_INV_request_put_BIT_1__ETC___d83;
  }
  DEF_NOT_14_MINUS_IF_request_put_BIT_19_THEN_0_MINU_ETC___d108 = DEF_frac__h145 == 0u;
  DEF_NOT_14_MINUS_IF_request_put_BIT_19_THEN_0_MINU_ETC___d157 = (((DEF_NOT_14_MINUS_IF_request_put_BIT_19_THEN_0_MINU_ETC___d64 || (DEF__12_MINUS_IF_14_MINUS_IF_request_put_BIT_19_THE_ETC___d66 ? DEF_IF_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_MI_ETC___d96 : DEF_NOT_NOT_14_MINUS_IF_request_put_BIT_19_THEN_0__ETC___d126)) || DEF_IF_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_MI_ETC___d133) || (DEF_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_MINUS_ETC___d87 && !DEF_INV_IF_NOT_IF_request_put_BIT_19_THEN_1_SL_14__ETC___d90)) || (!DEF__12_MINUS_IF_14_MINUS_IF_request_put_BIT_19_THE_ETC___d66 && ((!DEF__12_MINUS_IF_14_MINUS_IF_request_put_BIT_19_THE_ETC___d93 || DEF_truncated_frac_msb__h148) && (((DEF__12_MINUS_IF_14_MINUS_IF_request_put_BIT_19_THE_ETC___d98 || !DEF_NOT_14_MINUS_IF_request_put_BIT_19_THEN_0_MINU_ETC___d102) || DEF_truncated_frac_msb__h148) && (((DEF_NOT_14_MINUS_IF_request_put_BIT_19_THEN_0_MINU_ETC___d51 || !DEF_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_MINUS_ETC___d67) || DEF_NOT_NOT_14_MINUS_IF_request_put_BIT_19_THEN_0__ETC___d126) && (((!DEF_IF_NOT_IF_request_put_BIT_19_THEN_1_SL_14_MINU_ETC___d106 || DEF_NOT_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_M_ETC___d88) || !DEF_NOT_14_MINUS_IF_request_put_BIT_19_THEN_0_MINU_ETC___d108) || DEF_truncated_frac_msb__h148)))));
  DEF__theResult____h153 = (((!DEF_NOT_14_MINUS_IF_request_put_BIT_19_THEN_0_MINU_ETC___d64 && DEF_flag_prev_truncate__h152) && DEF_IF_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_MI_ETC___d85) && (DEF_NOT_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_M_ETC___d88 || DEF_INV_IF_NOT_IF_request_put_BIT_19_THEN_1_SL_14__ETC___d90)) && (DEF__12_MINUS_IF_14_MINUS_IF_request_put_BIT_19_THE_ETC___d66 || ((DEF__12_MINUS_IF_14_MINUS_IF_request_put_BIT_19_THE_ETC___d93 && DEF_IF_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_MI_ETC___d96) || (((!DEF__12_MINUS_IF_14_MINUS_IF_request_put_BIT_19_THE_ETC___d98 && DEF_NOT_14_MINUS_IF_request_put_BIT_19_THEN_0_MINU_ETC___d102) && DEF_IF_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_MI_ETC___d96) || ((DEF__14_MINUS_IF_request_put_BIT_19_THEN_0_MINUS_re_ETC___d24 && DEF_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_MINUS_ETC___d67) || (((DEF_IF_NOT_IF_request_put_BIT_19_THEN_1_SL_14_MINU_ETC___d106 && DEF_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_MINUS_ETC___d87) && DEF_NOT_14_MINUS_IF_request_put_BIT_19_THEN_0_MINU_ETC___d108) && DEF_IF_IF_14_MINUS_IF_request_put_BIT_19_THEN_0_MI_ETC___d96)))));
  DEF_y__h427 = primShiftR32(15u,
			     15u,
			     (tUInt32)(32767u & ((((tUInt32)((tUInt8)0u)) << 12u) | DEF_frac__h145)),
			     4u,
			     (tUInt8)(DEF__12_MINUS_IF_14_MINUS_IF_request_put_BIT_19_THE_ETC___d61));
  DEF_x__h425 = 32767u & (DEF_k_expo__h146 + DEF_y__h427);
  DEF_y__h426 = 32767u & ((tUInt32)(DEF__theResult____h153));
  DEF_b__h409 = 32767u & (DEF_x__h425 - DEF_y__h426);
  DEF_b__h2950 = 32767u & ((tUInt32)(DEF_flag_prev_truncate__h152));
  DEF_IF_NOT_IF_request_put_BIT_19_THEN_1_SL_14_MINU_ETC___d120 = 32767u & (DEF_b__h409 + DEF_b__h2950);
  DEF_IF_IF_NOT_IF_request_put_BIT_19_THEN_1_SL_14_M_ETC___d124 = (tUInt8)(DEF_b__h409 >> 14u) && !((tUInt8)(DEF_IF_NOT_IF_request_put_BIT_19_THEN_1_SL_14_MINU_ETC___d120 >> 14u)) ? 32767u : DEF_IF_NOT_IF_request_put_BIT_19_THEN_1_SL_14_MINU_ETC___d120;
  DEF_a__h197 = 32767u & (DEF_IF_IF_NOT_IF_request_put_BIT_19_THEN_1_SL_14_M_ETC___d124 + (32767u & ((tUInt32)(DEF_IF_IF_NOT_IF_request_put_BIT_19_THEN_1_SL_14_M_ETC___d124 == 0u && DEF_NOT_14_MINUS_IF_request_put_BIT_19_THEN_0_MINU_ETC___d157))));
  switch ((tUInt8)((tUInt8)3u & (ARG_request_put >> 21u))) {
  case (tUInt8)0u:
    DEF_output_regf_posit__h167 = 65535u & ((((tUInt32)(DEF_request_put_BIT_23___d3)) << 15u) | (DEF_request_put_BIT_23___d3 ? 32767u & (0u - DEF_a__h197) : DEF_a__h197));
    break;
  case (tUInt8)1u:
    DEF_output_regf_posit__h167 = 32768u;
    break;
  default:
    DEF_output_regf_posit__h167 = 0u;
  }
  DEF_IF_request_put_BITS_22_TO_21_EQ_0_THEN_request_ETC___d172 = 1048575u & (((DEF_output_regf_posit__h167 << 4u) | (((tUInt32)((tUInt8)((tUInt8)7u & (ARG_request_put >> 20u)))) << 1u)) | (tUInt32)((tUInt8)1u & (DEF_flag_prev_truncate__h152 - DEF__theResult____h153) || (DEF_a__h197 == 0u && DEF_NOT_14_MINUS_IF_request_put_BIT_19_THEN_0_MINU_ETC___d157)));
  INST_fifo_output_reg.METH_enq(DEF_IF_request_put_BITS_22_TO_21_EQ_0_THEN_request_ETC___d172);
}

tUInt8 MOD_mkNormalizer::METH_RDY_request_put()
{
  tUInt8 DEF_CAN_FIRE_request_put;
  tUInt8 PORT_RDY_request_put;
  DEF_CAN_FIRE_request_put = INST_fifo_output_reg.METH_i_notFull();
  PORT_RDY_request_put = DEF_CAN_FIRE_request_put;
  return PORT_RDY_request_put;
}

tUInt32 MOD_mkNormalizer::METH_response_get()
{
  tUInt32 DEF_response_get__avValue1;
  tUInt32 PORT_response_get;
  DEF_response_get__avValue1 = INST_fifo_output_reg.METH_first();
  PORT_response_get = DEF_response_get__avValue1;
  INST_fifo_output_reg.METH_deq();
  return PORT_response_get;
}

tUInt8 MOD_mkNormalizer::METH_RDY_response_get()
{
  tUInt8 DEF_CAN_FIRE_response_get;
  tUInt8 PORT_RDY_response_get;
  DEF_CAN_FIRE_response_get = INST_fifo_output_reg.METH_i_notEmpty();
  PORT_RDY_response_get = DEF_CAN_FIRE_response_get;
  return PORT_RDY_response_get;
}


/* Reset routines */

void MOD_mkNormalizer::reset_RST_N(tUInt8 ARG_rst_in)
{
  PORT_RST_N = ARG_rst_in;
  INST_fifo_output_reg.reset_RST(ARG_rst_in);
}


/* Static handles to reset routines */


/* Functions for the parent module to register its reset fns */


/* Functions to set the elaborated clock id */

void MOD_mkNormalizer::set_clk_0(char const *s)
{
  __clk_handle_0 = bk_get_or_define_clock(sim_hdl, s);
}


/* State dumping routine */
void MOD_mkNormalizer::dump_state(unsigned int indent)
{
  printf("%*s%s:\n", indent, "", inst_name);
  INST_fifo_output_reg.dump_state(indent + 2u);
}


/* VCD dumping routines */

unsigned int MOD_mkNormalizer::dump_VCD_defs(unsigned int levels)
{
  vcd_write_scope_start(sim_hdl, inst_name);
  vcd_num = vcd_reserve_ids(sim_hdl, 2u);
  unsigned int num = vcd_num;
  for (unsigned int clk = 0u; clk < bk_num_clocks(sim_hdl); ++clk)
    vcd_add_clock_def(sim_hdl, this, bk_clock_name(sim_hdl, clk), bk_clock_vcd_num(sim_hdl, clk));
  vcd_write_def(sim_hdl, bk_clock_vcd_num(sim_hdl, __clk_handle_0), "CLK", 1u);
  vcd_write_def(sim_hdl, num++, "RST_N", 1u);
  num = INST_fifo_output_reg.dump_VCD_defs(num);
  vcd_write_scope_end(sim_hdl);
  return num;
}

void MOD_mkNormalizer::dump_VCD(tVCDDumpType dt, unsigned int levels, MOD_mkNormalizer &backing)
{
  vcd_defs(dt, backing);
  vcd_prims(dt, backing);
}

void MOD_mkNormalizer::vcd_defs(tVCDDumpType dt, MOD_mkNormalizer &backing)
{
  unsigned int num = vcd_num;
  if (dt == VCD_DUMP_XS)
  {
    vcd_write_x(sim_hdl, num++, 1u);
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
    }
    else
    {
      vcd_write_val(sim_hdl, num++, PORT_RST_N, 1u);
      backing.PORT_RST_N = PORT_RST_N;
    }
}

void MOD_mkNormalizer::vcd_prims(tVCDDumpType dt, MOD_mkNormalizer &backing)
{
  INST_fifo_output_reg.dump_VCD(dt, backing.INST_fifo_output_reg);
}