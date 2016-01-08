// Automatically generated Verilog-2001
module SevenSeg_anode_1(ds1_i1
                       ,topLet_o);
  input [1:0] ds1_i1;
  output [3:0] topLet_o;
  reg [3:0] topLet_o_reg;
  always @(*) begin
    case(ds1_i1)
      2'd0 : topLet_o_reg = 4'b0001;
      2'd1 : topLet_o_reg = 4'b0010;
      2'd2 : topLet_o_reg = 4'b0100;
      2'd3 : topLet_o_reg = 4'b1000;
      default : topLet_o_reg = {4 {1'bx}};
    endcase
  end
  assign topLet_o = topLet_o_reg;
endmodule
