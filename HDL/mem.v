`timescale 1ps/1ps

module mem(input clk,
    // read ports
    input [29:0]addr, output [63:0]rData,
    input wEnable, input [63:0]wData);

    reg [63:0]mem[16383:0];

    /* Simulation -- read initial content from file */
    initial begin
        $readmemh("mem.hex",mem);
    end

    /* memory address register */
    reg [15:0]in0 = 16'hxxxx;
    reg [15:0]in1 = 16'hxxxx;

    /* memory data register */
    reg [15:0]out0 = 16'hxxxx;
    reg [15:0]out1 = 16'hxxxx;

    assign rdata0 = out0;
    assign rdata1 = out1;

    always @(posedge clk) begin
        if (writeEnable) begin
            $display("#mem[%x] <= %x",writeAddress,writeData);
            mem[writeAddress] <= writeData;
        end
        in0 <= raddr0;
        out0 <= mem[in0];
        in1 <= raddr1;
        out1 <= mem[in1];
    end

endmodule
