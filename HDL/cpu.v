`timescale 100fs/100fs

module main();

    wire clk;
    clock c0(clk);

    reg rst = 1;
    initial begin
        rst = 1;
        #1000;
        rst = 0;
    end

    wire [33:0] out;
    wire output_valid = out[33];
    wire [31:0] output_data = out[32:1];
    wire halt = out[0];

    haski evaluator(.clk(clk), .rst(rst), .out(out));

    always @(posedge clk) begin
        if (output_valid)
            $write("%c", output_data[7:0]);
        if (halt)
            $finish;
    end

endmodule
