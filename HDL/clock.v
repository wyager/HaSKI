/* clock */
`timescale 100fs/100fs
module clock(output clk);
    reg theClock = 1;

    assign clk = theClock;

    always begin
        #500;
        theClock = !theClock;
    end
endmodule
