`timescale 1ns / 1ps

module TM(
    input sw,
    input clk,
    output led
    );

process(sw)
begin
    led <=sw;
end    


endmodule
