`timescale 1ns / 1ps
// Just a clock divider

module clock_divider(
    input clk,
    output divided_clk //Goal: 1Hz
    );
    
integer counter_value = 0;

always @ (posedge clk)
begin
    if (counter_value == 1000)
        counter_value = 0;
    else
        counter_value <= counter_value + 1;
        
end

endmodule
