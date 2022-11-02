// Instantiate this memory in datapath to store 8*8 w matrix values and 8 'x' values. I pass a counter value as address. I need 4 bits for counter
// to check a condition (== s), whereas to refer 8 storages locations 3 bits are fine. But, memory module shows port size mismatch, when I pass address
//variable of 4 size and size as '8'. SO had to choose memory of '16' size for 'X' even though not needed. Utilizing an extra would solev this, but area is not
// storage locations are not a concern w.r.t 'X'. But they are for 'W', so an extra counter is added here as we move from 3*3 to 8*8.
module memory(clk, data_in, data_out, addr, wr_en);
	parameter WIDTH=16, SIZE=64;
	localparam LOGSIZE=$clog2(SIZE);
	input [WIDTH-1:0] data_in;
	output logic [WIDTH-1:0] data_out;
	input [LOGSIZE-1:0] addr;
	input clk, wr_en;
	logic [SIZE-1:0][WIDTH-1:0] mem;
	always_ff @(posedge clk) begin
	data_out <= mem[addr];
	if (wr_en)
		mem[addr] <= data_in;
	end
endmodule

// Same as Part2 main module instantiates datapath and controlpath.  Parameter 's' is changed to reflect size '8'
module matvec8_part3(clk, reset, input_valid, input_ready, input_data, new_matrix, output_valid, output_ready, output_data);
	input clk, reset, input_valid, output_ready;
	input signed [13:0] input_data; 
	output logic signed [27:0] output_data;
	output logic output_valid, input_ready;
	
	input new_matrix;
	
	logic wr_en_x, wr_en_w, clear_acc, en_acc;
	logic [5:0] addr_w;	logic [3:0] addr_x;
	
	logic [3:0] state, next_state;
	logic [13:0] w, x;
	
	
	parameter [3:0] s = 8;

	
	controlpath c(s, clk, reset, new_matrix, state, next_state, input_ready, input_valid, output_valid, output_ready, wr_en_x, addr_x, wr_en_w, addr_w, clear_acc, en_acc);
	datapath d(clk, reset, input_data, wr_en_x, addr_x, wr_en_w, addr_w, clear_acc, en_acc, output_data, w, x);
endmodule


// Datapath uses increased storage locations, and hence address variables with more bits, control logic flow is parameterized to handle different matrix sizes
module datapath(clk, reset, input_data, wr_en_x, addr_x, wr_en_w, addr_w, clear_acc, en_acc, output_data, w, x);
	input clk, reset, wr_en_x, wr_en_w, clear_acc, en_acc;
	input signed [13:0] input_data;
	input [5:0] addr_w; input [3:0] addr_x;
	output logic signed [27:0] output_data;
	
	
	output logic signed [13:0] w, x;
	logic signed [27:0] product, sum;
	
	memory #(14, 64) mw(clk, input_data, w, addr_w, wr_en_w);
	memory #(14, 16)   mx(clk, input_data, x, addr_x, wr_en_x);
	
	always_ff @(posedge clk) begin
		if (clear_acc == 1)
			output_data <= 0;
		else if (en_acc == 1)
			output_data <= sum;
			
	end
	
	always_comb begin
		product = w * x;
		sum = product + output_data;
		
		if ((product > 0) && (output_data > 0) && (sum < 0))
			sum = 28'h7ffffff;
		else if ((product < 0) && (output_data < 0) && (sum > 0))
			sum = 28'h8000000;
			
	end
	
endmodule


// same implementation of counter as Part2
module counter(s, sel, clk, clear, incr, count);
	
	parameter SIZE = 4;
	input clk, clear, incr, sel;
	output logic [SIZE-1:0] count;
	input [3:0]s;
	
	always_ff @(posedge clk) begin
	if (clear == 1)
		count <= 0;
	else if(incr == 1)
		count <= count + 1;
	else if (sel)
		count <= s*s;
	end
	
endmodule


// We pass value of counter that keeps track of read/write of 'w' as address to the datapath. We require checking a condition (counter value of w = s*s = 64)
// in 'State Reading' where as we  write test inputs into memory 'w', we check if count of w value == 64 (s*s) and if true move to start reading 'x'.
// But since value 64 needs 7 bits , we cannot both use the counter's value to be passed as address and as tracker. So, we added as new counter called 'count_trackw'
//which just keeps tracking of read/write 'w' and has 7-bit size. Other counter 'count_write' has 6-bit size and clears/increments for the same set of 
//conditions as the 7-bit counter, we pass its value to datapath. We are not worried of this counter's value overflowing to 'zero' because
//FSM flow has knowledge of when to consider a value at the output port of memory, and when it wants to write into a location, it actively provides 
//address to write at that location.
module controlpath(s, clk, reset, new_matrix, state, next_state, input_ready, input_valid, output_valid, output_ready, wr_en_x, count_writex, wr_en_w, count_write, clear_acc, en_acc);
	input clk, reset, input_valid, output_ready;
	output wr_en_x, wr_en_w, clear_acc, en_acc;
	
	logic sel;
	input new_matrix;
	
	
	logic clear_write, clear_writex, incr_write, incr_writex, clear_trackw, incr_trackw;
	output input_ready;
	output output_valid;
	output logic [5:0] count_write;
	output logic [3:0] count_writex;
	logic [6:0] count_trackw;
	
	counter #(6) write(s, sel, clk, clear_write, incr_write, count_write);
	counter #(4) writex(s, sel, clk, clear_writex, incr_writex, count_writex);
	counter #(7) trackw(s, sel, clk, clear_trackw, incr_trackw, count_trackw);
	
	input [3:0] s;
	
	parameter [3:0] STATE_START = 0, STATE_READING = 1, STATE_INPUTENDS = 2, STATE_READINPUTS = 3, STATE_FIRSTOUT = 4, STATE_FIRSTOUTREAD = 5, STATE_LASTOUT = 6, STATE_DONE = 7, STATE_IDLE = 8;
	output logic [3:0] state, next_state;
	
	
	 always_ff @(posedge clk) begin
        if (reset == 1)
            state <= STATE_START;
        else
            state <= next_state;
    end
	
	always_comb begin
		if (state == STATE_START)
			next_state = STATE_READING;
		
		else if (state == STATE_READING) begin
			if ((input_valid == 1) && (count_trackw < s*s) && (count_writex < (s-1))) begin
				next_state = STATE_READING;
			end
			else if ((input_valid == 1) && (count_trackw == s*s) && (count_writex < (s-1))) begin
				next_state = STATE_READING;
			end
			else if ((input_valid ==1) && (count_writex == (s-1)) && (count_trackw == s*s)) begin
				next_state = STATE_INPUTENDS;
			end
			else 
				next_state = STATE_READING;
		end
		
		else if (state == STATE_INPUTENDS) begin
			next_state = STATE_READINPUTS;
		end
		
		
		else if (state == STATE_READINPUTS) begin
			if ((count_trackw < s*s) && (count_writex < s)) begin
				next_state = STATE_READINPUTS;
			end
			else if ((count_writex == s) && (count_trackw < s*s))
				next_state = STATE_FIRSTOUT;
			else 	
				next_state = STATE_LASTOUT;
		end
		
		
		else if (state == STATE_FIRSTOUT) begin
			if (output_ready == 1) begin
				next_state = STATE_FIRSTOUTREAD;
			end
			else
				next_state = STATE_FIRSTOUT;
		end
		
		
		else if (state == STATE_FIRSTOUTREAD) begin
			next_state = STATE_READINPUTS;
		end
		
		
		else if (state == STATE_LASTOUT) begin
			if (output_ready == 0)
				next_state = STATE_LASTOUT;
			else
				next_state = STATE_DONE;
		end
		
		
		else if (state == STATE_DONE) begin
			if (input_valid && new_matrix == 1)
				next_state = STATE_READING;
			else if (input_valid && new_matrix == 0)
				next_state = STATE_READING;
			else 
				next_state = STATE_DONE;
		end
		
		else 
			next_state = STATE_IDLE;
		
	end
	
	
	assign output_valid = ((state == STATE_FIRSTOUT) || (state == STATE_LASTOUT));
	assign input_ready  = (state == STATE_READING) || (state == STATE_DONE);
	assign wr_en_w      = ((state == STATE_READING) && input_valid && (count_trackw < s*s)) || (state == STATE_DONE && input_valid && new_matrix);
	assign wr_en_x      = (state == STATE_DONE && input_valid && new_matrix == 0) || ((state == STATE_READING) && input_valid && (count_trackw == s*s) && (count_writex < (s-1))) || ((state == STATE_READING) && input_valid && (count_writex == (s-1)));
	assign en_acc       = ((state == STATE_READINPUTS) && (count_trackw < s*s) && (count_writex < s)) || ((state == STATE_READINPUTS) && (count_writex == s) && (count_trackw < s*s)) || (state == STATE_FIRSTOUTREAD) || ((state == STATE_READINPUTS) && (count_trackw == s*s));
	assign clear_acc    = (state == STATE_START) || ((state == STATE_FIRSTOUT) && output_ready) || ((state == STATE_LASTOUT) && output_ready); 
	assign clear_write 	= (state == STATE_START) || ((state == STATE_LASTOUT) && output_ready)|| ((state == STATE_READING) && input_valid && (count_writex == (s-1))) || ((state == STATE_READINPUTS) && (count_trackw == s*s));
	assign incr_write   = ((state == STATE_READING) && input_valid && (count_trackw < s*s))|| (state == STATE_DONE && input_valid && new_matrix) || (state == STATE_INPUTENDS) || ((state == STATE_READINPUTS) && (count_trackw < s*s) && (count_writex < s)) || ((state == STATE_FIRSTOUT) && output_ready) || (state == STATE_FIRSTOUTREAD);

	
	assign clear_writex = (state == STATE_START) || ((state == STATE_LASTOUT) && output_ready) || ((state == STATE_READING) && input_valid && (count_writex == (s-1))) || ((state == STATE_READINPUTS) && (count_writex == s) && (count_trackw < s*s)) || ((state == STATE_READINPUTS) && (count_trackw == s*s));
	assign incr_writex  = (state == STATE_DONE && input_valid && new_matrix == 0) || ((state == STATE_READING) && input_valid && (count_trackw == s*s) && count_writex < s-1) || (state == STATE_INPUTENDS) || ((state == STATE_READINPUTS) && (count_trackw < s*s) && (count_writex < s)) || ((state == STATE_FIRSTOUT) && output_ready) || (state == STATE_FIRSTOUTREAD);
	assign sel          = (state == STATE_DONE && input_valid && new_matrix == 0);

	assign clear_trackw 	= (state == STATE_START) || ((state == STATE_READING) && input_valid && (count_writex == (s-1))) || ((state == STATE_READINPUTS) && (count_trackw == s*s));
	assign incr_trackw   = ((state == STATE_READING) && input_valid && (count_trackw < s*s))|| (state == STATE_DONE && input_valid && new_matrix) || (state == STATE_INPUTENDS) || ((state == STATE_READINPUTS) && (count_trackw < s*s) && (count_writex < s)) || ((state == STATE_FIRSTOUT) && output_ready) || (state == STATE_FIRSTOUTREAD);


	
	

endmodule

