// Utilizing the memory module provided to store 'W'(9 values of 14 bit each) and 'X'(3 values of 14 bit each)

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

// This main module instantiates datapath and control path modules. It passes the new_matrix signal to control path
module matvec3_part2(clk, reset, input_valid, input_ready, input_data, new_matrix, output_valid, output_ready, output_data);
	input clk, reset, input_valid, output_ready;
	input signed [13:0] input_data; 
	output logic signed [27:0] output_data;
	output logic output_valid, input_ready;
	
	input new_matrix;
	
	logic wr_en_x, wr_en_w, clear_acc, en_acc;
	logic [3:0] addr_w;	logic [1:0] addr_x;
	
	logic [3:0] state, next_state;
	logic [13:0] w, x;
	
	
	parameter [3:0] s = 3;

	
	controlpath c(s, clk, reset, new_matrix, state, next_state, input_ready, input_valid, output_valid, output_ready, wr_en_x, addr_x, wr_en_w, addr_w, clear_acc, en_acc);
	datapath d(clk, reset, input_data, wr_en_x, addr_x, wr_en_w, addr_w, clear_acc, en_acc, output_data, w, x);
endmodule

// Datapath module, based on control signals writes into memory and reads from addresses passed. It does arithmetic, accumulates and displays 
//the values when conveyed by control path to the accumulator

module datapath(clk, reset, input_data, wr_en_x, addr_x, wr_en_w, addr_w, clear_acc, en_acc, output_data, w, x);
	input clk, reset, wr_en_x, wr_en_w, clear_acc, en_acc;
	input signed [13:0] input_data;
	input [3:0] addr_w; input [1:0] addr_x;
	output logic signed [27:0] output_data;
	
	
	output logic signed [13:0] w, x;
	logic signed [27:0] product, sum;
	
	memory #(14, 13) mw(clk, input_data, w, addr_w, wr_en_w);
	memory #(14,3)   mx(clk, input_data, x, addr_x, wr_en_x);
	
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


//Counter module, which is instantiated to generate 2 counters, one to track read/write on 'W' and other on 'X'
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


// This controlpath behaves same for the 1st set of input matrix and vector. When the system receives next set of Input, it decides whether to 
//read the input data as 'W' or 'X' depending on the new_matrix signal. After last output of first set of Matrix and Vector is displayed, control path 
// in the 'DONE' state starts reading inpuit as 'W' if new_matrix == 1, else it starts reading as 'X' and also sets count of 'w' to s*S(9), and moves on to state 
// 'Reading' and starts reading test inputs as it does in Part1. A change to counter module is made to set count of 'W' to s*s. 

module controlpath(s, clk, reset, new_matrix, state, next_state, input_ready, input_valid, output_valid, output_ready, wr_en_x, count_writex, wr_en_w, count_write, clear_acc, en_acc);
	input clk, reset, input_valid, output_ready;
	output wr_en_x, wr_en_w, clear_acc, en_acc;
	
	logic sel;
	
	input new_matrix;
	
	logic clear_write, clear_writex, incr_write, incr_writex;
	output input_ready;
	output output_valid;
	output logic [3:0] count_write;
	output logic [1:0] count_writex;
	
	counter #(4) write(s, sel, clk, clear_write, incr_write, count_write);
	counter #(2) writex(s, sel, clk, clear_writex, incr_writex, count_writex);
	
	input [3:0] s;
	
	parameter [3:0] STATE_START = 0, STATE_READING = 1, STATE_INPUTENDS = 2, STATE_READINPUTS = 3, STATE_FIRSTOUT = 4, STATE_FIRSTOUTREAD = 5, STATE_LASTOUT = 6, STATE_IDLE = 7, STATE_DONE = 8;
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
			if ((input_valid == 1) && (count_write < s*s) && (count_writex < (s-1))) begin
				next_state = STATE_READING;
			end
			else if ((input_valid == 1) && (count_write == s*s) && (count_writex < (s-1))) begin
				next_state = STATE_READING;
			end
			else if ((input_valid ==1) && (count_writex == (s-1)) && (count_write == s*s)) begin
				next_state = STATE_INPUTENDS;
			end
			else 
				next_state = STATE_READING;
		end
		
		else if (state == STATE_INPUTENDS) begin
			next_state = STATE_READINPUTS;
		end
		
		
		else if (state == STATE_READINPUTS) begin
			if ((count_write < s*s) && (count_writex < s)) begin
				next_state = STATE_READINPUTS;
			end
			else if ((count_writex == s) && (count_write < s*s))
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
	assign wr_en_w      = ((state == STATE_READING) && input_valid && (count_write < s*s)) || (state == STATE_DONE && input_valid && new_matrix);
	assign wr_en_x      = (state == STATE_DONE && input_valid && new_matrix == 0) || ((state == STATE_READING) && input_valid && (count_write == s*s) && (count_writex < (s-1))) || ((state == STATE_READING) && input_valid && (count_writex == (s-1)));
	assign en_acc       = ((state == STATE_READINPUTS) && (count_write < s*s) && (count_writex < s)) || ((state == STATE_READINPUTS) && (count_writex == s) && (count_write < s*s)) || (state == STATE_FIRSTOUTREAD) || ((state == STATE_READINPUTS) && (count_write == s*s));
	assign clear_acc    = (state == STATE_START) || ((state == STATE_FIRSTOUT) && output_ready) || ((state == STATE_LASTOUT) && output_ready); 
	assign clear_write 	= (state == STATE_START) || ((state == STATE_LASTOUT) && output_ready)|| ((state == STATE_READING) && input_valid && (count_writex == (s-1))) || ((state == STATE_READINPUTS) && (count_write == s*s));
	assign incr_write   = ((state == STATE_READING) && input_valid && (count_write < s*s))|| (state == STATE_DONE && input_valid && new_matrix) || (state == STATE_INPUTENDS) || ((state == STATE_READINPUTS) && (count_write < s*s) && (count_writex < s)) || ((state == STATE_FIRSTOUT) && output_ready) || (state == STATE_FIRSTOUTREAD);

	
	assign clear_writex = (state == STATE_START) || ((state == STATE_LASTOUT) && output_ready) || ((state == STATE_READING) && input_valid && (count_writex == (s-1))) || ((state == STATE_READINPUTS) && (count_writex == s) && (count_write < s*s)) || ((state == STATE_READINPUTS) && (count_write == s*s));
	assign incr_writex  = (state == STATE_DONE && input_valid && new_matrix == 0) || ((state == STATE_READING) && input_valid && (count_write == s*s) && count_writex < s-1) || (state == STATE_INPUTENDS) || ((state == STATE_READINPUTS) && (count_write < s*s) && (count_writex < s)) || ((state == STATE_FIRSTOUT) && output_ready) || (state == STATE_FIRSTOUTREAD);
	assign sel          = (state == STATE_DONE && input_valid && new_matrix == 0);

	

endmodule
