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

//Main module instantiates datapath and controlpath module. Designed controlpath as Mealy fsm, Datapath takes in the input data, does operations
// and displays output based on the control signals received from controlpath. Matrix size is parametrized(although not exhaustively, 
//i.e., some variables are assigned sizes and values separately)

module matvec3_part1(clk, reset, input_valid, input_ready, input_data, output_valid, output_ready, output_data);
	input clk, reset, input_valid, output_ready;
	input signed [13:0] input_data; 
	output logic signed [27:0] output_data;
	output logic output_valid, input_ready;
	
	logic wr_en_x, wr_en_w, clear_acc, en_acc;
	logic [3:0] addr_w;	logic [1:0] addr_x;
	
	logic [2:0] state, next_state;
	logic [13:0] w, x;
	
	parameter [3:0] s = 3;

	controlpath c(s, clk, reset, state, next_state, input_ready, input_valid, output_valid, output_ready, wr_en_x, addr_x, wr_en_w, addr_w, clear_acc, en_acc);
	datapath d(clk, reset, input_data, wr_en_x, addr_x, wr_en_w, addr_w, clear_acc, en_acc, output_data, w, x);
endmodule

// Based on control signals datapath writes and read data into memory. Arithmetic is done always, based on control signal accumulator adds up, clears
//and displays output values. Saturates the sum when overflow occurs.

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


// Built a counter to keep track of writing 'w' and 'x' into memories and also while reading. Created 2 instantiations of this counter, one keeps
// track of both writing 'W' into memory and reading from memory, and the other keeps track of 'X'. The same counter which is used to keep track of writing, 
//is cleared after the 'reading inputs' is finished , and used when reading from memory starts. Counters value is passed as address to datapath/main module.

module counter(clk, clear, incr, count);
	
	parameter SIZE = 4;
	input clk, clear, incr;
	output logic [SIZE-1:0] count;
	
	always_ff @(posedge clk) begin
	if (clear == 1)
		count <= 0;
	else if(incr)
		count <= count + 1;
	end
	
endmodule


//As a reset is hit, code flows into start state where it clears counters tracking 'w' and 'x and flow heads into 'state reading'. Here we read inputs 
// based on handshaking and counter values. Once it reads last input, and approaches 'state read inputs from memory', it clear the counters to 
// reuse these  to track reading  from 'w' and 'x'.  
//writes test inputs into memory, depending on the counter values which keep track of writing into 'w' and 'x'. We multiply matrices and keep incrementing
//  respective counters, once counter of 'x' reaches 3 and output is ready, we reset counter 'x back to 0 and resume counter 'w'. As we are about to display
//last output we had into 'state last out'. As we move into this state, we clear counters to be ready for next cycle use. Once we are in this state we have 
// last output of the operation available, so we set output is valid. If output is not not ready we keep looping, else we clear accumulator and move to 
//'state reading'. At this posedge handshaking happens and last output value is read. We start the process of reading test inputs as we are in state reading.

module controlpath(s, clk, reset, state, next_state, input_ready, input_valid, output_valid, output_ready, wr_en_x, count_writex, wr_en_w, count_write, clear_acc, en_acc);
	input clk, reset, input_valid, output_ready;
	output wr_en_x, wr_en_w, clear_acc, en_acc;
	
	logic clear_write, clear_writex, incr_write, incr_writex;
	output input_ready;
	output output_valid;
	output logic [3:0] count_write;
	output logic [1:0] count_writex;
	
	counter #(4) write(clk, clear_write, incr_write, count_write);
	counter #(2) writex(clk, clear_writex, incr_writex, count_writex);
	
	input [3:0] s;
	
	parameter [2:0] STATE_START = 0, STATE_READING = 1, STATE_INPUTENDS = 2, STATE_READINPUTS = 3, STATE_FIRSTOUT = 4, STATE_FIRSTOUTREAD = 5, STATE_LASTOUT = 6, STATE_IDLE = 7;
	output logic [2:0] state, next_state;
	
	
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
				next_state = STATE_READING;
		end
		
		
		else 
			next_state = STATE_IDLE;
		
	end
	
// control signals are asserted high when system is in a certain state and specific condition is met, else signals remain zero. 
	
	assign output_valid = ((state == STATE_FIRSTOUT) || (state == STATE_LASTOUT));
	assign input_ready  = (state == STATE_READING);
	assign wr_en_w      = ((state == STATE_READING) && input_valid && (count_write < s*s));
	assign wr_en_x      = ((state == STATE_READING) && input_valid && (count_write == s*s) && (count_writex < (s-1))) || ((state == STATE_READING) && input_valid && (count_writex == (s-1)));
	assign en_acc       = ((state == STATE_READINPUTS) && (count_write < s*s) && (count_writex < s)) || ((state == STATE_READINPUTS) && (count_writex == s) && (count_write < s*s)) || (state == STATE_FIRSTOUTREAD) || ((state == STATE_READINPUTS) && (count_write == s*s));
	assign clear_acc    = (state == STATE_START) || ((state == STATE_FIRSTOUT) && output_ready) || ((state == STATE_LASTOUT) && output_ready); 
	assign clear_write 	= (state == STATE_START) || ((state == STATE_LASTOUT) && output_ready)|| ((state == STATE_READING) && input_valid && (count_writex == (s-1))) || ((state == STATE_READINPUTS) && (count_write == s*s));
	assign incr_write   = ((state == STATE_READING) && input_valid && (count_write < s*s))|| (state == STATE_INPUTENDS) || ((state == STATE_READINPUTS) && (count_write < s*s) && (count_writex < s)) || ((state == STATE_FIRSTOUT) && output_ready) || (state == STATE_FIRSTOUTREAD);

	
	assign clear_writex = (state == STATE_START) || ((state == STATE_LASTOUT) && output_ready) || ((state == STATE_READING) && input_valid && (count_writex == (s-1))) || ((state == STATE_READINPUTS) && (count_writex == s) && (count_write < s*s)) || ((state == STATE_READINPUTS) && (count_write == s*s));
	assign incr_writex  = ((state == STATE_READING) && input_valid && (count_write == s*s) && count_writex < s-1) || (state == STATE_INPUTENDS) || ((state == STATE_READINPUTS) && (count_write < s*s) && (count_writex < s)) || ((state == STATE_FIRSTOUT) && output_ready) || (state == STATE_FIRSTOUTREAD);

endmodule
