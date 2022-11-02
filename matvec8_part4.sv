//instantiate 72 of these memories, 64 for 'w' and 8 for 'x'
module memory(clk, data_in, data_out, addr, wr_en);
	parameter WIDTH=14, SIZE=1;
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

module matvec8_part4(clk, reset, input_valid, input_ready, input_data, new_matrix, output_valid, output_ready, output_data);
	
	input clk, reset, input_valid, output_ready;
	input signed [13:0] input_data; 
	output logic signed [27:0] output_data;
	output logic output_valid, input_ready;
	
	input new_matrix;
	
	parameter s = 8;
	
	logic [3:0] en;
	logic addr_w, addr_x;
	logic [s-1:0][s-1:0] wr_en_w;
	logic [s-1:0] wr_en_x;
	
	
	controlpath c(s, clk, reset, new_matrix, input_ready, input_valid, output_valid, output_ready, wr_en_x, addr_x, wr_en_w, addr_w, en);
	datapath d(s, clk, reset, input_data, wr_en_x, addr_x, wr_en_w, addr_w, output_data, en);
endmodule

module datapath(s,clk, reset, input_data, wr_en_x, addr_x, wr_en_w, addr_w, output_data, en);
	input clk, reset;
	input signed [13:0] input_data;
	output logic signed [27:0] output_data;
	
	input [3:0] en;
	input [3:0] s;
	
	logic signed [s-1:0][27:0]sum;
	logic signed [s-1:0] [s-1:0] [27:0]product;
	logic addr_w, addr_x;
	
	logic [s-1:0] [s-1:0] [13:0]mw;
	output logic signed [s-1:0][s-1:0] [13:0] w;
	logic [s-1:0][s-1:0] wr_en_w;
	
	logic [s-1:0] [13:0]mx;
	output logic signed [s-1:0] [13:0] x;
	logic [s-1:0] wr_en_x;
	
	initial begin
		for (int i =0; i < s ; i++) begin
			for (int j=0; j < s; j++) begin
				memory mw[i][j](clk, input_data, w[i][j], addr_w, wr_en_w[i][j]);
			end
		end
	end 
	
	initial begin
		for (int i =0; i < s ; i++) begin
			memory mx[i](clk, input_data, x[j], addr_x, wr_en_x[i]);
		end 
	end
	
	
	always_comb begin 
		for (logic i=0; i < s*s; i++) begin 
			for (logic j=0; j<s; j++) begin 
				always_ff @(posedge clk) begin 
					if (clear == 1)
						product [i][j] <= 0;
					else 
						product [i][j] <= w[i]*x[j];
				end
			end
		end
	end 
	
	always_comb begin
		for (i=0; i<s; i++) begin
			for (j=0; j < s; j++) begin 
				if (product[i][j] > 0 && sum[i] > 0 && (sum[i] + product[i][j] < 0))
					sum[i] = 28'h7ffffff;
				else if (product[i][j] < 0 && (output_data < 0) && (sum > 0))
					sum[i] = 28'h8000000;
				else 
					sum [i] += product [i][j];
			end
		end
	end
	
	assign output_data = sum[en];
	
endmodule

//as we read test inputs, we write first 64 into an array to create a matrix, and next 8 into another. Then Datapath generates 64 partial prodcuts
// all at once using 64 multipliers row by column fashion using for loop. Here we have 64 regiosters between partial products stage and addition stage,
//to reduce critical path. In the next we generate 8 outputs , bunching 8 corresponding partial products each. Datapath stores these 8 outputs in an array,
// controlpath based on output ready and output tracking counter , relays count, which acts as the array location and datapath displays the value at that
//location as output data. After all outputs are displayed system starts taking in new set if test inputs.

module controlpath(s, clk, reset, new_matrix, input_ready, input_valid, output_valid, output_ready, wr_en_x, addr_x, wr_en_w, addr_w, count_out);
	input clk, reset, input_valid, output_ready, new_matrix;
	
	logic [s-1:0][s-1:0] wr_en_w;
	logic [s-1:0] wr_en_x;
	parameter addr_w = 0, addr_x = 0;
	
	input [3:0] s;
	
	logic clear_countw, incr_countw, clear_countx, incr_countx, clear_countout, incr_countout;
	logic [6:0] count_w;
	logic [3:0] count_x;
	logic [3:0] count_out;
	
	counter #(7) count_w(s, clk, clear_countw, incr_countw, count_w);
	counter #(4) count_x(s, clk, clear_countx, incr_countx, count_x);
	counter #(4) count_out(s, clk, clear_countout, incr_countout, count_out);


	
	parameter [3:0] STATE_START = 0, STATE_WRITE = 1, STATE_LASTINPUT = 2, STATE_READING = 3, STATE_REG = 4, STATE_OUTPUT = 5, STATE_DONE = 6, STATE_IDLE = 7;
	logic [3:0] state, next_state;
	
	always_ff @(posedge clk) begin
        if (reset == 1)
            state <= STATE_START;
        else
            state <= next_state;
    end
	
	always_comb begin
		if (state == STATE_START)
			next_state = STATE_WRITE;
			
		else if (state == STATE_WRITE) begin
			if ((input_valid == 1) && (count_w < s*s) && (count_x < (s-1))) begin
				next_state = STATE_WRITE;
			end
			else if ((input_valid == 1) && (count_w == s*s) && (count_writex < (s-1))) begin
				next_state = STATE_WRITE;
			end
			else if ((input_valid ==1) && (count_x == (s-1)) && (count_w == s*s)) begin
				next_state = STATE_LASTINPUT;
			end
			else 
				next_state = STATE_WRITE;
		end
		
		else if (state == STATE_LASTINPUT) begin
			next_state = STATE_READING;
		end
		
		else if (state == STATE_READING) begin
			next_state = STATE_REG;
		
		end
		
		else if (state == STATE_REG) begin
			if (output_ready ==1)
				next_state = STATE_OUTPUT;
			else
				next_state = STATE_REG;
		end
		
		else if (state == STATE_OUTPUT) begin 
			if (output_ready == 1 && count_out < s-1)
				next_state = STATE_OUTPUT;
			else if (output_ready == 1 && count_out == s-1)
				next_state = STATE_DONE;
			else if (output_ready == 0)
		end
		
		
		else if (state == STATE_DONE) begin
			if (input_valid && new_matrix == 1)
				next_state = STATE_WRITE;
			else if (input_valid && new_matrix == 0) begin
				count_w == s*s;
				next_state = STATE_WRITE;
			end 
			else 
				next_state = STATE_DONE;
		end
		
		else 
			next_state = STATE_IDLE;
		
	end
	


	assign output_valid  =  state == STATE_REG || state = STATE_OUTPUT;
	assign input_ready   =  state == STATE_WRITE;
	
	assign wr_en_w [count_w/8][count_w%8)] =   state == STATE_WRITE && input_valid == 1 && count_w < s*s && count_x < (s-1) || state == STATE_DONE && input_valid && new_matrix == 1;
	assign wr_en_x [count_x]  = (state == STATE_WRITE && input_valid == 1 && count_w == s*s && count_writex < (s-1)) || (state == STATE_WRITE && input_valid ==1 && count_x == (s-1) && count_w == s*s) || state == STATE_DONE && input_valid && new_matrix == 0;

	assign clear_countw  = state == STATE_START || state == STATE_WRITE && input_valid ==1 && count_x == (s-1) && count_w == s*s;
	assign incr_countw   = state == STATE_WRITE && input_valid == 1 && count_w < s*s && count_x < (s-1) || state == STATE_DONE && input_valid && new_matrix == 1;

	
	assign clear_countx   = state == STATE_START || state == STATE_WRITE && input_valid ==1 && count_x == (s-1) && count_w == s*s;
	assign incr_countx    = state == STATE_WRITE && input_valid == 1 && count_w == s*s && count_writex < (s-1) || state == STATE_DONE && input_valid && new_matrix == 0;
	
	
	assign clear_countout = state == STATE_START || state == STATE_DONE;
	assign incr_countout  = state == STATE_REG && output_ready ==1 || state == STATE_OUTPUT && output_ready == 1 && count_out < s-1;

	


endmodule
	
module counter(clk, clear, incr, count);
	
	parameter SIZE = 4;
	input clk, clear, incr;
	output logic [SIZE-1:0] count;
	
	always_ff @(posedge clk) begin
	if (clear == 1)
		count <= 0;
	else if(incr == 1)
		count <= count + 1;
	end
	
endmodule


