module matvec8_part3(state, nextState, clk, reset, input_valid, input_ready, input_data, new_matrix, output_valid, output_ready, output_data);
	
	input clk, reset, input_valid, new_matrix, output_ready;
	input signed [13:0] input_data;
	output logic signed [27:0] output_data;
	output logic output_valid, input_ready;
 
	logic wr_en_w, wr_en_x, clear_acc, en_acc;
	parameter K = 8;
	localparam LOGSIZEW = $clog2(K*K);
	localparam LOGSIZEX = $clog2(K);
	logic [LOGSIZEW-1:0] addr_w;
    logic [LOGSIZEX-1:0] addr_x;
	output logic [1:0] state, nextState;
 
	controlpath #(8) controlpathInst(state, nextState, new_matrix, clk, reset, input_valid, input_ready, wr_en_w, addr_w, wr_en_x, addr_x, clear_acc, en_acc, output_valid, output_ready);
	datapath #(8) datapathInst(clk, reset, input_data, wr_en_w, addr_w, wr_en_x, addr_x, clear_acc, en_acc, output_data);
 
 
endmodule 
 
 
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
 

module counterw(clk, incrw, clearw, countw);
	input clk, clearw, incrw;
	parameter W = 8;
	localparam LOGSIZE = $clog2(W*W);
	output logic [LOGSIZE-1:0] countw;
	always_ff @(posedge clk) begin
		if (clearw == 1)
			countw<=0;
		else if (incrw)
			countw<=countw+1;
	end
endmodule


module counterx(clk, incrx, clearx, countx);
	input clk, clearx, incrx;
	parameter X =8;
	localparam LOGSIZE = $clog2(X);
	output logic [LOGSIZE-1:0] countx;
	always_ff @(posedge clk) begin
		if (clearx == 1)
			countx<=0;
		else if (incrx)
			countx<=countx+1;
	end
endmodule

	
module datapath (clk, reset, input_data, wr_en_w, addr_w, wr_en_x, addr_x, clear_acc, en_acc, output_data);
	
	input clk, reset, wr_en_w, wr_en_x, clear_acc, en_acc;
	input signed [13:0] input_data;
	output logic signed [27:0] output_data;
	parameter D = 8;
	localparam LOGSIZEW = $clog2(D*D);
	localparam LOGSIZEX = $clog2(D);
	input [LOGSIZEW-1:0] addr_w;
	input [LOGSIZEX-1:0] addr_x;
	
	logic signed [13:0] wr, xr;
	logic signed [27:0] product, sum, preout, in_temp, out_temp;
	
	memory #(14, 64) mw(clk, input_data, wr, addr_w, wr_en_w);
	memory #(14, 8) mx(clk, input_data, xr, addr_x, wr_en_x);

/*
	always_comb begin
		product = wr*xr;
		sum = output_data + product;
		
		if ((product > 0) && (output_data > 0) && (sum < 0))
			preout = 28'h7ffffff;
		else if ((product < 0) && (output_data < 0) && (sum > 0))
			preout = 28'h8000000;
		else
			preout = sum;
	end
*/

	always_comb begin
		in_temp = wr*xr;
		sum = output_data + out_temp;
		if ((out_temp > 0) && (output_data > 0) && (sum < 0))
			preout = 28'h7ffffff;
		else if ((out_temp < 0) && (output_data < 0) && (sum > 0))
			preout = 28'h8000000;
		else
			preout = sum;
	end
	
	
	always_ff @(posedge clk) begin
		if (reset == 1)
			out_temp <= 0;
		else
			out_temp <= in_temp;
	end

	always_ff @(posedge clk) begin
		if (clear_acc == 1)
			output_data <= 0;
		else if (en_acc == 1)
			output_data <= preout;
	end


endmodule




module controlpath (state, nextState, new_matrix, clk, reset, input_valid, input_ready, wr_en_w, countwaddr, wr_en_x, countxaddr, clear_acc, en_acc, output_valid, output_ready );
	
	input clk, reset, input_valid, new_matrix, output_ready;
	output input_ready, wr_en_w, wr_en_x, clear_acc;
	output logic output_valid, en_acc;
	logic incrw, clearw, incrx, clearx, incrwaddr, clearwaddr, incrxaddr, clearxaddr;
	logic finish;
	logic en_temp;
	
	parameter [1:0] START = 0, WRITE = 1, READ = 2;
	output logic [1:0] state, nextState;
	
	parameter S = 8;
	localparam LOGSIZEW=$clog2(S*S+1);
	localparam LOGSIZEX=$clog2(S+1);
	logic [LOGSIZEW-1:0] countw;
	logic [LOGSIZEX-1:0] countx;
	
	counterw #(S+1) counterwInst(clk, incrw, clearw, countw);
	counterx #(S+1) counterxInst(clk, incrx, clearx, countx);
	
	localparam LOGSIZEWADDR=$clog2(S*S);
	localparam LOGSIZEXADDR=$clog2(S);
	output logic [LOGSIZEWADDR-1:0] countwaddr;
	output logic [LOGSIZEXADDR-1:0] countxaddr;
	
	counterw #(S) counterwInstwaddr(clk, incrwaddr, clearwaddr, countwaddr);
	counterx #(S) counterxInstxaddr(clk, incrxaddr, clearxaddr, countxaddr);
	
	always_ff @(posedge clk) begin
		if (reset == 1)
			state<=START;
		else 
			state<=nextState;
	end
	
	
	always_comb begin
		if (state == START)
			nextState = WRITE;
		
		else if (state == WRITE) begin
			if (countx<S)
				nextState = WRITE;
			else
				nextState = READ;
		end
		
		else if (state == READ) begin
			if (countw<S*S+1 && finish==0)
				nextState = READ;
			else 
				nextState = WRITE;
		end
		
		else
			nextState = START;
	end 


	always_ff @(posedge clk) begin
		if (state == WRITE) begin
			if (countx>S-1)
				output_valid<=0;	
		end
		else if (state == READ) begin
			if (countw<S*S+1 && finish==0) begin
				if (output_valid==0 || output_ready==0) begin
					if (output_valid==0) begin
						if (countx<S) begin
							en_temp<=1;
							en_acc<=en_temp;
						end
						else if (countx==S) begin
							en_temp<=0;
							en_acc<=en_temp;
						end
						else if (countx==S+1) begin
							output_valid<=1;
							en_acc<=en_temp;
						end
					end  
				end
				else
					output_valid<=0;
			end
			else
				output_valid<=0;
		end
	end
	
	
	
	
	
	assign clearw = ((state == START) || ((state == WRITE) && (countx>S-1)) || ((state == READ) && (finish==1)));
	assign clearx = ((state == START) || ((state == WRITE) && (countx>S-1)) || ((state == READ) && (countw<S*S+1 && finish==0) && (output_valid==0 || output_ready==0) && (output_valid==0) && (countx==S+1)) || ((state == READ) && (finish==1)));
	assign clear_acc = ((state == START) || ((state == READ) && (finish==1)) || ((state == READ) && (countw<S*S+1 && finish==0) && (output_valid==1 && output_ready==1) && (countw<S*S)));
	assign input_ready = ((state == WRITE) && (countx<S));
	assign wr_en_w = ((state == WRITE) && (countx<S) && (input_valid==1) && ((countw==0 && countx==0 && new_matrix==1) || (countw>0 && countw<S*S)));
	assign incrw = (((state == WRITE) && (countx<S) && (input_valid==1) && ((countw==0 && countx==0 && new_matrix==1) || (countw>0 && countw<S*S))) || ((state == READ) && (countw<S*S+1 && finish==0) && (output_valid==0 || output_ready==0) && (output_valid==0) && (countx<S)));
	assign wr_en_x = ((state == WRITE) && (countx<S) && (input_valid==1) && ((countw==0 && countx==0 && new_matrix==0) || ((countx>0 && countx<S) || (countw>S*S-1))));
	assign incrx = (((state == WRITE) && (countx<S) && (input_valid==1) && ((countw==0 && countx==0 && new_matrix==0) || ((countx>0 && countx<S) || (countw>S*S-1)))) || ((state == READ) && (countw<S*S+1 && finish==0) && (output_valid==0 || output_ready==0) && (output_valid==0) && (countx<S+1)));
	assign finish = ((state == READ) && (output_valid==1 && output_ready==1) && (countw==S*S));
	
	assign clearwaddr = ((state == START) || ((state == WRITE) && (countx>S-1)) || ((state == READ) && (finish==1)));
	assign clearxaddr = ((state == START) || ((state == WRITE) && (countx>S-1)) || ((state == READ) && (countw<S*S+1 && finish==0) && (output_valid==0 || output_ready==0) && (output_valid==0) && (countx==S+1)) || ((state == READ) && (finish==1)));
	assign incrwaddr = (((state == WRITE) && (countx<S) && (input_valid==1) && ((countw==0 && countx==0 && new_matrix==1) || (countw>0 && countw<S*S))) || ((state == READ) && (countw<S*S+1 && finish==0) && (output_valid==0 || output_ready==0) && (output_valid==0) && (countx<S)));
	assign incrxaddr = (((state == WRITE) && (countx<S) && (input_valid==1) && ((countw==0 && countx==0 && new_matrix==0) || ((countx>0 && countx<S) || (countw>S*S-1)))) || ((state == READ) && (countw<S*S+1 && finish==0) && (output_valid==0 || output_ready==0) && (output_valid==0) && (countx<S+1)));
	
	
endmodule


