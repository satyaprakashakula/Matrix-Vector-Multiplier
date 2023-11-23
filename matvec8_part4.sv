module matvec8_part4(counttail, counttailplus, wr[0], xr, product, out, sum, preout, state, nextState, clk, reset, input_valid, input_ready, input_data, new_matrix, output_valid, output_ready, output_data);
	
	parameter K = 8;
	input clk, reset, input_valid, new_matrix, output_ready;
	input signed [13:0] input_data;
	output logic signed [27:0] output_data;
	output logic output_valid, input_ready;
	
	output logic signed [13:0] wr [K*K-1:0];
	output logic signed [13:0] xr [K-1:0];
	output logic signed [27:0] out [K-1:0];
	output logic signed [27:0] product [K-1:0];
	output logic signed [27:0] sum [K-1:0];
	output logic signed [27:0] preout [K-1:0];
 
	logic wr_en_w, wr_en_x;
	localparam LOGSIZEW = $clog2(K*K);
	localparam LOGSIZEX = $clog2(K);
	logic [LOGSIZEW-1:0] addr_w;
    logic [LOGSIZEX-1:0] addr_x;
	output logic [1:0] state, nextState;
	logic address;
	
	logic signed [27:0] fifoin;
	logic wr_en_f;
	logic [($clog2(K+1))-1:0] capacity;
	logic [LOGSIZEX-1:0] countloadaddr;
	logic clear_acc, en_acc;
	
	output logic [LOGSIZEX-1:0] counttail, counttailplus;
 
	controlpath #(K) controlpathInst(state, nextState, new_matrix, clk, reset, input_valid, input_ready, wr_en_w, addr_w, wr_en_x, addr_x, address, clear_acc, en_acc, countloadaddr, capacity, wr_en_f);
	datapath #(K) datapathInst(wr, xr, product, out, sum, preout, clk, reset, input_data, wr_en_w, addr_w, wr_en_x, addr_x, address, clear_acc, en_acc, fifoin, countloadaddr);
	output_fifo #(28,K) fifoInst(counttail, counttailplus, clk, reset, fifoin, wr_en_f, capacity, output_data, output_valid, output_ready);
 
 
endmodule 
 
 
module memory(clk, data_in, data_out, addr, wr_en);
	parameter WIDTH=16, SIZE=64;
	localparam LOGSIZE=$clog2(SIZE);
	input [WIDTH-1:0] data_in;
	output logic [WIDTH-1:0] data_out;
	input addr;
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



	
module datapath (wr, xr, product, out, sum, preout, clk, reset, input_data, wr_en_w, addr_w, wr_en_x, addr_x, address, clear_acc, en_acc, outtofifo, countloadaddr);
	
	input clk, reset, wr_en_w, wr_en_x, clear_acc, en_acc;
	input signed [13:0] input_data;
	parameter D = 8;
	localparam LOGSIZEW = $clog2(D*D);
	localparam LOGSIZEX = $clog2(D);
	input [LOGSIZEW-1:0] addr_w;
	input [LOGSIZEX-1:0] addr_x;
	
	output logic signed [13:0] wr [D*D-1:0];
	output logic signed [13:0] xr [D-1:0];
	output logic signed [27:0] sum [D-1:0];
	output logic signed [27:0] preout [D-1:0];
	output logic signed [27:0] product[D-1:0];
	output logic signed [27:0] out [D-1:0];
	input address;
	
	
	logic [D*D-1:0][0:0] en_w;
	logic [D-1:0][0:0] en_x ;
	output logic signed [27:0] outtofifo;
	input [LOGSIZEX-1:0] countloadaddr;
	
	


	generate
		genvar i;
		for (i=0; i < D*D; i=i+1) begin
			memory #(14, 1) w(clk, input_data, wr[i], address, en_w[i]);
		end
	endgenerate
	
	generate
		genvar k;
		for (k=0; k< D*D; k=k+1) begin
			assign en_w[k] = ((k==addr_w) && (wr_en_w==1)) ? 1 : 0;
		end
	endgenerate
	
	generate
		genvar j;
		for (j=0; j < D; j=j+1) begin
			memory #(14, 1) x(clk, input_data, xr[j], address, en_x[j]);
		end
	endgenerate
	
	generate
		genvar m;
		for (m=0; m< D; m=m+1) begin
			assign en_x[m] = ((m==addr_x) && (wr_en_x==1)) ? 1 : 0;
		end
	endgenerate


	
	integer p;
	
	always_comb begin
		for (p=0; p <D; p++) begin
			product[p] = wr[8*p+addr_x] * xr[addr_x];
			sum[p] = out[p] + product[p];
			if ((product[p] > 0) && (out[p] > 0) && (sum[p] < 0))
				preout[p] = 28'h7ffffff;
			else if ((product[p] < 0) && (out[p] < 0) && (sum[p] > 0))
				preout[p] = 28'h8000000;
			else
			
				preout[p] = sum[p];	
			end
	end
	
	
	generate
		genvar s;
		for (s=0; s<D; s++) begin
			always_ff @(posedge clk) begin
				if (clear_acc == 1)
					out[s] <= 0;
				else if (en_acc == 1)
					out[s] <= preout[s];
			end
		end
	endgenerate
	
	
	always_comb begin
		outtofifo = out[countloadaddr];
	end



endmodule




module controlpath (state, nextState, new_matrix, clk, reset, input_valid, input_ready, wr_en_w, countwaddr, wr_en_x, countxaddr, address, clear_acc, en_acc, countloadaddr, capacity, wr_en_f);
	
	input clk, reset, input_valid, new_matrix;
	output input_ready, wr_en_w, wr_en_x;
	logic incrw, clearw, incrx, clearx, incrwaddr, clearwaddr, incrxaddr, clearxaddr, incrload, clearload, incrloadaddr, clearloadaddr;
	logic finish;
	output logic address;
	output logic clear_acc, en_acc;
	
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
	
	
	
	logic [LOGSIZEX-1:0] countload;
	output logic [LOGSIZEXADDR-1:0] countloadaddr; 
	
	counterx #(S+1) counterload(clk, incrload, clearload, countload);
	counterx #(S) counterloadaddr(clk, incrloadaddr, clearloadaddr, countloadaddr);
	
	input [LOGSIZEX-1:0] capacity;
	output logic wr_en_f;
	
	
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
			if (countload<S)
				nextState = READ;
			else 
				nextState = WRITE;
		end
		
		else
			nextState = START;
	end 

	/*
	always_ff @(posedge clk) begin
		if (state == READ) begin
			if (countload < S) begin
				if (countx < S)
					en_acc<=1;
				else if (countx==S)
					en_acc<=0;
			end
		end
	end
	
	*/
	
	assign en_acc = ((state == READ) && (countload<S) && (countx<S));
	
	
	
	assign clearw = ((state == START) || ((state == WRITE) && (countx>S-1)));
	assign clearx = ((state == START) || ((state == WRITE) && (countx>S-1)) || ((state == READ) && (countload>S-1)));
	assign input_ready = ((state == WRITE) && (countx<S));
	assign incrw = ((state == WRITE) && (countx<S) && (input_valid==1) && ((countw==0 && countx==0 && new_matrix==1) || (countw>0 && countw<S*S)));
	assign incrx = (((state == WRITE) && (countx<S) && (input_valid==1) && ((countw==0 && countx==0 && new_matrix==0) || ((countx>0 && countx<S) || (countw>S*S-1)))) || ((state == READ) && (countload<S) && (countx<S+1)));
	
	
	assign clearwaddr = ((state == START) || ((state == WRITE) && (countx>S-1)));
	assign clearxaddr = ((state == START) || ((state == WRITE) && (countx>S-1)) || ((state == READ) && (countload>S-1)));
	assign incrwaddr = ((state == WRITE) && (countx<S) && (input_valid==1) && ((countw==0 && countx==0 && new_matrix==1) || (countw>0 && countw<S*S)));
	assign incrxaddr = (((state == WRITE) && (countx<S) && (input_valid==1) && ((countw==0 && countx==0 && new_matrix==0) || ((countx>0 && countx<S) || (countw>S*S-1)))) || ((state == READ) && (countload<S) && (countx<S+1)));
	

	assign wr_en_w = ((state == WRITE) && (countx<S) && (input_valid==1) && ((countw==0 && countx==0 && new_matrix==1) || (countw>0 && countw<S*S)));
	assign wr_en_x = ((state == WRITE) && (countx<S) && (input_valid==1) && ((countw==0 && countx==0 && new_matrix==0) || ((countx>0 && countx<S) || (countw>S*S-1))));
	
	assign address = 0;
	
	assign clearload = ((state == START) || ((state == READ) && (countload>S-1)));
	assign incrload = ((state == READ) && (countload<S) && (countx>S) && (capacity>0));
	assign clearloadaddr =  ((state == START) || ((state == READ) && (countload>S-1)));
	assign incrloadaddr = ((state == READ) && (countload<S) && (countx>S) && (capacity>0));
	
	assign clear_acc = ((state == START) || ((state == READ) && (countload>S-1)));
	assign wr_en_f = ((state==READ) && (countload<S) && (countx>S) && (capacity>0));
	
	
endmodule


module output_fifo (counttail, counttailplus, clk, reset, data_in, wr_en, capacity, output_data, output_valid, output_ready);
	parameter OUTW=28, DEPTH=8;
	localparam LOGDEPTH=$clog2(DEPTH);
	
	input clk, reset, wr_en, output_ready;
	input signed [OUTW-1:0] data_in;
	output logic signed [OUTW-1:0] output_data;
	output logic output_valid;
	output logic [($clog2(DEPTH+1))-1:0] capacity;
	
	logic clearhead, incrhead, cleartail, clearcap, incrcap, decrcap;
	logic incrtail;
	logic [LOGDEPTH-1:0] counthead;
	output logic [LOGDEPTH-1:0] counttail, counttailplus;
	
	memory_dual_port #(28, DEPTH) memdual(counttailplus, incrtail, data_in, output_data, counthead, counttail, clk, wr_en);
	
	head #(DEPTH) counterhead (clk, clearhead, incrhead, counthead);
	tail #(DEPTH) countertail (clk, cleartail, incrtail, counttail);
	tailplus #(DEPTH) countertailplus (clk, cleartail, incrtail, counttailplus);
	capacity #(DEPTH) countercapacity (clk, clearcap, incrcap, decrcap, capacity);
	
	
	parameter START = 0, OPERATION = 1;
	logic state, nextState;
	
	always_ff @(posedge clk) begin
		if (reset == 1)
			state <= START;
		else
			state <= nextState;
	end
	
	always_comb begin
		if (state == START)
			nextState = OPERATION;
		else if (state == OPERATION)
			nextState = OPERATION;
		else 
			nextState = OPERATION;
	end
	
	
	assign clearhead = (state==START);
	assign cleartail = (state==START);
	assign clearcap = (state==START);
	assign incrhead = ((state==OPERATION) && (wr_en==1) && (capacity>0));
	assign incrtail = ((state==OPERATION) && (capacity<DEPTH-1) && (output_ready==1));
	assign incrcap = ((state==OPERATION) && (capacity<DEPTH-1) && (output_ready==1) && (incrhead==0));
	assign decrcap = ((state==OPERATION) && (wr_en==1) && (capacity>0) && (incrtail==0));
	assign output_valid = ((state==OPERATION) && (capacity<DEPTH-1));
	
	
endmodule 



module memory_dual_port (counttailplus, incrtail, data_in, data_out, write_addr, read_addr, clk, wr_en);

	parameter WIDTH=28, SIZE=8;
	localparam LOGSIZE=$clog2(SIZE);
	logic [SIZE-1:0][WIDTH-1:0] mem;
	
	input incrtail;
	input clk, wr_en;
	input signed [WIDTH-1:0] data_in;
	input [LOGSIZE-1:0] write_addr, read_addr, counttailplus;
	output logic signed [WIDTH-1:0] data_out;


	always_ff @(posedge clk) begin
		/*
		if (incrtail == 1) begin
			if (read_addr==SIZE-1)
				data_out <= mem[0];
			else
				data_out <= mem[read_addr+1];
		end
		else if (incrtail == 1)
			data_out <= mem[read_addr];
		*/
		
		data_out <= (incrtail) ? mem[counttailplus] : mem[read_addr];
		
		if (wr_en) begin
			mem[write_addr] <= data_in;
			if (read_addr == write_addr)
				data_out <= data_in;
		end
	end
	
endmodule


module head(clk, clearhead, incrhead, counthead);
	parameter H = 8;
	localparam LOGSIZE = $clog2(H);
	
	input clk, clearhead, incrhead;
	output logic [LOGSIZE-1:0] counthead;
	
	always_ff @(posedge clk) begin
		if (clearhead == 1)
			counthead<=0;
		else if (incrhead==1) begin
			if (counthead==H-1)
				counthead<=0;
			else
				counthead<=counthead+1;
		end
	end
	
endmodule
	

module tail(clk, cleartail, incrtail, counttail);
	parameter T =8;
	localparam LOGSIZE = $clog2(T);
	
	input clk, cleartail, incrtail;
	output logic [LOGSIZE-1:0] counttail;
	
	always_ff @(posedge clk) begin
		if (cleartail == 1)
			counttail<=0;
		else if (incrtail==1) begin
			if (counttail==T-1)
				counttail<=0;
			else
				counttail<=counttail+1;
		end
	end

endmodule	
	

module tailplus(clk, cleartail, incrtail, counttailplus);
	parameter T =8;
	localparam LOGSIZE = $clog2(T);
	
	input clk, cleartail, incrtail;
	output logic [LOGSIZE-1:0] counttailplus;
	
	always_ff @(posedge clk) begin
		if (cleartail == 1)
			counttailplus<=1;
		else if (incrtail==1) begin
			if (counttailplus==T-1)
				counttailplus<=0;
			else
				counttailplus<=counttailplus+1;
		end
	end

endmodule	



module capacity(clk, clearcap, incrcap, decrcap, countcap);
	parameter C =8;
	localparam LOGSIZE = $clog2(C);
	
	input clk, clearcap, incrcap, decrcap;
	output logic [($clog2(C+1))-1:0] countcap;
	
	always_ff @(posedge clk) begin
		if (clearcap == 1)
			countcap<=C;
		else if (incrcap==1)
			countcap<=countcap+1;
		else if (decrcap==1)
			countcap<=countcap-1;
	end
	
endmodule





  


