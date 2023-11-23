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



/*


module check_timing();

   logic               clk, reset, input_valid, input_ready, output_valid, output_ready, new_matrix;

   
   logic signed [13:0] input_data;
   logic signed [27:0] output_data;
   logic [1:0] state, nextState;
   logic signed [13:0] wr [63:0];
   logic signed [27:0] out [7:0];
   logic signed [13:0] xr [7:0];
   logic signed [27:0] product [7:0];
   logic signed [27:0] sum [7:0]; 
   logic signed [27:0]  preout [7:0];
   
   logic [2:0] counttail, counttailplus;

   initial clk=0;
   always #5 clk = ~clk;


   // Instantiate DUT
   matvec8_part4 dut (counttail, counttailplus, wr[0], xr, product, out, sum, preout, state, nextState, clk, reset, input_valid, input_ready, input_data, new_matrix, output_valid, output_ready, output_data);


   //////////////////////////////////////////////////////////////////////////////////////////////////
   // code to feed some test inputs

   // rb and rb2 represent random bits. Each clock cycle, we will randomize the value of these bits.
   // When rb is 0, we will not let our testbench send new data to the DUT.
   // When rb is 1, we can send data.
   logic rb, rb2;
   always begin
      @(posedge clk);
      #1;
      std::randomize(rb, rb2); // randomize rb
   end

   // Put our test data into these arrays. These are the values we will feed as input into the system.
   logic [13:0] invals[0:79] = '{10, -20,   30, -40,   50,  -60,   70,  80,   // first row of matrix
                                -90, 100, -110, 120, -130,  140, -150, 160,
                                 -5,  15,  -55,  42,  119,  -41,    6,  -2,
                                 11, -11,   15, -92,   36,  -44,   17,  22,
                                -84, 106, -104, 126,  115,  -45,    2,  -6,
                                  1,  21,  -49,  48,   32,  -48,   13,  18,
                                 17,  -5,   21, -86,  111,  -49,   -2, -10,
                                -78, 112,  -98, 132,   28,  -52,    9,  14,   // last row of matrix
                                -50,  40,   32, -16,   11,  -49,   49, 111,   // first input vector
                                 22, -41,   42,  62,    4,  -55,    7, -8};   // second input vector


   logic [0:79] newInputMatrixVals = {1'b1, 71'bx, 1'b0, 7'bx};

   logic signed [27:0] expectedOut[0:15] = '{16100, 5180, 1808, 6789, 5998, 3793, 3412, 7983, // first output vector
                                             3170, -13810, 2358, -1874, 247, 2794, -666, 683}; // second output vector

   logic [15:0] j;

   // If input_valid is set to 1, we will put data on input_data.
   // If input_valid is 0, we will put an X on the input_data to test that your system does not
   // process the invalid input.
   always @* begin
      if (input_valid == 1) begin
         input_data = invals[j];
         new_matrix = newInputMatrixVals[j];
      end
      else begin
         input_data = 'x;
         new_matrix = 'x;
      end
   end

   // If our random bit rb is set to 1, and if j is within the range of our test vector (invals),
   // we will set input_valid to 1.
   always @* begin
      if ((j>=0) && (j<80) && (rb==1'b1)) begin
         input_valid=1;
      end
      else
         input_valid=0;
   end

   // If we set input_valid and input_ready on this clock edge, we will increment j just after
   // this clock edge.
   always @(posedge clk) begin
      if (input_valid && input_ready)
         j <= #1 j+1;
   end

   ////////////////////////////////////////////////////////////////////////////////////////
   // code to receive the output values

   // we will use another random bit (rb2) to determine if we can assert output_ready.
   logic [31:0] i;
   always @* begin
      if ((i>=0) && (i<16) && (rb2==1'b1))
         output_ready = 1;
      else
         output_ready = 0;
   end

   integer errors=0;

   always @(posedge clk) begin
      if (output_ready && output_valid) begin
         if (output_data !== expectedOut[i])
            $display("ERROR:   y[%d] = %d     expected output = %d" , i, output_data, expectedOut[i]);
         else
            $display("SUCCESS: y[%d] = %d", i, output_data);

         i=i+1;
      end
   end

   ////////////////////////////////////////////////////////////////////////////////

   initial begin
      j=0; i=0;

      // Before first clock edge, initialize
      output_ready = 0;
      reset = 0;

      // reset
      @(posedge clk); #1; reset = 1;
      @(posedge clk); #1; reset = 0;

      wait(i==16);

      // Now we're done!

      // Just as a test: wait another 100 cycles and make sure the DUT doesn't assert output_valid again.
      // It shouldn't, because the system finished the inputs it was given, so it should be providing
      // for new output data.
      repeat(100) begin
         @(posedge clk);
         if (output_valid == 1)
             $display("ERROR: DUT asserted output_valid incorrectly");
      end

      $finish;
    end

   // This is just here to keep the testbench from running forever in case of error.
   // In other words, if your system never produces three outputs, this code will stop
   // the simulation after 1000 clock cycles.
   initial begin
      repeat(10000) begin
         @(posedge clk);
      end
      $display("Warning: Output not produced within 10000 clock cycles; stopping simulation so it doesn't run forever");
      $stop;
   end

endmodule

*/

// Peter Milder, 10/2/22
// Testbench for ESE 507 Project 2 Part 3

// This testbench does the following:
//    - opens file random_in.hex (input values to simulate) and stores its values in an array
//    - opens expected_out.hex (expected output values) and stores its values in an array
//    - on each clock cycle, randomly picks values for valid and ready control signals
//    - uses these random control values to decide when to give new input and receive output
//    - automatically checks expected output is correct

module tbench3();

   parameter numInputs = 50000;
   parameter K = 8;

   logic               clk, reset, input_valid, input_ready, output_valid, output_ready, new_matrix;

   logic signed [13:0] input_data;
   logic signed [27:0] output_data;
   logic [1:0] state, nextState;
   logic signed [13:0] wr [63:0];
   logic signed [27:0] out [7:0];
   logic signed [13:0] xr [7:0];
   logic signed [27:0] product [7:0];
   logic signed [27:0] sum [7:0]; 
   logic signed [27:0]  preout [7:0];
   
   logic [2:0] counttail, counttailplus;

   initial clk=0;
   always #5 clk = ~clk;


   matvec8_part4 dut (counttail, counttailplus, wr[0], xr, product, out, sum, preout, state, nextState, clk, reset, input_valid, input_ready, input_data, new_matrix, output_valid, output_ready, output_data);


   //////////////////////////////////////////////////////////////////////////////////////////////////
   // code to feed some test inputs

   // rb and rb2 represent random bits. Each clock cycle, we will randomize the value of these bits.
   // When rb is 0, we will not let our testbench send new data to the DUT.
   // When rb is 1, we can send data.
   logic rb, rb2;
   always begin
      @(posedge clk);
      #1;
      std::randomize(rb, rb2); // randomize rb
   end


   // Put our test data into this array. These are the values we will feed as input into the system.

   logic signed [13:0] invals[(K*K+K)*numInputs-1:0]; //hold input data
   initial $readmemh("random_in.hex", invals);     //get and store input data from file inputData

   // Store the expected values in another array
   logic signed [27:0] expectedOut[K*numInputs-1:0];
   initial $readmemh("expected_out.hex", expectedOut);

   logic newInputMatrixVals[(K*K+K)*numInputs-1:0];
   initial $readmemh("random_in_newmatrix.hex", newInputMatrixVals);




   logic [31:0] j;

   // If input_valid is set to 1, we will put data on input_data.
   // If input_valid is 0, we will put an X on the input_data to test that your system does not
   // process the invalid input.
   always @* begin
      if (input_valid == 1) begin
         input_data = invals[j];
         new_matrix = newInputMatrixVals[j];
      end
      else begin
         input_data = 'x;
         new_matrix = 'x;
      end
   end

   // If our random bit rb is set to 1, and if j is within the range of our test vector (invals),
   // we will set input_valid to 1.
   always @* begin
      if ((j>=0) && (j<(K*K+K)*numInputs) && (rb==1'b1) && (reset==1'b0)) begin
         input_valid=1;
      end
      else
         input_valid=0;
   end

   // If we set input_valid and input_ready on this clock edge, we will increment j just after
   // this clock edge.
   always @(posedge clk) begin
      if (input_valid && input_ready)
         j <= #1 j+1;
   end
   ////////////////////////////////////////////////////////////////////////////////////////
   // code to receive the output values

   // we will use another random bit (rb2) to determine if we can assert output_ready.
   logic [31:0] i;
   always @* begin
      if ((i>=0) && (i<K*numInputs) && (rb2==1'b1))
         output_ready = 1;
      else
         output_ready = 0;
   end

   integer errors = 0;

   always @(posedge clk) begin
      if (output_ready && output_valid) begin
         if (output_data !== expectedOut[i]) begin
            $display("ERROR: y[%d] = %d     expected output = %d" , i, output_data, expectedOut[i]);
            errors = errors+1;
         end
         i=i+1;
      end
   end

   ////////////////////////////////////////////////////////////////////////////////

   initial begin
      j=0; i=0;

      // Before first clock edge, initialize
      output_ready = 0;
      reset = 0;

      // reset
      @(posedge clk); #1; reset = 1;
      @(posedge clk); #1; reset = 0;

      wait(i==K*numInputs);

      $display("\n------------- simulation finished ------------------");
      $display("Simulated ", numInputs, " matrix-vector products");
      if (errors > 0)
         $display("Detected ", errors, " errors");
      else
         $display("No errors detected");
      $display("----------------------------------------------------\n");

      $finish;
   end


   // This is just here to keep the testbench from running forever in case of error.
   // In other words, if your system never produces three outputs, this code will stop
   // the simulation eventually
   initial begin
      repeat(numInputs*2000) begin
         @(posedge clk);
      end
      $display("Warning: Output not produced within %d clock cycles; stopping simulation so it doesn't run forever", numInputs*2000);
      $display("So far, have received %d out of %d output values", i, K*numInputs);
      $stop;
   end

endmodule






