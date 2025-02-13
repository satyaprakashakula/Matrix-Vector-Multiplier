// Testbench Project 2 Part 3

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
