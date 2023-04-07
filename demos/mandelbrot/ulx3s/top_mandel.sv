// Project F: Mandelbrot Set (ULX3S)
// (C)2023 Will Green, Tristan Itschner, open source hardware released under the MIT License
// Learn more at https://projectf.io/posts/mandelbrot-set-verilog/

`default_nettype none
`timescale 1ns / 1ps

module top_mandel (
    input  wire logic clk_25mhz,    // 25 MHz clock
	input  wire logic [6:0] btn,
    output      logic [2:0] led,    // four green LEDs
	output wire logic [3:0] gpdi_dp
    );

    wire logic btn_fire  = btn[1];  // fire  button
    wire logic btn_up    = btn[3];  // up    button
    wire logic btn_dn    = btn[4];  // down  button
    wire logic btn_left  = btn[5];  // left  button
    wire logic btn_right = btn[6];  // right button

    // Mandelbrot parameters
    localparam FP_WIDTH =   25;  // total width of fixed-point number: integer + fractional bits
    localparam FP_INT =      4;  // integer bits in fixed-point number
    localparam ITER_MAX =  255;  // maximum iterations: minimum of 128, but (2^n-1 recommneded)
    localparam SUPERSAMPLE = 1;  // combine multiple samples for each pixel
    localparam COLR_SCHEME = 1;  // 0: blue-purple-gold, 1: blue-green

    // starting coordinates (width must match FP_WIDTH)
    localparam X_START = 25'b1100_1000_0000_0000_0000_0000_0;  // starting left: -3.5
    localparam Y_START = 25'b1110_1000_0000_0000_0000_0000_0;  // starting top:  -1.5i
    localparam STEP    = 25'b0000_0000_0100_0000_0000_0000_0;  // starting step: 1/64 (320x180)

	// generate pixel clock
	logic clk_tmds_half;
	logic clk_pix;
	logic clk_pix_locked;
	clock_480p clock_pix_inst (
	   .clk_25m(clk_25mhz),
	   .clk_tmds_half,
	   .clk_pix,
	   .LOCK(clk_pix_locked)
	);
	logic clk_sys = clk_pix; // use pix clock, to be somewhat faster
	logic rst_sys;
    always_ff @(posedge clk_sys) rst_sys <= !clk_pix_locked;  // wait for clock lock

	logic rst_pix = rst_sys;

    // display sync signals and coordinates
    localparam CORDW = 16;  // signed coordinate width (bits)
    logic signed [CORDW-1:0] sx, sy;
    logic hsync, vsync;
    logic de, frame, line;
    display_480p #(.CORDW(CORDW)) display_inst (
        .clk_pix,
        .rst_pix,
        .sx,
        .sy,
        .hsync,
        .vsync,
        .de,
        .frame,
        .line
    );

    // debounce buttons
    logic sig_mode, sig_up, sig_dn, sig_left, sig_right;
    /* verilator lint_off PINCONNECTEMPTY */
    debounce deb_fire  (.clk(clk_sys), .in(btn_fire),  .out(), .ondn(), .onup(sig_mode));
    debounce deb_up    (.clk(clk_sys), .in(btn_up),    .out(), .ondn(), .onup(sig_up));
    debounce deb_dn    (.clk(clk_sys), .in(btn_dn),    .out(), .ondn(), .onup(sig_dn));
    debounce deb_left  (.clk(clk_sys), .in(btn_left),  .out(), .ondn(), .onup(sig_left));
    debounce deb_right (.clk(clk_sys), .in(btn_right), .out(), .ondn(), .onup(sig_right));
    /* verilator lint_on PINCONNECTEMPTY */

    // colour parameters
    localparam CHANW = 8;  // colour channel width (bits)
    localparam CIDXW = 8;  // colour index width (bits)

    // framebuffer (FB)
    localparam FB_WIDTH  = 320;  // framebuffer width in pixels
    localparam FB_HEIGHT = 180;  // framebuffer height in pixels
    localparam FB_SCALE  =   2;  // framebuffer display scale (1-63)
    localparam FB_OFFX   =   0;  // horizontal offset
    localparam FB_OFFY   =  60;  // vertical offset
    localparam FB_PIXELS = FB_WIDTH * FB_HEIGHT;  // total pixels in buffer
    localparam FB_ADDRW  = $clog2(FB_PIXELS);  // address width
    localparam FB_DATAW  = CIDXW;  // colour bits per pixel

    // pixel read and write addresses and colours
    logic [FB_ADDRW-1:0] fb_addr_write, fb_addr_read;
    logic [FB_DATAW-1:0] fb_colr_write, fb_colr_read;
    logic fb_we;  // framebuffer write enable

    // framebuffer memory
    bram_sdp #(
        .WIDTH(FB_DATAW),
        .DEPTH(FB_PIXELS),
        .INIT_F("")
    ) bram_inst (
        .clk_write(clk_sys),
        .clk_read(clk_sys),
        .we(fb_we),
        .addr_write(fb_addr_write),
        .addr_read(fb_addr_read),
        .data_in(fb_colr_write),
        .data_out(fb_colr_read)
    );

	// bypass cdc (we don't do cdc on ulx3s)
    logic frame_sys, line_sys, line0_sys;
	always_comb frame_sys = frame;
	always_comb line_sys  = line;
	always_comb line0_sys = line && sy == FB_OFFY;

    //
    // update rendering params
    //
    logic signed [FP_WIDTH-1:0] x_start, x_start_p;  // left x-coordinate
    logic signed [FP_WIDTH-1:0] y_start, y_start_p;  // top y-coordinate
    logic signed [FP_WIDTH-1:0] step, step_p;        // coordinate step

    logic start_func, render_required;  // control start of function
    logic drawing;  // actively drawing in framebuffer
    logic render_busy;  // rendering in progress
    logic changed_params;  // function params have changed

	// NOTE: we use left and right buttons and thus one state is removed vs xilinx version
    enum {MOVE, ZOOM, ITER} state;
    always_ff @(posedge clk_sys) begin
        x_start_p <= x_start;  // no change in params by default
        y_start_p <= y_start;
        step_p <= step;

        if (!changed_params && !render_busy) begin
            case (state)
                MOVE: begin
                    if (sig_left) begin
                        x_start_p <= x_start - (step <<< 4);
                        changed_params <= 1;
                        $display(">> Move left");
                    end else if (sig_right) begin
                        x_start_p <= x_start + (step <<< 4);
                        changed_params <= 1;
                        $display(">> Move right");
                    end else if (sig_up) begin
                        y_start_p <= y_start - (step <<< 4);
                        changed_params <= 1;
                        $display(">> Move up");
                    end else if (sig_dn) begin
                        y_start_p <= y_start + (step <<< 4);
                        changed_params <= 1;
                        $display(">> Move down");
                    end
                    if (sig_mode) begin
                        state <= ZOOM;
                        $display(">> Mode: vertical");
                    end
                end
                ZOOM: begin  // zoom values need adjusting if resolution is not ~320x180
                    if (sig_up) begin
                        x_start_p <= x_start - (step <<< 7);
                        y_start_p <= y_start - (step <<< 6) - (step <<< 5);
                        step_p <= 2 * step;
                        changed_params <= 1;
                        $display(">> Zoom out");
                    end else if (sig_dn) begin
                        x_start_p <= x_start + (step <<< 6);
                        y_start_p <= y_start + (step <<< 5) + (step <<< 4);
                        step_p <= step / 2;
                        changed_params <= 1;
                        $display(">> Zoom in");
                    end
                    if (sig_mode) begin
                        state <= MOVE;
                        $display(">> Mode: horizontal");
                    end
                end
            endcase
        end

        // call function start once unless position/zoom updated
        start_func <= 0;
        if (changed_params && !render_busy) begin  // register new params to improve timing
            changed_params <= 0;
            if (step_p != 0 && step_p <= STEP) begin  // don't zoom in or out too far
                render_required <= 1;
                x_start <= x_start_p;
                y_start <= y_start_p;
                step <= step_p;
            end
        end else if (render_required) begin
            start_func <= 1;
            render_required <= 0;
        end

        if (rst_sys) begin
            state      <= MOVE;
            x_start    <= X_START;
            y_start    <= Y_START;
            step       <= STEP;
            render_required <= 1;
            changed_params  <= 0;
        end
    end

    //
    // draw in framebuffer
    //

    logic clip;  // location is clipped
    logic signed [CORDW-1:0] drx, dry;  // draw coordinates

    render_mandel # (
        .CORDW       (CORDW),
        .FB_WIDTH    (FB_WIDTH),
        .FB_HEIGHT   (FB_HEIGHT),
        .CIDXW       (CIDXW),
        .FP_WIDTH    (FP_WIDTH),
        .FP_INT      (FP_INT),
        .ITER_MAX    (ITER_MAX),
        .SUPERSAMPLE (SUPERSAMPLE)
    ) render_mandel_instance  (
        .clk   (clk_sys),
        .rst   (rst_sys),
        .start (start_func),
        .x_start,
        .y_start,
        .step,
        .x     (drx),
        .y     (dry),
        .cidx  (fb_colr_write),
        .drawing,
        .busy  (render_busy),
        /* verilator lint_off PINCONNECTEMPTY */
        .done ()
        /* verilator lint_on PINCONNECTEMPTY */
    );

    // calculate pixel address in framebuffer (three-cycle latency)
    bitmap_addr #(
        .CORDW (CORDW),
        .ADDRW (FB_ADDRW)
    ) bitmap_addr_instance (
        .clk  (clk_sys),
        .bmpw (FB_WIDTH),
        .bmph (FB_HEIGHT),
        .x    (drx),
        .y    (dry),
        .offx (0),
        .offy (0),
        .addr (fb_addr_write),
        .clip
    );

    // delay write enable to match address calculation
    localparam LAT_ADDR = 3;  // latency (cycles)
    logic [LAT_ADDR-1:0] fb_we_sr;
    always_ff @(posedge clk_sys) begin
        fb_we_sr <= {drawing, fb_we_sr[LAT_ADDR-1:1]};
        if (rst_sys) fb_we_sr <= 0;
    end
    always_comb fb_we = fb_we_sr[0] && !clip;  // check for clipping

    //
    // read framebuffer for display output via linebuffer
    //

    // count lines for scaling via linebuffer
    logic [$clog2(FB_SCALE):0] cnt_lb_line;
    always_ff @(posedge clk_sys) begin
        if (line0_sys) cnt_lb_line <= 0;
        else if (line_sys) begin
            cnt_lb_line <= (cnt_lb_line == FB_SCALE-1) ? 0 : cnt_lb_line + 1;
        end
    end

    // which screen lines need linebuffer?
    logic lb_line;
    always_ff @(posedge clk_sys) begin
        if (line0_sys) lb_line <= 1;  // enable from sy==0
        if (frame_sys) lb_line <= 0;  // disable at frame start
    end

    // enable linebuffer input
    logic lb_en_in;
    logic [$clog2(FB_WIDTH)-1:0] cnt_lbx;  // horizontal pixel counter
    always_comb lb_en_in = (lb_line && cnt_lb_line == 0 && cnt_lbx < FB_WIDTH);

    // calculate framebuffer read address for linebuffer
    always_ff @(posedge clk_sys) begin
        if (line_sys) begin  // reset horizontal counter at start of line
            cnt_lbx <= 0;
        end else if (lb_en_in) begin  // increment address when LB enabled
            fb_addr_read <= fb_addr_read + 1;
            cnt_lbx <= cnt_lbx + 1;
        end
        if (frame_sys) fb_addr_read <= 0;  // reset address at frame start
    end

    // enable linebuffer output
    logic lb_en_out;
    localparam LAT_LB = 3;  // output latency compensation: lb_en_out+1, LB+1, gen-colour+1
    always_ff @(posedge clk_pix) begin
        lb_en_out <= (sy >= FB_OFFY && sy < (FB_HEIGHT * FB_SCALE) + FB_OFFY
            && sx >= FB_OFFX - LAT_LB && sx < (FB_WIDTH * FB_SCALE) + FB_OFFX - LAT_LB);
    end

    // display linebuffer
    logic [FB_DATAW-1:0] lb_colr_out;
    linebuffer_simple #(
        .DATAW(FB_DATAW),
        .LEN(FB_WIDTH)
    ) linebuffer_instance (
        .clk_sys,
        .clk_pix,
        .line,
        .line_sys,
        .en_in(lb_en_in),
        .en_out(lb_en_out),
        .scale(FB_SCALE),
        .data_in(fb_colr_read),
        .data_out(lb_colr_out)
    );

    // generate colour
    logic [FB_DATAW-1:0] mandel_r, mandel_g, mandel_b;
    always_ff @(posedge clk_pix) begin
        if (COLR_SCHEME) begin
            mandel_r <= (lb_colr_out >> 1);  // reduce red by a factor of two
            mandel_g <= lb_colr_out;
            mandel_b <= lb_colr_out;
        end else begin
            if (lb_colr_out == 0) begin  // black in the set
                mandel_r <= 8'h00;
                mandel_g <= 8'h00;
                mandel_b <= 8'h00;
            end else if (lb_colr_out <= 8'h66) begin  // blue then purple
                mandel_r <= 8'h00 + lb_colr_out;
                mandel_g <= 8'h00 + (lb_colr_out >> 1);  // divide by 2
                mandel_b <= 8'h33 + lb_colr_out;
            end else begin  // turning to gold
                mandel_r <= 8'h66 + lb_colr_out - 8'h66;
                mandel_g <= 8'h33 + lb_colr_out - 8'h66;
                mandel_b <= 8'h99 + 8'h66 - lb_colr_out;
            end
        end
    end

    // paint colour
    logic paint_area;  // high in area of screen to paint
    logic [CHANW-1:0] paint_r, paint_g, paint_b;  // colour channels
    always_comb begin
        paint_area = (sy >= FB_OFFY && sy < (FB_HEIGHT * FB_SCALE) + FB_OFFY
            && sx >= FB_OFFX && sx < FB_WIDTH * FB_SCALE + FB_OFFX);
        {paint_r, paint_g, paint_b} = paint_area ? {mandel_r, mandel_g, mandel_b} : 24'h000000;
    end

	// gpdi output
	pix2gpdi pix2gpdi_inst(
		.clk_pix, // ~25 MHz
		.clk_tmds_half, // 5* clk_pix, ~ 125 MHz, must be phase aligned
		.r ( { paint_r, 4'b0000 } ),
		.g ( { paint_g, 4'b0000 } ),
		.b ( { paint_b, 4'b0000 } ),
		.de,
		.hs (hsync),
		.vs (vsync),
		.gpdi_dp
	);

    // show status with LEDs
    always_ff @(posedge clk_sys) begin
        led[2] <= render_busy;
        led[1] <= (state == MOVE);
        led[0] <= (state == ZOOM);
    end
endmodule
