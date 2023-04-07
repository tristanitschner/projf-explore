## Project F: Modelbrot - ULX3S Makefile
## (C) 2023 Will Green, (C) 2023 Tristan Itschner, 
## open source hardware released under the MIT License
## Learn more at https://projectf.io/posts/framebuffers/

PATH_LIB = ../../../lib

# configuration
SHELL     = /bin/sh
FPGA_PKG  = CABGA381
FPGA_TYPE = 85k
PCF       = $(PATH_LIB)/boards/ulx3s/ulx3s_v20.lpf

# included modules
ADD_SRC += ${PATH_LIB}/clock/ulx3s/clock_480p.v
ADD_SRC += ${PATH_LIB}/clock/ulx3s/clock_720p.v
ADD_SRC += ${PATH_LIB}/display/ulx3s/pix2gpdi.sv
ADD_SRC += ${PATH_LIB}/display/ulx3s/tmds_8b_10b.sv
ADD_SRC += ${PATH_LIB}/display/bitmap_addr.sv
ADD_SRC += ${PATH_LIB}/display/display_720p.sv
ADD_SRC += ${PATH_LIB}/display/display_480p.sv
ADD_SRC += ${PATH_LIB}/display/linebuffer_simple.sv
ADD_SRC += ${PATH_LIB}/display/tmds_encoder_dvi.sv
ADD_SRC += ${PATH_LIB}/essential/xc7/async_reset.sv
ADD_SRC += ${PATH_LIB}/essential/debounce.sv
ADD_SRC += ${PATH_LIB}/maths/mul.sv
ADD_SRC += ${PATH_LIB}/memory/bram_sdp.sv
ADD_SRC += ../render_mandel.sv
ADD_SRC += ../mandelbrot.sv

all: mandelbrot

mandelbrot: mandel.bit

echo:
	echo "$(ADD_SRC)"

%.json: top_%.sv $(ADD_SRC)
	yosys -ql $(basename $@)-synth.log -p 'synth_ecp5 -abc9 -nowidelut -top top_$(basename $@) -json $@' $< $(ADD_SRC)

%.config: %.json
	nextpnr-ecp5 --${FPGA_TYPE} --package ${FPGA_PKG} --json $< --lpf ${PCF} --textcfg $@ --log $(basename $@)-impl.log --timing-allow-fail -r

%.bit: %.config
	ecppack $< $(subst top_,,$@)

clean:
	rm -f *.json *.log *.bit *.config

.PHONY: all clean
