VFILES=$(wildcard *.v)

cpu : $(VFILES) Makefile
	iverilog -o cpu $(VFILES)

run : cpu
	timeout 10 ./cpu

clean :
	rm -rf cpu

