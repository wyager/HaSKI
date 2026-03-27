HASKI_V = ../Haskell/build/Hardware.topEntity/haski.v
LOCAL_V = clock.v cpu.v

cpu: $(LOCAL_V) $(HASKI_V) Makefile
	iverilog -o cpu $(LOCAL_V) $(HASKI_V)

$(HASKI_V):
	cd ../Haskell && clash --verilog Hardware.hs -outputdir build

run: cpu
	timeout 10 ./cpu

clean:
	rm -rf cpu
