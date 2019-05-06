ANALYSIS = errDemo01 errDemo02 errDemo03 errDemo04
TACDEMO = demo01 demo02 demo03 demo04 demo05 demo06

all:
	happy -gca ParRnb.y
	alex -g LexRnb.x
	ghc --make Compiler.hs -o rnbc
	-rm -f *.log *.aux *.hi *.o *.dvi *.bak

compiler:
	ghc --make Compiler.hs -o rnbc

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

demo: $(TACDEMO)
$(TACDEMO):
	@echo
	@echo "================= DEMO ================"
	@echo
	./rnbc tests/$(@).r
	@echo
	@echo "Output written on file tests/results/"$(@)"-res.txt"
	@echo "===========================================================" > tests/results/$(@)-res.txt
	@echo "========================= TEST ============================" >> tests/results/$(@)-res.txt
	@echo "===========================================================" >> tests/results/$(@)-res.txt
	@echo >> tests/results/$(@)-res.txt
	cat tests/$(@).r >> tests/results/$(@)-res.txt
	@echo >> tests/results/$(@)-res.txt
	@echo >> tests/results/$(@)-res.txt
	@echo "===========================================================" >> tests/results/$(@)-res.txt
	@echo "========================= RESULT ==========================" >> tests/results/$(@)-res.txt
	@echo "===========================================================" >> tests/results/$(@)-res.txt
	@echo >> tests/results/$(@)-res.txt
	./rnbc --nocolor tests/$(@).r  >> tests/results/$(@)-res.txt

analysis: $(ANALYSIS)
$(ANALYSIS):
	@echo
	@echo "=============== ANALYSIS =============="
	@echo
	./rnbc tests/$(@).r


distclean: clean
	-rm -f DocRnb.* LexRnb.* ParRnb.* LayoutRnb.* SkelRnb.* PrintRnb.* TestRnb.* AbsRnb.* TestRnb ErrM.* SharedString.* ComposOp.* Rnb.dtd XMLRnb.* Makefile*

