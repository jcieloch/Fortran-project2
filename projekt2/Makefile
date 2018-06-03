TOP_DIR = $(shell pwd)
tests:
	cd $(TOP_DIR)/zad1 && make test
	cd $(TOP_DIR)/zad2/cache && make test
	cd $(TOP_DIR)/zad2/cache+dot_prod && make test
	cd $(TOP_DIR)/zad2/dot_prod && make test
zad3:
	cd $(TOP_DIR)/zad3 && make all
clean:
	cd $(TOP_DIR)/zad1 && make clean
	cd $(TOP_DIR)/zad2/cache && make clean
	cd $(TOP_DIR)/zad2/cache+dot_prod && make clean
	cd $(TOP_DIR)/zad2/dot_prod && make clean
	cd $(TOP_DIR)/zad3 && make clean
