#=============================================================
#	Makefile for the PhD dissertation of Jung-Suk Goo
#		Ver 1.2		Jan. 5, 2001	
#=============================================================
#  <<< Usage >>>>
#	make			! Compile as it is
#	-------------------------------------------------------------
#	make [options...]	! Compile with changed options
#		update		! Run "latex" only once (default)
#		full		! Run "latex" twice as well as "bibtex"
#		draft		! draft version
#		final		! final version
#		<chapter_name>	! compile one chapter
#		all		! compile all the chapters
#	-------------------------------------------------------------
#	make [options...]	! Run the designated task
#		pdf		! create PDF file
#		print		! print out to the designated printer
#		cleanall	! remove files except source files
#		backup		! backup essential files to local disk
#=============================================================

#-- Source Files for LaTeX tup -------------------------------
#... Specify the chapter to compile here or 
CHAPTER	=	intro		\
		fundament	\
		parasitic	\
		simulation	\
		channel		\
		compact		\
		lna		\
		concl


TEXSRC	=	main.tex

SRCS	=	$(TEXSRC)
TARGET	=	main.ps
SRCNAME	= 	${TARGET:.ps=}

#-- Software Environment Setup -------------------------------
LATEX	= latex
BIBTEX	= bibtex
DVIPS	= dvips -Ptype1
PDF	= distill
PRINTER	= lpr -Plager
DISK1	= /lighthouse/goojs/LATEX/THESIS
DISK2	= /barney-steels/goojs/LATEX/THESIS

#=============================================================
#-- Compile the Source Files ---------------------------------
#=============================================================
.PHONY : default update full full_revision draft final $(CHAPTER) \
		all pdf print clean cleanall

#-- Default Setting ------------------------------------------
default : update clean

update : $(SRCS)
	$(LATEX)  $(SRCNAME)			# Recompile
	$(DVIPS) -o $(TARGET) $(SRCNAME)	# Create .ps from .dvi

#-- Compile Option Setting -----------------------------------
full : full_revision clean

full_revision : $(SRCS)
	$(LATEX)  $(SRCNAME)			# Create   *.aux
	$(BIBTEX) $(SRCNAME)			# Create   $(SRCNAME).bbl
	$(LATEX)  $(SRCNAME)			# Update   *.aux
	$(LATEX)  $(SRCNAME)			# Finalize *.aux
	$(DVIPS) -o $(TARGET) $(SRCNAME)	# Create .ps from .dvi

draft :
	echo '\setboolean{draftmode}{true}' > SETUP/draftmode.tex
	$(MAKE) full

final :
	echo '\setboolean{draftmode}{false}' > SETUP/draftmode.tex
	$(MAKE) full

$(CHAPTER) :
	echo '\includeonly{BODY/$@}' > SETUP/includeonly.tex
	$(MAKE) full

all :
	echo ' ' > SETUP/includeonly.tex
	$(MAKE)

#-- Optional Tasks -------------------------------------------
pdf : $(TARGET)
	$(PDF) $(TARGET)

print : $(TARGET)
	$(PRINTER) $(TARGET)

clean :
	-rm *.blg *.dvi *.log 

cleanall : clean
	-rm *.aux *.bbl *.lof *.lot *.toc *.ps *.pdf
	-cd PREFACE; rm *.aux
	-cd BODY; rm *.aux
	-cd APPENDIX; rm *.aux

backup : cleanall
	-rm -r   ${DISK1:THESIS=THESIS/*}
	-cp -r * ${DISK1:THESIS=THESIS/.}
	-rm -r   ${DISK2:THESIS=THESIS/*}
	-cp -r * ${DISK2:THESIS=THESIS/.}
